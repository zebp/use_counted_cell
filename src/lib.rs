use std::{
    cell::UnsafeCell,
    error::Error,
    fmt::{Debug, Display},
    ops::{Deref, DerefMut},
    sync::atomic::{AtomicUsize, Ordering},
};

type InternalBorrowFlag = usize;

const NOT_BORROWED: InternalBorrowFlag = 0;
const EXCLUSIVELY_BORROWED: InternalBorrowFlag = 1;
const SHARED_BORROWED: InternalBorrowFlag = 2;

#[derive(Debug)]
pub enum UseCountedCellBorrowError {
    AlreadyBorrowed,
}

impl Display for UseCountedCellBorrowError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "AlreadyBorrowed")
    }
}

impl Error for UseCountedCellBorrowError {}

#[derive(Debug, Clone, PartialEq)]
pub enum BorrowFlag {
    NotBorrowed,
    BorrowedShared,
    BorrowedMut,
}

/// Similar to a [RefCell](https://doc.rust-lang.org/std/cell/struct.RefCell.html) but this will revert the value to the default state of `T`
/// after it has been borrowed immutably or mutably `max_uses` times.
///
/// The cell is dynamically borrow checked so there is some runtime overhead.
pub struct UseCountedRefCell<T: Default> {
    inner: UnsafeCell<T>,
    max_uses: usize,
    uses: AtomicUsize,
    borrow_flags: AtomicUsize,
    shared_references: AtomicUsize,
}

impl<T: Default> UseCountedRefCell<T> {
    /// Creates a new `UseCountedRefCell` that will reset after is it borrowed `max_uses` times.
    /// The initial value of the cell is the value provided by `T::default()`.
    ///
    /// # Examples
    /// ```rust
    /// use use_counted_cell::UseCountedRefCell;
    ///
    /// let cell: UseCountedRefCell<bool> = UseCountedRefCell::new(10);
    /// ```
    pub fn new(max_uses: usize) -> Self {
        Self {
            inner: UnsafeCell::new(T::default()),
            max_uses,
            uses: AtomicUsize::new(0),
            borrow_flags: AtomicUsize::new(NOT_BORROWED),
            shared_references: AtomicUsize::new(0),
        }
    }

    /// Tries to get a shared reference from the cell.
    ///
    /// # Error
    /// Will return an error if the cell has been mutably borrowed.
    /// 
    /// # Examples
    /// ```rust
    /// use use_counted_cell::UseCountedRefCell;
    ///
    /// let cell: UseCountedRefCell<bool> = UseCountedRefCell::new(10);
    /// let value = *cell.try_borrow().unwrap();
    ///
    /// // bool's default is false
    /// assert_eq!(value, false);
    /// ```
    ///
    /// An example of an error:
    ///  ```rust
    /// use use_counted_cell::UseCountedRefCell;
    ///
    /// let cell: UseCountedRefCell<bool> = UseCountedRefCell::new(10);
    /// let value_mut = cell.try_borrow_mut().unwrap();
    /// let borrow_result = cell.try_borrow();
    /// 
    /// assert!(borrow_result.is_err());
    /// ```
    pub fn try_borrow(&self) -> Result<UseCountedRefMut<'_, T>, UseCountedCellBorrowError> {
        let borrow_flag = self.borrow_flags.load(Ordering::SeqCst);

        // The only time we can't take a shared reference to the data is when it's already exclusively borrowed.
        if borrow_flag == EXCLUSIVELY_BORROWED {
            return Err(UseCountedCellBorrowError::AlreadyBorrowed);
        }

        let value = unsafe { &mut *self.inner.get() };
        self.borrow_flags.store(SHARED_BORROWED, Ordering::SeqCst);
        self.shared_references.fetch_add(1, Ordering::SeqCst);

        if self.uses.fetch_add(1, Ordering::SeqCst) >= self.max_uses {
            let new_value = T::default();
            core::mem::replace(value, new_value);
            self.uses.store(0, Ordering::SeqCst);
        }

        Ok(UseCountedRefMut { value, cell: self })
    }

    /// Tries to get a mutable (exclusive) reference from the cell.
    ///
    /// # Error
    /// Will return an error if the cell has been borrowed.
    /// 
    /// # Examples
    /// ```rust
    /// use use_counted_cell::UseCountedRefCell;
    ///
    /// let cell: UseCountedRefCell<bool> = UseCountedRefCell::new(10);
    /// let mut value_ref = cell.try_borrow_mut().unwrap();
    /// *value_ref = true;
    /// assert_eq!(*value_ref, true);
    /// ```
    ///
    /// An example of an error:
    ///  ```rust
    /// use use_counted_cell::UseCountedRefCell;
    ///
    /// let cell: UseCountedRefCell<bool> = UseCountedRefCell::new(10);
    /// let value = cell.try_borrow().unwrap();
    /// let borrow_mut_result = cell.try_borrow_mut();
    /// 
    /// assert!(borrow_mut_result.is_err());
    /// ```
    pub fn try_borrow_mut(&self) -> Result<UseCountedRefMut<'_, T>, UseCountedCellBorrowError> {
        let borrow_flag = self.borrow_flags.load(Ordering::SeqCst);

        // We can only take a mutable reference when we haeve exclusive access to the value.
        if borrow_flag != NOT_BORROWED {
            return Err(UseCountedCellBorrowError::AlreadyBorrowed);
        }

        let value = unsafe { &mut *self.inner.get() };
        self.borrow_flags
            .store(EXCLUSIVELY_BORROWED, Ordering::SeqCst);

        if self.uses.fetch_add(1, Ordering::SeqCst) >= self.max_uses {
            let new_value = T::default();
            core::mem::replace(value, new_value);
            self.uses.store(0, Ordering::SeqCst);
        }

        Ok(UseCountedRefMut { value, cell: self })
    }

    /// Gets the state of the cell's borrow status, this is used to dynamically borrow check at runtime.
    /// This can also be used to check if a call to [try_borrow](struct.UseCountedRefCell.html#method.try_borrow) or [try_borrow_mut](struct.UseCountedRefCell.html#method.try_borrow_mut) will fail.
    pub fn borrow_flag(&self) -> BorrowFlag {
        match self.borrow_flags.load(Ordering::SeqCst) {
            NOT_BORROWED => BorrowFlag::NotBorrowed,
            SHARED_BORROWED => BorrowFlag::BorrowedShared,
            EXCLUSIVELY_BORROWED => BorrowFlag::BorrowedMut,
            _ => unreachable!(),
        }
    }
}

impl<T: Default + Debug> Debug for UseCountedRefCell<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let max_uses = self.max_uses;
        let uses = self.uses.load(Ordering::SeqCst);

        let mut debug_struct_formatter = f.debug_struct("UseCountedRefCell");
        let formatter = match self.borrow_flag() {
            BorrowFlag::NotBorrowed | BorrowFlag::BorrowedShared => {
                let value = self.try_borrow().unwrap();
                debug_struct_formatter.field("value", &value)
            }
            BorrowFlag::BorrowedMut => debug_struct_formatter.field("value", &"<mutable borrow>"),
        };

        formatter
            .field("max_uses", &max_uses)
            .field("uses", &uses)
            .finish()
    }
}

unsafe impl<T: Default> Sync for UseCountedRefCell<T> {}

pub struct UseCountedRefMut<'b, T: Default> {
    value: &'b mut T,
    cell: &'b UseCountedRefCell<T>,
}

impl<'b, T: Default> Drop for UseCountedRefMut<'b, T> {
    fn drop(&mut self) {
        self.cell.borrow_flags.store(NOT_BORROWED, Ordering::SeqCst);
    }
}

impl<'b, T: Default> Deref for UseCountedRefMut<'b, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.value
    }
}

impl<'b, T: Default> DerefMut for UseCountedRefMut<'b, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.value
    }
}

impl<'b, T: Default + Debug> Debug for UseCountedRefMut<'b, T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        Debug::fmt(&**self, f)
    }
}

pub struct UseCountedRef<'b, T: Default> {
    value: &'b T,
    cell: &'b UseCountedRefCell<T>,
}

impl<'b, T: Default> Drop for UseCountedRef<'b, T> {
    fn drop(&mut self) {
        let prev_shared_refs = self.cell.shared_references.fetch_sub(1, Ordering::SeqCst);

        // If we were previously the only shared reference there are no longer any references to the value,
        // so we can reset the borrow flag.
        if prev_shared_refs == 1 {
            self.cell.borrow_flags.store(NOT_BORROWED, Ordering::SeqCst);
        }
    }
}

impl<'b, T: Default> Deref for UseCountedRef<'b, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.value
    }
}

impl<'b, T: Default + Debug> Debug for UseCountedRef<'b, T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        Debug::fmt(&**self, f)
    }
}

// TODO: Unit tests
#[cfg(test)]
mod tests {
    use std::prelude::v1::*;
}
