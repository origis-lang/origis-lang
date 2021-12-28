use gc::{GcCell, GcCellRef, GcCellRefMut};

#[derive(Debug, PartialEq, PartialOrd, gc::Trace, gc::Finalize)]
pub struct Gc<T>(gc::Gc<GcCell<T>>)
where
    T: gc::Trace + 'static;

impl<T> Gc<T>
where
    T: gc::Trace,
{
    pub fn new(val: T) -> Self {
        Gc(gc::Gc::new(GcCell::new(val)))
    }

    pub fn borrow(&self) -> GcCellRef<'_, T> {
        self.0.borrow()
    }

    pub fn borrow_mut(&mut self) -> GcCellRefMut<'_, T> {
        self.0.borrow_mut()
    }
}

impl<T> Clone for Gc<T>
where
    T: gc::Trace,
{
    fn clone(&self) -> Self {
        Gc(gc::Gc::clone(&self.0))
    }
}
