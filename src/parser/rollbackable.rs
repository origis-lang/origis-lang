use std::cell::UnsafeCell;

#[derive(Clone, Debug)]
pub struct Rollbackable<I>
where
    I: Iterator,
{
    iter: I,
    buf: Vec<I::Item>,
    index: usize,
}

pub fn rollbackable<I>(iterable: I) -> Rollbackable<I::IntoIter>
where
    I: IntoIterator,
{
    Rollbackable {
        iter: iterable.into_iter(),
        buf: Vec::new(),
        index: 0,
    }
}

impl<I> Rollbackable<I>
where
    I: Iterator,
{
    pub fn reset(&mut self) {
        self.index = 0;
    }

    pub fn last(&mut self) -> Option<&I::Item> {
        self.buf.get(self.index.saturating_sub(1))
    }

    pub fn next(&mut self) -> Option<&I::Item> {
        self.peek()?;
        self.index += 1;
        self.last()
    }

    #[allow(clippy::needless_borrow)]
    pub fn peek(&mut self) -> Option<&I::Item> {
        let buf = UnsafeCell::new(&mut self.buf);
        if let Some(item) = unsafe { &mut *buf.get() }.get(self.index)
        {
            Some(item)
        } else {
            let item = self.iter.next()?;
            unsafe { &mut *buf.get() }.push(item);
            unsafe { &mut *buf.get() }.last()
        }
    }

    pub fn current(&self) -> usize {
        self.index
    }

    pub fn to(&mut self, index: usize) -> Option<()> {
        loop {
            if self.buf.get(index).is_some() {
                self.index = index;
                break Some(());
            } else if self.next().is_none() {
                break None;
            }
        }
    }

    pub fn back(&mut self) -> Option<()> {
        if self.index >= 1 {
            self.to(self.index - 1)
        } else {
            None
        }
    }
}
