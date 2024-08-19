pub fn put_back<I>(iter: I) -> PutBack<I::IntoIter>
where
    I: IntoIterator,
{
    PutBack::new(iter.into_iter())
}

#[derive(Debug, Clone)]
pub struct PutBack<I>
where
    I: Iterator,
{
    inner: I,
    stack: Vec<I::Item>,
}

impl<I> PutBack<I>
where
    I: Iterator,
{
    pub fn new(inner: I) -> PutBack<I> {
        PutBack {
            inner,
            stack: Vec::new(),
        }
    }

    pub fn put_back(&mut self, item: I::Item) {
        self.stack.push(item);
    }
}

impl<I> Iterator for PutBack<I>
where
    I: Iterator,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if self.stack.is_empty() {
            self.inner.next()
        } else {
            self.stack.pop()
        }
    }
}
