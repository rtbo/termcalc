/// Byte position into an input stream.
pub type Pos = u32;

/// Byte span into an input stream
/// (first pos, one past last pos)
pub type Span = (Pos, Pos);

pub trait HasSpan {
    fn span(&self) -> Span;
}

/// A cursor over an input stream of characters.
/// It keeps track of the current position in the stream.
/// It has the ability to put characters back into the stream.
#[derive(Debug, Clone)]
pub struct Cursor<I> {
    // input iterator
    input: I,
    // current position in the stream
    cur_pos: u32,
}

impl<I> Cursor<I> {
    pub fn new(input: I) -> Cursor<I> {
        Cursor { input, cur_pos: 0 }
    }

    pub fn pos(&self) -> Pos {
        self.cur_pos
    }
}

impl<I: Iterator<Item = char> + Clone> Cursor<I> {
    pub fn first(&self) -> Option<char> {
        self.input.clone().next()
    }
}

impl<I: Iterator<Item = char>> Iterator for Cursor<I> {
    type Item = char;
    fn next(&mut self) -> Option<char> {
        let c = self.input.next();
        if c.is_some() {
            self.cur_pos += 1;
        }
        c
    }
}

#[cfg(test)]
mod tests {
    use super::Cursor;

    #[test]
    fn test_cursor() {
        let mut c = Cursor::new("some string".chars());
        assert_eq!(c.pos(), 0);
        assert_eq!(c.next(), Some('s'));
        assert_eq!(c.next(), Some('o'));
        assert_eq!(c.next(), Some('m'));
        assert_eq!(c.next(), Some('e'));
        assert_eq!(c.pos(), 4);
        assert_eq!(c.first(), Some(' '));
        assert_eq!(c.pos(), 4);
        assert_eq!(c.next(), Some(' '));
        assert_eq!(c.pos(), 5);

        let mut cl = c.clone();
        assert_eq!(cl.pos(), 5);
        assert_eq!(cl.next(), Some('s'));
        assert_eq!(cl.next(), Some('t'));
        assert_eq!(cl.pos(), 7);

        assert_eq!(c.pos(), 5);
        assert_eq!(c.next(), Some('s'));
        assert_eq!(c.next(), Some('t'));
        assert_eq!(c.next(), Some('r'));
        assert_eq!(c.next(), Some('i'));
        assert_eq!(c.next(), Some('n'));
        assert_eq!(c.pos(), 10);
        assert_eq!(c.next(), Some('g'));
        assert_eq!(c.pos(), 11);
        assert_eq!(c.next(), None);
    }
}
