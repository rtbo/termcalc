
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
pub struct Cursor<T> {
    // input iterator
    input: T,
    // current position in the stream
    cur_pos: u32,
    // position of each encountered new line    
    nl_pos: Vec<u32>,
    // put_back stack
    stack: Vec<char>,
}

impl<T> Cursor<T> {
    pub fn new(input: T) -> Cursor<T> {
        Cursor {
            input,
            cur_pos: 0,
            nl_pos: Vec::new(),
            stack: Vec::new(),
        }
    }

    pub fn pos(&self) -> Pos {
        self.cur_pos
    }

    pub fn nl_pos(&self) -> &[u32] {
        &self.nl_pos
    }

    /// Put back the given character in the stream.
    /// It will be read again at the next call to `next()`.
    /// It is not checked whether `c` is effectively the last character extracted.
    pub fn put_back(&mut self, c: char) {
        self.stack.push(c);
        debug_assert!(self.cur_pos > 0);
        self.cur_pos -= 1;
        match self.nl_pos.last() {
            Some(pos) if *pos == self.cur_pos => {
                self.nl_pos.pop();
            }
            _ => (),
        }
    }
}

impl <T: Iterator<Item = char> + Clone> Cursor<T> {
    pub fn first(&self) -> Option<char> {
        self.input.clone().next()
    }
}

impl <T: Iterator<Item = char>> Iterator for Cursor<T> {
    type Item = char;
    fn next(&mut self) -> Option<char> {
        let c = if self.stack.is_empty() {
            self.input.next()
        } else {
            self.stack.pop()
        };
        if let Some(c) = c {
            if c == '\n' {
                self.nl_pos.push(self.cur_pos);
            }
            self.cur_pos += 1;
        }
        c
    }
}

#[test]
fn test_cursor() {
    let mut c = Cursor::new("some\nstring".chars());
    assert_eq!(c.pos(), 0);
    assert_eq!(c.nl_pos(), &[]);
    assert_eq!(c.next(), Some('s'));
    assert_eq!(c.next(), Some('o'));
    assert_eq!(c.next(), Some('m'));
    assert_eq!(c.next(), Some('e'));

    assert_eq!(c.pos(), 4);
    assert_eq!(c.nl_pos(), &[]);
    c.put_back('e');
    assert_eq!(c.pos(), 3);
    assert_eq!(c.nl_pos(), &[]);
    assert_eq!(c.next(), Some('e'));

    assert_eq!(c.pos(), 4);
    assert_eq!(c.nl_pos(), &[]);
    assert_eq!(c.next(), Some('\n'));

    assert_eq!(c.pos(), 5);
    assert_eq!(c.nl_pos(), &[4]);
    c.put_back('\n');

    assert_eq!(c.pos(), 4);
    assert_eq!(c.nl_pos(), &[]);
    assert_eq!(c.next(), Some('\n'));

    assert_eq!(c.pos(), 5);
    assert_eq!(c.nl_pos(), &[4]);
    assert_eq!(c.next(), Some('s'));
    assert_eq!(c.next(), Some('t'));
    assert_eq!(c.next(), Some('r'));
    assert_eq!(c.next(), Some('i'));
    assert_eq!(c.next(), Some('n'));

    assert_eq!(c.pos(), 10);
    assert_eq!(c.nl_pos(), &[4]);
    assert_eq!(c.next(), Some('g'));

    assert_eq!(c.pos(), 11);
    assert_eq!(c.nl_pos(), &[4]);
    assert_eq!(c.next(), None);
}