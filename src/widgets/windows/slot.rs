use tui::{buffer::Buffer, layout::Rect};

use crate::{
    editing::action::{EditInfo, Jumpable, UIResult},
    editing::application::ApplicationInfo,
    editing::base::{CloseFlags, MoveDir1D, PositionList, WordStyle, WriteFlags},
    editing::store::Store,
    widgets::{TermOffset, TerminalCursor, WindowOps},
};

#[derive(Debug, Eq, PartialEq)]
pub struct WindowSlot<W> {
    current: W,
    prev: Vec<W>,
    next: Vec<W>,
}

impl<W> WindowSlot<W> {
    pub fn new(current: W) -> Self {
        WindowSlot { current, prev: vec![], next: vec![] }
    }

    pub fn open(&mut self, w: W) {
        let alt = std::mem::replace(&mut self.current, w);

        self.prev.push(alt);
    }

    pub fn alternate(&mut self) {
        if let Some(alt) = self.prev.last_mut() {
            std::mem::swap(alt, &mut self.current);
        }
    }

    pub fn offset(&mut self, dir: MoveDir1D, count: usize) {
        match dir {
            MoveDir1D::Previous => {
                for _ in 0..count {
                    if let Some(prev) = self.prev.pop() {
                        let next = std::mem::replace(&mut self.current, prev);

                        self.next.push(next);
                    }
                }
            },
            MoveDir1D::Next => {
                for _ in 0..count {
                    if let Some(next) = self.next.pop() {
                        let prev = std::mem::replace(&mut self.current, next);

                        self.prev.push(prev);
                    }
                }
            },
        }
    }

    pub fn get_off(&self, dir: MoveDir1D, count: usize) -> Option<&W> {
        if count == 0 {
            return Some(&self.current);
        }

        match dir {
            MoveDir1D::Previous => {
                let plen = self.prev.len();
                let idx = plen.saturating_sub(count);

                return self.prev.get(idx);
            },
            MoveDir1D::Next => {
                let idx = count.saturating_sub(1);

                return self.next.get(idx);
            },
        }
    }

    pub fn get_alt(&self) -> Option<&W> {
        self.prev.last()
    }

    pub fn get(&self) -> &W {
        &self.current
    }

    pub fn get_mut(&mut self) -> &mut W {
        &mut self.current
    }
}

impl<W> From<W> for WindowSlot<W> {
    fn from(w: W) -> Self {
        WindowSlot::new(w)
    }
}

impl<C, W, I> Jumpable<C, I> for WindowSlot<W>
where
    W: Jumpable<C, I>,
    I: ApplicationInfo,
{
    fn jump(
        &mut self,
        list: PositionList,
        dir: MoveDir1D,
        mut count: usize,
        ctx: &C,
    ) -> UIResult<usize, I> {
        while count > 0 {
            count = self.current.jump(list, dir, count, ctx)?;

            if count == 0 {
                break;
            }

            match dir {
                MoveDir1D::Previous => {
                    if let Some(prev) = self.prev.pop() {
                        let next = std::mem::replace(&mut self.current, prev);

                        self.next.push(next);
                    } else {
                        // No more windows.
                        return Ok(count);
                    }
                },
                MoveDir1D::Next => {
                    if let Some(next) = self.next.pop() {
                        let prev = std::mem::replace(&mut self.current, next);

                        self.prev.push(prev);
                    } else {
                        // No more windows.
                        return Ok(count);
                    }
                },
            }

            count -= 1;
        }

        return Ok(0);
    }
}

impl<W> TerminalCursor for WindowSlot<W>
where
    W: TerminalCursor,
{
    fn get_term_cursor(&self) -> Option<TermOffset> {
        self.current.get_term_cursor()
    }
}

impl<W, I> WindowOps<I> for WindowSlot<W>
where
    W: WindowOps<I>,
    I: ApplicationInfo,
{
    fn dup(&self, store: &mut Store<I>) -> Self {
        WindowSlot {
            current: self.current.dup(store),
            prev: self.prev.iter().map(|w| w.dup(store)).collect(),
            next: self.next.iter().map(|w| w.dup(store)).collect(),
        }
    }

    fn close(&mut self, flags: CloseFlags, store: &mut Store<I>) -> bool {
        if !self.current.close(flags, store) {
            return false;
        }

        while let Some(alt) = self.prev.pop() {
            self.current = alt;

            if !self.current.close(flags, store) {
                return false;
            }
        }

        while let Some(alt) = self.next.pop() {
            self.current = alt;

            if !self.current.close(flags, store) {
                return false;
            }
        }

        return true;
    }

    fn write(
        &mut self,
        path: Option<&str>,
        flags: WriteFlags,
        store: &mut Store<I>,
    ) -> UIResult<EditInfo, I> {
        self.current.write(path, flags, store)
    }

    fn draw(&mut self, area: Rect, buf: &mut Buffer, focused: bool, store: &mut Store<I>) {
        self.current.draw(area, buf, focused, store);
    }

    fn get_cursor_word(&self, style: &WordStyle) -> Option<String> {
        self.current.get_cursor_word(style)
    }

    fn get_selected_word(&self) -> Option<String> {
        self.current.get_selected_word()
    }
}
