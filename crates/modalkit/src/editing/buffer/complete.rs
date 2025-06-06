use std::borrow::Cow;
use std::sync::Arc;

use crate::editing::{
    application::ApplicationInfo,
    completion::{complete_path, CompletionList},
    cursor::Adjustable,
    store::Store,
};
use crate::errors::EditResult;
use crate::prelude::*;

use super::{CursorGroupIdContext, EditBuffer};

pub trait CompletionActions<C, I>
where
    I: ApplicationInfo,
{
    fn complete_auto(
        &mut self,
        selection: &CompletionSelection,
        display: &CompletionDisplay,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I>;

    fn complete_file(
        &mut self,
        selection: &CompletionSelection,
        display: &CompletionDisplay,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I>;

    fn complete_line(
        &mut self,
        scope: &CompletionScope,
        selection: &CompletionSelection,
        display: &CompletionDisplay,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I>;

    fn complete_word(
        &mut self,
        scope: &CompletionScope,
        selection: &CompletionSelection,
        display: &CompletionDisplay,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I>;
}

impl<'a, I> CompletionActions<CursorGroupIdContext<'a>, I> for EditBuffer<I>
where
    I: ApplicationInfo,
{
    fn complete_auto(
        &mut self,
        selection: &CompletionSelection,
        display: &CompletionDisplay,
        ctx: &CursorGroupIdContext<'a>,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let gid = ctx.0;

        if !matches!(selection, CompletionSelection::None) {
            if let Some(list) = self.completions.get_mut(&gid) {
                if let Some(val) = list.select(selection) {
                    let adjs = list.replace(&mut self.text, val);
                    self._adjust_all(adjs, store);
                }

                return Ok(None);
            }
        }

        let cursor = self.get_leader(gid);
        let mut start = cursor.clone();
        let list =
            store
                .completer
                .complete(&self.text, &mut start, &self.id, &mut store.application);

        if list.is_empty() {
            let scope = CompletionScope::Global;

            return self.complete_word(&scope, selection, display, ctx, store);
        }

        let so = self.text.cursor_to_offset(&start);
        let eo = self.text.cursor_to_offset(&cursor);
        let prefix = self.text.slice(so..eo).to_string();

        let mut list = CompletionList {
            prefix,
            candidates: Arc::new(list),
            selected: None,
            display: display.clone(),
            cursor,
            start,
        };

        if let Some(val) = list.select(selection) {
            let adjs = list.replace(&mut self.text, val);
            list.cursor.adjust(&adjs);
            self._adjust_all(adjs, store);
        }

        self.completions.insert(gid, list);

        Ok(None)
    }

    fn complete_file(
        &mut self,
        selection: &CompletionSelection,
        display: &CompletionDisplay,
        ctx: &CursorGroupIdContext<'a>,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let gid = ctx.0;

        if !matches!(selection, CompletionSelection::None) {
            if let Some(list) = self.completions.get_mut(&gid) {
                if let Some(val) = list.select(selection) {
                    let adjs = list.replace(&mut self.text, val);
                    self._adjust_all(adjs, store);
                }

                return Ok(None);
            }
        }

        let cursor = self.get_leader(gid);
        let mut start = cursor.clone();

        let list = complete_path(&self.text, &mut start);

        if list.is_empty() {
            return Ok(None);
        }

        let so = self.text.cursor_to_offset(&start);
        let eo = self.text.cursor_to_offset(&cursor);
        let prefix = self.text.slice(so..eo).to_string();

        let mut list = CompletionList {
            prefix,
            candidates: Arc::new(list),
            selected: None,
            display: display.clone(),
            cursor,
            start,
        };

        if let Some(val) = list.select(selection) {
            let adjs = list.replace(&mut self.text, val);
            list.cursor.adjust(&adjs);
            self._adjust_all(adjs, store);
        }

        self.completions.insert(gid, list);

        Ok(None)
    }

    fn complete_line(
        &mut self,
        scope: &CompletionScope,
        selection: &CompletionSelection,
        display: &CompletionDisplay,
        ctx: &CursorGroupIdContext<'a>,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let gid = ctx.0;

        if !matches!(selection, CompletionSelection::None) {
            if let Some(list) = self.completions.get_mut(&gid) {
                if let Some(val) = list.select(selection) {
                    let adjs = list.replace(&mut self.text, val);
                    self._adjust_all(adjs, store);
                }

                return Ok(None);
            }
        }

        let cursor = self.get_leader(gid);
        let mut start = cursor.clone();
        start.x = 0;

        let begc = self.text.cursor_to_offset(&start);
        let endc = self.text.cursor_to_offset(&cursor);
        let prefix = self.text.slice(begc..endc);

        let source = match scope {
            CompletionScope::Buffer => &self.lines,
            CompletionScope::Global => &store.completions.lines,
        };

        let prefix = Cow::from(&prefix);
        let list = source.complete_line(prefix.as_ref());

        if list.is_empty() {
            return Ok(None);
        }

        let mut list = CompletionList {
            prefix: prefix.into_owned(),
            candidates: Arc::new(list),
            selected: None,
            display: display.clone(),
            cursor,
            start,
        };

        if let Some(val) = list.select(selection) {
            let adjs = list.replace(&mut self.text, val);
            list.cursor.adjust(&adjs);
            self._adjust_all(adjs, store);
        }

        self.completions.insert(gid, list);

        Ok(None)
    }

    fn complete_word(
        &mut self,
        scope: &CompletionScope,
        selection: &CompletionSelection,
        display: &CompletionDisplay,
        ctx: &CursorGroupIdContext<'a>,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let gid = ctx.0;

        if !matches!(selection, CompletionSelection::None) {
            if let Some(list) = self.completions.get_mut(&gid) {
                if let Some(val) = list.select(selection) {
                    let adjs = list.replace(&mut self.text, val);
                    self._adjust_all(adjs, store);
                }

                return Ok(None);
            }
        }

        let cursor = self.get_leader(gid);
        let mut start = cursor.clone();

        let prefix = match self.text.get_prefix_word_mut(&mut start, &WordStyle::Big) {
            None => return Ok(None),
            Some(p) => p,
        };

        let source = match scope {
            CompletionScope::Buffer => &self.lines,
            CompletionScope::Global => &store.completions.lines,
        };

        let prefix = Cow::from(&prefix);
        let list = source.complete_word(prefix.as_ref());

        if list.is_empty() {
            return Ok(None);
        }

        let mut list = CompletionList {
            prefix: prefix.into_owned(),
            candidates: Arc::new(list),
            selected: None,
            display: display.clone(),
            cursor,
            start,
        };

        if let Some(val) = list.select(selection) {
            let adjs = list.replace(&mut self.text, val);
            list.cursor.adjust(&adjs);
            self._adjust_all(adjs, store);
        }

        self.completions.insert(gid, list);

        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::super::tests::*;
    use super::*;

    use std::fs::File;
    use std::path::{Path, MAIN_SEPARATOR};
    use temp_dir::TempDir;

    use crate::editing::{
        application::{ApplicationInfo, ApplicationStore},
        completion::{Completer, CompletionMap},
    };

    struct TestStore {
        completions: CompletionMap<String, ()>,
    }

    impl Default for TestStore {
        fn default() -> Self {
            let mut completions = CompletionMap::default();
            completions.insert("pressed".into(), ());
            completions.insert("dressed".into(), ());

            TestStore { completions }
        }
    }

    impl ApplicationStore for TestStore {}

    #[derive(Clone, Debug, Eq, PartialEq)]
    enum TestInfo {}

    impl ApplicationInfo for TestInfo {
        type Error = String;
        type Action = ();
        type Store = TestStore;
        type WindowId = String;
        type ContentId = String;

        fn content_of_command(ct: CommandType) -> String {
            EmptyInfo::content_of_command(ct)
        }
    }

    pub struct TestCompleter;

    impl Completer<TestInfo> for TestCompleter {
        fn complete(
            &mut self,
            text: &EditRope,
            cursor: &mut Cursor,
            _: &String,
            store: &mut TestStore,
        ) -> Vec<String> {
            let word = text
                .get_prefix_word_mut(cursor, &WordStyle::Little)
                .unwrap_or_else(EditRope::empty);
            let word = Cow::from(&word);
            store.completions.complete(word.as_ref())
        }
    }

    #[test]
    fn test_complete_auto() {
        let mut ebuf = EditBuffer::new("".to_string());
        let gid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let vctx = EditContext::default();
        let mut store = Store::<TestInfo>::default();
        store.completer = Box::new(TestCompleter);
        let prev = MoveDir1D::Previous;
        let next = MoveDir1D::Next;

        // Empty text completes to "dressed" first.
        ebuf.complete_auto(
            &CompletionSelection::List(next),
            &CompletionDisplay::None,
            ctx!(gid, vwctx, vctx),
            &mut store,
        )
        .unwrap();
        assert_eq!(ebuf.get_text(), "dressed\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 7));

        // Then completes to "pressed".
        ebuf.complete_auto(
            &CompletionSelection::List(next),
            &CompletionDisplay::None,
            ctx!(gid, vwctx, vctx),
            &mut store,
        )
        .unwrap();
        assert_eq!(ebuf.get_text(), "pressed\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 7));

        // Move backwards to "dressed".
        ebuf.complete_auto(
            &CompletionSelection::List(prev),
            &CompletionDisplay::None,
            ctx!(gid, vwctx, vctx),
            &mut store,
        )
        .unwrap();
        assert_eq!(ebuf.get_text(), "dressed\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 7));

        // Move backwards to "".
        ebuf.complete_auto(
            &CompletionSelection::List(prev),
            &CompletionDisplay::None,
            ctx!(gid, vwctx, vctx),
            &mut store,
        )
        .unwrap();
        assert_eq!(ebuf.get_text(), "\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 0));

        // Clear completion list.
        ebuf.completions.remove(&gid);

        // Type "d".
        type_char!(ebuf, 'd', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 1));

        // Complete to "dressed".
        ebuf.complete_auto(
            &CompletionSelection::List(prev),
            &CompletionDisplay::None,
            ctx!(gid, vwctx, vctx),
            &mut store,
        )
        .unwrap();
        assert_eq!(ebuf.get_text(), "dressed\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 7));
    }

    #[test]
    fn test_complete_file() {
        // First, create temporary and files to complete.
        let tmp = TempDir::new().unwrap();
        let file1 = tmp.child("file1");
        let file2 = tmp.child("file2");
        let hidden = tmp.child(".hidden");

        let _ = File::create(file1.as_path()).unwrap();
        let _ = File::create(file2.as_path()).unwrap();
        let _ = File::create(hidden.as_path()).unwrap();

        let file1 = file1.as_os_str().to_string_lossy();
        let file2 = file2.as_os_str().to_string_lossy();
        let hidden = hidden.as_os_str().to_string_lossy();
        let next = MoveDir1D::Next;

        // create buffer with path to temporary directory.
        let path = tmp.path().as_os_str().to_string_lossy();
        let (mut ebuf, gid, vwctx, mut vctx, mut store) = mkfivestr(path.as_ref());
        vctx.persist.insert = Some(InsertStyle::Insert);

        // Move to end of line.
        ebuf.set_leader(gid, Cursor::new(0, 0));

        let mv = mv!(MoveType::LinePos(MovePosition::End), 0);
        edit!(ebuf, EditAction::Motion, mv, ctx!(gid, vwctx, vctx), store);

        // Type path separator.
        type_char!(ebuf, MAIN_SEPARATOR, gid, vwctx, vctx, store);

        // First, complete to file1.
        ebuf.complete_file(
            &CompletionSelection::List(next),
            &CompletionDisplay::None,
            ctx!(gid, vwctx, vctx),
            &mut store,
        )
        .unwrap();
        assert_eq!(ebuf.get_text().trim_end(), file1);

        // Then complete to file2.
        ebuf.complete_file(
            &CompletionSelection::List(next),
            &CompletionDisplay::None,
            ctx!(gid, vwctx, vctx),
            &mut store,
        )
        .unwrap();
        assert_eq!(ebuf.get_text().trim_end(), file2);

        // Then return to parent.
        ebuf.complete_file(
            &CompletionSelection::List(next),
            &CompletionDisplay::None,
            ctx!(gid, vwctx, vctx),
            &mut store,
        )
        .unwrap();
        let result = ebuf.get_text();
        let result = Path::new(result.trim_end());
        assert_eq!(result, tmp.path());

        // Clear completion list.
        ebuf.completions.remove(&gid);

        // Type "." so we can see hidden files.
        type_char!(ebuf, '.', gid, vwctx, vctx, store);

        // Complete to ".hidden".
        ebuf.complete_file(
            &CompletionSelection::List(next),
            &CompletionDisplay::None,
            ctx!(gid, vwctx, vctx),
            &mut store,
        )
        .unwrap();
        assert_eq!(ebuf.get_text().trim_end(), hidden);

        // Return to parent.
        ebuf.complete_file(
            &CompletionSelection::List(next),
            &CompletionDisplay::None,
            ctx!(gid, vwctx, vctx),
            &mut store,
        )
        .unwrap();
        let result = ebuf.get_text();
        let result = Path::new(result.trim_end());
        assert_eq!(result, tmp.path());

        // Clear completion list.
        ebuf.completions.remove(&gid);

        // Type "h" so we can complete the filename.
        type_char!(ebuf, 'h', gid, vwctx, vctx, store);

        // Complete to ".hidden".
        ebuf.complete_file(
            &CompletionSelection::List(next),
            &CompletionDisplay::None,
            ctx!(gid, vwctx, vctx),
            &mut store,
        )
        .unwrap();
        assert_eq!(ebuf.get_text().trim_end(), hidden);
    }

    #[test]
    fn test_complete_line() {
        let (mut ebuf1, gid, vwctx, vctx, mut store) = mkfivestr("foo\n1 2 3\n");
        let mut ebuf2 = mkbuf();
        ebuf2.set_text("foo bar baz\nfoo bar quux\n\n");
        ebuf2.checkpoint(ctx!(gid, vwctx, vctx), &mut store).unwrap();

        let prev = MoveDir1D::Previous;
        let next = MoveDir1D::Next;

        ebuf1.set_leader(gid, Cursor::new(0, 3));
        type_char!(ebuf1, ' ', gid, vwctx, vctx, store);
        type_char!(ebuf1, 'b', gid, vwctx, vctx, store);

        // Completion with Buffer scope does nothing.
        ebuf1
            .complete_line(
                &CompletionScope::Buffer,
                &CompletionSelection::List(next),
                &CompletionDisplay::None,
                ctx!(gid, vwctx, vctx),
                &mut store,
            )
            .unwrap();
        assert_eq!(ebuf1.get_text(), "foo b\n1 2 3\n");
        assert_eq!(ebuf1.get_leader(gid), Cursor::new(0, 5));

        // Completion with Global scope results in "foo bar baz".
        ebuf1
            .complete_line(
                &CompletionScope::Global,
                &CompletionSelection::List(next),
                &CompletionDisplay::None,
                ctx!(gid, vwctx, vctx),
                &mut store,
            )
            .unwrap();
        assert_eq!(ebuf1.get_text(), "foo bar baz\n1 2 3\n");
        assert_eq!(ebuf1.get_leader(gid), Cursor::new(0, 11));

        // Doing it again results in "foo bar quux".
        ebuf1
            .complete_line(
                &CompletionScope::Global,
                &CompletionSelection::List(next),
                &CompletionDisplay::None,
                ctx!(gid, vwctx, vctx),
                &mut store,
            )
            .unwrap();
        assert_eq!(ebuf1.get_text(), "foo bar quux\n1 2 3\n");
        assert_eq!(ebuf1.get_leader(gid), Cursor::new(0, 12));

        // Once more returns to "foo b".
        ebuf1
            .complete_line(
                &CompletionScope::Global,
                &CompletionSelection::List(next),
                &CompletionDisplay::None,
                ctx!(gid, vwctx, vctx),
                &mut store,
            )
            .unwrap();
        assert_eq!(ebuf1.get_text(), "foo b\n1 2 3\n");
        assert_eq!(ebuf1.get_leader(gid), Cursor::new(0, 5));

        // Go backwards to "foo bar quux".
        ebuf1
            .complete_line(
                &CompletionScope::Global,
                &CompletionSelection::List(prev),
                &CompletionDisplay::None,
                ctx!(gid, vwctx, vctx),
                &mut store,
            )
            .unwrap();
        assert_eq!(ebuf1.get_text(), "foo bar quux\n1 2 3\n");
        assert_eq!(ebuf1.get_leader(gid), Cursor::new(0, 12));
    }

    #[test]
    fn test_complete_word() {
        let (mut ebuf1, gid, vwctx, vctx, mut store) = mkfivestr("foo\nbar\n");
        let mut ebuf2 = mkbuf();
        ebuf2.set_text("foo baz\n");
        ebuf2.checkpoint(ctx!(gid, vwctx, vctx), &mut store).unwrap();

        let prev = MoveDir1D::Previous;
        let next = MoveDir1D::Next;

        ebuf1.set_leader(gid, Cursor::new(0, 3));
        type_char!(ebuf1, ' ', gid, vwctx, vctx, store);
        type_char!(ebuf1, 'b', gid, vwctx, vctx, store);

        // Completion with Buffer scope only completes to bar.
        ebuf1
            .complete_word(
                &CompletionScope::Buffer,
                &CompletionSelection::List(next),
                &CompletionDisplay::None,
                ctx!(gid, vwctx, vctx),
                &mut store,
            )
            .unwrap();
        assert_eq!(ebuf1.get_text(), "foo bar\nbar\n");
        assert_eq!(ebuf1.get_leader(gid), Cursor::new(0, 7));

        // Completing again returns to "b".
        ebuf1
            .complete_word(
                &CompletionScope::Buffer,
                &CompletionSelection::List(next),
                &CompletionDisplay::None,
                ctx!(gid, vwctx, vctx),
                &mut store,
            )
            .unwrap();
        assert_eq!(ebuf1.get_text(), "foo b\nbar\n");
        assert_eq!(ebuf1.get_leader(gid), Cursor::new(0, 5));

        // Clear completion list.
        ebuf1.completions.remove(&gid);

        // Completion with Global scope first yields "bar".
        ebuf1
            .complete_word(
                &CompletionScope::Global,
                &CompletionSelection::List(next),
                &CompletionDisplay::None,
                ctx!(gid, vwctx, vctx),
                &mut store,
            )
            .unwrap();
        assert_eq!(ebuf1.get_text(), "foo bar\nbar\n");
        assert_eq!(ebuf1.get_leader(gid), Cursor::new(0, 7));

        // Doing it again yields "baz".
        ebuf1
            .complete_word(
                &CompletionScope::Global,
                &CompletionSelection::List(next),
                &CompletionDisplay::None,
                ctx!(gid, vwctx, vctx),
                &mut store,
            )
            .unwrap();
        assert_eq!(ebuf1.get_text(), "foo baz\nbar\n");
        assert_eq!(ebuf1.get_leader(gid), Cursor::new(0, 7));

        // And then returns to "b".
        ebuf1
            .complete_word(
                &CompletionScope::Global,
                &CompletionSelection::List(next),
                &CompletionDisplay::None,
                ctx!(gid, vwctx, vctx),
                &mut store,
            )
            .unwrap();
        assert_eq!(ebuf1.get_text(), "foo b\nbar\n");
        assert_eq!(ebuf1.get_leader(gid), Cursor::new(0, 5));

        // Go backwards to "baz".
        ebuf1
            .complete_word(
                &CompletionScope::Global,
                &CompletionSelection::List(prev),
                &CompletionDisplay::None,
                ctx!(gid, vwctx, vctx),
                &mut store,
            )
            .unwrap();
        assert_eq!(ebuf1.get_text(), "foo baz\nbar\n");
        assert_eq!(ebuf1.get_leader(gid), Cursor::new(0, 7));
    }
}
