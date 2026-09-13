use arboard::{Get, LinuxClipboardKind, Set};
use editor_types::prelude::TargetShape;

use crate::editing::rope::EditRope;
use crate::editing::store::{RegisterCell, RegisterError};

#[cfg(target_os = "linux")]
use arboard::{GetExtLinux, SetExtLinux};
#[cfg(target_os = "linux")]
use wl_clipboard_rs::utils::is_primary_selection_supported;

#[cfg(target_os = "linux")]
/// Implement vim-compatible keyboard usage.
///
/// This preserves the selection shape when pasting between vim-like applications.
///
/// # Encoding
///
/// The mime type `application/x-vim-enc-text` starts with a single byte that defines the
/// [`TargetShape`]. After that comes the used text encoding as a null-terminated ascii string. We
/// only implement `utf-8` for now. The rest of the clipboard content is the encoded text.
///
/// # Documentation
///
/// I haven't found actual documentation and based this implementation on the vim source code:
///
/// - [mime type](https://github.com/vim/vim/blob/dfe8124814ca9e65369afa5d1e32fcaa323ac76a/src/vim.h#L2322-L2331)
/// - [target shape](https://github.com/vim/vim/blob/dfe8124814ca9e65369afa5d1e32fcaa323ac76a/src/vim.h#L1683-L1685)
/// - [clipboard copying](https://github.com/vim/vim/blob/dfe8124814ca9e65369afa5d1e32fcaa323ac76a/src/clipboard.c#L2763)
/// - [clipboard pasting](https://github.com/vim/vim/blob/dfe8124814ca9e65369afa5d1e32fcaa323ac76a/src/clipboard.c#L3745)
/// - [encoding definition](https://github.com/vim/vim/blob/dfe8124814ca9e65369afa5d1e32fcaa323ac76a/src/option.h#L504) and [initialization](https://github.com/vim/vim/blob/dfe8124814ca9e65369afa5d1e32fcaa323ac76a/src/option.c#L594)
mod wayland {
    use std::io::{Cursor, Read as _};

    use arboard::ImageData;
    use image::ImageReader;
    use wl_clipboard_rs::{copy, paste};

    use super::*;

    const MIME_VIMENC_NAME: &str = "application/x-vim-enc-text";
    const MIME_VIM_NAME: &str = "application/x-vim-text";
    const MIME_PNG_NAME: &str = "image/png";

    #[derive(Debug, thiserror::Error)]
    enum Error {
        #[error(transparent)]
        Paste(#[from] paste::Error),

        #[error(transparent)]
        Io(#[from] std::io::Error),

        #[error(transparent)]
        ArBoard(#[from] arboard::Error),
    }

    fn decode_vimenc(mut buffer: Vec<u8>) -> Option<RegisterCell> {
        if buffer.len() < 3 {
            return None;
        }

        // vim motion type
        let shape = match buffer[0] {
            0 => TargetShape::CharWise,
            1 => TargetShape::LineWise,
            2 => TargetShape::BlockWise,

            // fall back to charwise matching
            _ => TargetShape::CharWise,
        };

        let encoding_end_pos = buffer[1..].iter().position(|x| *x == 0)?;
        let text_encoding = &buffer[1..1 + encoding_end_pos];
        match text_encoding {
            b"utf-8" => (),
            _ => return None,
        }

        // remove encoding prelude
        std::mem::drop(buffer.drain(..=1 + encoding_end_pos));

        let text = String::from_utf8(buffer).ok()?;

        Some(RegisterCell::new(shape, EditRope::from(text)))
    }

    fn read_clipboard_text(clipboard: paste::ClipboardType) -> Result<RegisterCell, Error> {
        let mime_type = paste::MimeType::TextWithPriority(MIME_VIMENC_NAME);

        let (mut reader, mime_type) =
            paste::get_contents(clipboard, paste::Seat::Unspecified, mime_type)?;

        let mut buffer = vec![];
        reader.read_to_end(&mut buffer)?;

        if mime_type == MIME_VIMENC_NAME {
            return decode_vimenc(buffer).ok_or(arboard::Error::ConversionFailure.into());
        }

        let text = String::from_utf8(buffer).map_err(|_| arboard::Error::ConversionFailure)?;

        Ok(with_guessed_target_shape(text))
    }

    fn read_clipboard_image(clipboard: paste::ClipboardType) -> Option<ImageData<'static>> {
        let mime = paste::MimeType::Specific(MIME_PNG_NAME);
        let (mut pipe, _) = paste::get_contents(clipboard, paste::Seat::Unspecified, mime).ok()?;

        let mut buffer = vec![];
        pipe.read_to_end(&mut buffer).ok()?;

        let image = ImageReader::new(Cursor::new(buffer))
            .with_guessed_format()
            .ok()?
            .decode()
            .ok()?;
        let image = image.into_rgb8();

        Some(ImageData {
            width: image.width() as usize,
            height: image.height() as usize,
            bytes: image.into_raw().into(),
        })
    }

    pub fn read_clipboard(kind: LinuxClipboardKind) -> Result<RegisterCell, RegisterError> {
        let clipboard = kind.try_into().unwrap_or_default();

        if paste::get_mime_types(clipboard, paste::Seat::Unspecified)
            .is_ok_and(|mimes| mimes.contains(MIME_PNG_NAME))
        {
            if let Some(image) = read_clipboard_image(clipboard) {
                return Err(RegisterError::ClipboardImage(image));
            }
        }

        Ok(read_clipboard_text(clipboard).unwrap_or_default())
    }

    fn prepare_vim_sources(cell: &RegisterCell) -> Vec<copy::MimeSource> {
        let text = cell.value.to_string();

        let text_bytes = text.into_bytes();

        let mut vim_bytes = Vec::from([0]);
        vim_bytes.extend_from_slice(&text_bytes);

        let mut vim_enc_bytes = Vec::from(b"\0utf-8\0");
        vim_enc_bytes.extend_from_slice(&text_bytes);

        match cell.shape {
            TargetShape::CharWise => {
                // the bytes are already zeroed
            },
            TargetShape::LineWise => {
                vim_bytes[0] = 1;
                vim_enc_bytes[0] = 1;
            },
            TargetShape::BlockWise => {
                vim_bytes[0] = 2;
                vim_enc_bytes[0] = 2;
            },
        }

        vec![
            copy::MimeSource {
                source: copy::Source::Bytes(vim_enc_bytes.into()),
                mime_type: copy::MimeType::Specific(MIME_VIMENC_NAME.into()),
            },
            copy::MimeSource {
                source: copy::Source::Bytes(vim_bytes.into()),
                mime_type: copy::MimeType::Specific(MIME_VIM_NAME.into()),
            },
            copy::MimeSource {
                source: copy::Source::Bytes(text_bytes.into()),
                mime_type: copy::MimeType::Text,
            },
        ]
    }

    pub fn write_clipboard(kind: LinuxClipboardKind, cell: &RegisterCell) {
        let clipboard = kind.try_into().unwrap_or_default();

        let mut options = copy::Options::new();
        options.clipboard(clipboard);

        let _ = options.copy_multi(prepare_vim_sources(cell));
    }
}

pub fn get_clipboard(clipboard: &mut arboard::Clipboard, _kind: LinuxClipboardKind) -> Get<'_> {
    let get = clipboard.get();

    #[cfg(target_os = "linux")]
    let get = get.clipboard(_kind);

    get
}

pub fn set_clipboard(clipboard: &mut arboard::Clipboard, _kind: LinuxClipboardKind) -> Set<'_> {
    let set = clipboard.set();

    #[cfg(target_os = "linux")]
    let set = set.clipboard(_kind);

    set
}

/// Guess the target shape based on string format
///
/// If the text ends on a newline, assume it is [`LineWise`](`TargetShape::LineWise`) and else
/// [`CharWise`](`TargetShape::CharWise`).
///
/// This copies what vim does
/// [here](https://github.com/vim/vim/blob/2ec2a612c7ac9315a17d70f6a2a5ea89ddaea854/src/register.c#L3246-L3248).
fn with_guessed_target_shape(text: String) -> RegisterCell {
    let shape = match text.as_bytes().last() {
        Some(b'\n') | Some(b'\r') => TargetShape::LineWise,
        _ => TargetShape::CharWise,
    };

    RegisterCell::new(shape, EditRope::from(text))
}

pub enum Clipboard {
    Generic(arboard::Clipboard),

    #[cfg(target_os = "linux")]
    Wayland,
}

impl Clipboard {
    pub fn new() -> Result<Self, arboard::Error> {
        #[cfg(target_os = "linux")]
        if std::env::var_os("WAYLAND_DISPLAY").is_some() && is_primary_selection_supported().is_ok()
        {
            return Ok(Self::Wayland);
        }

        Ok(Self::Generic(arboard::Clipboard::new()?))
    }

    pub fn get(&mut self, kind: LinuxClipboardKind) -> Result<RegisterCell, RegisterError> {
        let reg = match self {
            Clipboard::Generic(clipboard) => {
                if let Ok(image) = get_clipboard(clipboard, kind).image() {
                    return Err(RegisterError::ClipboardImage(image));
                }

                if let Ok(text) = get_clipboard(clipboard, kind).text() {
                    with_guessed_target_shape(text)
                } else {
                    RegisterCell::default()
                }
            },

            #[cfg(target_os = "linux")]
            Clipboard::Wayland => wayland::read_clipboard(kind)?,
        };

        Ok(reg)
    }

    pub fn set(&mut self, kind: LinuxClipboardKind, cell: &RegisterCell) {
        match self {
            Clipboard::Generic(clipboard) => {
                let op = set_clipboard(clipboard, kind);
                let _ = op.text(&cell.value);
            },

            #[cfg(target_os = "linux")]
            Clipboard::Wayland => wayland::write_clipboard(kind, cell),
        }
    }
}
