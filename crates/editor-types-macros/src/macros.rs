macro_rules! id_match_branch {
    ($self: ident, $id: ident, $path: path, $rest: expr, $span: ident) => {
        if let Some(id) = $id {
            if $rest.is_empty() {
                let id = format_ident!("{id}", span = $span);
                quote! { $path::from(#id) }
            } else {
                $self.fail(format!("no arguments were expected after `{{{}}}`", id), $span)
            }
        } else {
            if $rest.is_empty() {
                if let Some(id) = $self.advance() {
                    quote! { $path::from(#id) }
                } else {
                    $self.fail("expected another positional argument", $span)
                }
            } else {
                $self.fail(format!("no arguments were expected after positional argument"), $span)
            }
        }
    };
}

macro_rules! bad_word_match_branch {
    ($w: ident, $msg: expr, $span: ident) => {
        fail(format!("`{}` is not a valid {}", $w, $msg), $span)
    };
}

macro_rules! enum_no_args_branch {
    ($path: path, $w: expr, $rest: ident, $span: ident) => {
        if $rest.is_empty() {
            quote! { $path }
        } else {
            fail(format!("no arguments were expected after `{}`", $w), $span)
        }
    };
}
