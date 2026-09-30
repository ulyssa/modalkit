macro_rules! id_match_branch {
    ($self: ident, $id: ident, $path: path, $rest: expr) => {
        if let Some(id) = $id {
            if $rest.is_empty() {
                let id = format_ident!("{id}", span = $self.span);
                quote! { $path::from(#id) }
            } else {
                $self.fail(format!("no arguments were expected after `{{{}}}`", id))
            }
        } else {
            if $rest.is_empty() {
                if let Some(id) = $self.advance() {
                    quote! { $path::from(#id) }
                } else {
                    $self.fail("expected another positional argument")
                }
            } else {
                $self.fail(format!("no arguments were expected after positional argument"))
            }
        }
    };
}

macro_rules! bad_word_match_branch {
    ($self: ident, $w: ident, $msg: expr) => {
        $self.fail(format!("`{}` is not a valid {}", $w, $msg))
    };
}

macro_rules! enum_no_args_branch {
    ($self: ident, $path: path, $w: expr, $rest: ident) => {
        if $rest.is_empty() {
            quote! { $path }
        } else {
            $self.fail(format!("no arguments were expected after `{}`", $w))
        }
    };
}
