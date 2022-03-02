use tui::layout::Rect;

#[inline]
pub fn rect_zero_height(area: Rect) -> Rect {
    Rect::new(area.x, area.y, area.width, 0)
}

#[inline]
pub fn rect_zero_width(area: Rect) -> Rect {
    Rect::new(area.x, area.y, 0, area.height)
}

#[inline]
pub fn rect_down(area: Rect, height: u16) -> Rect {
    Rect::new(area.x, area.y + area.height, area.width, height)
}

#[inline]
pub fn rect_right(area: Rect, width: u16) -> Rect {
    Rect::new(area.x + area.width, area.y, width, area.height)
}
