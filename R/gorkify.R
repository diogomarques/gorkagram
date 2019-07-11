# Aspect ratio
aspect_ratio = function() {
  1 / 3
}

# Helper to fix aspect ration to 3:1. Returns old plot area values, for
# resetting.
fix_plot_area = function() {
  previous_pin = graphics::par("pin")
  graphics::par(pin = c(previous_pin[1],
                        previous_pin[1] * aspect_ratio()))
  previous_pin
}

# Helper to remove left and righ margins.
remove_plot_margins = function() {
  previous_mar = graphics::par("mar")
  graphics::par("mar" = c(previous_mar[1], 0,
                          previous_mar[3], 0))
  previous_mar
}

# Helper to reset par options after plotting
reset_plot_dimensions = function(previous_plot_region,
                                 previous_margins) {
  graphics::par(pin = previous_plot_region)
  graphics::par(mar = previous_margins)
}

# Helper to start 1x1 empty plot
start_plot = function() {
  graphics::plot(0, xlim = 0:1, ylim = 0:1,
                 type = "n", bty = "n",
                 xaxt = "n", yaxt = "n",
                 ann = FALSE)
}

# Helper to add axis arrow
add_axis = function(axis_lwd, axis_arrow_size, axis_labels_h_offset) {
  xstart = 0
  xend = 1 - axis_labels_h_offset - axis_arrow_size
  y = 0.5
  graphics::segments(xstart, y, xend, y,
                     lwd = axis_lwd, lend = 2)
  axis_arrow_size
  graphics::polygon(x = c(xend, xend, xend + axis_arrow_size),
                    y = c(y + axis_arrow_size / 2 / aspect_ratio(),
                          y - axis_arrow_size / 2 / aspect_ratio(), y),
                    col = 1)
}

# Helper to add axis labels
add_axis_labels <- function(from, to, family, col_from, col_to,
                            axis_labels_cex, axis_labels_font,
                            axis_labels_h_offset, axis_labels_v_offset) {
  y = 0.5 + axis_labels_v_offset
  # right-aligned with adj = 0
  graphics::text(x = 0 + axis_labels_h_offset, y = y, labels = from,
                 adj = 0, family = family, font = axis_labels_font,
                 col = col_from, cex = axis_labels_cex)
  # left-aligned with adj = 1
  graphics::text(x = 1, y = y, labels = to,
                 adj = 1, family = family, font = axis_labels_font,
                 col = col_to, cex = axis_labels_cex)
}

# Helper to add "what" label
add_what_label <- function(where, what, family, what_label_font, col_what,
                           what_label_v_offset, what_label_cex) {
  where_y = 0.5 - what_label_v_offset
  graphics::text(x = where, y = where_y, labels = what,
                 family = family, font = what_label_font, col = col_what,
                 cex = what_label_cex, pos = 3, offset = 0)
}

# Helper to add ellipse around label
add_what_ellipse <- function(where, what, what_label_v_offset,
                             what_label_cex, what_ellipse_lwd) {
  major_axis = graphics::strwidth(what, "user", cex = what_label_cex)
  minor_axis = graphics::strheight(what, "user", cex = what_label_cex) * 2.75
  where_y = 0.5 - what_label_v_offset
  theta = seq(0, 2 * pi, by = 0.01)
  ellipse_xs = where + major_axis * cos(theta)
  ellipse_ys = where_y + minor_axis * sin(theta)
  graphics::lines(ellipse_xs, ellipse_ys, lwd = what_ellipse_lwd)
}

# Helper to add arrows coming out of ellipse and respective
# question marks.
add_what_arrows <- function(where, what_label_v_offset, what, what_label_cex,
                            what_arrow_length, what_arrow_gap_lh,
                            what_arrow_gap_rh, what_question_mark_gap,
                            what_arrow_head_size, family, what_question_cex) {
  ellipse_length = graphics::strwidth(what, "user", cex = what_label_cex) * 2
  # rh
  segment_start_left = where + ellipse_length / 2 + what_arrow_gap_rh
  segment_end_left = segment_start_left + what_arrow_length
  arrow_end_left = segment_end_left + what_arrow_head_size
  # lh
  segment_start_right = where - ellipse_length / 2 - what_arrow_gap_lh
  segment_end_right = segment_start_right - what_arrow_length
  arrow_end_right = segment_end_right - what_arrow_head_size
  # ys
  y = 0.5 - what_label_v_offset
  what_arrow_y_size = what_arrow_head_size / aspect_ratio()
  # add segments
  graphics::segments(
    x0 = c(segment_start_left, segment_start_right),
    x1 = c(segment_end_left, segment_end_right),
    y0 = y,
    y1 = y
  )
  # add arrow heads
  graphics::polygon(x = c(arrow_end_left, segment_end_left, segment_end_left),
                    y = c(y, y + what_arrow_y_size / 2, y - what_arrow_y_size / 2),
                    col = 1)
  graphics::polygon(
    x = c(arrow_end_right, segment_end_right, segment_end_right),
    y = c(y, y + what_arrow_y_size / 2, y - what_arrow_y_size / 2),
    col = 1)
  # add question marks
  xs = c(arrow_end_left + what_question_mark_gap,
         arrow_end_right - what_question_mark_gap)
  ys = rep(y, 2)
  graphics::text(
    x = xs,
    y = ys,
    labels = "?",
    family = family,
    cex = what_question_cex
  )
}
