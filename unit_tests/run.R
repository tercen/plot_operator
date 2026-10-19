# Unit tests for the bar placement in utils.R.
#
# Run inside the operator image, from /operator:
#   docker build -t plot_operator:test .
#   docker run --rm -v "$PWD/unit_tests:/operator/unit_tests:ro" \
#     --entrypoint R plot_operator:test --no-save --slave -f unit_tests/run.R
#
# Base R only (no testthat), so the production image needs nothing extra. Each
# test builds the bar layer the operator draws and reads back what ggplot
# computed (ggplot_build): positions, groups and stacked heights.
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})
source("utils.R")

failures <- character()
check <- function(desc, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) {
    message("  error: ", conditionMessage(e))
    FALSE
  })
  cat(if (ok) "PASS " else "FAIL ", desc, "\n", sep = "")
  if (!ok) failures <<- c(failures, desc)
}

# Two cells, three labels each, values that do not sum to 1, in a scrambled read
# order. `xl` is the crosstab's .xLevels: ascending value within the cell.
cells <- data.frame(
  .ri   = c(0, 0, 0, 0, 0, 0),
  .ci   = c(0, 0, 0, 1, 1, 1),
  label = c("C", "A", "B", "B", "C", "A"),
  .y    = c(5, 2, 9, 1, 4, 7),
  xl    = c(1, 0, 2, 0, 1, 2),
  .axisIndex = 0L
)

# The bars ggplot computes for the operator's bar layer, as generate_plot sets it
# up: the same global mapping and one panel per crosstab cell.
bars <- function(df, has_x_axis, bar_position, dodge_width = 0.8) {
  collapsed <- collapse_bars(df, has_x_axis, bar_position)
  if (collapsed) df <- stack_cells(df)
  p <- suppressWarnings(ggplot(mapping = aes_string(
    x = ".x", y = ".y", fill = "label", group = "label", order = "label"
  ))) +
    suppressWarnings(bar_layer(df, collapsed, bar_position, dodge_width, size = 0.5, color = "black")) +
    facet_grid(.ri ~ .ci)
  d <- suppressWarnings(layer_data(p, 1))
  d$height <- d$ymax - d$ymin
  d
}

# Labels of one panel from bottom to top, identified by their height (the test
# values are distinct within a cell). For fill, heights are shares of the total.
stack_order <- function(d, panel, df, ci, fill = FALSE) {
  cell <- df[df$.ci == ci, ]
  d <- d[d$PANEL == panel, ]
  d <- d[order(d$ymin), ]
  h <- if (fill) d$height * sum(cell$.y) else d$height
  vapply(h, function(v) cell$label[which.min(abs(cell$.y - v))], "")
}

no_x <- crosstab_x_position(cells, cells$xl)

cat("-- x position without an x axis\n")
check("uses the crosstab position (.xLevels + 1)", identical(no_x$.x, as.integer(cells$xl) + 1L))
fallback <- crosstab_x_position(cells)
check("falls back to read order per cell without .xLevels",
      identical(fallback$.x, c(1L, 2L, 3L, 1L, 2L, 3L)))

cat("-- when cells are drawn as one bar\n")
check("no x axis, stack: one bar per cell", collapse_bars(no_x, FALSE, "stack"))
check("no x axis, fill: one bar per cell", collapse_bars(no_x, FALSE, "fill"))
check("no x axis, dodge: separate bars", !collapse_bars(no_x, FALSE, "dodge"))
x_label <- mutate(cells, .x = label)
check("x = colour factor (one value per x), stack: one bar", collapse_bars(x_label, TRUE, "stack"))
check("x = colour factor, dodge: separate bars", !collapse_bars(x_label, TRUE, "dodge"))
by_day <- bind_rows(mutate(cells, .x = 1), mutate(cells, .x = 2, .y = .y + 1))
check("numeric x with several values per x, stack: per-x stacking", !collapse_bars(by_day, TRUE, "stack"))
mixed <- bind_rows(mutate(cells[1:3, ], .x = 1), mutate(cells[4:6, ], .x = c(1, 2, 3)))
check("one cell with values sharing an x keeps per-x stacking", !collapse_bars(mixed, TRUE, "stack"))

cat("-- no x axis\n")
d <- bars(no_x, FALSE, "stack")
check("stack: one bar per cell", all(table(d$PANEL) == 3) && all(tapply(d$x, d$PANEL, function(v) length(unique(v))) == 1))
check("stack: bar height is the cell total", isTRUE(all.equal(as.vector(tapply(d$ymax, d$PANEL, max)), c(16, 12))))
check("stack: smallest value at the bottom (crosstab order), cell 1",
      identical(stack_order(d, 1, no_x, 0), c("A", "C", "B")))
check("stack: smallest value at the bottom (crosstab order), cell 2",
      identical(stack_order(d, 2, no_x, 1), c("B", "C", "A")))
d <- bars(no_x, FALSE, "fill")
check("fill: every bar reaches 1", isTRUE(all.equal(as.vector(tapply(d$ymax, d$PANEL, max)), c(1, 1))))
check("fill: same order as stack", identical(stack_order(d, 1, no_x, 0, fill = TRUE), c("A", "C", "B")))
d <- bars(no_x, FALSE, "dodge")
p1 <- d[d$PANEL == 1, ]
check("dodge: one bar per value, ordered by magnitude", identical(p1$height[order(p1$x)], c(2, 5, 9)))

cat("-- x axis = the colour factor\n")
d <- bars(x_label, TRUE, "stack")
check("stack: one bar per cell", all(tapply(d$x, d$PANEL, function(v) length(unique(v))) == 1))
check("stack: in x-axis order, first at the bottom, cell 1",
      identical(stack_order(d, 1, x_label, 0), c("A", "B", "C")))
check("stack: in x-axis order, first at the bottom, cell 2",
      identical(stack_order(d, 2, x_label, 1), c("A", "B", "C")))
d <- bars(x_label, TRUE, "fill")
check("fill: every bar reaches 1, in x-axis order",
      isTRUE(all.equal(as.vector(tapply(d$ymax, d$PANEL, max)), c(1, 1))) &&
      identical(stack_order(d, 2, x_label, 1, fill = TRUE), c("A", "B", "C")))

cat("-- x axis with values sharing an x: plain ggplot behaviour\n")
plain <- function(df, position) {
  p <- suppressWarnings(ggplot(mapping = aes_string(
    x = ".x", y = ".y", fill = "label", group = "label", order = "label"
  ))) +
    suppressWarnings(geom_bar(data = df, position = position, stat = "identity", size = 0.5, width = 0.5, color = "black")) +
    facet_grid(.ri ~ .ci)
  suppressWarnings(layer_data(p, 1))
}
same_bars <- function(a, b) {
  cols <- c("PANEL", "x", "xmin", "xmax", "ymin", "ymax", "group")
  isTRUE(all.equal(a[cols], b[cols], check.attributes = FALSE))
}
check("stack: one stacked bar per x, as ggplot", same_bars(bars(by_day, TRUE, "stack"), plain(by_day, "stack")))
check("fill: as ggplot", same_bars(bars(by_day, TRUE, "fill"), plain(by_day, "fill")))
check("dodge: as ggplot", same_bars(bars(by_day, TRUE, "dodge", 0.8), plain(by_day, position_dodge(width = 0.8))))
check("dodge with x = colour factor: as ggplot",
      same_bars(bars(x_label, TRUE, "dodge", 0.8), plain(x_label, position_dodge(width = 0.8))))

cat("\n", length(failures), " failed\n", sep = "")
if (length(failures)) quit(status = 1)
