# Plot

##### Description

The `Plot operator` produces a plot

##### Usage

Input data|.
---|---
`X axis`        | A variable representing the x axis to be displayed.
`Y axis`        | A variable representing the y axis to be displayed.
`Rows`           | One or multiple factors to stratify the data per row. Will be used as a y axis for heatmaps.
`Columns`        | One or multiple factors to stratify the data per row. Will be used as an x axis for heatmaps. 
`Colors`        | One or multiple factors used as colors.
`Error bar`     | Error bars to be added on the graph. Will be ignored for heatmaps.

Settings|.
---|---
`plot_type`        | Any of `png`, `pdf` or `svg`

Output data|.
---|---
`Table 1`        | Exported chart

##### Details

The operator maps the input data projected in Tercen to a ggplot chart.

**Bar charts.** `bar.position` sets how the bars of a cell are drawn:

* `dodge`: one bar per value, side by side.
* `stack` and `fill`: values that share an x position are stacked; `fill` scales each stack to 1.
  When no two values of a cell share an x position (no x axis, or the colour factor on x), the
  whole cell is drawn as one stacked bar, first position at the bottom.

The order of the bars, and of the layers of a stacked cell, is the x axis order. Without an x
axis it is the crosstab order, by magnitude.

**Output file name.** The image is named `<workflow name>_<step name>_plot.png`. Spaces, slashes
and other characters that do not belong in a file name become `_`. Set `file.name.prefix` to use
your own name instead. With several images (`split_cells`, or a page factor) each image is
`<prefix>_<row>_<column>.png` and all of them are also in `<prefix>.zip`.

##### Unit tests

`unit_tests/run.R` checks the bar placement (order, stacking, fill) by building the bar layer and
reading back what ggplot computes. It runs in the operator image, on every pull request
(`.github/workflows/unit-tests.yml`) and locally:

    docker build -t plot_operator:test .
    docker run --rm -v "$PWD/unit_tests:/operator/unit_tests:ro" \
      --entrypoint R plot_operator:test --no-save --slave -f unit_tests/run.R
