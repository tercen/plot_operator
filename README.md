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
