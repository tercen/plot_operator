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

## Test GithubAction locally

NOTE: Must disable start-tercen in the workflow file
```bash
act workflow_dispatch -W .github/workflows/estimate-memory.yml \
--input service_uri='http://127.0.0.1:5400' \
--input repo_url='https://github.com/tercen/plot_operator' \
--input repo_branch='master' \
--input team_name='test' \
--secret TERCEN_USER=test \
--secret TERCEN_PASSW=test \
--secret GITHUB_TOKEN=$GITHUB_TOKEN
```