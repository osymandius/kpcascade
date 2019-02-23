defaultDataTableOptions <- function() {
  # list(
  #   paging = FALSE,
  #   dom = "lrt"     # https://datatables.net/reference/option/dom
  # )

  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Cascade State'),
        th(rowspan = 2, 'KP'),
        th(rowspan = 2, 'Year'),
        th(rowspan = 2, 'City/Region'),
        th(colspan = 3, '90-90-90'),
        th(colspan = 3, '90-81-72')
      ),
      tr(
        lapply(rep(c('Point Estimate', 'Lower 95% Bound', "Upper 95% Bound"), 2), th)
      )
    )
  ))
}