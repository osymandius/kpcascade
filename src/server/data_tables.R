wide6 <- function(){
  htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Cascade State'),
        th(rowspan = 2, 'KP'),
        th(rowspan = 2, 'Year'),
        th(rowspan = 2, 'Province'),
        th(rowspan = 2, 'District'),
        th(colspan = 3, '90-90-90'),
        th(colspan = 3, '90-81-73')
      ),
      tr(
        lapply(rep(c('Point Estimate', 'Lower 95% Bound', "Upper 95% Bound"), 2), th)
      )
    )
  ))
}
  
wide3_90 <- function(){
  htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Cascade State'),
        th(rowspan = 2, 'KP'),
        th(rowspan = 2, 'Year'),
        th(rowspan = 2, 'Province'),
        th(rowspan = 2, 'District'),
        th(colspan = 3, '90-90-90')
      ),
      tr(
        lapply(rep(c('Point Estimate', 'Lower 95% Bound', "Upper 95% Bound"), 1), th)
      )
    )
  ))
}
  
wide3_nohead <- function(){
  htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 1, 'Cascade State'),
        th(rowspan = 1, 'KP'),
        th(rowspan = 1, 'Year'),
        th(rowspan = 1, 'Province'),
        th(rowspan = 1, 'District'),
        th(rowspan = 1, 'Point Estimate'),
        th(rowspan = 1, 'Lower 95% Bound'),
        th(rowspan = 1, "Upper 95% Bound")
      )
    )
  ))
}

wide3_73 <- function(){
  htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Cascade State'),
        th(rowspan = 2, 'KP'),
        th(rowspan = 2, 'Year'),
        th(rowspan = 2, 'Province'),
        th(rowspan = 2, 'District'),
        th(colspan = 3, '90-81-73')
      ),
      tr(
        lapply(rep(c('Point Estimate', 'Lower 95% Bound', "Upper 95% Bound"), 1), th)
      )
    )
  ))
}

wide3_count <- function(){
  htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Cascade State'),
        th(rowspan = 2, 'KP'),
        th(rowspan = 2, 'Year'),
        th(rowspan = 2, 'Province'),
        th(rowspan = 2, 'District'),
        th(colspan = 3, 'Size Estimate')
      ),
      tr(
        lapply(rep(c('Point Estimate', 'Lower 95% Bound', "Upper 95% Bound"), 1), th)
      )
    )
  ))
}