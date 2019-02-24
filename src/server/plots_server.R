plots_server <- function(input, output) {
  
  output$cascade_percent <- renderPlot({
      cascade_percent <- switch(input$cascade,
                                "90-90-90" = proportion_data %>%
                                  filter(casState != "sizeEst" & casState!= "prev") %>%
                                  melt(id=c("casState", "KP", "year", "city")) %>%
                                  filter(variable=="point_90") %>%
                                  filter(year %in% input$year) %>%
                                  ggplot(aes(x=casState, y=value, group=year)) +
                                  geom_col(position="dodge", stat="identity", aes(fill=year)) +
                                  geom_segment(aes(x=0.5, xend=1.4, y=0.9, yend=0.9), color="red")+
                                  geom_segment(aes(x=1.6, xend=2.4, y=0.9, yend=0.9), color="red")+
                                  geom_segment(aes(x=2.6, xend=3.5, y=0.9, yend=0.9), color="red")+
                                  facet_wrap(~city),
                                "90-81-72" = proportion_data %>%
                                  filter(casState != "sizeEst" & casState!= "prev") %>%
                                  melt(id=c("casState", "KP", "year", "city")) %>%
                                  filter(variable=="point_72") %>%
                                  filter(year %in% input$year) %>%
                                  ggplot(aes(x=casState, y=value, group=year)) +
                                  geom_col(position="dodge", stat="identity", aes(fill=year)) +
                                  geom_segment(aes(x=0.5, xend=1.4, y=0.9, yend=0.9), color="red")+
                                  geom_segment(aes(x=1.6, xend=2.4, y=0.81, yend=0.81), color="red")+
                                  geom_segment(aes(x=2.6, xend=3.5, y=0.72, yend=0.72), color="red")+
                                  facet_wrap(~city)
      )
      
      cascade_percent
    })
  
  output$cascade_count <- renderPlot({
    cascade_count <- switch(input$cascade,
                            "90-90-90" = NULL,
                            "90-81-72" = switch(length(input$year),
                                                "1" = count_data %>%
                                                      melt(id=c("casState", "KP", "year", "city")) %>%
                                                      filter(variable=="point_72") %>%
                                                      filter(year %in% input$year) %>%
                                                      ggplot(aes(x=casState, y=value, group=year)) +
                                                      geom_col(position="dodge", stat="identity", aes(fill=year)) +
                                                      geom_segment(data=count_data %>% filter(!is.na(count_target) & year %in% input$year), aes(x=x, xend=xend, y=count_target, yend=count_target, color="red"))+
                                                      facet_wrap(~city, scales="free"),
                                                "2" = count_data %>%
                                                        melt(id=c("casState", "KP", "year", "city")) %>%
                                                        filter(variable=="point_72") %>%
                                                        filter(year %in% input$year) %>%
                                                        ggplot(aes(x=casState, y=value, group=year)) +
                                                        geom_col(position="dodge", stat="identity", aes(fill=year)) +
                                                        facet_wrap(~city, scales="free"),
                                                "3" = count_data %>%
                                                        melt(id=c("casState", "KP", "year", "city")) %>%
                                                        filter(variable=="point_72") %>%
                                                        filter(year %in% input$year) %>%
                                                        ggplot(aes(x=casState, y=value, group=year)) +
                                                        geom_col(position="dodge", stat="identity", aes(fill=year)) +
                                                        facet_wrap(~city, scales="free")
                                          )
                            )
    cascade_count
  })
  
  
  output$cascade_count_warning <- renderText({
    switch(input$cascade,
           "90-90-90" = "Size estimate visualisation only available with 90-81-72 cascade selection",
           "90-81-72" = NULL
           )
  })
  
  output$cascade_table_proportion <- renderDT(
    
    datatable(proportion_data %<>%
      namesToHuman() %>%
      rename("Point Estimate" = "point_90", "Lower 95% Bound" = "ll_90", "Upper 95% Bound" = "ul_90", "Point Estimate" = "point_72", "Lower 95% Bound" = "ll_72", "Upper 95% Bound" = "ul_72"),
      container = htmltools::withTags(table(
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
      )), rownames=FALSE), 
    )
  
  output$cascade_table_count <- renderDT(
    datatable(count_data %<>%
      select(-c(count_target, x, xend)) %>%
      namesToHuman() %>%
      rename("Point 90-81-72" = "point_72", "Lower 90-81-72" = "ll_72", "Upper 90-81-72" = "ul_72"),
    container = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Cascade State'),
          th(rowspan = 2, 'KP'),
          th(rowspan = 2, 'Year'),
          th(rowspan = 2, 'City/Region'),
          th(colspan = 3, '90-81-72')
        ),
        tr(
          lapply(rep(c('Point Estimate', 'Lower 95% Bound', "Upper 95% Bound"), 1), th)
        )
      )
    )), rownames=FALSE)
  )
}