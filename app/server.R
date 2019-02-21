server <- function(input, output) {
  
  output$cascade_plots <- renderPlot({
    cascade_plots <- switch(input$cascade,
                            "90-90-90" = data %>%
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
                            "90-81-72" = data %>%
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
    
    cascade_plots
  })
  
  output$cascade_table <- renderDataTable(data)
  
  output$template_download <- downloadHandler(
    filename = function() {
      paste("KP_cascade_template.csv", sep='')
    },
    content = function (con) {
      write.csv(example_data, con, na="")
    }
  )
  
  output$data_upload <- renderDataTable({
    req(input$data_input)
    read.csv(input$data_input$datapath, header = TRUE, sep = ",")
  })
}