plots_server <- function(input, output) {
  
  output$cascade_percent <- renderPlot({
      switch(input$cascade,
             "90-90-90" = proportion_data %>%
               filter(casState != "sizeEst" & casState!= "prev") %>%
               filter(year %in% input$year) %>%
               filter(city %in% input$subnat) %>%
               ggplot(aes(x=casState, group=year)) +
                 geom_col(aes(y=point_90, fill=year), position="dodge", stat="identity") +
                 geom_errorbar(aes(ymin=ll_90, ymax=ul_90), width=0.2, position=position_dodge(1))+
                 geom_segment(aes(x=0.5, xend=1.4, y=0.9, yend=0.9, color="red"))+
                 geom_segment(aes(x=1.6, xend=2.4, y=0.9, yend=0.9, color="red"))+
                 geom_segment(aes(x=2.6, xend=3.5, y=0.9, yend=0.9, color="red"))+
                 scale_y_continuous(limits=c(0,1), labels=percent)+
                 scale_x_discrete(labels=c("Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
                 theme(axis.text = element_text(size=rel(1.2)),
                       strip.text = element_text(size=rel(1.3)))+
                 xlab("")+
                 ylab("")+
                 guides(color=FALSE)+
                 facet_wrap(~city),
             "90-81-72" = proportion_data %>%
               filter(casState != "sizeEst" & casState!= "prev") %>%
               filter(year %in% input$year) %>%
               filter(city %in% input$subnat) %>%
               ggplot(aes(x=casState, group=year)) +
                 geom_col(aes(y=point_72, fill=year), position="dodge", stat="identity") +
                 geom_errorbar(aes(ymin=ll_72, ymax=ul_72), width=0.2, position=position_dodge(1))+
                 geom_segment(aes(x=0.5, xend=1.4, y=0.9, yend=0.9, color="red"))+
                 geom_segment(aes(x=1.6, xend=2.4, y=0.81, yend=0.81, color="red"))+
                 geom_segment(aes(x=2.6, xend=3.5, y=0.72, yend=0.72, color="red"))+
                 scale_y_continuous(limits=c(0,1), labels=percent)+
                 scale_x_discrete(labels=c("Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
                 theme(axis.text = element_text(size=rel(1.2)),
                       strip.text = element_text(size=rel(1.3)))+
                 xlab("")+
                 ylab("")+
                 guides(color=FALSE)+
                 facet_wrap(~city)
      )
    })
  
  output$cascade_count <- renderPlot({
    switch(input$cascade,
           "90-90-90" = NULL,
           "90-81-72" = switch(length(input$year),
                               "1" = count_data %>%
                                 filter(year %in% input$year) %>%
                                 filter(city %in% input$subnat) %>%
                                 ggplot(aes(x=casState, group=year)) +
                                   geom_col(aes(y=point_72, fill=year), position="dodge", stat="identity") +
                                   geom_errorbar(aes(ymin=ll_72, ymax=ul_72), width=0.2, position=position_dodge(1))+
                                   geom_segment(data=count_data %>% filter(!is.na(count_target) & year %in% input$year), aes(x=x, xend=xend, y=count_target, yend=count_target, color="red"))+
                                   scale_x_discrete(labels=c("Size\nEstimate", "KPLHIV", "Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
                                   theme(axis.text = element_text(size=rel(1.2)),
                                         strip.text = element_text(size=rel(1.3)))+
                                   xlab("")+
                                   guides(color=FALSE)+
                                   ylab("Number of people")+
                                   facet_wrap(~city, scales="free"),
                               "2" = count_data %>%
                                 filter(year %in% input$year) %>%
                                 filter(city %in% input$subnat) %>%
                                 ggplot(aes(x=casState, group=year)) +
                                   geom_col(aes(y=point_72, fill=year), position="dodge", stat="identity") +
                                   geom_errorbar(aes(ymin=ll_72, ymax=ul_72), width=0.2, position=position_dodge(1))+
                                   scale_x_discrete(labels=c("Size\nEstimate", "KPLHIV", "Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
                                   theme(axis.text = element_text(size=rel(1.2)),
                                         strip.text = element_text(size=rel(1.3)))+
                                   xlab("")+
                                   ylab("Number of people")+
                                   facet_wrap(~city, scales="free"),
                               "3" = count_data %>%
                                 filter(year %in% input$year) %>%
                                 filter(city %in% input$subnat) %>%
                                 ggplot(aes(x=casState, group=year)) +
                                   geom_col(aes(y=point_72, fill=year), position="dodge", stat="identity") +
                                   geom_errorbar(aes(ymin=ll_72, ymax=ul_72), width=0.2, position=position_dodge(1))+
                                   scale_x_discrete(labels=c("Size\nEstimate", "KPLHIV", "Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
                                   theme(axis.text = element_text(size=rel(1.2)),
                                         strip.text = element_text(size=rel(1.3)))+
                                   xlab("")+
                                   ylab("Number of people")+
                                   facet_wrap(~city, scales="free")
           )
    )
  })
  
  
  output$cascade_count_warning <- renderText({
    switch(input$cascade,
           "90-90-90" = "Size estimate visualisation only available with 90-81-72 cascade selection",
           "90-81-72" = NULL
           )
  })
  
  output$cascade_table_proportion <- renderDT(
    datatable(proportion_data %>%
      filter(casState != "sizeEst") %>%
      namesToHuman(),
    container = wide6(), rownames=FALSE, options = list(pageLength=999, dom='t')) %>%
      formatRound(columns = 5:10, digits=3) 
  )
  
  output$cascade_table_count <- renderDT(
    datatable(count_data %>%
      select(-c(count_target, x, xend)) %>%
      filter(casState != "sizeEst") %>%
      namesToHuman() %>%
      mutate(Cascade.status = ifelse(Cascade.status=="Prevalence", "KPLHIV", Cascade.status)),
    container = wide3_72(), rownames=FALSE, options = list(pageLength=999, dom='t')) %>%
      formatRound(columns=5:7, digits=0)
  )
  
  output$download_proportion_cascade <- downloadHandler(
    filename = function() {
      paste("KP_proportion_cascade.csv", sep='')
    },
    content = function (con) {
      write.csv(proportion_data %>%
                  filter(casState != "sizeEst") %>%
                  namesToHuman() %>%
                  mutate(Cascade.status = ifelse(Cascade.status=="Prevalence", "KPLHIV", Cascade.status)) %>%
                  rename("Point 90-90-90" = "point_90", "Lower 90-90-90" = "ll_90", "Upper 90-90-90" = "ul_90", "Point 90-81-72" = "point_72", "Lower 90-81-72" = "ll_72", "Upper 90-81-72" = "ul_72"), 
                con, na="")
    }
  )
  
  output$download_count_cascade <- downloadHandler(
    filename = function() {
      paste("KP_count_cascade.csv", sep='')
    },
    content = function (con) {
      write.csv(count_data %>%
                  select(-c(count_target, x, xend)) %>%
                  filter(casState != "sizeEst") %>%
                  namesToHuman() %>%
                  mutate(Cascade.status = ifelse(Cascade.status=="Prevalence", "KPLHIV", Cascade.status)) %>%
                  rename("Point 90-81-72" = "point_72", "Lower 90-81-72" = "ll_72", "Upper 90-81-72" = "ul_72"),
                con, na="")
    }
  )
}


