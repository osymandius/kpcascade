plots_server <- function(input, output) {
  
  
  observeEvent(input$subnat, { 
    if (length(input$subnat)>3) {
      output$cascade_percent <- renderPlot({
        switch(input$cascade,
               "90-90-90" = proportion_data %>%
                 filter(casState != "sizeEst" & casState!= "prev") %>%
                 filter(year %in% input$year) %>%
                 filter(city %in% input$subnat) %>%
                 arrange(year) %>%
                 ggplot(aes(x=casState, group=year)) +
                 geom_col(aes(y=point_90, fill=year), position="dodge", stat="identity") +
                 geom_errorbar(aes(ymin=ll_90, ymax=ul_90), width=0.2, position=position_dodge(1))+
                 geom_segment(aes(x=0.5, xend=1.4, y=0.9, yend=0.9), color="navy", linetype=5)+
                 geom_segment(aes(x=1.6, xend=2.4, y=0.9, yend=0.9), color="navy", linetype=5)+
                 geom_segment(aes(x=2.6, xend=3.5, y=0.9, yend=0.9), color="navy", linetype=5)+
                 geom_text(aes(y=0.95, label="90%"), color="navy", linetype=5)+
                 scale_y_continuous(limits=c(0,1), labels=percent)+
                 scale_x_discrete(labels=c("Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
                 theme(axis.text = element_text(size=rel(1.2)),
                       strip.text = element_text(size=rel(1.3)))+
                 xlab("")+
                 ylab("")+
                 guides(color=FALSE)+
                 facet_rep_wrap(~city, ncol=3, repeat.tick.labels = TRUE),
               "90-81-72" = proportion_data %>%
                 filter(casState != "sizeEst" & casState!= "prev") %>%
                 filter(year %in% input$year) %>%
                 filter(city %in% input$subnat) %>%
                 arrange(year) %>%
                 ggplot(aes(x=casState, group=year)) +
                 geom_col(aes(y=point_72, fill=year), position="dodge", stat="identity") +
                 geom_errorbar(aes(ymin=ll_72, ymax=ul_72), width=0.2, position=position_dodge(1))+
                 geom_segment(aes(x=0.5, xend=1.4, y=0.9, yend=0.9), color="navy", linetype=5)+
                 geom_segment(aes(x=1.6, xend=2.4, y=0.81, yend=0.81), color="navy", linetype=5)+
                 geom_segment(aes(x=2.6, xend=3.5, y=0.72, yend=0.72), color="navy", linetype=5)+
                 geom_text(aes(x=1, y=0.95, label="90%"), color="navy", linetype=5)+
                 geom_text(aes(x=2, y=0.86, label="81%"), color="navy", linetype=5)+
                 geom_text(aes(x=3, y=0.77, label="72%"), color="navy", linetype=5)+
                 scale_y_continuous(limits=c(0,1), labels=percent)+
                 scale_x_discrete(labels=c("Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
                 theme(axis.text = element_text(size=rel(1.2)),
                       strip.text = element_text(size=rel(1.3)))+
                 xlab("")+
                 ylab("")+
                 guides(color=FALSE)+
                 facet_rep_wrap(~city, ncol=3, repeat.tick.labels = TRUE)
        )
      }, height=800)
    } else {
      output$cascade_percent <- renderPlot({
        switch(input$cascade,
               "90-90-90" = proportion_data %>%
                 filter(casState != "sizeEst" & casState!= "prev") %>%
                 filter(year %in% input$year) %>%
                 filter(city %in% input$subnat) %>%
                 arrange(year) %>%
                 ggplot(aes(x=casState, group=year)) +
                 geom_col(aes(y=point_90, fill=year), position="dodge", stat="identity") +
                 geom_errorbar(aes(ymin=ll_90, ymax=ul_90), width=0.2, position=position_dodge(1))+
                 geom_segment(aes(x=0.5, xend=1.4, y=0.9, yend=0.9), color="navy", linetype=5)+
                 geom_segment(aes(x=1.6, xend=2.4, y=0.9, yend=0.9), color="navy", linetype=5)+
                 geom_segment(aes(x=2.6, xend=3.5, y=0.9, yend=0.9), color="navy", linetype=5)+
                 geom_text(aes(y=0.95, label="90%"), color="navy", linetype=5)+
                 scale_y_continuous(limits=c(0,1), labels=percent)+
                 scale_x_discrete(labels=c("Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
                 theme(axis.text = element_text(size=rel(1.2)),
                       strip.text = element_text(size=rel(1.3)))+
                 xlab("")+
                 ylab("")+
                 guides(color=FALSE)+
                 facet_rep_wrap(~city, ncol=3, repeat.tick.labels = TRUE),
               "90-81-72" = proportion_data %>%
                 filter(casState != "sizeEst" & casState!= "prev") %>%
                 filter(year %in% input$year) %>%
                 filter(city %in% input$subnat) %>%
                 arrange(year) %>%
                 ggplot(aes(x=casState, group=year)) +
                 geom_col(aes(y=point_72, fill=year), position="dodge", stat="identity") +
                 geom_errorbar(aes(ymin=ll_72, ymax=ul_72), width=0.2, position=position_dodge(1))+
                 geom_segment(aes(x=0.5, xend=1.4, y=0.9, yend=0.9), color="navy", linetype=5)+
                 geom_segment(aes(x=1.6, xend=2.4, y=0.81, yend=0.81), color="navy", linetype=5)+
                 geom_segment(aes(x=2.6, xend=3.5, y=0.72, yend=0.72), color="navy", linetype=5)+
                 geom_text(aes(x=1, y=0.95, label="90%"), color="navy", linetype=5)+
                 geom_text(aes(x=2, y=0.86, label="81%"), color="navy", linetype=5)+
                 geom_text(aes(x=3, y=0.77, label="72%"), color="navy", linetype=5)+
                 scale_y_continuous(limits=c(0,1), labels=percent)+
                 scale_x_discrete(labels=c("Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
                 theme(axis.text = element_text(size=rel(1.2)),
                       strip.text = element_text(size=rel(1.3)))+
                 xlab("")+
                 ylab("")+
                 guides(color=FALSE)+
                 facet_rep_wrap(~city, ncol=3, repeat.tick.labels = TRUE)
        )
      }, height=400)
    }
  })
  
  observeEvent(input$subnat, { 
    if (length(input$subnat)>3) {
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
                                     geom_segment(data=count_data %>% filter(!is.na(count_target) & year %in% input$year & city %in% input$subnat), aes(x=x, xend=xend, y=count_target, yend=count_target), color="navy", linetype=5)+
                                     scale_x_discrete(labels=c("Size\nEstimate", "KPLHIV", "Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
                                     theme(axis.text = element_text(size=rel(1.15)),
                                           strip.text = element_text(size=rel(1.3)))+
                                     xlab("")+
                                     guides(color=FALSE)+
                                     ylab("Number of people")+
                                     facet_wrap(~city, scales="free", ncol=3),
                                   "2" = count_data %>%
                                     filter(year %in% input$year) %>%
                                     filter(city %in% input$subnat) %>%
                                     ggplot(aes(x=casState, group=year)) +
                                     geom_col(aes(y=point_72, fill=year), position="dodge", stat="identity") +
                                     geom_errorbar(aes(ymin=ll_72, ymax=ul_72), width=0.2, position=position_dodge(1))+
                                     scale_x_discrete(labels=c("Size\nEstimate", "KPLHIV", "Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
                                     theme(axis.text = element_text(size=rel(1.15)),
                                           strip.text = element_text(size=rel(1.3)))+
                                     xlab("")+
                                     ylab("Number of people")+
                                     facet_wrap(~city, scales="free", ncol=3),
                                   "3" = count_data %>%
                                     filter(year %in% input$year) %>%
                                     filter(city %in% input$subnat) %>%
                                     ggplot(aes(x=casState, group=year)) +
                                     geom_col(aes(y=point_72, fill=year), position="dodge", stat="identity") +
                                     geom_errorbar(aes(ymin=ll_72, ymax=ul_72), width=0.2, position=position_dodge(1))+
                                     scale_x_discrete(labels=c("Size\nEstimate", "KPLHIV", "Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
                                     theme(axis.text = element_text(size=rel(1.15)),
                                           strip.text = element_text(size=rel(1.3)))+
                                     xlab("")+
                                     ylab("Number of people")+
                                     facet_wrap(~city, scales="free", ncol=3)
               )
        )
      }, height=800)
    } else {
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
                                     geom_segment(data=count_data %>% filter(!is.na(count_target) & year %in% input$year & city %in% input$subnat), aes(x=x, xend=xend, y=count_target, yend=count_target), color="navy", linetype=5)+
                                     scale_x_discrete(labels=c("Size\nEstimate", "KPLHIV", "Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
                                     theme(axis.text = element_text(size=rel(1.15)),
                                           strip.text = element_text(size=rel(1.3)))+
                                     xlab("")+
                                     guides(color=FALSE)+
                                     ylab("Number of people")+
                                     facet_wrap(~city, scales="free", ncol=3),
                                   "2" = count_data %>%
                                     filter(year %in% input$year) %>%
                                     filter(city %in% input$subnat) %>%
                                     ggplot(aes(x=casState, group=year)) +
                                     geom_col(aes(y=point_72, fill=year), position="dodge", stat="identity") +
                                     geom_errorbar(aes(ymin=ll_72, ymax=ul_72), width=0.2, position=position_dodge(1))+
                                     scale_x_discrete(labels=c("Size\nEstimate", "KPLHIV", "Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
                                     theme(axis.text = element_text(size=rel(1.15)),
                                           strip.text = element_text(size=rel(1.3)))+
                                     xlab("")+
                                     ylab("Number of people")+
                                     facet_wrap(~city, scales="free", ncol=3),
                                   "3" = count_data %>%
                                     filter(year %in% input$year) %>%
                                     filter(city %in% input$subnat) %>%
                                     ggplot(aes(x=casState, group=year)) +
                                     geom_col(aes(y=point_72, fill=year), position="dodge", stat="identity") +
                                     geom_errorbar(aes(ymin=ll_72, ymax=ul_72), width=0.2, position=position_dodge(1))+
                                     scale_x_discrete(labels=c("Size\nEstimate", "KPLHIV", "Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
                                     theme(axis.text = element_text(size=rel(1.15)),
                                           strip.text = element_text(size=rel(1.3)))+
                                     xlab("")+
                                     ylab("Number of people")+
                                     facet_wrap(~city, scales="free", ncol=3)
               )
        )
      }, height=400)
    }
  })
  
  
  output$cascade_count_warning <- renderText({
    switch(input$cascade,
           "90-90-90" = "Size estimate visualisation only available with 90-81-72 cascade selection",
           "90-81-72" = NULL
    )
  })
  
  # observeEvent(input$subnat, {
  #   if (length(input$subnat)>3) {
  #     addClass(id="cascade_plot", class="plot_height")
  #   } else {
  #     removeClass(id="cascade_plot", class="plot_height")
  #   }
  # })
  
  # plot_height <- reactive({
  #   observeEvent(input$subnat, {
  #     ifelse(length(input$subnat)>3, 800, 400)
  #   })
  # })
  
  # observeEvent(input$subnat, {
  #   output$plot_height <- renderText({
  #     ifelse(length(input$subnat)>3, "big", "small")
  #   })
  # })
  
  # observeEvent(input$subnat, {
  #   output$high_plot <- ifelse(length(input$subnat)>3, TRUE, FALSE)
  # })
  
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
                con, na="", row.names = FALSE)
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
                con, na="", row.names = FALSE)
    }
  )
}


