plots_server <- function(input, output) {
  
  ## Option control
  
  observeEvent(input$single_pop, {
    hide("viz_examples")
    show("main_plot_body")
    show("select_inputs")
    show("single_pop_kp")
    show("single_pop_year")
    hide("multiple_pop_kp")
    hide("multiple_pop_year")
    output$viz_option <- renderText("single")
  })
  
  observeEvent(input$multiple_pop, {
    hide("viz_examples")
    show("main_plot_body")
    show("select_inputs")
    hide("single_pop_kp")
    hide("single_pop_year")
    show("multiple_pop_kp")
    show("multiple_pop_year")
    output$viz_option <- renderText("multiple")
  })
  
  observeEvent(input$cascade, {
    if(input$cascade=="Custom") {
      show("custom_90s")
    } else {
      hide("custom_90s")
    }
  })
  
  nineties <- reactive(
    if(input$cascade=="90-90-90") {
      c(90,90,90)
    } else if(input$cascade=="90-81-73") {
      c(90,81,73)
    } else {
      c(input$first90,input$second90,input$third90)
    }
  )
  
  # Work out how many plots are going to be generated
  
  num_plots_viz1 <- 3
  num_plots_viz1 <- reactive(
    nrow(proportion_data %>%
                   filter(KP == input$single_kp) %>%
                   filter(year %in% input$multiple_year) %>%
                   filter(district %in% input$subnat1) %>%
                   group_by(district) %>%
                   summarise()
    )
  )
  
  num_rows_viz1 <- reactive(ceiling(num_plots_viz1()/3))
  
  plot_height_viz1 <- reactive(400*num_rows_viz1())
  plot_width_viz1 <- reactive(ifelse(num_plots_viz1()==1, 375, ifelse(
    num_plots_viz1()==2, 750, 1125)
  )
  )
  
  num_plots_viz2 <- 3
  num_plots_viz2 <- reactive(
    nrow(proportion_data %>%
                   filter(KP %in% input$multiple_kp) %>%
                   filter(year == input$single_year) %>%
                   filter(district %in% input$subnat2) %>%
                   group_by(district) %>%
                   summarise()
    )
  )
  
  num_rows_viz2 <- reactive(ceiling(num_plots_viz2()/3))
  
  plot_height_viz2 <- reactive(400*num_rows_viz2())
  plot_width_viz2 <- reactive(ifelse(num_plots_viz2()==1, 375, ifelse(
    num_plots_viz2()==2, 750, 1125)
  )
  )
  
  viz1_prop <- reactive(
    proportion_data %>%
      filter(casState != "sizeEst" & casState!= "prev") %>%
      filter(KP == input$single_kp) %>%
      filter(year %in% input$multiple_year) %>%
      filter(district %in% input$subnat1)
  )
  
  viz2_prop <- reactive(
    proportion_data %>%
      filter(casState != "sizeEst" & casState!= "prev") %>%
      filter(KP %in% input$multiple_kp) %>%
      filter(year == input$single_year) %>%
      filter(district %in% input$subnat2)
  )
  
  viz1_count <- reactive(
    count_data %>%
      filter(KP == input$single_kp) %>%
      filter(year %in% input$multiple_year) %>%
      filter(district %in% input$subnat1)
  )
  
  #### Viz 1: Single KP, multi year, multi subnat
  # Viz1: Percent
  # 
  # output$viz1_cascade_percent <- renderPlotly({
  #   ggplotly(
  #   if(input$cascade=="90-81-73"){
  #     viz1_prop() %>%
  #       ggplot(aes(x=casState, fill=year, group=year)) +
  #       geom_bar(aes(y=point_73), position=position_dodge(), stat="identity") +
  #       geom_errorbar(aes(ymin=ll_73, ymax=ul_73))+
  #       geom_segment(aes(x=0.5, xend=1.4, y=nineties()[1]/100, yend=nineties()[1]/100), color="navy", linetype=5)+
  #       geom_segment(aes(x=1.6, xend=2.4, y=nineties()[2]/100, yend=nineties()[2]/100), color="navy", linetype=5)+
  #       geom_segment(aes(x=2.6, xend=3.5, y=nineties()[3]/100, yend=nineties()[3]/100), color="navy", linetype=5)+
  #       geom_text(aes(y=1, label=paste(rep(nineties(), times=nrow(viz1_prop())/3), "%", sep="")), color="navy")+
  #       scale_y_continuous(limits=c(0,1), labels=percent)+
  #       scale_x_discrete(labels=c("Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
  #       theme(axis.text = element_text(size=rel(1.2)),
  #             strip.text = element_text(size=rel(1.3)))+
  #       xlab("")+
  #       ylab("")+
  #       guides(color=FALSE)+
  #       facet_rep_wrap(~district, ncol=3, repeat.tick.labels = TRUE)
  #   } else {
  #     viz1_prop() %>%
  #       ggplot(aes(x=casState, fill=year, group=year)) +
  #       geom_bar(aes(y=point_90), position=position_dodge(), stat="identity")+
  #       geom_errorbar(aes(ymin=ll_90, ymax=ul_90))+
  #       geom_segment(aes(x=0.5, xend=1.4, y=nineties()[1]/100, yend=nineties()[1]/100), color="navy", linetype=5)+
  #       geom_segment(aes(x=1.6, xend=2.4, y=nineties()[2]/100, yend=nineties()[2]/100), color="navy", linetype=5)+
  #       geom_segment(aes(x=2.6, xend=3.5, y=nineties()[3]/100, yend=nineties()[3]/100), color="navy", linetype=5)+
  #       geom_text(aes(y=1, label=paste(rep(nineties(), times=nrow(viz1_prop())/3), "%", sep="")), color="navy")+
  #       scale_y_continuous(limits=c(0,1), labels=percent)+
  #       scale_x_discrete(labels=c("Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
  #       coord_cartesian(clip = 'off') +
  #       theme(axis.text = element_text(size=rel(1.2)),
  #             strip.text = element_text(size=rel(1.3)))+
  #       xlab("")+
  #       ylab("")+
  #       guides(color=FALSE)+
  #       facet_rep_wrap(~district, ncol=3, repeat.tick.labels = TRUE)
  #   })
  # })
  # 
  output$viz1_cascade_percent <- renderPlot({
    
    if(input$cascade=="90-81-73"){
      viz1_prop() %>%
        ggplot(aes(x=casState, group=year)) +
        geom_col(aes(y=point_73, fill=year), position="dodge", stat="identity") +
        geom_errorbar(aes(ymin=ll_73, ymax=ul_73), width=0.2, position=position_dodge(1))+
        geom_segment(aes(x=0.5, xend=1.4, y=nineties()[1]/100, yend=nineties()[1]/100), color="navy", linetype=5)+
        geom_segment(aes(x=1.6, xend=2.4, y=nineties()[2]/100, yend=nineties()[2]/100), color="navy", linetype=5)+
        geom_segment(aes(x=2.6, xend=3.5, y=nineties()[3]/100, yend=nineties()[3]/100), color="navy", linetype=5)+
        geom_text(aes(y=1, label=paste(rep(nineties(), times=nrow(viz1_prop())/3), "%", sep="")), color="navy")+
        scale_y_continuous(limits=c(0,1), labels=percent)+
        scale_x_discrete(labels=c("Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
        theme(axis.text = element_text(size=rel(1.2)),
              strip.text = element_text(size=rel(1.3)))+
        labs(x=element_blank(), y=element_blank(), fill="Year") +
        guides(color=FALSE)+
        facet_rep_wrap(~district, ncol=3, repeat.tick.labels = TRUE) +
        theme_light() +
        theme(
          legend.text = element_text(size = 12),
          legend.title = element_text(size=12),
          axis.text = element_text(size=14),
          strip.text = element_text(size=14, margin = margin(0,0,10,0, unit = "pt"), hjust=0.05, color = "black", face="bold"),
          strip.background = element_blank()
         )
    } else {
      viz1_prop() %>%
        ggplot(aes(x=casState, group=year)) +
        geom_col(aes(y=point_90, fill=year), position="dodge", stat="identity") +
        geom_errorbar(aes(ymin=ll_90, ymax=ul_90), width=0.2, position=position_dodge(1))+
        geom_segment(aes(x=0.5, xend=1.4, y=nineties()[1]/100, yend=nineties()[1]/100), color="navy", linetype=5)+
        geom_segment(aes(x=1.6, xend=2.4, y=nineties()[2]/100, yend=nineties()[2]/100), color="navy", linetype=5)+
        geom_segment(aes(x=2.6, xend=3.5, y=nineties()[3]/100, yend=nineties()[3]/100), color="navy", linetype=5)+
        geom_text(aes(y=1, label=paste(rep(nineties(), times=nrow(viz1_prop())/3), "%", sep="")), color="navy")+
        scale_y_continuous(limits=c(0,1), labels=percent)+
        scale_x_discrete(labels=c("Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
        coord_cartesian(clip = 'off') +
        theme(axis.text = element_text(size=rel(1.2)),
              strip.text = element_text(size=rel(1.3)))+
        xlab("")+
        ylab("")+
        guides(color=FALSE)+
        facet_rep_wrap(~district, ncol=3, repeat.tick.labels = TRUE) +
        theme_light() +
        theme(
          legend.text = element_text(size = 12),
          legend.title = element_text(size=12),
          axis.text = element_text(size=14),
          strip.text = element_text(size=14, margin = margin(0,0,10,0, unit = "pt"), hjust=0.05, color = "black", face="bold"),
          strip.background = element_blank()
        )
    }
  }, height=reactive(plot_height_viz1()), width=reactive(plot_width_viz1()))
  # 
  #Viz 1: Count
  output$cascade_count <- renderPlot({
    if(input$cascade=="90-81-73") {
      if(length(input$multiple_year)==1) {
        viz1_count() %>%
          ggplot(aes(x=casState, group=year)) +
          geom_col(aes(y=point_73, fill=year), position="dodge", stat="identity") +
          geom_errorbar(aes(ymin=ll_73, ymax=ul_73), width=0.2, position=position_dodge(1))+
          geom_segment(data=count_data %>% filter(!is.na(count_target) & KP == input$single_kp & year %in% input$multiple_year & district %in% input$subnat), aes(x=x, xend=xend, y=count_target, yend=count_target), color="navy", linetype=5)+
          scale_x_discrete(labels=c("Size\nEstimate", "KPLHIV", "Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
          theme(axis.text = element_text(size=rel(1.15)),
                strip.text = element_text(size=rel(1.3)))+
          xlab("")+
          guides(color=FALSE)+
          ylab("Number of people")+
          facet_wrap(~district, scales="free", ncol=3)  +
          theme_light() +
          theme(
            legend.text = element_text(size = 12),
            legend.title = element_text(size=12),
            axis.text = element_text(size=14),
            strip.text = element_text(size=14, margin = margin(0,0,10,0, unit = "pt"), hjust=0.05, color = "black", face="bold"),
            strip.background = element_blank()
          )
      } else {
        viz1_count() %>%
          ggplot(aes(x=casState, group=year)) +
          geom_col(aes(y=point_73, fill=year), position="dodge", stat="identity") +
          geom_errorbar(aes(ymin=ll_73, ymax=ul_73), width=0.2, position=position_dodge(1))+
          scale_x_discrete(labels=c("Size\nEstimate", "KPLHIV", "Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
          theme(axis.text = element_text(size=rel(1.15)),
                strip.text = element_text(size=rel(1.3)))+
          xlab("")+
          ylab("Number of people")+
          facet_wrap(~district, scales="free", ncol=3) +
          theme_light() +
          theme(
            legend.text = element_text(size = 12),
            legend.title = element_text(size=12),
            axis.text = element_text(size=14),
            strip.text = element_text(size=14, margin = margin(0,0,10,0, unit = "pt"), hjust=0.05, color = "black", face="bold"),
            strip.background = element_blank()
          )
      }
    } else {
      NULL
    }
  }, height=reactive(plot_height_viz1()), width=reactive(plot_width_viz1()))
  
  
  ### Viz 2: Multi KP, single year, multi subnat
  #Percent
  
  output$viz2_cascade_percent <- renderPlot({
    if(input$cascade == "90-81-73") {
      viz2_prop() %>%
        ggplot(aes(x=casState, group=KP)) +
        geom_col(aes(y=point_73, fill=KP), position="dodge", stat="identity") +
        geom_errorbar(aes(ymin=ll_73, ymax=ul_73), width=0.2, position=position_dodge(1))+
        geom_segment(aes(x=0.5, xend=1.4, y=nineties()[1]/100, yend=nineties()[1]/100), color="navy", linetype=5)+
        geom_segment(aes(x=1.6, xend=2.4, y=nineties()[2]/100, yend=nineties()[2]/100), color="navy", linetype=5)+
        geom_segment(aes(x=2.6, xend=3.5, y=nineties()[3]/100, yend=nineties()[3]/100), color="navy", linetype=5)+
        geom_text(aes(y=1, label=paste(rep(nineties(), times=nrow(viz2_prop())/3), "%", sep="")), color="navy")+
        scale_y_continuous(limits=c(0,1), labels=percent)+
        scale_x_discrete(labels=c("Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
        theme(axis.text = element_text(size=rel(1.2)),
              strip.text = element_text(size=rel(1.3)))+
        xlab("")+
        ylab("")+
        guides(color=FALSE)+
        facet_rep_wrap(~district, ncol=3, repeat.tick.labels = TRUE)  +
        theme_light() +
        theme(
          legend.text = element_text(size = 12),
          legend.title = element_text(size=12),
          axis.text = element_text(size=14),
          strip.text = element_text(size=14, margin = margin(0,0,10,0, unit = "pt"), hjust=0.05, color = "black", face="bold"),
          strip.background = element_blank()
        )
    } else {        
      viz2_prop() %>%
        ggplot(aes(x=casState, group=KP)) +
        geom_col(aes(y=point_90, fill=KP), position="dodge", stat="identity") +
        geom_errorbar(aes(ymin=ll_90, ymax=ul_90), width=0.2, position=position_dodge(1))+
        geom_segment(aes(x=0.5, xend=1.4, y=nineties()[1]/100, yend=nineties()[1]/100), color="navy", linetype=5)+
        geom_segment(aes(x=1.6, xend=2.4, y=nineties()[2]/100, yend=nineties()[2]/100), color="navy", linetype=5)+
        geom_segment(aes(x=2.6, xend=3.5, y=nineties()[3]/100, yend=nineties()[3]/100), color="navy", linetype=5)+
        geom_text(aes(y=1, label=paste(rep(nineties(), times=nrow(viz2_prop())/3), "%", sep="")), color="navy")+
        scale_y_continuous(limits=c(0,1), labels=percent)+
        scale_x_discrete(labels=c("Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
        theme(axis.text = element_text(size=rel(1.2)),
              strip.text = element_text(size=rel(1.3)))+
        xlab("")+
        ylab("")+
        guides(color=FALSE)+
        facet_rep_wrap(~district, ncol=3, repeat.tick.labels = TRUE)  +
        theme_light() +
        theme(
          legend.text = element_text(size = 12),
          legend.title = element_text(size=12),
          axis.text = element_text(size=14),
          strip.text = element_text(size=14, margin = margin(0,0,10,0, unit = "pt"), hjust=0.05, color = "black", face="bold"),
          strip.background = element_blank()
        )
    }
  }, height=reactive(plot_height_viz2()), width=reactive(plot_width_viz2()))

  
  output$cascade_count_warning <- renderText({
    if(input$cascade != "90-81-73") {
      "Size estimate visualisation only available with 90-81-73 cascade selection"
    }
  })
  
  
## Results in tabular form

  
  output$viz1_cascade_table_proportion <- renderDT(
    datatable(proportion_data %>%
                filter(KP == input$single_kp) %>%
                filter(year %in% input$multiple_year) %>%
                filter(district %in% input$subnat1) %>%
                select(-c(cas90_81_73, Method, Source)) %>%
                filter(casState != "sizeEst") %>%
                namesToHuman(),
              container = wide6(), rownames=FALSE, options = list(pageLength=999, dom='t')) %>%
      formatRound(columns = 5:10, digits=3) 
  )
  
  output$viz1_cascade_table_count <- renderDT(
    datatable(count_data %>%
                select(-c(count_target, x, xend, cas90_81_73, Method, Source)) %>%
                filter(casState != "sizeEst") %>%
                filter(KP == input$single_kp) %>%
                filter(year %in% input$multiple_year) %>%
                filter(district %in% input$subnat1) %>%
                namesToHuman() %>%
                mutate(Cascade.status = ifelse(Cascade.status=="Prevalence", "KPLHIV", Cascade.status)),
              container = wide3_count(), rownames=FALSE, options = list(pageLength=999, dom='t')) %>%
      formatRound(columns=5:7, digits=0)
  )
  
  output$viz2_cascade_table_proportion <- renderDT(
    datatable(proportion_data %>%
                filter(KP %in% input$multiple_kp) %>%
                filter(year == input$single_year) %>%
                filter(district %in% input$subnat1) %>%
                select(-c(cas90_81_73, Method, Source)) %>%
                filter(casState != "sizeEst") %>%
                namesToHuman(),
              container = wide6(), rownames=FALSE, options = list(pageLength=999, dom='t')) %>%
      formatRound(columns = 5:10, digits=3) 
  )
  
  output$viz2_cascade_table_count <- renderDT(
    datatable(count_data %>%
                select(-c(count_target, x, xend, cas90_81_73, Method, Source)) %>%
                filter(casState != "sizeEst") %>%
                filter(KP %in% input$multiple_kp) %>%
                filter(year == input$single_year) %>%
                filter(district %in% input$subnat1) %>%
                namesToHuman() %>%
                mutate(Cascade.status = ifelse(Cascade.status=="Prevalence", "KPLHIV", Cascade.status)),
              container = wide3_count(), rownames=FALSE, options = list(pageLength=999, dom='t')) %>%
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
                  rename("Point 90-90-90" = "point_90", "Lower 90-90-90" = "ll_90", "Upper 90-90-90" = "ul_90", "Point 90-81-73" = "point_73", "Lower 90-81-73" = "ll_73", "Upper 90-81-73" = "ul_73"), 
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
                  rename("Point 90-81-73" = "point_73", "Lower 90-81-73" = "ll_73", "Upper 90-81-73" = "ul_73"),
                con, na="", row.names = FALSE)
    }
  )
  
  # output$viz1_download_plots <- downloadHandler(
  #   filename = function() {
  #     paste("KPCascade_plots.pdf")
  #   },
  #   content = function (con) {
  #     ggsave(con, plot = , device = "pdf")
  #   }
  # )
  
  # output$viz1_plot_download <- downloadHandler(
  #   filename = "KPCascade_plots.pdf",
  #   content = function(file) {
  #     tempReport <- file.path(tempdir(), "KPCascade_plots.Rmd")
  #     file.copy("KPCascade_plots", tempReport, overwrite = TRUE)
  #     
  #     # Set up parameters to pass to Rmd document
  #     params <- list(viz1_prop = output$viz1_cascade_percent,
  #                    viz1_count  = output$cascade_count
  #                    )
  #     
  #     # Knit the document, passing in the `params` list, and eval it in a
  #     # child of the global environment (this isolates the code in the document
  #     # from the code in this app).
  #     rmarkdown::render(tempReport, output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv())
  #     )
  #   }
  # )
  
}


