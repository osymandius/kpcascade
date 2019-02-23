clean_data <- function(data_input) {

  cleaned_data <- data_input %>%
    rename("casState" = "Cascade.status", "KP" = "Key.population", "year" = "Year", "city" = "City.Region", "point_90" = "Point.Estimate", "ll_90" = "Lower.Bound", "ul_90" = "Upper.Bound") %>%
    mutate(casState = ifelse(casState == "Size Estimate", "sizeEst", 
                             ifelse(casState == "Prevalence", "prev", 
                                    ifelse(casState == "Aware of Status", "aware",
                                           ifelse(casState == "On ART", "onART", "vSupp")))))
  
  cleaned_data$casState <- factor(cleaned_data$casState, levels=unique(cleaned_data$casState))
  cleaned_data$year <- as.character(cleaned_data$year)
  
  return(cleaned_data)
  
}
  
proportion_manip <- function(cleaned_data) {
  
  proportion_data <- cleaned_data %>%
    group_by(KP, year, city) %>%
    mutate(point_72 = as.numeric(ifelse(casState == "sizeEst", point_90[casState=="sizeEst"],
                                        ifelse(casState == "prev", point_90[casState=="prev"], 
                                               ifelse(casState == "aware", point_90[casState=="aware"], 
                                                      ifelse(casState == "onART", point_90[casState=="aware"]*point_90[casState=="onART"], 
                                                             ifelse(casState == "vSupp", point_90[casState=="aware"]*point_90[casState=="onART"]*point_90[casState=="vSupp"], ""))))))) %>%
    mutate(ll_72 = as.numeric(ifelse(casState == "sizeEst", ll_90[casState=="sizeEst"],
                                     ifelse(casState == "prev", ll_90[casState=="prev"], 
                                            ifelse(casState == "aware", ll_90[casState=="aware"], 
                                                   ifelse(casState == "onART", ll_90[casState=="aware"]*ll_90[casState=="onART"], 
                                                          ifelse(casState == "vSupp", ll_90[casState=="aware"]*ll_90[casState=="onART"]*ll_90[casState=="vSupp"], ""))))))) %>%
    mutate(ul_72 = as.numeric(ifelse(casState == "sizeEst", ul_90[casState=="sizeEst"],
                                     ifelse(casState == "prev", ul_90[casState=="prev"], 
                                            ifelse(casState == "aware", ul_90[casState=="aware"], 
                                                   ifelse(casState == "onART", ul_90[casState=="aware"]*ul_90[casState=="onART"], 
                                                          ifelse(casState == "vSupp", ul_90[casState=="aware"]*ul_90[casState=="onART"]*ul_90[casState=="vSupp"], "")))))))
  
  return(proportion_data)
  
}


count_manip <- function(proportion_data) {
  
  count_data <- proportion_data %>%
    group_by(KP, year, city) %>%
    select(-c(point_90, ul_90, ll_90)) %>%
    mutate(point_72 = as.numeric(ifelse(casState == "sizeEst", point_72[casState=="sizeEst"],
                                        ifelse(casState == "prev", point_72[casState=="sizeEst"]*point_72[casState=="prev"], 
                                               ifelse(casState == "aware", point_72[casState=="sizeEst"]*point_72[casState=="prev"]*point_72[casState=="aware"], 
                                                      ifelse(casState == "onART", point_72[casState=="sizeEst"]*point_72[casState=="prev"]*point_72[casState=="onART"], 
                                                             ifelse(casState == "vSupp", point_72[casState=="sizeEst"]*point_72[casState=="prev"]*point_72[casState=="vSupp"], ""))))))) %>%
    mutate(ll_72 = as.numeric(ifelse(casState == "sizeEst", ll_72[casState=="sizeEst"],
                                     ifelse(casState == "prev", ll_72[casState=="sizeEst"]*ll_72[casState=="prev"], 
                                            ifelse(casState == "aware", ll_72[casState=="sizeEst"]*ll_72[casState=="prev"]*ll_72[casState=="aware"], 
                                                   ifelse(casState == "onART", ll_72[casState=="sizeEst"]*ll_72[casState=="prev"]*ll_72[casState=="onART"], 
                                                          ifelse(casState == "vSupp", ll_72[casState=="sizeEst"]*ll_72[casState=="prev"]*ll_72[casState=="vSupp"], ""))))))) %>%
    mutate(ul_72 = as.numeric(ifelse(casState == "sizeEst", ul_72[casState=="sizeEst"],
                                     ifelse(casState == "prev", ul_72[casState=="sizeEst"]*ul_72[casState=="prev"], 
                                            ifelse(casState == "aware", ul_72[casState=="sizeEst"]*ul_72[casState=="prev"]*ul_72[casState=="aware"], 
                                                   ifelse(casState == "onART", ul_72[casState=="sizeEst"]*ul_72[casState=="prev"]*ul_72[casState=="onART"], 
                                                          ifelse(casState == "vSupp", ul_72[casState=="sizeEst"]*ul_72[casState=="prev"]*ul_72[casState=="vSupp"], ""))))))) %>%
    mutate(count_target = as.numeric(ifelse(casState == "aware", point_72[casState=="prev"]*0.9, 
                                            ifelse(casState == "onART", point_72[casState=="prev"] * 0.81, 
                                                   ifelse(casState == "vSupp", point_72[casState=="prev"]*0.72, ""))))) %>%
    mutate(x=as.numeric(casState)-0.5, xend = as.numeric(casState)+0.5)
  
  return(count_data)

}