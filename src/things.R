# download_template <- data.frame(matrix(ncol = 7, nrow = 10))
# colnames(download_template) <- c("Cascade status", "Key population", "Year", "City/Region", "Point Estimate", "Lower Bound", "Upper Bound")



# data %>%
#   filter(casState != "sizeEst" & casState!= "prev") %>%
#   filter(year==2018) %>%
#   select(-c(ll_90, ul_90, point_72, ll_72, ul_72)) %>%
#   melt(id=c("casState", "KP", "year", "city")) %>%
#   ggplot(aes(x=casState, y=value)) +
#   geom_bar(position="stack", stat="identity", aes(fill=rev(variable))) +
#   facet_wrap(~city)
# 
# data %>%
#   filter(casState != "sizeEst" & casState!= "prev") %>%
#   melt(id=c("casState", "KP", "year", "city")) %>%
#   filter(variable=="point_90") %>%
#   ggplot(aes(x=casState, y=value, group=year)) +
#     geom_bar(position="dodge", stat="identity", aes(fill=year)) +
#     geom_segment(aes(x=0.5, xend=1.4, y=0.9, yend=0.9), color="red")+
#     geom_segment(aes(x=1.6, xend=2.4, y=0.9, yend=0.9), color="red")+
#     geom_segment(aes(x=2.6, xend=3.5, y=0.9, yend=0.9), color="red")+
#     facet_wrap(~city)
#  

#Works if filtered on single year to produce grey bars up to target
# ggplot(data=test, aes(x=casState, y=value, group=year)) +
#     geom_bar(stat="identity", fill="light grey") +
#     geom_bar(data=test %>% filter(variable=="point_90"), stat="identity", aes(fill="red")) +
#     facet_wrap(~city)
