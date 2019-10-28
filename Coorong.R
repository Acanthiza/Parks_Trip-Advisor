
  library(lubridate)
  
  datCoorong <- dat %>%
    dplyr::filter(Date > ymd(Sys.Date())-years(10)) %>%
    dplyr::filter(grepl("Coorong",Park)) %>%
    dplyr::mutate(y = Stars, x = Date)
  
  ggplot(datCoorong, aes(Date,Stars)) +
    geom_point() +
    geom_smooth(method = "lm") +
    labs(title = "Stars given by TripAdvisor reviews for Coorong National Park")
  