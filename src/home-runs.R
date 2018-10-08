## Ryan Elmore
## Looking at home runs
## 8 Oct 2018

library(baseballr)
library(tidyverse)
library(lubridate)

## Get all batting data fro 2018

date_seq <- seq(ymd("2018-04-01"), ymd("2018-10-08"), by = "week")

df <- scrape_statcast_savant(start_date = date_seq[1], 
                             end_date = date_seq[2] - 1,
                             player_type = "batter") %>%
  filter(events == "home_run")

for(i in 2:(length(date_seq)-1)){
  cat(sprintf("Date: %s at %s \n", date_seq[i], Sys.time()))
  df <- bind_rows(df, 
                  scrape_statcast_savant(start_date = date_seq[i], 
                                         end_date = date_seq[i+1] - 1,
                                         player_type = "batter") %>%
                    filter(events == "home_run"))
}

