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

saveRDS(df, "data/home-runs.rds")

## Bar Chart
df_2 <- mutate(df, 
             count = paste(balls, strikes, sep = "-")) %>%
  group_by(count, stand, p_throws) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  arrange(n)

p <- ggplot(data = group_by(df_2, count) %>%
              summarize(nn = sum(n)) %>%
              ungroup() %>%
              arrange(desc(nn)),
            aes(reorder(count, nn), nn))
p + geom_col() +
  labs(x = "count",
       y = "number of home runs") +
  theme_bw() +
  coord_flip()
ggsave("fig/home-runs-by-count.png")

p <- ggplot(data = df_2,
            aes(reorder(count, n), n, fill = p_throws))
p + geom_col() +
  facet_wrap(~ stand) +
  scale_fill_brewer("pitcher", palette = "Dark2") +
  labs(x = "count",
       y = "number of home runs") +
  coord_flip() +
  theme_bw()
ggsave("fig/home-runs-by-count-pitching.png")
