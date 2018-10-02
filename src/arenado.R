## Ryan Elmore
## Arenado vs Lester

library(tidyverse)
library(baseballr)
library(mgcv)

arenado_id <- playerid_lookup(last_name = "Arenado")

df_arenado <- scrape_statcast_savant(start_date = "2018-04-02",
                                     end_date = "2018-10-01",
                                     playerid = 571448)

lester_id <- playerid_lookup(last_name = "Lester")

df_arenado_lester <- filter(df_arenado,
                            pitcher == "452657")

top_zone <- 3.5
bot_zone <- 1.6
left_zone <- -0.75
right_zone <- 0.75
strike_zone_df <- data.frame(
  x = c(left_zone, left_zone, right_zone, right_zone, left_zone),
  y = c(bot_zone, top_zone, top_zone, bot_zone, bot_zone)
)

df_arenado <- df_arenado %>%
  mutate(plate_x = as.numeric(levels(plate_x))[plate_x],
         plate_z = as.numeric(levels(plate_z))[plate_z])

p <- ggplot(df_arenado %>%
              filter(p_throws == "L"),
            aes(x = plate_x,
                y = plate_z,
                col = type))
p + geom_point(alpha = 0.75) +
  geom_path(data = strike_zone_df,aes(x, y), lwd = 1.5, color = "red") +
  labs(caption = "Data courtesy of MLBAM",
       x = "horizontal location (ft)",
       y = "vertical location (ft)") +
  xlim(-3.5, 3.5) +
  scale_color_brewer("", labels = c("Ball", "Strike", "In Play"), 
                     palette = "Dark2") +
  coord_fixed()

df_arenado_in_play <- df_arenado %>%
  filter(p_throws == "L") %>%
  mutate(in_play = if_else(type == "X", 1, 0))

arenado_gam <- gam(in_play ~ s(plate_x, plate_z),
                   family = binomial,
                   data = df_arenado_in_play)

x <- seq(-1.5, 1.5, length = 50)
z <- seq(0.5, 5, length = 50)
swing_predict_data <- data_frame(plate_x = c(outer(x, z * 0 + 1)),
                                 plate_z = c(outer(x * 0 + 1, z)))

swing_preds <- predict(arenado_gam, swing_predict_data) 
swing_predict_data <- swing_predict_data %>%
  mutate(swing_prob = exp(swing_preds) / (1 + exp(swing_preds))) 

p <- ggplot(data = swing_predict_data,
            aes(x = plate_x, y = plate_z))
p + geom_tile(aes(fill = swing_prob)) + 
  scale_fill_distiller("prob", palette = "Reds",direction = 1, limit = c(0,1)) + 
  geom_path(data = strike_zone_df, aes(x, y), linetype = 2, color = "navy") + 
  coord_fixed() + 
  labs(x = "horizontal location (ft)",
       y = "vertical location (ft)",
       caption = "Data courtesy of MLBAM")
