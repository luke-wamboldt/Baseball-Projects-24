library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)

pitches %>%
  mutate(pitch_name = ifelse(pitch_type == "FF", "Fastball",
                             ifelse(pitch_type == "CH", "Changeup",
                                    ifelse(pitch_type == "CB", "Curve", "Slider")))) %>%
  ggplot(aes(rel_speed, after_stat(count) / nrow(pitches))) +
  geom_density(aes(col = "All Pitches"), lty = 2, bw = 1, show.legend = F) +
  geom_density(data = filter(pitches, pitch_type == "CB"), aes(col = "Curve"), fill = "dodgerblue3", bw = 1, alpha = 0.5, key_glyph = draw_key_smooth) +
  geom_density(data = filter(pitches, pitch_type == "SL"), aes( col = "Slider"), fill = "yellow4", bw = 1, alpha = 0.5, show.legend = F) +
  geom_density(data = filter(pitches, pitch_type == "CH"), aes(col = "Changeup"), fill = "springgreen4", bw = 1, alpha = 0.5, show.legend = F) +
  geom_density(data = filter(pitches, pitch_type == "FF"), aes(col = "Fastball"), fill = "red", bw = 1, alpha = 0.5, show.legend = F) +
  geom_abline(aes(intercept = 0, slope = 0), col = "white") +
  scale_x_continuous(name = "Pitch Speed", limits = c(74, 103), breaks = c(75, 80, 85, 90, 95, 100), labels = c("75 MPH", "80 MPH", "85 MPH", "90 MPH", "95 MPH", "100 MPH")) +
  scale_y_continuous(name =  "Frequency of Speed", breaks = seq(0, .1, .02), labels = scales::percent) +
  scale_color_manual(name = "",
                     breaks = c("All Pitches", "Curve", "Slider", "Changeup", "Fastball"),
                     values = c("All Pitches" = "black", "Curve" = "dodgerblue3", "Slider" = "yellow4", "Changeup" = "springgreen4", "Fastball" = "red")) +
  ggtitle("2022 Frequency of Pitches by Pitch Speed") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 11),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(margin = margin(r = 3, unit = "cm")),
        axis.text.x = element_text(margin = margin(t = -0.5, unit = "cm")),
        axis.text.y = element_text(margin = margin(l = 1, unit = "cm")),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 1))
