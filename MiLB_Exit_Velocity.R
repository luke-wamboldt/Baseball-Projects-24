library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)

balls_in_play <- SAL_Cumulative_2015 %>%
  select(PlayResult, ExitSpeed, Angle, Distance) %>%
  filter(PlayResult != "Undefined" & !is.na(ExitSpeed) & !is.na(Angle) & !is.na(Distance)) %>%
  mutate(ExitSpeed = round(ExitSpeed/5)*5, Angle = round(Angle/5)*5) %>%
  mutate(H = ifelse(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), 1, 0)) %>%
  mutate(XBH = ifelse(PlayResult == "Double", 1,
                      ifelse(PlayResult == "Triple", 1,
                             ifelse(PlayResult == "HomeRun", 1, 0)))) %>%
  mutate(TB = ifelse(PlayResult == "Single", 1,
                     ifelse(PlayResult == "Double", 2,
                            ifelse(PlayResult == "Triple", 3,
                                   ifelse(PlayResult == "HomeRun", 4, 0))))) %>%
  mutate(HR = ifelse(PlayResult == "HomeRun", 1, 0)) %>%
  group_by(ExitSpeed) %>%
  summarize(h_pct = sum(H) / length(H),
            xbh_pct = sum(XBH) / length(XBH),
            tb_pct = sum(TB) / length(TB),
            hr_pct = sum(HR) / length(HR)) %>%
  ungroup()

balls_in_play %>%
  ggplot(aes(x = ExitSpeed)) +
  geom_line(aes(y = h_pct, color = "Hits"), linewidth = 1.5) +
  geom_line(aes(y = hr_pct, color = "Home Runs"), size = 1.5) +
  geom_line(aes(y = xbh_pct, color = "Extra-Base Hits"), size = 1.5) +
  geom_vline(aes(xintercept = 95), lty = 2) +
  labs(x = "Exit Velocity (MPH)",
       y = "Rate of Result on Batted Balls",
       color = "Legend",
       title = "Hit, Extra-Base Hit, & Home Run Rates on Batted Balls by Exit Velocity",
       subtitle = "",
       caption = "Data Source: Trackman - South Atlantic League, 2015") +
  annotate(geom = "text", x = 86, y = 0.62, label = "Hard-Hit Ball Threshold: EV = 95 MPH", size = 4) +
  scale_x_continuous(breaks = seq(25, 115, 5)) +
  scale_y_continuous(breaks = seq(0.0, 0.75, 0.1)) +
  scale_color_manual(values = c("Hits" = "darkblue", "Extra-Base Hits" = "maroon", "Home Runs" = "darkgreen"),
                     breaks = c("Hits", "Extra-Base Hits", "Home Runs")) +
  theme_economist() +
  theme(axis.title.x = element_text(vjust = -3, size = 12),
        axis.title.y = element_text(vjust = 5, size = 12),
        legend.position = "bottom",
        legend.text = element_text(margin = margin(l = -40), size=12),
        legend.title = element_blank(),
        legend.spacing.x = unit(.8, "inches"),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.4), 
                           "inches"))
