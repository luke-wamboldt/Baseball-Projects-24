library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)

dat <- SAL_Cumulative_2015

kZone = data.frame(x = c(.83, -.83, -.83, .83, .83),
                   y = c(1.52, 1.52, 3.67, 3.67, 1.52))

hPlate = data.frame(x = c(.71, -.71, -.71, 0, .71, .71),
                    y = c(0, 0, -.25, -.5, -.25, 0))

dat1 <- dat %>%
  filter(Batter == "Devers, Rafael" &
           PitchCall %in% c("InPlay", "StrikeSwinging") &
           !(PitchCall == "InPlay" & is.na(ExitSpeed)) &
           !is.na(TaggedPitchType)) %>%
  mutate(SwingResult = ifelse(is.na(ExitSpeed) & PitchCall == "StrikeSwinging", "Whiff",
                              ifelse(ExitSpeed >= 95, "Hard Contact", "Soft Contact"))) %>%
  mutate(PitchType = ifelse(TaggedPitchType %in% c("Fastball", "Cutter", "Sinker"), "Fastball",
                            ifelse(TaggedPitchType %in% c("Curveball", "Slider"), "Breaking Ball","Changeup"))) %>%
  mutate(PitchType = factor(PitchType, levels = c("Fastball", "Breaking Ball", "Changeup"))) %>%
  mutate(PitcherSide = ifelse(PitcherThrows == "Right", "vs. RHP", "vs. LHP")) %>%
  mutate(PitcherSide = factor(PitcherSide, levels = c("vs. RHP", "vs. LHP")))

dat1 %>%
  ggplot() +
  geom_point(aes(PlateLocSide, PlateLocHeight, color = SwingResult), size = 3, alpha = .75) +
  facet_grid(PitcherSide ~ PitchType) +
  scale_x_reverse(limits = c(2, -2), breaks = c()) +
  scale_y_continuous(breaks = c()) +
  geom_path(aes(x, y), data = kZone) +
  geom_path(aes(x, y), data = hPlate, color = "darkgrey") +
  xlab("") +
  ylab("") +
  scale_color_discrete(name = "Swing Result") +
  ggtitle("Rafael Devers 2015 Swing Results by Pitch Type, Pitcher Handedness, & Pitch Location") +
  labs(subtitle = "") +
  theme(legend.position = "bottom") +
  labs(caption = "Batter/Catcher Pespective") +
  theme_fivethirtyeight()


dat2 <- dat %>%
  filter(Batter == "Devers, Rafael" &
           PlateLocHeight <= 3.67 & PlateLocHeight >= 1.52 &
           PlateLocSide <= .83 & PlateLocSide >= -.83 &
           PitchCall %in% c("StrikeCalled", "BallCalled") &
           !is.na(TaggedPitchType)) %>%
  mutate(PitchType = ifelse(TaggedPitchType %in% c("Fastball", "Cutter", "Sinker"), "Fastball",
                            ifelse(TaggedPitchType %in% c("Curveball", "Slider"), "Breaking Ball","Changeup"))) %>%
  mutate(PitchType = factor(PitchType, levels = c("Fastball", "Breaking Ball", "Changeup"))) %>%
  mutate(PitcherSide = ifelse(PitcherThrows == "Right", "vs. RHP", "vs. LHP")) %>%
  mutate(PitcherSide = factor(PitcherSide, levels = c("vs. RHP", "vs. LHP"))) %>%
  mutate(Count = paste(Balls, "-", Strikes))

dat2 %>%
  ggplot() +
  geom_point(aes(PlateLocSide, PlateLocHeight, color = PitchType), size = 3, alpha = .75) +
  facet_grid(PitcherSide ~ Count) +
  scale_x_reverse(limits = c(2, -2), breaks = c()) +
  scale_y_continuous(breaks = c()) +
  geom_path(aes(x, y), data = kZone) +
  geom_path(aes(x, y), data = hPlate, color = "darkgrey") +
  xlab("") +
  ylab("") +
  scale_color_discrete(name = "Pitch Type") +
  ggtitle("Rafael Devers 2015 Takes in the Zone by Count, Pitcher Handedness, & Pitch Type") +
  labs(subtitle = "") +
  theme(legend.position = "bottom") +
  labs(caption = "Batter/Catcher Pespective") +
  theme_fivethirtyeight()
