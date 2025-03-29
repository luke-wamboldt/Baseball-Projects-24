library(tidyverse)
library(dplyr)

league_average <- Pitching %>%
  select(yearID, BB, SO, BFP) %>%
  group_by(yearID) %>%
  summarize(BB = sum(BB), SO = sum(SO), BFP = sum(BFP)) %>%
  ungroup() %>%
  mutate(K_BB_PCT = round(((SO / BFP) - (BB / BFP)), 3))

pitching_subset1 <- Pitching %>%
  select(playerID, yearID, IPouts, BB, SO, BFP) %>%
  group_by(yearID, playerID) %>%
  summarize(sum(IPouts), sum(BB), sum(SO), sum(BFP)) %>%
  rename(IPouts = "sum(IPouts)", BB = "sum(BB)", SO = "sum(SO)", BFP = "sum(BFP)") %>%
  ungroup() %>%
  mutate(IP = round(IPouts / 3, 1), K_BB_PCT = round(((SO / BFP) - (BB / BFP)), 3)) %>%
  filter(IP >= 150) %>%
  select(playerID, yearID, IP, K_BB_PCT) %>%
  arrange(desc(K_BB_PCT)) %>%
  mutate(Rank = rank(desc(K_BB_PCT), ties.method = "min")) %>%
  filter(Rank <= 100)

ind1 <- match(pitching_subset1$playerID, People$playerID)
ind2 <- match(pitching_subset1$yearID, league_average$yearID)

pitching_subset1 <- pitching_subset1 %>%
  mutate(Name = paste(People$nameFirst[ind1], People$nameLast[ind1]), IP = as.character(IP)) %>%
  mutate(IP = sub(".3", ".1", IP, fixed = T)) %>%
  mutate(IP = sub(".7", ".2", IP, fixed = T)) %>%
  mutate(IP = as.numeric(IP)) %>%
  mutate(IP = round(IP, 1)) %>%
  mutate(avg_KBBPCT = league_average$K_BB_PCT[ind2]) %>%
  mutate(K_BB_PCT_Plus = round(100 + ((K_BB_PCT - avg_KBBPCT) / avg_KBBPCT) * 100, 0)) %>%
  mutate(K_BB_PCT = scales::percent(K_BB_PCT, accuracy = .1)) %>%
  mutate(avg_KBBPCT = scales::percent(avg_KBBPCT, accuracy = .1))
