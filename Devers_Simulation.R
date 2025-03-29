library(tidyverse)
library(dplyr)

set.seed(11)

bip <- SAL_Cumulative_2015 %>%
  select(PlayResult, ExitSpeed, Angle) %>%
  filter(PlayResult != "Undefined" & !is.na(ExitSpeed) & !is.na(Angle)) %>%
  mutate(ExitSpeed = round(ExitSpeed/5)*5, Angle = round(Angle/1)*1) %>%
  mutate(H = ifelse(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), 1, 0)) %>%
  mutate(XBH = ifelse(PlayResult == "Double", 1,
                      ifelse(PlayResult == "Triple", 1,
                             ifelse(PlayResult == "HomeRun", 1, 0)))) %>%
  mutate(HR = ifelse(PlayResult == "HomeRun", 1, 0)) %>%
  group_by(ExitSpeed, Angle) %>%
  summarize(H_prob = sum(H) / length(H),
            XBH_prob = sum(XBH) / length(XBH),
            HR_prob = sum(HR) / length(HR),
            Obs = length(H)) %>%
  mutate(bucket = paste(ExitSpeed, Angle, sep = "."))

Devers_bip <- SAL_Cumulative_2015 %>%
  select(Batter, PlayResult, ExitSpeed, Angle) %>%
  filter(Batter == "Devers, Rafael" & PlayResult != "Undefined" & !is.na(ExitSpeed) & !is.na(Angle))

Devers_bip <- Devers_bip %>%
  mutate(ExitSpeed = round(ExitSpeed/5)*5, Angle = round(Angle/1)*1) %>%
  mutate(BIP = ifelse(PlayResult == "Undefined", 0, 1)) %>%
  group_by(ExitSpeed, Angle) %>%
  summarize(bb_prob = sum(BIP) / nrow(Devers_bip)) %>%
  mutate(bucket = paste(ExitSpeed, Angle, sep = ".")) %>%
  mutate(n = round(386 * bb_prob, 0))

index <- which(bip$bucket %in% Devers_bip$bucket)
BIP <- 391

S_H <- replicate(1000,{
  s <- replicate(BIP, {
  bucket <- sample(Devers_bip$bucket, prob = Devers_bip$bb_prob, 1)
  ind <- which(bip$bucket %in% bucket)
  p <- bip$H_prob[ind]
  sample(c(1,0), prob = c(p, 1-p), 1)
  })
sum(s)
})

S_XBH <- replicate(1000,{
  s <- replicate(BIP, {
    bucket <- sample(Devers_bip$bucket, prob = Devers_bip$bb_prob, 1)
    ind <- which(bip$bucket %in% bucket)
    p <- bip$XBH_prob[ind]
    sample(c(1,0), prob = c(p, 1-p), 1)
  })
  sum(s)
})

S_HR <- replicate(1000,{
  s <- replicate(BIP, {
    bucket <- sample(Devers_bip$bucket, prob = Devers_bip$bb_prob, 1)
    ind <- which(bip$bucket %in% bucket)
    p <- bip$HR_prob[ind]
    sample(c(1,0), prob = c(p, 1-p), 1)
  })
  sum(s)
})

results <- c(round(mean(S_H), 0), round(mean(S_XBH), 0), round(mean(S_HR), 0))
results

sd(S_H)
sd(S_XBH)
sd(S_HR)

sd(S_H) / sqrt(1000)
sd(S_XBH) / sqrt(1000)
sd(S_HR) / sqrt(1000)

mean(S_H <= 135)
mean(S_XBH <= 50)
mean(S_HR <= 11)
