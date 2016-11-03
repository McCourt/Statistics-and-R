## Load library and sourse data
library(ggplot2)
library(dplyr)
mb_yawn = read.csv("https://stat.duke.edu/~mc301/data/mb_yawn.csv")

## Table of Original Data mb_yawn
table(mb_yawn) %>% addmargins()

## Simulation of mb_yawn
n_sim = 10000
deck = c(rep("F", 14), rep("T", 36))
d = data.frame(prop_diff = rep(NA, n_sim))
for(i in 1:n_sim){
  shuffled_deck = sample(deck, 50, replace = FALSE)
  control = shuffled_deck[1:16]
  treatment = shuffled_deck[-(1:16)]
  p_yawn_trt = sum(treatment == "F") / length(treatment)
  p_yawn_ctl = sum(control == "F") / length(control)
  d$prop_diff[i] = p_yawn_trt - p_yawn_ctl
}

## Another method of Simulation of mb_yawn
for(i in 1:n_sim){
  mb_yawn_permute = mb_yawn %>% mutate(group = sample(group))
  tbl = mb_yawn_permute %>% table() %>% addmargins()
  prop = tbl[1:2, 2] / tbl[1:2, 3]
  d$prop_diff[i] = prop[2] - prop[1]
}

## Ploting & Conclusion of mb_yawn
ggplot(d, aes(x = prop_diff)) +
  geom_histogram(bins = 10) +
  geom_vline(xintercept = 0.0441, color = "red")
(p_value = sum(d$prop_diff > 0.0441) / nrow(d))

## Boottrap Simulation of mb_yawn
d = data.frame(prop_diff = rep(NA, n_sim))
for (i in 1:n_sim){
  yawn_boot = mb_yawn %>% sample_n(nrow(mb_yawn),replace = TRUE)
  tbl = yawn_boot %>% table() %>% addmargins()
  prop = tbl[1:2, 2] / tbl[1:2, 3]
  d$prop_diff[i] = prop[2] - prop[1]
}

## Ploting & Conclusion of mb_yawn
ggplot(d, aes(x = prop_diff)) +
  geom_histogram(bins = 10) +
  geom_vline(xintercept = quantile(d$prop_diff, probs = c(0.025, 0.975)), color = "red") +
  geom_vline(xintercept = quantile(d$prop_diff, probs = c(0.005, 0.995)), color = "blue")
(p_value = sum(d$prop_diff > 0.0441) / nrow(d))