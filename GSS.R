## Load the library and sourse data
gss = read.csv("https://stat.duke.edu/~mc301/data/gss2010.csv")
library(ggplot2)
library(dplyr)

## calculate some property of original data
gss_relax = gss %>% select(hrsrelax, sex) %>% na.omit()
means = gss_relax_mutate %>% group_by(sex) %>% summarise(mean = mean(hrsrelax))
xbar_diff = unlist(means[1,2] - means[2,2])

## Simulation
n_sim = 10000
d = data.frame(mean_diff = rep(NA, n_sim))
for (i in 1:n_sim){
  gss_relax_mutate = gss_relax %>% mutate(sex = sample(sex))
  means = gss_relax_mutate %>% group_by(sex) %>% summarise(mean = mean(hrsrelax))
  d$mean_diff[i] = unlist(means[1,2] - means[2,2])
}

## Ploting & Conclusion
ggplot(d, aes(x = mean_diff)) +
  geom_histogram(bins = 10) +
  geom_vline(xintercept = xbar_diff, color = "red")
(p_value = sum(d$mean_diff > xbar_diff) / nrow(d))