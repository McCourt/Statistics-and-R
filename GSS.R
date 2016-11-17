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

## Student T-Test sample
gss %>% 
  filter(!is.na(hrsrelax)) %>%
  summarise(mean(hrsrelax), median(hrsrelax), sd(hrsrelax), length(hrsrelax))
hrsrelax_summ = gss %>% 
  filter(!is.na(hrsrelax)) %>%
  summarise(xbar = mean(hrsrelax), s = sd(hrsrelax), n = n())
se = hrsrelax_summ$s / sqrt(hrsrelax_summ$n)
t = (hrsrelax_summ$xbar - 3) / se
df = hrsrelax_summ$n - 1
(p_value = pt(t, df, lower.tail = FALSE))

## Confidence interval for a mean
t_star = qt(0.95, df)
pt_est = hrsrelax_summ$xbar
confidence_interval = round(pt_est + c(-1,1) * t_star * se, 2)
t.test(gss$hrsrelax, mu = 3, alternative = "greater")
t.test(gss$hrsrelax, conf.level = 0.90)$conf.int

## Summary Statistics
(hrsrelax_sex_summ = gss %>% 
  filter(!is.na(hrsrelax)) %>%
  group_by(sex) %>%
  summarise(xbar = mean(hrsrelax), s = sd(hrsrelax), n = length(hrsrelax)))

## Calculating the test statistic
(se = sqrt((hrsrelax_sex_summ$s[1]^2 / hrsrelax_sex_summ$n[1]) 
           + (hrsrelax_sex_summ$s[2]^2 / hrsrelax_sex_summ$n[2])))
(t = ((hrsrelax_sex_summ$xbar[1] - hrsrelax_sex_summ$xbar[2]) - 0) / se)
(df = min(hrsrelax_sex_summ$n[1], hrsrelax_sex_summ$n[2]) - 1)

## p-value
pt(t, df) * 2
pt(t, df) + pt(-t, df, lower.tail=FALSE)

## Hypothesis testing for a proportion
grass_summ = gss %>%
  filter(!is.na(grass)) %>%
  summarise(x = sum(grass == "NOT LEGAL"), n = length(grass), p_hat = x / n)
p_0 = 0.5
se = sqrt(p_0 * (1-p_0) / grass_summ$n)
Z = (grass_summ$p_hat - p_0) / se
p-value = pnorm(Z, lower.tail = FALSE)