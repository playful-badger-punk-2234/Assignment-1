# Assignment 1 - Q3
# Load Libaries
library(lme4)
library(multcomp)
library(lmerTest)
library(emmeans)
library(patchwork)
library(ggplot2)


# Exercise 3 --------------------------------------------------------------
load("data/in/SawBliss.RData")
saw_data = df

# Exploratory Plots
par(mfrow = c(1, 3))
boxplot(ly ~ brand, data = saw_data, col = "lightblue", main = "log(Time) by Brand")
boxplot(ly ~ bark, data = saw_data, col = "lightgreen", main = "log(Time) by Bark")
boxplot(ly ~ species, data = saw_data, col = "lightpink", main = "log(Time) by Species")
par(mfrow = c(1, 1))

# Interaction between brand and species model
m1 <- lmer(log(y_min) ~ brand*species + bark + (1 | team) + (1 | saw_id), data = df)
summary(m1)
drop1(m1, test = "F")

# Additive Model
m2_log <- lmer(log(y_min) ~ brand + species + bark + (1 | team) + (1 | saw_id), data = df)
m2 <- lmer(ly ~ brand + species + bark + (1 | team) + (1 | saw_id), data = df)
summary(m2)
drop1(m2, test = "F")

## Task i ------------------------------------------------------------------
# Task i: Compare the efficiency of the three saw brands
brand_eff <- emmeans(m2, pairwise ~ brand, type = "response")
brand_eff_normal <- emmeans(m2_log, pairwise ~ brand, type = "response")
print(brand_eff)
print(brand_eff_normal)
# Plot side by side
red_line <- geom_vline(xintercept = 0, linetype = "dashed", color = "red")
p1 <- plot(brand_eff_normal$emmeans) +  ggtitle("95% C.I of Brand Means") +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) # Centers the title
p2 <- plot(brand_eff$contrasts) +  ggtitle("95% C.I of Brand Contrast") +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) # Centers the title
e1t1plot <- p1 + p2 + red_line

## Task ii ------------------------------------------------------------------
# Task ii: Estimate the differences due to debarking
bark_eff <- emmeans(m2, pairwise ~ bark, type = "response")
bark_eff_normal <- emmeans(m2_log, pairwise ~ bark, type = "response")
print(bark_eff)

# Plot side by side
p1 <- plot(bark_eff_normal$emmeans) +  ggtitle("95% C.I of Bark Means") +
  theme(plot.title = element_text(hjust = 0.5)) # Centers the title
p2 <- plot(bark_eff$contrasts) +  ggtitle("95% C.I of Bark Contrast") +
  theme(plot.title = element_text(hjust = 0.5)) # Centers the title
e3t2plot <- p1 + p2 + red_line

## Task iii ------------------------------------------------------------------

# Task iii: Estimate the differences due to species
species_eff <- emmeans(m2, pairwise ~ species, type = "response")
species_eff_normal <- emmeans(m2_log, pairwise ~ species, type = "response")
print(species_eff)

# Plot side by side
p1 <- plot(species_eff$emmeans) +  ggtitle("95% C.I of Species Means") +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) # Centers the title
p2 <- plot(species_eff$contrasts) +  ggtitle("95% C.I of Species Contrast") +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) # Centers the title
e3t3plot <- p1 + p2 + red_line

