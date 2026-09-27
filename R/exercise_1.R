# Assignment 1 - Q3
# Load Libaries
library(lme4)
library(multcomp)
library(lmerTest)
library(emmeans)
library(patchwork)


# Exercise 1 --------------------------------------------------------------

# 1. Input the data
perc <- c(46, 31, 37, 62, 30,
          70, 59,
          52, 44, 57, 40, 67, 64, 70,
          47, 21, 70, 46, 14,
          42, 64, 50, 69, 77, 81, 87,
          35, 68, 59, 38, 57, 76, 57, 29, 60)

# Create a grouping factor for the 6 bulls
bull <- as.factor(c(rep(1, 5), rep(2, 2), rep(3, 7),
                    rep(4, 5), rep(5, 7), rep(6, 9)))

df <- data.frame(bull, perc)

# 2. Exploratory Analysis
# Calculate raw means and variances per bull
(means <- tapply(df$perc, df$bull, mean))
(variances <- tapply(df$perc, df$bull, var))

# Visualize between-bull vs within-bull variability
boxplot(perc ~ bull, data = df, xlab = 'Bull', ylab = '% of Conceptions', las=1, col=2:7, main = "Box Plot of Bull Conceptions Rates")
points(1:6, means, pch = 23, bg = "black")

# Fit the linear mixed model
m1 <- lmer(perc ~ 1 + (1|bull), data = df)

# Display variance components and the fixed intercept estimate
summary(m1)

# Generate the 95% confidence intervals for both fixed and random effects
confint(m1, signames=FALSE)

# Extract variance components from the model
var_components <- as.data.frame(VarCorr(m_lmer))
bull_var <- var_components$vcov[1]   # 76.8
resid_var <- var_components$vcov[2]  # 248.7

# Calculate the Heritability parameter
heritability <- (4 * bull_var) / (bull_var + resid_var)









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

