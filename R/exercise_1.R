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

drop1(m1, test = "F")

# Calculate confidence interval
conf <- confint(m1, parm = "(Intercept)", method = "profile")
estimate <- paste0(round(m1@beta, 2), "%, 95% CI [", round(conf[1,1], 2), "%, ", round(conf[1,2], 2), "%]")

# Display variance components and the fixed intercept estimate
summary(m1)

vc <- as.data.frame(VarCorr(m1))

sigma_within <- vc$vcov[2]
sigma_bull  <- vc$vcov[1]

# Generate the 95% confidence intervals for both fixed and random effects
conf <- confint(m1, signames=FALSE)

conf_within <- paste0("95% CI [", sprintf("%.2f", conf[1, 1]), "%, ", round(conf[1,2], 2), "%]")
conf_bull <- paste0("95% CI [", round(conf[2,1], 2), "%, ", round(conf[2,2], 2), "%]")

# Extract variance components from the model
var_components <- as.data.frame(VarCorr(m1))
bull_var <- var_components$vcov[1]   # 76.8
resid_var <- var_components$vcov[2]  # 248.7



# Calculate the Heritability parameter
heritability <- (4 * bull_var) / (bull_var + resid_var)







