# Load Libaries
library(lme4)
library(multcomp)
library(lmerTest)
library(emmeans)
library(patchwork)




# Exercise 2 --------------------------------------------------------------

# Load the data
rats_data <- read.csv("data/in/RatsMcCullagh.csv")

# Ensure categorical variables are factors
rats_data$rat <- as.factor(rats_data$rat)
rats_data$site <- as.factor(rats_data$site)

## Visualization ##
# Calculate the raw means to overlay on the plots
means_site <- tapply(rats_data$logY, rats_data$site, mean, na.rm = TRUE)
means_treat <- tapply(rats_data$logY, rats_data$treat, mean, na.rm = TRUE)

# Set up a 1x2 grid for side-by-side boxplots
par(mfrow = c(1, 2))

# Boxplot: Raw log(Strength) across the 5 Sites
boxplot(logY ~ site, data = rats_data,
        xlab = 'Site on Back', ylab = 'log(Breaking Strength)',
        col = rainbow(5), main = "Strength by Site")
points(1:5, means_site, pch = 23, bg = "black") # Add diamond markers for the means

# Boxplot: Raw log(Strength) across the 2 Treatments
boxplot(logY ~ treat, data = rats_data,
        xlab = 'Treatment', ylab = 'log(Breaking Strength)',
        col = c("lightblue", "lightgreen"), main = "Strength by Treatment")
points(1:2, means_treat, pch = 23, bg = "black")

# Reset the plot grid to 1x1
par(mfrow = c(1, 1))

# Interaction Plot: Mean log(Strength) for Sites split by Treatment
with(rats_data, {
  interaction.plot(site, treat, logY,
                   type = "b", pch = 19, col = c("blue", "red"),
                   xlab = "Site on Back", ylab = "Mean log(Breaking Strength)",
                   main = "Interaction: Site vs. Treatment")
})

## Task i ----------------------------------------------------------------

## Modelling ##
# Initial model with interaction
m1 <- lmer(logY ~ site * treat + (1 | rat), data = rats_data)
summary(m1)
drop1(m1, test = "F")

# As the interaction p-value is > 0.05, reduce to the additive model
m2 <- lmer(logY ~ site + treat + (1 | rat), data = rats_data)
summary(m2)
anova(m2)
#drop1(m2) #equivelent

## Task ii ----------------------------------------------------------------

# Calculate the estimated marginal means for each site and run pairwise comparisons
# Check if site "5" has the lowest estimated logY and if it is significantly different from 1-4
site_means <- emmeans(m2, pairwise ~ site)
plot(site_means, comparisons = TRUE,
     xlab = "EMM",
     ylab = "Site on Back") + ggtitle("Estimated Marginal Mean of logY") +
  theme(plot.title = element_text(hjust = 0.5))
summary(site_means)

# Plot Site means 95% CI
mult_site <- glht(m2, linfct = mcp(site = "Tukey"))
par(mai=c(1, 1.25, 1, 0.5))
plot(mult_site, col=2:11,
     main = "95% family-wise confidence level: Site Differences")
par(mai=c(1, 1, 1, 1))


## Task iii ----------------------------------------------------------------
# Ignoring the Random Effect
# Fit a standard linear model without the (1 | rat) term to replicate the analytical error
lm_fit <- lm(logY ~ site + treat, data = rats_data)
summary(lm_fit)
# Extract the F-tests and p-values to compare against the anova(m_rats) results
drop1(lm_fit, test = "F")

summary(m2)
summary(lm_fit)

# 2. Compare Confidence Intervals
# The LM intervals will likely be narrower, indicating false confidence
confint(lm_fit)
confint(m2)

# 3. Calculate the ICC from the Mixed Model
# This proves why the observations are NOT independent
var_components <- as.data.frame(VarCorr(m2))
rat_var <- var_components$vcov[1]
resid_var <- var_components$vcov[2]

icc <- rat_var / (rat_var + resid_var)
print(paste("Intraclass Correlation:", round(icc, 4)))


# 2. Extract the 95% Confidence Intervals
ci_lm <- confint(lm_fit)["treatHypO2", ]
ci_lmm <- confint(m2, signames = FALSE)["treatHypO2", ]
lower_bounds <- c(ci_lm[1], ci_lmm[1])
upper_bounds <- c(ci_lm[2], ci_lmm[2])


# 4. Create the plot structure
plot(1:2, c(summary(lm_fit)$coefficients["treatHypO2", "Estimate"],summary(m2)$coefficients["treatHypO2", "Estimate"] ),
     ylim = range(c(lower_bounds, upper_bounds, 0)), # Ensure 0 is in the plot bounds
     xlim = c(0.5, 2.5), xaxt = "n", pch = 19, cex = 1.5,
     ylab = "Estimated Effect of Treatment (logY)", xlab = "",
     main = "Comparison of 95% Confidence Intervals\nLM vs LMM")

# Add  x-axis labels
axis(1, at = 1:2, labels = c("Standard LM", "Mixed Model"))
# Add the confidence interval lines (whiskers)
segments(x0 = 1:2, y0 = lower_bounds, x1 = 1:2, y1 = upper_bounds, lwd = 2)
# Add a horizontal line at 0 (No effect)
abline(h = 0, col = "red", lty = 2)




