# Example 2. Grouped or clustered observations (Bulls).

# Example 13.7.1,Snedecor and Cochran, Statistical Methods, p 246
# Illustrative example on Fixed and Random effects
library(MASS)
library(emmeans) #For computing conditional/marginal means
library(multcomp)
library(ggplot2)

rm(list=(ls()))

options(width = 160, digits = 4)

#Read in the data
perc<-c(46,31,37,62,30,
        70, 59,
        52,44,57,40,67,64,70,
        47,21,70,46,14,
        42,64,50,69,77,81,87,
        35,68,59,38,57,76,57,29,60)
bull<-c(1,1,1,1,1,
        2,2,
        rep(3,7),
        rep(4,5),
        rep(5,7),
        rep(6, 9))

df<-data.frame(bull, perc)
df
str(df)
df$bull<- as.factor(df$bull)
str(df)

tapply(df$perc, df$bull, mean)

library(dplyr)

# Calculate bull-specific means
means <- df %>%
  group_by(bull) %>%
  summarise(mean_perc = mean(perc), .groups = "drop")


# Exploratory analysis

# Plot
plot_bulls <- ggplot(df, aes(x = factor(bull), y = perc)) +
  # geom_boxplot(fill = "lightblue") +
  geom_boxplot(
    fill = 2:7,
    whisker.linetype = "dashed",
    whisker.linewidth = .4,
    staplewidth = .4
    ) +
  # geom_boxplot(aes(fill = bull)) +
  geom_point(
    data = means,
    aes(y = mean_perc),
    shape = 21,
    size = 2,
    fill = "red"
  ) +
  labs(
    x = "Bull",
    y = "Percentage of conceptions"
  ) +
  theme_light()

plot_observations <- ggplot(df, aes(factor(bull), y = perc)) +
  geom_jitter(
    width = 0.1,
    colour = bull,
    size = 2
    ) +
  geom_point(
    data = means,
    aes(y = mean_perc),
    shape = 22,
    size = 2,
    fill = "red"
  ) +
  labs(
    x = "Bull",
    y = "Percentage of conceptions"
  ) +
  theme_light()

(mean<-mean(df$perc))
(means<-with(df,tapply(perc, bull, mean)))


(s2<-var(df$perc))
#(s2_i<-(vars<-tapply(df$perc, df$bull, var)))
(s2_i<-tapply(df$perc, df$bull, var))
(n_i<-table(df$bull))

library(tidyverse)
#
# # Exploratory plots
# par(mfrow=c(1,2))
#   with(df, boxplot(perc ~ bull, xlab='bull', ylab='% of conceptions', las=1, col=2:5))
#     #alternatively:
#     # boxplot(df$perc ~ df$bull, xlab='bull', ylab='% of conceptions', las=1, col=2:5)
#     points(1:6, means, pch = 23, cex = 0.95, bg = "red")
#
#
#   with(df, stripchart(perc ~ bull, xlab='bull', ylab='% of conceptions',
#                       vertical=TRUE,cex=1.2,pch=16, las=1, col=2:5))
#   points(1:6, means, pch = 17, cex = 1.5, bg = "black")
#   abline(h=mean(df$perc), lty = 2)
#
#
#   par(mfrow=c(1,1))
#
#

# Fitting two models

library(lme4)

m0 <- lm(perc ~ 1, data = df)

m1 <- lmer(perc ~ (1 | bull),
           data = df,
           REML = FALSE)

VarCorr(m1)
fixef(m1)


# Model 1. Simple linear model,
# It ignores that the percentages of the same bull are correlated observations.
m_lm_null <- lm(perc~1,data=df)
anova(m_lm_null)
drop1(m_lm_null, test="F")
summary(m_lm_null)
#estimated mean = 53.54 = observed mean due to the balanced data
confint(m_lm_null, level=0.95)
cbind(coef(m_lm_null), confint(m_lm_null, level=0.95))

mean(df$perc)
sqrt(var(df$perc)/35)


# Model 2. Random effects model with bulls as random effect

# Now we assume that the six bulls are a random sample from a large population
# of bulls and that one wishes to estimate the average percentage of conceptions,
# and that the percentages of conceptions of the bulls are on average the same
# in the population.
# The observations within each bull are expected to be correlated.
# The observations between bulls are expected to be uncorrelated.

#install.packages("lme4")
library(lme4)

m_lmer<-lmer(perc ~ (1|bull), data=df)
summary(m_lmer) #A lot of information!

estimate <- m_lmer@beta

# Calculate confidence intervals
confint(m_lmer)
# Calculate profile likelihood confidence intervals
confint(m_lmer, method = "profile")

# Calculate confidence interval
conf <- confint(m_lmer, parm = "(Intercept)", method = "profile")

estimate <- paste0(round(m_lmer@beta, 2), "% (95% CI: ", round(conf[1,1], 2), " - ", round(conf[1,2], 2), ")")

VarCorr(m_lmer)

vc <- as.data.frame(VarCorr(m_lmer))

sigma_within <- vc$vcov[2]
sigma_bull  <- vc$vcov[1]

ICC <- sigma_bull / (sigma_bull + sigma_within)
ICC
h <- 4 * sigma_bull /
  (sigma_bull + sigma_within)

round(h * 100, 0)

76.8+248.7 #=325.5

76.8/(76.8+248.7) #= 0.24
#variance due to the bulls:76.8
# Total variance= 76.8+248.7=325.5

#Fixed effects: none!
drop1(m_lmer, test="Chisq")
anova(m_lmer)

# Random effects
#See confidence intervals:
confint(m_lmer, signames=F)

cbind(coef(m_lmer), confint(m_lmer, level=0.95))
cbind(coef(m_lm_null), confint(m_lm_null, level=0.95))

#Notice that the CI are wider in the random effects setting than in the fixed.

#-----end--------
library(lattice)
# See the ranking og the percentage of conceptions of the bulls.

dotplot(ranef(m_lmer, condVar=TRUE), strip = FALSE)

predict(m_lmer)
par(mai=c(1,1,1,1))
plot(predict(m_lmer), m_lm_null$fitted.values, col=df$bull, pch=19)
abline(0,1)

str(ranef(m_lmer))

par(mfrow=c(1,2))
qqnorm(ranef(m_lmer)$bull[,"(Intercept)"], main="Random effects", pch=19, cex=.5)
qqline(ranef(m_lmer)$bull[,"(Intercept)"])
qqnorm(resid(m_lmer), main="Residuals",pch=19, cex=.5)
qqline(resid(m_lmer))
par(mfrow=c(1,1))


#----end----
