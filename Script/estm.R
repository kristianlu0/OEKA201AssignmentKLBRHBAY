# estm.R

data <- read.csv("data-raw/csv/Dataset_in.csv")
head(data)

# Simple regression
enkelreg <- lm(MORT ~ INCC, data = data)
enkelreg
summary(enkelreg)

# Scatterplot
plot(data$INCC, data$MORT,
     main = "Scatterplot of mortality rate and income",
     xlab = "INCC",
     ylab = "MORT",
     pch = 19)
abline(enkelreg)

# Multiple regression
multireg <- lm(MORT ~ INCC + POV + EDU1 + EDU2 + ALCC + TOBC + HEXC + PHYS + URB + AGED, data = data)
summary(multireg)

