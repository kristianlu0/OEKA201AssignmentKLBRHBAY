# estm.R

df <- read.csv("data-raw/csv/Dataset_in.csv")
head(df)

# Simple regression
enkelreg <- lm(MORT ~ INCC, data = df)
enkelreg
summary(enkelreg)

# Scatterplot
plot(df$INCC, df$MORT,
     main = "Scatterplot of mortality rate and income",
     xlab = "INCC",
     ylab = "MORT",
     pch = 19)
abline(enkelreg)

# Multiple regression
multireg <- lm(MORT ~ INCC + POV + EDU1 + EDU2 + ALCC + TOBC + HEXC + PHYS + URB + AGED, data = df)
summary(multireg)

