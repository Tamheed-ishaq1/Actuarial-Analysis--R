###############################################
# LAB: Weibull Parametric Regression
# Dataset: lung (survival package)
###############################################

library(survival)
library(flexsurv)

# Load and clean dataset
data("lung")
lung$event <- ifelse(lung$status == 2, 1, 0)  # convert to 0/1 event

head(lung)

# Fit Weibull regression model
fit_weib <- flexsurvreg(Surv(time, event) ~ age + sex + ph.ecog,
                        data = lung,
                        dist = "weibull")

# Summary output
summary(fit_weib)

# Plot survival curve
plot(fit_weib, type = "survival", main = "Weibull Survival Curve")

# Plot hazard curve
plot(fit_weib, type = "hazard", main = "Weibull Hazard Curve")

# Predictions for selected profiles
newdata <- data.frame(age = c(50, 70),
                      sex = c(1, 2),
                      ph.ecog = c(1, 2))

predict(fit_weib, newdata = newdata, type = "survival", t = c(100, 300, 600))

# Extract model parameters
fit_weib$coefficients
fit_weib$res
