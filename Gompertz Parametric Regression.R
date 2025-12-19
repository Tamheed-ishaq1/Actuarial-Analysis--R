
# LAB: Gompertz Parametric Regression 
# Dataset: lung (survival package) 

library(survival) 
library(flexsurv) 

# Load and clean data 
data("lung") 
lung$event <- ifelse(lung$status == 2, 1, 0) 

# Fit Gompertz regression model 
fit_gomp <- flexsurvreg(Surv(time, event) ~ age + sex + ph.ecog, 
                        data = lung, 
                        dist = "gompertz") 

# Show summary 
summary(fit_gomp) 

# Plot survival curve 
plot(fit_gomp, type = "survival", main = "Gompertz Survival Curve") 

# Plot hazard curve 
plot(fit_gomp, type = "hazard", main = "Gompertz Hazard Curve") 

# Predictions at selected times 
newdata <- data.frame(age = c(50, 70), 
                      sex = c(1, 2), 
                      ph.ecog = c(1, 2)) 

predict(fit_gomp, newdata = newdata, type = "survival", t = c(100, 300, 600)) 

# Extract parameters 
fit_gomp$coefficients 
fit_gomp$res 
