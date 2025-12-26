
# LAB: Gompertz Parametric Regression 
# Dataset: lung (survival package) 

library(survival) 
library(flexsurv) 

# Load and clean data 
lung 
lung$event <- ifelse(lung$status == 2, 1, 0) 

# Fit Gompertz regression model 
fit_gomp <- flexsurvreg(Surv(time,status) ~ age + sex + ph.ecog, 
                        data = lung, 
                        dist = "gompertz") 

# Show summary 
summary(fit_gomp) 

# Plot survival curve 
plot(fit_gomp, type = "survival", main = "Gompertz Survival Curve") 

# Plot hazard curve 
plot(fit_gomp, type = "hazard", main = "Gompertz Hazard Curve") 

# Predictions at selected times 
patients <- data.frame(age = c(50, 70), 
                      sex = c(1, 2), 
                      ph.ecog = c(1, 2)) 

summary(fit_gomp, newdata = patients,t = c(100, 300, 600)) 

# Extract parameters 
fit_gomp$coefficients 
fit_gomp$res 
