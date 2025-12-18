
               # LAB PROGRAM: Kaplan–Meier                    
              #(Using ONLY survival package)                 

# Install if needed
# install.packages("survival")

library(survival)

# 1. Load dataset

# Using the built-in lung dataset
lung

# Convert event variable: (1=censored, 2=death) → (0/1)
lung$status2 <- ifelse(lung$status == 2, 1, 0)

# Inspect data
head(lung)

# 2. Create Surv object

S <- Surv(time = lung$time, event = lung$status)
S

# 3. Fit Kaplan–Meier estimator

km_fit <- survfit(S ~ 1)

# Show results
summary(km_fit)
summary(km_fit)$surv


# 4. Plot Kaplan–Meier survival curve

plot(
  km_fit,
  xlab = "Time (days)",
  ylab = "Survival Probability",
  main = "Kaplan–Meier Survival Curve",
  col = "darkgreen",
  lwd = 2,
  mark.time = TRUE           
)

# 5. Group-wise KM curves (Example: Sex)

lung$sex <- factor(lung$sex, labels = c("Male", "Female"))

km_fit_group <- survfit(Surv(time, status2) ~ sex, data = lung)

# Plot group curves
plot(
  km_fit_group,
  xlab = "Time (days)",
  ylab = "Survival Probability",
  main = "Kaplan–Meier Survival Curves by Sex",
  col = c("blue", "red"),
  lwd = 2,
  mark.time = TRUE
)

legend(
  "topright",
  legend = c("Male", "Female"),
  col = c("blue", "red"),
  lwd = 2
)


# 6. Log-rank test between groups

logrank_test <- survdiff(Surv(time, status2) ~ sex, data = lung)
logrank_test

#Kaplan Meier.txt
#Displaying Kaplan Meier.txt.