
#           LAB PROGRAM: Nelson–Aalen Estimator               #
#                  (Using ONLY survival package)              #


# Install if needed
# install.packages("survival")

library(survival)

# 1. Load dataset

# We use the built-in 'lung' dataset
lung

# Convert event status: (1=censored, 2=death) → (0/1)
lung$status <- ifelse(lung$status == 2, 1, 0)

# View first rows
head(lung)
View(lung)
# 2. Create Surv object

S <- Surv(time = lung$time, event = lung$status)
S


# 3. Fit Nelson–Aalen estimator

# type = "fh" → Fleming–Harrington, equivalent to Nelson–Aalen
na_fit <- survfit(S ~ 1, type = "fh")

# Show estimation result
summary(na_fit)
summary(na_fit)$cumhaz #to check cumulative hazard

# 4. Plot cumulative hazard H(t)

plot(
  na_fit,
  fun = "cumhaz",              # cumulative hazard,If you remove fun = "cumhaz",
                              #R will plot the Kaplan–Meier survival curve by default.
  xlab = "Time (days)",
  ylab = "Cumulative Hazard H(t)",
  main = "Nelson–Aalen Cumulative Hazard Estimate",
  col = "blue",
  lwd = 1
)


# 5. Group-wise Nelson–Aalen estimator (Example: Sex)

lung$sex <- factor(lung$sex, labels = c("Male", "Female"))

na_fit_group <- survfit(Surv(time, status) ~ sex,
                        data = lung,
                        type = "fh")
summary(na_fit_group)

# Plot group curves
plot(
  na_fit_group,
  fun = "cumhaz",
  col = c("red", "blue"),
  lwd = 2,
  xlab = "Time (days)",
  ylab = "Cumulative Hazard H(t)",
  main = "Nelson–Aalen Cumulative Hazard by Sex"
)

legend(
  "topleft",
  legend = c("Male", "Female"),
  col = c("red", "blue"),
  lwd = 2
)

