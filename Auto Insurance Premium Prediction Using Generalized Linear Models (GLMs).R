library(insuranceData)
library(tidyverse)
data("AutoCollision")
head(AutoCollision)
summary(AutoCollision)
str(AutoCollision)
#Check average claims by group:
aggregate(Claim_Count~Age+Vehicle_Use,data=AutoCollision,mean)
#Check severity pattern:
aggregate(Severity~Age+Vehicle_Use,
          data=AutoCollision,mean)
#“Take both variables as separate columns”
aggregate(cbind(Severity,Claim_Count)~Age+Vehicle_Use,data=AutoCollision,mean)

mean(AutoCollision$Severity)
#This gives:
#Average claim cost across ALL groups

#Interpretation:

#Higher Claim_Count → risky group
#Higher Severity → expensive group
#Step 3: Frequency Modeling (Poisson Regression)
freq_model <- glm(Claim_Count ~ Age + Vehicle_Use, 
                  family = poisson(link = "log"), 
                  data = AutoCollision)

summary(freq_model)
#Step 4: Severity Modeling (Gamma Regression)
sev_model <- glm(Severity ~ Age + Vehicle_Use, 
                 family = Gamma(link = "log"), 
                 data = AutoCollision)

summary(sev_model)

#Step 5: Pure Premium Calculation
#Actuarial pricing formula:
#Pure Premium=Frequency×Severity
AutoCollision$freq_pred <- predict(freq_model, type = "response")
AutoCollision$sev_pred<- predict(sev_model, type="response")

AutoCollision$pure_premium <- AutoCollision$freq_pred * AutoCollision$sev_pred

head(AutoCollision)