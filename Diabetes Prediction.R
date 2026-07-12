#Diabetes Prediction using R
#1.Loading the Required Libraries and Dataset
library(readr)
library(caret)
library(e1071)
library(caTools)
dataDI<-read.csv("diabetes.csv")
View(dataDI)
head(dataDI)

#2.Data Preprocessing
summary(dataDI)

colSums(is.na(dataDI))
X<-dataDI[,1:8]
Y<- dataDI[,9]

scaled_X<- data.frame(scale(X))
scaled_data<-cbind(scaled_X,Y)

X<-scaled_data[,1:8]
Y<-scaled_data[,9]

set.seed(123)
sample<-sample.split(Y,SplitRatio=0.7)
table(sample)

X_train<-X[sample==TRUE,]
Y_train<-Y[sample==TRUE]
X_test<-X[sample==FALSE,]
Y_test<-Y[sample==FALSE]

#3.Exploratory Data Analysis (EDA)
#3.1Correlation Heatmap

library(ggplot2)
library(reshape2)
correlation_matrix<-cor(dataDI)
correlation_melted<- melt(correlation_matrix)

ggplot(correlation_melted,aes(Var1,Var2,fill=value))+
  geom_tile(color="white")+
  scale_fill_gradient2(low="blue",high="red",mid="white",midpoint=0,
                       limit=c(-1,1),name="correlation")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle = 45,hjust = 1))+
  labs(title="correlation_heatMap",x="features",y="features")

#3.2 Distribution of Diabetes Outcomes #BAR
outcome_counts<-table(dataDI$Outcome)
outcome_df<-data.frame(Outcome=names(outcome_counts),
                       count=as.numeric(outcome_counts))

ggplot(outcome_df,aes(x=Outcome,y=count))+
  geom_bar(stat = "identity",fill="steelblue")+
  labs(title = "Distribution of Diabetes outcomes",x="Outcome",y="count")+
  theme_minimal()+
  theme(axis.text.x=element_text(size=13),
        axis.text.y=element_text(size=13),
        axis.title=element_text(size=13),
        plot.title=element_text(size=13,face="bold"))

#3.3 Histograms with Outcome Split
diabetes_subset<-dataDI[,c("Pregnancies","Glucose","BloodPressure","BMI",
                           "Age","Outcome")]
ggplot(diabetes_subset, aes(x = Pregnancies, fill = factor(Outcome))) +
  geom_histogram(position = "identity", bins = 30, alpha = 0.7) +
  scale_fill_manual(values = c("#4E79A7", "#F28E2B")) +
  labs(title = "Distribution of Pregnancies by Outcome",
       fill = "Outcome") +
  facet_wrap(~Outcome, scales = "free_y") +
  theme_minimal()
  
#3.4Boxplot for BMI by Outcome

ggplot(dataDI, aes(x = factor(Outcome), y = BMI, fill = factor(Outcome))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#4E79A7", "#F28E2B")) +
  labs(title = "BMI Distribution by Outcome",
       x = "Outcome",
       y = "BMI",
       fill = "Outcome") +
  theme_minimal()

#4.Logistic Regression model to predict diabetes based on medical features.
log_model<-glm(Y_train~ .,data=X_train, family=binomial)
summary(log_model)

#5.valuating the Model (good predictions or bad predictions?")
Predictions<-predict(log_model,newdata=X_test,type="response")
Predictions<-factor(ifelse(Predictions>0.5,1,0),
                    levels=levels(as.factor(Y_test)))
confusionMatrix(Predictions, as.factor(Y_test))

#6.Making Predictions using the Model
predict_diabetes <- function(pregnancies, glucose, bloodpressure, skinthickness, 
                             insulin, bmi, diabetespedigreefunction, age) {
  input_data <- data.frame(
    Pregnancies = pregnancies,
    Glucose = glucose,
    BloodPressure = bloodpressure,
    SkinThickness = skinthickness,
    Insulin = insulin,
    BMI = bmi,
    DiabetesPedigreeFunction = diabetespedigreefunction,
    Age = age
  )
  input <- as.data.frame(input_data)
  Prediction <- predict(log_model, newdata = input, type = "response")
  Prediction <- factor(ifelse(Prediction > 0.5, 1, 0), 
                       levels = levels(as.factor(Prediction)))
  
  return(Prediction)

}
new_patient <- data.frame(
  pregnancies = 6,
  glucose = 148,
  bloodpressure = 72,
  skinthickness = 35,
  insulin = 0,
  bmi = 33.6,
  diabetespedigreefunction = 0.627,
  age = 50
)

Prediction <- predict_diabetes(
  new_patient$pregnancies,
  new_patient$glucose,
  new_patient$bloodpressure,
  new_patient$skinthickness,
  new_patient$insulin,
  new_patient$bmi,
  new_patient$diabetespedigreefunction,
  new_patient$age
)

if (any(Prediction == 1)) {
  cat("Based on the model's prediction, there is a higher chance of diabetes.")
} else {
  cat("Based on the model's prediction, the risk of diabetes appears lower.")
}







