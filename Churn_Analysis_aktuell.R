# Clear environment  --------------------------------------------------------
rm(list = ls())

# Load packages -----------------------------------------------------------
library(ggplot2)
library(ggthemes)
library(dplyr)
library(MASS)
library(faraway)
library(mice)
library(tidyr)
library(readr)
library(e1071)
library(nnet)
library(keras)
library(lime)
library(caret)
library(devtools)
library(adabag)
library(party)
library(randomForest)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(partykit)
library(partykit)
library(stats)

# Data Import & Inspection -------------------------------------------------------------
data <- read.csv("Telco_Customer.csv",header=T) 

summary(data)
str(data)
glimpse(data)

# Exploratory Data Analysis ---------------------------------------------------------------------
#Plot categorical features
data %>%
  mutate(gender = as.character(gender), SeniorCitizen = as.character(SeniorCitizen), Partner = as.character(Partner), Dependents = as.character(Dependents), PhnoneService = as.character(PhoneService), MultipleLines = as.character(MultipleLines), InternetService = as.character(InternetService), OnlineSecurity = as.character(OnlineSecurity), OnlineBackup = as.character(OnlineBackup), DeviceProtection = as.character(DeviceProtection), TechSupport = as.character(TechSupport), StreamingTV = as.character(StreamingTV), StreamingMovies = as.character(StreamingMovies), Contract = as.character(Contract), PaperlessBilling = as.character(PaperlessBilling), PaymentMethod = as.character(PaymentMethod), Churn = as.character(Churn)) %>% 
  select_if(is.character) %>% 
  dplyr::select(Churn, everything()) %>%
  gather(x, y, gender:PaymentMethod) %>%
  count(Churn, x, y) %>%
  ggplot(aes(x = y, y = n, fill = Churn, color = Churn)) +
  facet_wrap(~ x, ncol = 4, scales = "free") +
  geom_bar(stat = "identity", alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") +
  scale_color_tableau() +
  scale_fill_tableau()

#Plot numerical features
data %>%
  dplyr::select(-customerID) %>%
  dplyr::select(Churn, MonthlyCharges, tenure, TotalCharges) %>%
  gather(x, y, MonthlyCharges:TotalCharges) %>%
  ggplot(aes(x = y, fill = Churn, color = Churn)) +
  facet_wrap(~ x, ncol = 3, scales = "free") +
  geom_density(alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") +
  scale_color_tableau() +
  scale_fill_tableau()

#Checking for outliers
boxplot(data$tenure)$out
boxplot(data$MonthlyCharges)$out
boxplot(data$TotalCharges)$out

# Data Preprocessing  -----------------------------------------------------
#Remove customer ID as it doesn't provide information
data <- data %>% dplyr::select(-customerID)

#Dealing with missing values
md.pattern(data)

#Impute missing data
predictorMatrix <- matrix(0,nrow = ncol(data), ncol = ncol(data))
predictorMatrix[1:19,20] <- 1
diag(predictorMatrix)<- 0

data_imputed <- mice(data, predictorMatrix = predictorMatrix, m=3, maxit = 50, seed = 500)
summary(data_imputed)
data_imputed <- mice::complete(data_imputed,3)

#Cleaning the Categorical features
data_imputed <- data.frame(lapply(data_imputed, function(x){
  gsub("No internet service", "No", x)
}))

data_imputed <- data.frame(lapply(data_imputed, function(x){
  gsub("No phone service", "No", x)
}))

#Change format of continuous features
columns <- c("tenure", "MonthlyCharges", "TotalCharges")
data_imputed[columns] <- sapply(data_imputed[columns], as.numeric)

data_imputed_int <- data_imputed[,c("tenure", "MonthlyCharges", "TotalCharges")]
data_imputed_int <- data.frame(data_imputed_int)

#Add new variable for tenure (bins)
data_imputed <- data_imputed %>% mutate(tenure_bin = tenure)
data_imputed$tenure_bin[data_imputed$tenure_bin>=0 & data_imputed$tenure_bin<=12] <- "0-1 year"
data_imputed$tenure_bin[data_imputed$tenure_bin>12 & data_imputed$tenure_bin<=24] <- "1-2 year"
data_imputed$tenure_bin[data_imputed$tenure_bin>24 & data_imputed$tenure_bin<=36] <- "2-3 year"
data_imputed$tenure_bin[data_imputed$tenure_bin>36 & data_imputed$tenure_bin<=48] <- "3-4 year"
data_imputed$tenure_bin[data_imputed$tenure_bin>48 & data_imputed$tenure_bin<=60] <- "4-5 year"
data_imputed$tenure_bin[data_imputed$tenure_bin>60 & data_imputed$tenure_bin<=82] <- "5-6 year"

#Inspect churn pattern due to tenure 
ggplot(data_imputed, aes(tenure_bin, fill = tenure_bin)) + geom_bar()

#Create dummy variables
data_imputed_cat <- data_imputed[,-c(5,18,19)]
data_imputed_cat <- data.frame(sapply(data_imputed_cat,function(x){data.frame(model.matrix(~x-1,data=data_imputed_cat))[,-1]
}))

#Creating final dataset
data_final <- cbind(data_imputed_int,data_imputed_cat)
head(data_final)

#Add Churn_String variable
data_final$Churn_String <- ifelse(data_final$Churn==1,"Churn","No Churn")
data_final$Churn_String <- as.factor(data_final$Churn_String)

colnames(data_final) <- c("Tenure","Monthly_Charges","Total_Charges","Gender","Senior_Citizen","Partner","Dependents","Phone_Service","Multiple_Lines","Internet_Service_Fiber.optic","Internet_Service_No","Online_Security","Online_Backup","Device_Protection","Tech_Support","Streaming_TV","Streaming_Movies","Contract_1year","Contract_2year","Paperless_Billing","Payment_Method_Credit.card","Payment_Method_Electronic.check","Payment_Method_Mailed.check","Churn","Tenure_bin_1_2years","Tenure_bin_2_3years","Tenure_bin_3_4years","Tenure_bin_4_5years","Tenure_bin_5_6years","Churn_String")
data_final <- data_final[,c("Churn","Churn_String","Monthly_Charges","Total_Charges","Gender","Senior_Citizen","Partner","Dependents","Tenure","Tenure_bin_1_2years","Tenure_bin_2_3years","Tenure_bin_3_4years","Tenure_bin_4_5years","Tenure_bin_5_6years","Phone_Service","Multiple_Lines","Internet_Service_Fiber.optic","Internet_Service_No","Online_Security","Online_Backup","Device_Protection","Tech_Support","Streaming_TV","Streaming_Movies","Contract_1year","Contract_2year","Paperless_Billing","Payment_Method_Credit.card","Payment_Method_Electronic.check","Payment_Method_Mailed.check")]

# Analysis ----------------------------------------------------------------
#Create training and test set
set.seed(123)
data_final <- data_final[sample(1:nrow(data_final), nrow(data_final), replace = F),]
training <- data_final[1:floor(nrow(data_final)*.75), ]
evaluation <- data_final[(floor(nrow(data_final)*.75)+1):nrow(data_final), ]

#Create function for LiftPlot
makeLiftPlot <- function(Prediction, Evaluate, ModelName){
  iPredictionsSorted <- sort(Prediction,index.return=T,decreasing=T)[2]$ix #extract the index order according to predicted retention
  CustomersSorted <- Evaluate$Churn_String[iPredictionsSorted] #sort the true behavior of customers according to predictions
  SumChurnReal<- sum(Evaluate$Churn_String == "Churn") #total number of real churners in the evaluation set
  CustomerCumulative=seq(nrow(Evaluate))/nrow(Evaluate) #cumulative fraction of customers
  ChurnCumulative=apply(matrix(CustomersSorted=="Churn"),2,cumsum)/SumChurnReal #cumulative fraction of churners
  ProbTD = sum(CustomersSorted[1:floor(nrow(Evaluate)*.1)]=="Churn")/floor(nrow(Evaluate)*.1) #probability of churn in 1st decile
  ProbOverall = SumChurnReal / nrow(Evaluate) #overall churn probability
  TDL = ProbTD / ProbOverall
  GINI = sum((ChurnCumulative-CustomerCumulative)/(t(matrix(1,1,nrow(Evaluate))-CustomerCumulative)),na.rm=T)/nrow(Evaluate)
  plot(CustomerCumulative,ChurnCumulative,type="l",main=paste("Lift curve of", ModelName),xlab="Cumulative fraction of customers (sorted by predicted churn probability)",ylab="Cumulative fraction of churners")
  lines(c(0,1),c(0,1),col="blue",type="l",pch=22, lty=2)
  legend(.66,.2,c("According to model","Random selection"),cex=0.8,  col=c("black","blue"), lty=1:2)
  text(0.15,1,paste("TDL = ",round(TDL,2), "; GINI = ", round(GINI,2) ))
  return(data.frame(TDL,GINI))
}

#Run LOGIT model
Churn_Logit <- glm(Churn ~ Monthly_Charges+Total_Charges+Gender+Senior_Citizen+Partner+Dependents+Tenure+Tenure_bin_1_2years+Tenure_bin_2_3years+Tenure_bin_3_4years+Tenure_bin_4_5years+Tenure_bin_5_6years+Phone_Service+Multiple_Lines+Internet_Service_Fiber.optic+Internet_Service_No+Online_Security+Online_Backup+Device_Protection+Tech_Support+Streaming_TV+Streaming_Movies+Contract_1year+Contract_2year+Paperless_Billing+Payment_Method_Credit.card+Payment_Method_Electronic.check+Payment_Method_Mailed.check,data = training, family = "binomial")
summary(Churn_Logit)

evaluation$predictionlogit <- predict(Churn_Logit, newdata=evaluation, type = "response")
evaluation$predictionlogitclass[evaluation$predictionlogit>.5] <- 1
evaluation$predictionlogitclass[evaluation$predictionlogit<=.5] <- 0
 
evaluation$correctlogit <- evaluation$predictionlogitclass == evaluation$Churn
print(paste("% of predicted classifications correct", mean(evaluation$correctlogit)))

LogitOutput <- makeLiftPlot(evaluation$predictionlogit,evaluation,"Logit")
LogitOutput$PercCorrect <- mean(evaluation$correctlogit)*100

#Only include tenure_0-1 & tenure_1-2 as they show the highest count for churn 
#create & add a variable for tenure_0-1 as it has been reference level beforehand
training$Tenure_bin_0_1_years <- ifelse(training$Tenure_bin_1_2years==0 & training$Tenure_bin_2_3years==0 & training$Tenure_bin_3_4years==0 & training$Tenure_bin_4_5years==0 & training$Tenure_bin_5_6years==0,1,0)
evaluation$Tenure_bin_0_1_years <- ifelse(evaluation$Tenure_bin_1_2years==0 & evaluation$Tenure_bin_2_3years==0 & evaluation$Tenure_bin_3_4years==0 & evaluation$Tenure_bin_4_5years==0 & evaluation$Tenure_bin_5_6years==0,1,0)

Churn_Logit_2 <- glm(Churn ~ Monthly_Charges+Total_Charges+Gender+Senior_Citizen+Partner+Dependents+Tenure+Tenure_bin_0_1_years+Tenure_bin_5_6years+Phone_Service+Multiple_Lines+Internet_Service_Fiber.optic+Internet_Service_No+Online_Security+Online_Backup+Device_Protection+Tech_Support+Streaming_TV+Streaming_Movies+Contract_1year+Contract_2year+Paperless_Billing+Payment_Method_Credit.card+Payment_Method_Electronic.check+Payment_Method_Mailed.check,data = training, family = "binomial")
summary(Churn_Logit_2)

evaluation$predictionlogit_2 <- predict(Churn_Logit_2, newdata=evaluation, type = "response")
evaluation$predictionlogitclass_2[evaluation$predictionlogit>.5] <- 1
evaluation$predictionlogitclass_2[evaluation$predictionlogit<=.5] <- 0

evaluation$correctlogit_2 <- evaluation$predictionlogitclass_2 == evaluation$Churn
print(paste("% of predicted classifications correct", mean(evaluation$correctlogit_2)))

vif(Churn_Logit_2)

LogitOutput <- makeLiftPlot(evaluation$predictionlogit,evaluation,"Logit")
LogitOutput$PercCorrect <- mean(evaluation$correctlogit)*100

#Use AIC for variable selection --> find subset of variables that gives the best performing model
Churn_Logit_3 <- stepAIC(Churn_Logit_2, direction="both")
summary(Churn_Logit_3)

evaluation$predictionlogit_3 <- predict(Churn_Logit_3, newdata=evaluation, type = "response")
evaluation$predictionlogitclass_3[evaluation$predictionlogit_3>.5] <- 1
evaluation$predictionlogitclass_3[evaluation$predictionlogit_3<=.5] <- 0

evaluation$correctlogit_3 <- evaluation$predictionlogitclass_3 == evaluation$Churn
print(paste("% of predicted classifications correct", mean(evaluation$correctlogit_3)))

vif(Churn_Logit_3)
LogitOutput <- makeLiftPlot(evaluation$predictionlogit,evaluation,"Logit")
LogitOutput$PercCorrect <- mean(evaluation$correctlogit)*100

#PCA Analysis
training_PCA <- training %>% select_if(is.numeric) %>% dplyr::select(-Tenure_bin_0_1_years)
pc <- princomp(training_PCA,cor = TRUE, score = TRUE)
summary(pc)

plot(pc, type="l")
biplot(pc)

pc$loadings
pc$scores
#Has to be finished


#Nearest Neighbour
Churn_NearN <- knn3(Churn_String ~ Monthly_Charges+Total_Charges+Gender+Senior_Citizen+Partner+Dependents+Tenure+Tenure_bin_1_2years+Tenure_bin_2_3years+Tenure_bin_3_4years+Tenure_bin_4_5years+Tenure_bin_5_6years+Phone_Service+Multiple_Lines+Internet_Service_Fiber.optic+Internet_Service_No+Online_Security+Online_Backup+Device_Protection+Tech_Support+Streaming_TV+Streaming_Movies+Contract_1year+Contract_2year+Paperless_Billing+Payment_Method_Credit.card+Payment_Method_Electronic.check+Payment_Method_Mailed.check, data=training, k=10, prob = FALSE, use.all = TRUE)
summary(Churn_NearN)

evaluation$predictionNearN <- predict(Churn_NearN, newdata = evaluation, type="class")
evaluation$correctNearN <- evaluation$predictionNearN == evaluation$Churn_String

print(paste("% of predicted classifications correct", mean(evaluation$correctNearN)))
evaluation$probabilitiesNearN <- predict(Churn_NearN, newdata = evaluation, type="prob")[,1]

NearNOutput <- makeLiftPlot(evaluation$probabilitiesNearN,evaluation,"Nearest Neighbour")
NearNOutput$PercCorrect <- mean(evaluation$correctNearN)*100


#Naive Bayes
classifier<-naiveBayes(Churn_String ~ Monthly_Charges+Total_Charges+Gender+Senior_Citizen+Partner+Dependents+Tenure+Tenure_bin_1_2years+Tenure_bin_2_3years+Tenure_bin_3_4years+Tenure_bin_4_5years+Tenure_bin_5_6years+Phone_Service+Multiple_Lines+Internet_Service_Fiber.optic+Internet_Service_No+Online_Security+Online_Backup+Device_Protection+Tech_Support+Streaming_TV+Streaming_Movies+Contract_1year+Contract_2year+Paperless_Billing+Payment_Method_Credit.card+Payment_Method_Electronic.check+Payment_Method_Mailed.check,data=training) 
naiveBayesCorrect <- table(predict(classifier, evaluation[,3:30]), evaluation[,2])

print(paste("% of predicted classifications correct", sum(diag(naiveBayesCorrect))/sum(naiveBayesCorrect))) 
naiveBayesPredict <- predict(classifier, evaluation[,3:30], type = "raw")

naiveBayesOutput <- makeLiftPlot(naiveBayesPredict[,1],evaluation,"Naive Bayes")
naiveBayesOutput$PercCorrect <- sum(diag(naiveBayesCorrect))/sum(naiveBayesCorrect)*100


#Run SVM model
Churn_SVM <- svm(Churn_String ~ Monthly_Charges+Total_Charges+Gender+Senior_Citizen+Partner+Dependents+Tenure+Tenure_bin_1_2years+Tenure_bin_2_3years+Tenure_bin_3_4years+Tenure_bin_4_5years+Tenure_bin_5_6years+Phone_Service+Multiple_Lines+Internet_Service_Fiber.optic+Internet_Service_No+Online_Security+Online_Backup+Device_Protection+Tech_Support+Streaming_TV+Streaming_Movies+Contract_1year+Contract_2year+Paperless_Billing+Payment_Method_Credit.card+Payment_Method_Electronic.check+Payment_Method_Mailed.check, data = training, probability=T, type="C")
summary(Churn_SVM)

evaluation$predictionSVM <- predict(Churn_SVM, newdata=evaluation, probability = T)
evaluation$correctSVM <- evaluation$predictionSVM == evaluation$Churn_String
print(paste("% of predicted classifications correct", mean(evaluation$correctSVM)))

# Extract the class probabilities
evaluation$probabilitiesSVM <- attr(evaluation$predictionSVM,"probabilities")[,2]
SVMOutput <- makeLiftPlot(evaluation$probabilitiesSVM,evaluation,"SVM")
SVMOutput$PercCorrect <- mean(evaluation$correctSVM)*100


# Run Neural Network model
Churn_NNet <- train(Churn_String ~ Monthly_Charges+Total_Charges+Gender+Senior_Citizen+Partner+Dependents+Tenure+Tenure_bin_1_2years+Tenure_bin_2_3years+Tenure_bin_3_4years+Tenure_bin_4_5years+Tenure_bin_5_6years+Phone_Service+Multiple_Lines+Internet_Service_Fiber.optic+Internet_Service_No+Online_Security+Online_Backup+Device_Protection+Tech_Support+Streaming_TV+Streaming_Movies+Contract_1year+Contract_2year+Paperless_Billing+Payment_Method_Credit.card+Payment_Method_Electronic.check+Payment_Method_Mailed.check, data=training, method='nnet',trControl=trainControl(method='cv'))
summary(Churn_NNet)

evaluation$predictionNNet <- predict(Churn_NNet, evaluation, type="raw")
evaluation$correctNNet <- evaluation$predictionNNet == evaluation$Churn_String
print(paste("% of predicted classifications correct", mean(evaluation$correctNNet)))

source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(Churn_NNet)

evaluation$probabilitiesNNet <- predict(Churn_NNet, newdata = evaluation, type='prob')[,1]
NNetOutput <- makeLiftPlot(evaluation$probabilitiesNNet,evaluation,"Neural Network")
NNetOutput$PercCorrect <- mean(evaluation$correctNNet)*100


# Run Bagging model
Churn_Bagging <- bagging(Churn_String ~ Monthly_Charges+Total_Charges+Gender+Senior_Citizen+Partner+Dependents+Tenure+Tenure_bin_1_2years+Tenure_bin_2_3years+Tenure_bin_3_4years+Tenure_bin_4_5years+Tenure_bin_5_6years+Phone_Service+Multiple_Lines+Internet_Service_Fiber.optic+Internet_Service_No+Online_Security+Online_Backup+Device_Protection+Tech_Support+Streaming_TV+Streaming_Movies+Contract_1year+Contract_2year+Paperless_Billing+Payment_Method_Credit.card+Payment_Method_Electronic.check+Payment_Method_Mailed.check, data=training, control = cforest_unbiased(mtry = 3), importance=T)
evaluation$predictionBagging <- predict(Churn_Bagging, newdata=evaluation)$class

evaluation$correctBagging <- evaluation$predictionBagging == evaluation$Churn_String
print(paste("% of predicted classifications correct", mean(evaluation$correctBagging)))

evaluation$probabilitiesBagging <- predict(Churn_Bagging,newdata=evaluation)$prob[,1]
print(Churn_Bagging$importance)
BaggingOutput <- makeLiftPlot(evaluation$probabilitiesBagging,evaluation,"Bagging")
BaggingOutput$PercCorrect <- mean(evaluation$correctBagging)*100

# Run Boosting model
Churn_Boosting <- boosting(Churn_String ~ Monthly_Charges+Total_Charges+Gender+Senior_Citizen+Partner+Dependents+Tenure+Tenure_bin_1_2years+Tenure_bin_2_3years+Tenure_bin_3_4years+Tenure_bin_4_5years+Tenure_bin_5_6years+Phone_Service+Multiple_Lines+Internet_Service_Fiber.optic+Internet_Service_No+Online_Security+Online_Backup+Device_Protection+Tech_Support+Streaming_TV+Streaming_Movies+Contract_1year+Contract_2year+Paperless_Billing+Payment_Method_Credit.card+Payment_Method_Electronic.check+Payment_Method_Mailed.check, data=training, control = cforest_unbiased(mtry = 3), importance=T)
evaluation$predictionBoosting <- predict(Churn_Boosting, newdata=evaluation)$class

evaluation$correctBoosting <- evaluation$predictionBoosting == evaluation$Churn_String
print(paste("% of predicted classifications correct", mean(evaluation$correctBoosting)))

evaluation$probabilitiesBoosting <- predict(Churn_Boosting,newdata=evaluation)$prob[,1]
print(Churn_Boosting$importance)
BoostingOutput <- makeLiftPlot(evaluation$probabilitiesBoosting,evaluation,"Boosting")
BoostingOutput$PercCorrect <- mean(evaluation$correctBoosting)*100

# Run Random Forest model
Churn_RF <- randomForest(Churn_String ~ Monthly_Charges+Total_Charges+Gender+Senior_Citizen+Partner+Dependents+Tenure+Tenure_bin_1_2years+Tenure_bin_2_3years+Tenure_bin_3_4years+Tenure_bin_4_5years+Tenure_bin_5_6years+Phone_Service+Multiple_Lines+Internet_Service_Fiber.optic+Internet_Service_No+Online_Security+Online_Backup+Device_Protection+Tech_Support+Streaming_TV+Streaming_Movies+Contract_1year+Contract_2year+Paperless_Billing+Payment_Method_Credit.card+Payment_Method_Electronic.check+Payment_Method_Mailed.check, data=training, control = cforest_unbiased(mtry = 3), importance=T)
evaluation$predictionRF <- predict(Churn_RF, newdata=evaluation, type = "response")

evaluation$correctRF <- evaluation$predictionRF == evaluation$Churn_String
print(paste("% of predicted classifications correct", mean(evaluation$correctRF)))

evaluation$probabilitiesRF <- predict(Churn_RF,newdata=evaluation,type="prob")[,1]
print(Churn_RF$importance)
RFOutput <- makeLiftPlot(evaluation$probabilitiesRF,evaluation,"Random Forest")
RFOutput$PercCorrect <- mean(evaluation$correctRF)*100

#Run the TREE model
tree.2 <- rpart(Churn_String ~ Monthly_Charges+Total_Charges+Gender+Senior_Citizen+Partner+Dependents+Tenure+Tenure_bin_1_2years+Tenure_bin_2_3years+Tenure_bin_3_4years+Tenure_bin_4_5years+Tenure_bin_5_6years+Phone_Service+Multiple_Lines+Internet_Service_Fiber.optic+Internet_Service_No+Online_Security+Online_Backup+Device_Protection+Tech_Support+Streaming_TV+Streaming_Movies+Contract_1year+Contract_2year+Paperless_Billing+Payment_Method_Credit.card+Payment_Method_Electronic.check+Payment_Method_Mailed.check,training)

# A fast plot
prp(tree.2)

# A fancy plot from rat
fancyRpartPlot(tree.2)

evaluation$predictionTree <- predict(tree.2, newdata = evaluation, type = "vector")
summary(evaluation$predictionTree)
evaluation$predictionTreeString[evaluation$predictionTree==1]="Churn"
evaluation$predictionTreeString[evaluation$predictionTree==2]="No Churn"
evaluation$predictionTreeString <- as.factor(evaluation$predictionTreeString)
evaluation$correctTree <- evaluation$predictionTreeString == evaluation$Churn_String

print(paste("% of predicted classifications correct", mean(evaluation$correctTree)))
evaluation$probabilitiesTree <- predict(tree.2, newdata = evaluation, type = "prob")[,1]
TreeOutput <- makeLiftPlot(evaluation$probabilitiesTree,evaluation,"Tree")
TreeOutput$PercCorrect <- mean(evaluation$correctTree)*100






