# Install packages --------------------------------------------------------
install.packages("data.table")



# Load packages -----------------------------------------------------------
require(data.table)



# Data Import -------------------------------------------------------------
data = fread("WA_Fn-UseC_-Telco-Customer-Churn.csv")

summary(data)
str(data)
View(data)



# Data Preprocessing  -----------------------------------------------------



