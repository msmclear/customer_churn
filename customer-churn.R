
library(dplyr)
library(caTools)

#read and check out data
churn <- read.csv("customer_churn.csv")
str(churn)

#testing results of regression model or decision tree to predict customer churn

#starting with a logistic regression model 

model_1 = glm(Churn ~ gender +  SeniorCitizen + Partner + Dependents + tenure + PhoneService +
                MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection +
                TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod +
                MonthlyCharges + TotalCharges, data = churn, family = "binomial") %>%
  summary()

model_1

#Calculate the Pseudo R2 for the logistic regression.

library(DescTools)

glm(Churn ~ gender +  SeniorCitizen + Partner + Dependents + tenure + PhoneService +
      MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection +
      TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod +
      MonthlyCharges + TotalCharges, data = churn, 
    family = "binomial") %>%
  PseudoR2()


#split data and train model

split <- sample.split(churn, SplitRatio = 0.7)
train <- subset(churn, split==TRUE)
test <- subset(churn, split==FALSE)


churn_train<-glm(Churn ~ gender +  SeniorCitizen + Partner + Dependents + tenure + PhoneService +
                        MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection +
                        TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod +
                        MonthlyCharges + TotalCharges, data = train, family = "binomial") %>%
                        summary()
churn_train


#predict using test data, confusion matrix

library(broom)

churn_test <- glm(Churn ~ gender +  SeniorCitizen + Partner + Dependents + tenure + PhoneService +
                    MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection +
                    TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod +
                    MonthlyCharges + TotalCharges, data = train, family = "binomial") %>%
  augment(type.predict = "response", newdata = test) %>%
  mutate(predicted = ifelse(.fitted>0.5, 1, 0))

table(test$Churn, churn_test$predicted)


# ROC curve and calculate AUC

library(pROC)
Roc_log <- roc(test$Churn, churn_test$predicted)
plot(Roc_log)
auc(Roc_log)


#now for the decision tree
install.packages("rpart")
library(rpart)

tree <- rpart(Churn ~ gender +  SeniorCitizen + Partner + Dependents + tenure + PhoneService +
                MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection +
                TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod +
                MonthlyCharges + TotalCharges, data = train, method = "class")

library(rpart.plot)
prp(tree, extra = 1)

#ROC curve and calculate AUC
# needs pruning
ROC_tree <- roc(test$Churn, churn_test$.fitted)
plot(ROC_tree)
auc(ROC_tree)


#prune tree

pruned_tree <- prune(tree, cp=0.01)
prp(pruned_tree, cex = .6)

#ROC curve and AUC


predicted_tree <- pruned_tree %>%
  predict(type = "class", newdata=test)

table(test$Churn, predicted_tree)

ROC_prune <- roc(test$Churn, predicted_tree[, 2])
plot(ROC_prune)
auc(ROC_prune)

# comparing the two models, the decision tree works better at predicting customer churn