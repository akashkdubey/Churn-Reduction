rm(list = ls())

#Getting Current working directory.
getwd()

#Setting working directory
setwd('/Users/akash/Desktop/Project 1 -Churn Reduction')

getwd()

######################################### READING DATA #################################################

#Load CSV files
train_data = read.csv('Train_data.csv' , header = TRUE , stringsAsFactors = FALSE)
test_data = read.csv('Test_data.csv' , header = TRUE , stringsAsFactors = FALSE)

#Getting the dimensions of data
dim(train_data)
dim(test_data)

#Retrieving Column names of train and test data.
colnames(train_data)
colnames(test_data)

#Changing column names of train and test data by stripping the space between them by Underscore
colnames(train_data) = c('state', 'account_length', 'area_code', 'phone_number',
                         'international_plan', 'voice_mail_plan', 'number_vmail_messages',
                         'total_day_minutes', 'total_day_calls', 'total_day_charge',
                         'total_eve_minutes', 'total_eve_calls', 'total_eve_charge',
                         'total_night_minutes', 'total_night_calls', 'total_night_charge',
                         'total_intl_minutes', 'total_intl_calls', 'total_intl_charge',
                         'number_customer_service_calls', 'Churn')
colnames(test_data) = c('state', 'account_length', 'area_code', 'phone_number',
                        'international_plan', 'voice_mail_plan', 'number_vmail_messages',
                        'total_day_minutes', 'total_day_calls', 'total_day_charge',
                        'total_eve_minutes', 'total_eve_calls', 'total_eve_charge',
                        'total_night_minutes', 'total_night_calls', 'total_night_charge',
                        'total_intl_minutes', 'total_intl_calls', 'total_intl_charge',
                        'number_customer_service_calls', 'Churn')

###################################  CHECKING FOR DATA IMBALANCE ########################################
# Getting data points by label
table(train_data$Churn)  # Train data contains 2850 negative labels and 483 positive labels.
table(test_data$Churn)   # Test data contains 1443 negative labels and 224 positive labels.


# Checking Class distribution
prop.table(table(train_data$Churn))  # Train data contains 85.5% negaitve labels and 14.5% positive labels.
prop.table(table(test_data$Churn))   # Test data contains 86.5 %  negative labels and 13.5% positive labels.


# Plot for data imbalance

# For train data
library(ggplot2)
pl = ggplot(train_data ,aes(x = Churn)) + ggtitle("Train data distribution")
print(pl + geom_bar(fill = 'blue'))

# For test data
library(ggplot2)
pl = ggplot(test_data ,aes(x = Churn)) + ggtitle("Test data distribution")
print(pl + geom_bar(fill = 'blue'))

"
INFERENCE : 
 It is obvious from the above visualisation that, the data (both the test and the train data) is imbalanced.
          - In Train_data :
                True : 483
                Flase : 2850

          - In test_data :
               True : 224
               False : 1443

  - Also, Negative Label (False) contributes to almost 85.5 percent of train data and Positive Label (True) 
    contributes to only 14.5 percent of data in the train dataset.<br>
  
  - Also, Negative Label (False) contributes to almost 86.6 percent of test data and Positive Label (True)
    contributes to only 13.4 percent of data in the test dataset. <br>
          }
"

####################################   CHECKING FOR MISSING VALUES #########################################

# Checking for missing values in train data
sum(is.na(train_data))   # Output : [1] 0

#checking for missing values in test data
sum(is.na(test_data))    # Output : [1] 0


"
INFERENCE :
  So, both of our train data and test data does not contain any missing values.
"

############################################  DATA STATISTICS  ################################################
library(psych)
describe(train_data)
describe(test_data)

############################################  ADDING EXTRA VARIABLES TO THE DATA ##############################
"
It seems like, we can add few more features of our own to add some extra information to our dataset like 
'total minutes' , 'total_calls' and 'total charge' that might add some extra information to the dataset.
"
# Adding to Train Data
train_data$total_minutes = train_data$total_day_minutes + train_data$total_eve_minutes + train_data$total_night_minutes + train_data$total_intl_minutes
train_data$total_calls = train_data$total_day_calls + train_data$total_eve_calls + train_data$total_night_calls + train_data$total_intl_calls
train_data$total_charge = train_data$total_day_charge + train_data$total_eve_charge + train_data$total_night_charge + train_data$total_intl_charge

dim(train_data) # 3333 Rows and 24 Columns

# Adding to Test data
test_data$total_minutes = test_data$total_day_minutes + test_data$total_eve_minutes + test_data$total_night_minutes + test_data$total_intl_minutes
test_data$total_calls = test_data$total_day_calls + test_data$total_eve_calls + test_data$total_night_calls + test_data$total_intl_calls
test_data$total_charge = test_data$total_day_charge + test_data$total_eve_charge + test_data$total_night_charge + test_data$total_intl_charge

dim(test_data) # 1667 Rows and 24 Columns



################################# TRANSFORMING CATEGORICAL VARIABLES INTO NUMERIC ##############################
"
In the train and test data given to us, 'international_plan' , 'Voice_mail_plan' and 'Churn' are categorical features. So, we convert them into numerical 
features by creating dummies for each of them also called one hot encoding.
"
####### Train Data
train_data = fastDummies::dummy_cols(train_data , select_columns = "international_plan" , remove_first_dummy = TRUE)
train_data = fastDummies::dummy_cols(train_data , select_columns = "voice_mail_plan" , remove_first_dummy = TRUE)
train_data = fastDummies::dummy_cols(train_data , select_columns = "Churn" , remove_first_dummy = TRUE)


knitr::kable(train_data)


# Deleting the columns for which dummies are created
train_data = subset(train_data, select = -c(international_plan, voice_mail_plan, Churn))


# Deleting the column names like state, phone_number and account_length because it does not contain any extra information
# and also increases the feature space after we perform one hot encoding on them.
train_data = subset(train_data, select = -c(state, phone_number, account_length ))



# Changing the column names for the dummies that we created
library("plyr")
train_data = plyr::rename(train_data,c("international_plan_ yes" = "international_plan", "voice_mail_plan_ no" = "voice_mail_plan", "Churn_ True." = "Churn"))


####### Test Data
test_data = fastDummies::dummy_cols(test_data , select_columns = "international_plan" , remove_first_dummy = TRUE)
test_data = fastDummies::dummy_cols(test_data , select_columns = "voice_mail_plan" , remove_first_dummy = TRUE)
test_data = fastDummies::dummy_cols(test_data , select_columns = "Churn" , remove_first_dummy = TRUE)


knitr::kable(test_data)


# Deleting the columns for which dummies are created
test_data = subset(test_data, select = -c(international_plan, voice_mail_plan, Churn))

# Deleting the column names like state, phone_number and account_length because it does not contain any extra information
# and also increases the feature space after we perform one hot encoding on them.
test_data = subset(test_data, select = -c(state, phone_number, account_length))


# Changing the column names for the dummies that we created
library("plyr")
test_data = plyr::rename(test_data,c("international_plan_ yes" = "international_plan", "voice_mail_plan_ yes" = "voice_mail_plan","Churn_ True."="Churn"))


####################################### SPLITTING THE DATA IN TRAIN AND TEST SET ############################################

#Splitting Train data in X_train and y_train
X_train = subset(train_data,select = -c(Churn))
y_train = subset(train_data,select = c(Churn))

##Splitting Test data in X_test and y_test
X_test = subset(test_data,select = -c(Churn))
y_test = subset(test_data,select = c(Churn))

#Checking dimension of X_train and y_train
dim(X_train)
dim(y_train)

#Checking dimension of X_test and y_test
dim(X_test)
dim(y_test)



####################################################### DATA VISUALISATION ####################################################


                     
                                      ################## UNIVARIATE ANALYSIS ###################


#### Box-Plot Visualisation of Individual Features

boxplot(train_data[,c('total_day_minutes',
                        'total_day_calls', 'total_day_charge', 'total_eve_minutes',
                        'total_eve_calls', 'total_eve_charge')])

boxplot(train_data[,c('total_night_minutes',
                      'total_night_calls', 'total_night_charge', 'total_intl_minutes')])


boxplot(train_data[,c('total_intl_minutes',
                      'total_intl_calls', 'total_intl_charge',
                      'number_customer_service_calls', 'total_minutes', 'total_calls',
                      'total_charge')])


#### Box-Plot Visualisation of Features based on label
boxplot(total_day_minutes~Churn, data=train_data, col=(c("gold","darkgreen")),main="total_day_minutes", xlab="Churn")
boxplot(total_day_calls~Churn, data=train_data, col=(c("gold","darkgreen")),main="total_day_calls", xlab="Churn")
boxplot(total_day_charge~Churn, data=train_data, col=(c("gold","darkgreen")),main="total_day_charge", xlab="Churn")
boxplot(total_eve_minutes~Churn, data=train_data, col=(c("gold","darkgreen")),main="total_eve_minutes", xlab="Churn")
boxplot(total_eve_calls~Churn, data=train_data, col=(c("gold","darkgreen")),main="total_eve_calls", xlab="Churn")
boxplot(total_eve_charge~Churn, data=train_data, col=(c("gold","darkgreen")),main="total_eve_charge", xlab="Churn")
boxplot(total_night_minutes~Churn, data=train_data, col=(c("gold","darkgreen")),main="total_night_minutes", xlab="Churn")
boxplot(total_night_calls~Churn, data=train_data, col=(c("gold","darkgreen")),main="total_night_calls", xlab="Churn")
boxplot(total_night_charge~Churn, data=train_data, col=(c("gold","darkgreen")),main="total_night_charge", xlab="Churn")
boxplot(total_intl_minutes~Churn, data=train_data, col=(c("gold","darkgreen")),main="total_intl_minutes", xlab="Churn")
boxplot(total_intl_calls~Churn, data=train_data, col=(c("gold","darkgreen")),main="total_intl_calls", xlab="Churn")
boxplot(total_intl_charge~Churn, data=train_data, col=(c("gold","darkgreen")),main="total_intl_charge", xlab="Churn")
boxplot(number_customer_service_calls~Churn, data=train_data, col=(c("gold","darkgreen")),main="number_customer_service_calls", xlab="Churn")
boxplot(total_minutes~Churn, data=train_data, col=(c("gold","darkgreen")),main="total_minutes", xlab="Churn")
boxplot(total_calls~Churn, data=train_data, col=(c("gold","darkgreen")),main="total_calls", xlab="Churn")
boxplot(total_charge~Churn, data=train_data, col=(c("gold","darkgreen")),main="total_charge", xlab="Churn")


##### Violin-Plot Visualisation of Individual Features
library(vioplot)
vioplot(train_data$total_day_minutes , train_data$total_day_calls ,train_data$total_day_charge )
vioplot(train_data$total_eve_minutes,train_data$total_eve_calls,train_data$total_eve_charge)
vioplot(train_data$total_night_minutes,train_data$total_night_calls,train_data$total_night_charge)
vioplot(train_data$total_intl_minutes,train_data$total_intl_calls,train_data$total_intl_charge)
vioplot(train_data$total_minutes,train_data$total_calls,train_data$total_charge, train_data$number_customer_service_calls)


##### Violin-Plot Visualisation of Individual Features based on label

library(ggpubr)
viplot = function(y)
{
ggviolin(train_data, "Churn", "y" , fill ="Churn",palette = c("#00AFBB", "#E7B800"),
         add = "boxplot", add.params = list(fill = "white"))
}

viplot("total_day_minutes")
viplot("total_day_calls")
viplot("total_day_charge")
viplot("total_night_minutes")
viplot("total_night_calls")
viplot("total_night_charge")
viplot("total_eve_calls")
viplot("total_eve_minutes")
viplot("total_eve_charge")
viplot("total_intl_minutes")
viplot("total_intl_calls")
viplot("total_intl_charges")
viplot("number_customer_service_calls")

##### Normality check

qqnorm(train_data$total_day_minutes)
qqnorm(train_data$total_day_calls)
qqnorm(train_data$total_day_charge)
qqnorm(train_data$total_eve_minutes)
qqnorm(train_data$total_eve_calls)
qqnorm(train_data$total_eve_charge)
qqnorm(train_data$total_night_minutes)
qqnorm(train_data$total_night_calls)
qqnorm(train_data$total_night_charge)
qqnorm(train_data$total_intl_minutes)
qqnorm(train_data$total_intl_calls)
qqnorm(train_data$total_intl_charge)
qqnorm(train_data$number_customer_service_calls)
qqnorm(train_data$total_charge)
qqnorm(train_data$total_calls)
qqnorm(train_data$total_minutes)


                  ################################## MULTIVARIATE ANALYSIS ####################################

#### PCA Visualisation
library(ggfortify)
autoplot(prcomp(train_data), data = train_data, colour = 'Churn')

#### t-SNE Visualisation
library(Rtsne)
require(tsne)


train_matrix <- as.matrix(train_data[,1:20])
set.seed(42) # Set a seed if you want reproducible results
tsne_out <- Rtsne(train_matrix) # Run TSNE
# Show the objects in the 2D tsne representation
plot(tsne_out$Y,col=train_data$Churn)

library(ggplot2)
tsne_plot <- data.frame(x = tsne_out$Y[,1], y = tsne_out$Y[,2], col = train_data$Churn)
ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=col))

################################################ DATA PREPARATION AND CLEANING #############################################

#### Scaling the data
cnames= colnames(X_train)

for(i in cnames){
  print(i)
  train_data[,i] = (train_data[,i]-mean(train_data[,i])) / sd(train_data[,i])
  test_data[,i] = (test_data[,i]-mean(test_data[,i])) / sd(test_data[,i])
}


#### Feature Selection

# Correlation Plot
library(corrgram)
corrgram(train_data[,cnames] , order =F, upper.panel = panel.pie , text.panel = panel.txt , main = "Correlation Plot")


# Deleting correlated features
train_data = subset(train_data , select = -c(total_charge,total_intl_charge,total_night_charge,total_day_charge,total_eve_charge,voice_mail_plan))
test_data = subset(test_data , select = -c(total_charge,total_intl_charge,total_night_charge,total_day_charge,total_eve_charge,voice_mail_plan))

dim(train_data)
dim(test_data)





######################################################## MACHINE LEARNING MODELS #############################################################
library(sigmoid)

# Coverting predicter variable into factor
train_data$Churn = as.factor(train_data$Churn)
test_data$Churn = as.factor(test_data$Churn)

        ########## NAIVE BAYES ###############
library(e1071)

classifier = naiveBayes(x = train_data[-15] , y = train_data$Churn )
y_pred =predict(classifier , newdata = test_data[-15])
conf_matrix=table(test_data[,15], y_pred)
caret::confusionMatrix(test_data[,15], y_pred)



       ################# KNN #################


library(tidyverse)
getNamespace("grDevices")
library(rpart)
library(class)
KNN_predictions = knn(train_data[,1:14] , test_data[,1:14] , train_data[,15] , k= 3 , prob = TRUE)
KNN_predictions #Probablity Score for getting probablity of data points in test data
conf_matrix=table(KNN_predictions , test_data$Churn)
caret::confusionMatrix(KNN_predictions, test_data[,15])




       ########## Logistic Regression ########


logit_model = glm(Churn ~ . ,data =train_data , family = 'binomial')
summary(logit_model)
logit_predictions = predict(logit_model, newdata = test_data , type = 'response') 
logit_predictions  #Probablity Score for getting probablity of data points in test data
logit_predictions = ifelse(logit_predictions > 0.5 , 1 ,0)
confMatrix=table(test_data$Churn , logit_predictions )
caret::confusionMatrix(confMatrix)



       ########### Decision Trees ############

library(rpart)
DT_model = rpart(Churn ~ . ,data =train_data )
summary(DT_model)
DT_predictions_proba = predict(DT_model, newdata = test_data[-15])
DT_predictions_proba  ##Probablity Score for getting probablity of data points in test data
DT_predictions = predict(DT_model, newdata = test_data[-15] , type = 'class')

confMatrix=table(test_data$Churn, DT_predictions )
caret::confusionMatrix(confMatrix)



       ############## Random Forest #############


library(randomForest)
RF_model = randomForest(x =train_data[-15] , y= train_data$Churn ,importance =  TRUE, ntree = 500)
summary(RF_model)
RF_predictions = predict(RF_model, newdata = test_data[-15] , type = 'class')
confMatrix=table(test_data$Churn , RF_predictions )
caret::confusionMatrix(confMatrix)




      ################# XGBoost ################


train_data$Churn = as.integer(train_data$Churn) - 1
test_data$Churn = as.integer(test_data$Churn) - 1

library(xgboost)
XG_model = xgboost( data = as.matrix(train_data[-15]), label = train_data$Churn, nrounds = 10,  objective="binary:logistic")
XG_pred = predict(objective =logistic ,XG_model , newdata = as.matrix(test_data[-15]) )
XG_pred   ##Probablity Score for getting probablity of data points in test data
XG_pred = ifelse(XG_pred > 0.5 , 1 ,0)
confMatrix=table(test_data$Churn , XG_pred)
caret::confusionMatrix(confMatrix)



###########################################----------------------------------------################################################
              "XGBoost and random forest gives best results out of all these machine learning models"












