# Classification project

setwd("C:/Users/xps/Desktop/Classification Project") # choosing the directory
options(scipen = 10)

library(tidyverse)
library(dplyr)
library(caret)
library(ggplot2)
library(tidyr)
library(DataExplorer) #for the plot_missing function
library(pROC)
library(corrplot)
library(naivebayes)
library(psych)
library(FSelector)
library(rpart)
library(rpart.plot)
library(data.tree)
library(caTools)
library(party)
library(mlbench)
library(rattle)

#Uploading the dataset 
loan <- read_csv("loan.csv", col_names = TRUE)
 

#A first impression of the missing data problem their distribution among variables
#plot_missing(loan)


#We can have an overview to the main dataset about the distribuion and the missing values
colSums(is.na(loan)) %>%  sort()


#Level "Current" describes an active loan  
#We want to create a classification model for clients who have already concluded their practice
##Level "Current" describes an active loan and we don't know how this practice will be classified so we decided to remove it.
loan <- loan[loan$loan_status!="Current",]

#Our target variable is now a 8 level variable where only 'CHARGED OFF' and 'FULLY PAID' are significantly consistent
#with high number of observation. i decided to group some levels together under the same charachteristics about a client who can be defined a Default client
#and the rest group together as a Fully Paid client.
#In my reasonin there no need for a third level because of insufficent observations
#After the re-grouping we will notice the unbalanced number of observations between Default and Fully Paid
count(loan,loan$loan_status)

#1 Charged Off                                          261655
#2 Default                                                  31
#3 Does not meet the credit policy. Status:Charged Off     761
#4 Does not meet the credit policy. Status:Fully Paid     1988
#5 Fully Paid                                          1041952
#6 In Grace Period                                        8952
#7 Late (16-30 days)                                      3737
#8 Late (31-120 days)                                    21897



#We merge the levels at call it "Suitable" for all of the clients who tend or are more likely to manage to pay their loan

loan$loan_status[loan$loan_status == "Does not meet the credit policy. Status:Fully Paid" |
                  loan$loan_status == "Fully Paid" |
                  loan$loan_status == "In Grace Period" |
                  loan$loan_status == "Late (16-30 days)"] <- "Suitable"


#We merge with the same criteria the levels which suits more to a client who is not going to be able to pay his loan.

loan$loan_status[loan$loan_status == "Charged Off" |
                   loan$loan_status == "Default" |
                   loan$loan_status == "Does not meet the credit policy. Status:Charged Off" |
                   loan$loan_status == "Late (31-120 days)"] <- "Default"


#At first I decided to remove every column where number of NA's exeeds 5% of all observations. 
clean_loan <- loan[, colSums(is.na(loan))<= 0.05*nrow(loan)]


#After droping the columns we drop the rest of NA remained 
clean_loan <- na.omit(clean_loan)

#In this dataset we have an isignifican number of Joint App which Indicates whether the loan is an individual 
#application or a joint application with two co:borrowers
#so to remove any doubt in our final classification i decided to filter and keep only the Individual applications

#Individual  Joint App 
#1314044      26929 

clean_loan <- clean_loan[clean_loan$application_type=="Individual",]



#Im going to apply the same filter for "disbursement_method" variable where direct pay is only less than 5% of all the observations
#      Cash       DirectPay 
#     1307572        6472 

clean_loan <- clean_loan[clean_loan$disbursement_method=="Cash",]



#This is the list of the remaining colums. Part of the columns 
#are dedicated for joints applicants, and part of them are useless without any effect on our analysis


#loan_amnt"                  "funded_amnt"                "funded_amnt_inv"            "term"                      
#"int_rate"                   "installment"                "grade"                      "sub_grade"                 
#"emp_length"                 "home_ownership"             "annual_inc"                 "verification_status"       
#"issue_d"                    "loan_status"                "pymnt_plan"                 "purpose"                   
# title"                      "zip_code"                   "addr_state"                 "dti"                       
# "delinq_2yrs"                "earliest_cr_line"           "inq_last_6mths"             "open_acc"                  
# "pub_rec"                    "revol_bal"                  "revol_util"                 "total_acc"                 
# "initial_list_status"        "out_prncp"                  "out_prncp_inv"              "total_pymnt"               
# "total_pymnt_inv"            "total_rec_prncp"            "total_rec_int"              "total_rec_late_fee"        
# "recoveries"                 "collection_recovery_fee"    "last_pymnt_d"               "last_pymnt_amnt"           
# "last_credit_pull_d"         "collections_12_mths_ex_med" "policy_code"                "application_type"          
# "acc_now_delinq"             "acc_open_past_24mths"       "bc_open_to_buy"             "bc_util"                   
# "chargeoff_within_12_mths"   "delinq_amnt"                "mort_acc"                   "mths_since_recent_bc"      
# "num_bc_sats"                "num_sats"                   "percent_bc_gt_75"           "pub_rec_bankruptcies"      
# "tax_liens"                  "total_bal_ex_mort"          "total_bc_limit"             "hardship_flag"             
# "disbursement_method"        "debt_settlement_flag"     


#So i decide to remove all the variables that i considered unuseful for our analysis
#That cannot provide any useful information in predicting your response 
#(zero information with a constant variable) and can lead to numerical problems for some linear models 
# R documentation of all the variables
# https://www.rdocumentation.org/packages/creditmodel/versions/1.1.1/topics/lendingclub


clean_loan <-  clean_loan[,-which(names(clean_loan) %in% c(
      "sub_grade",             # LC assigned loan subgrade
      "issue_d",               # Column of date format
      "pymnt_plan",            # Column of constat values "n" in almost 99% of it       
      "title",                 # Copy of "purpose" column so was unnecessary to keep it
      "delinq_2yrs",           # Column made in the 80% of values zero
      "pub_rec",               # # Column made in the 85% of values zero
      "zip_code",              # Column not of our interest in the the analysis
      "addr_state",            # Column of adreses, not valuable for our analysis
      "earliest_cr_line",      # The date the borrower's earliest reported credit line was opened. Is another date format column
      "initial_list_status",   # The initial listing status of the loan. Possible values are - W, F
      "out_prncp_inv",         # Column made in the 96% of values zero 
      "out_prncp",             # Column made in the 97% of values zero 
      "total_rec_late_fee",    # Late fees received to date. Column made in 90% of it of values zero
      "delinq_amnt",           # Column made in the 95% of values zero
      "pub_rec_bankruptcies",  # Column made in the 90% of values zero
      "recoveries",            # Post charge off gross recovery. Column made in 87% of it of values zero
      "collection_recovery_fee",# Column made in the 88% of values zero
      "last_pymnt_d",         # Column of date format. 
      "last_credit_pull_d",    # Column of date format.
      "collections_12_mths_ex_med",# Number of collections in 12 months excluding medical collections. Column made in 98% of it of values zero
      "policy_code",           # Column made in 100% of it of values 1 
      "acc_now_delinq",        # The number of accounts on which the borrower is now delinquent. Column made in 98% of it of values zero
      "chargeoff_within_12_mths",# Column made in the 90% of values zero 
      "delinq_amt",            # The past-due amount owed for the accounts on which the borrower is now delinquent. Column made in 98% of it of values zero
      "tax_liens",             # Number of tax liens. Column made in 95% of it of values zero
      "hardship_flag",         # Column made in the 99.9% of values N 
      "debt_settlement_flag",  # Column made in the 99.9% of values N 
      "application_type",      #Indicates whether the loan is an individual application or a joint application with two co:borrowers
      "disbursement_method"    # The method by which the borrower receives their loan. Possible values are: CASH, DIRECT_PAY
      )
  )]


#To avoid introducing a bias in test using train-data, the train-test split should be performed before (most) data preparation steps
#We divide the dataset in training which rapresents the data which will be used to create the model and this is 70% of the all data
#the remaining 30% will be called test and be used for testing the accuracy of our model.

set.seed(987654321)

idx = sample(nrow(clean_loan), round(0.70*nrow(clean_loan)))

training_loan <- clean_loan[idx,]
test_loan <- clean_loan[-idx,]



## Data Trasformation is the most important step for this analysis if we want to have good results
## We are transforming this is a dichotomous variable by assignin value 1 to Suitable and zero to Defalt
training_loan$loan_status[training_loan$loan_status == "Suitable"] <- 1
training_loan$loan_status[training_loan$loan_status != 1 ] <- 0
str(training_loan$loan_status)
training_loan$loan_status <- as.numeric(training_loan$loan_status)


# Loan Term variable has only 2 levels 36 and 60 so i choosed to transfrom in a dummy variable 
#where the 36 months term assumes value 1 and 60 zero.
training_loan$term <- gsub("months", "", training_loan$term)
training_loan$term <- as.integer(training_loan$term)
training_loan$term[training_loan$term == 36 ] <- 1
training_loan$term[training_loan$term != 1] <- 0
training_loan$term <- as.numeric(training_loan$term)
str(training_loan$term)



#Assigning a number to each grade will make it easier to transform the variable into a numeric one
#and will be possible to check to correlation between variables
str(training_loan$grade)
training_loan$grade[training_loan$grade == "A"] <- 1
training_loan$grade[training_loan$grade == "B"] <- 2
training_loan$grade[training_loan$grade == "C"] <- 3
training_loan$grade[training_loan$grade == "D"] <- 4
training_loan$grade[training_loan$grade == "E"] <- 5
training_loan$grade[training_loan$grade == "F"] <- 6
training_loan$grade[training_loan$grade == "G"] <- 7
training_loan$grade <- as.integer(training_loan$grade)




#As we can see here the 2 consistent levels of this variable are MORTAGE and RENT so
#this is another case of merging levels together and transforming this into a dummy varibale where by logic 
#i would say there is an connection between OWN and MORTAGE so i will assign value 1 to this and zero to the rest

#ANY   MORTGAGE    NONE    OTHER      OWN      RENT 
#199    450759      34      123      99048    369668

training_loan$home_ownership[training_loan$home_ownership=="OWN" | training_loan$home_ownership=="MORTGAGE"  ] <- 1       
training_loan$home_ownership[training_loan$home_ownership!=1] <- 0
training_loan$home_ownership <- as.numeric(training_loan$home_ownership)
str(training_loan$home_ownership)



#This variable has almost equaly distributed number of observations for each level 
#so i decided to keep all the three levels and not apply a merge
training_loan$verification_status <- factor(training_loan$verification_status,
                           levels = c("Verified",
                                      "Source Verified",
                                      "Not Verified"),
                           ordered = TRUE) 
training_loan$verification_status <- as.numeric(training_loan$verification_status)


#I used the same logic like in the previous variable by assigning a level to each 
#but with the difference that here the levels are not ordered
training_loan$purpose <- factor(training_loan$purpose,
                                levels = c("car",
                                           "credit_card",
                                           "debt_consolidation",
                                           "educational",
                                           "home_improvement",
                                           "house",
                                           "major_purchase",
                                           "medical",
                                           "moving",
                                           "other",
                                           "renewable_energy",
                                           "small_business",
                                           "vacation",
                                           "wedding"),
                                ordered = FALSE)
training_loan$purpose <- as.numeric(training_loan$purpose)



#First droping the chararter part of the variable and transform it to an integer
unique(training_loan$emp_length)
training_loan$emp_length <- gsub("<", "", training_loan$emp_length)
training_loan$emp_length <- gsub("years", "", training_loan$emp_length)
training_loan$emp_length <- gsub("year", "", training_loan$emp_length)
training_loan$emp_length <- gsub("n/a", "", training_loan$emp_length)
training_loan$emp_length <- gsub(" ", "", training_loan$emp_length)
training_loan$emp_length <- gsub("\\+", "", training_loan$emp_length)
training_loan$emp_length <- ifelse(training_loan$emp_length =="", 10, training_loan$emp_length)
training_loan$emp_length <- as.factor(training_loan$emp_length)
unique(training_loan$emp_length)

str(training_loan$emp_length)

###################################################################################################################################################

#For the correlation we need only the numeric variables so we select them with supply.
loan_numeric_var <- sapply(training_loan, is.numeric) %>% which() %>% names()
loan_numeric_var


#Correlation of the variables
loans_corr <-
  cor(training_loan[, loan_numeric_var],
      use = "pairwise.complete.obs")

loans_corr

#ploting the correlation to make it easier to understand the correlation between variables
corrplot(loans_corr, 
         method = "number", type = "lower", order = "hclust")



# these are potential candidates
# to be excluded from the model
findCorrelation(loans_corr, names = TRUE, cutoff = 0.80)

#We remove all the variables which have a correlation higher than 80%
training_loan <-
  training_loan[,-which(
    names(training_loan) %in% c(
      "bc_util",
      "int_rate",
      "revol_util",
      "open_acc",
      "total_pymnt_inv",
      "total_bc_limit",
      "total_rec_prncp",
      "total_pymnt",
      "loan_amnt",
      "funded_amnt",
      "funded_amnt_inv",
      "grade"
    )
  )]


#Numeric variables after removing the correlated ones 
loan_numeric_var_noVIF <- sapply(training_loan, is.numeric) %>% which() %>% names()
loan_numeric_var_noVIF

#Correlation
loans_corr_noVIF <-
  cor(training_loan[, loan_numeric_var_noVIF],
      use = "pairwise.complete.obs")

#Correlation plot
corrplot(loans_corr_noVIF, 
         method = "number", type = "lower", order = "hclust")

#########################################################################################################################################

#To implement CBS (choice-based sampling), divide the dependent variable into two groups (all 1's, all 0's). Then take a random sample 
#of size N from EACH group (so that the combined dataset now has 2N observations in which half the responses are 1's
#Then, run your logit model on the 2N observation dataset -- as if this were your original dataset.
#It can shown that the logit intercept will be biased, but that all other model coefficients will be unbiased. 
#(This is a remarkable fact. It is a consequence of the structure of the binary logit model.) 
#Usually, the intercept is not of interest in interpretation and so can be ignored. 
#The CBS technique is widely used in database marketing and biostatistics where the population probability of a "1" is very low.
#CBS analysis leads to higher quality estimates of the Beta coefficients.

#Creating a subsample with arget variable 1 values
balance_train_1 <- training_loan %>%
  filter(training_loan$loan_status == 1)
balance_train_1 <- balance_train_1[sample(nrow(balance_train_1), 150000), ]

#Creating a subsample with arget variable 0 values
balance_train_0 <- training_loan %>%
  filter(training_loan$loan_status == 0)
balance_train_0 <- balance_train_0[sample(nrow(balance_train_0), 150000), ]

#We bind together the 2 samples creating a dataset where our target variable is balanced
balance_train <- rbind(balance_train_1, balance_train_0)
table(balance_train$loan_status)


rm(balance_train_1,balance_train_0)


#Because we have to make a lot of data trasformation we created this subsets from the already transformed Train and Test samples.
#We did the same for our test dataset
balance_test_1 <- test_loan %>%
  filter(test_loan$loan_status == 1)
balance_test_1 <- balance_test_1[sample(nrow(balance_test_1), 150000), ]

balance_test_0 <- training_loan %>%
  filter(training_loan$loan_status == 0)
balance_test_0 <- balance_test_0[sample(nrow(balance_test_0), 150000), ]

balance_test <- rbind(balance_test_1, balance_test_0)
table(balance_test$loan_status)


rm(balance_test_1, balance_test_0)



#This is my final model with all the variables significant after we removed the not significant
balance_train_model <- glm(formula = loan_status ~ term + installment + home_ownership + annual_inc + verification_status + 
                             purpose + dti + inq_last_6mths + revol_bal + total_acc + total_rec_int + last_pymnt_amnt + 
                             acc_open_past_24mths + bc_open_to_buy + mort_acc + mths_since_recent_bc + num_bc_sats + num_sats + 
                             percent_bc_gt_75 + total_bal_ex_mort, family = binomial, 
                          data = balance_train)




#Prediction model is made to predict result of training model on test dataset 
#which has 30% of the amount of data.
#the pupose is to understand the fitting of the data 
balance_prediction <- predict.glm(balance_train_model, newdata = balance_test, type = "response")


#Create a column in test set with the result of prediction
balance_test$alpha <- balance_prediction

#Used a threshold of 50% for my prediction which means that will see how the client will be classified with a prob of 50%
balance_test$alpha <- ifelse(balance_test$alpha >= 0.5, 1, 0)

#Assing this data to test dataset
balance_test$alpha <- as.factor(balance_test$alpha)

balance_test$loan_status <- as.factor(balance_test$loan_status)

#Cheking the accuracy of prediction  #Accuracy : 86.1%
#means that we classified with 72.4% accuracy the client (Suitable, Default)
confusionMatrix(balance_test$alpha, balance_test$loan_status) 


########################################################################################################################################
#This is the model made with loan status like our target variable
#In the summary we can see that a couple of variables are insignificant so they have been removed
model_1 <- glm(loan_status ~., data = training_loan, family = binomial)
summary(model_1)




#this is my final model with all the significant variables after we removed the insignificant
train_model <- glm(formula = loan_status ~ term + installment + home_ownership + annual_inc + verification_status + 
                     purpose + dti + inq_last_6mths + revol_bal + total_acc + total_rec_int + last_pymnt_amnt + 
                     acc_open_past_24mths + bc_open_to_buy + mort_acc + mths_since_recent_bc + num_bc_sats + num_sats + 
                     percent_bc_gt_75 + total_bal_ex_mort, family = binomial, 
                     data = training_loan)


#WARNING!
#When i apply the predicition i get this error : #Error: variables 'term', 'grade', 'emp_length', 'home_ownership', 'purpose' 
#were specified with different types from the fit
#Probably due to the fact that variables type in train and test are of different type after the manipulation
#after online serach only palusible answer i found about my error is to to apply the same transformation to the test sample

test_loan$loan_status[test_loan$loan_status == "Suitable"] <- 1
test_loan$loan_status[test_loan$loan_status != 1 ] <- 0
str(test_loan$loan_status)
test_loan$loan_status <- as.numeric(test_loan$loan_status)


# Loan Term variable has only 2 levels 36 and 60 so i choosed to transfrom in a dummy variable 
#where the 36 months term assumes value 1 and 60 zero.
test_loan$term <- gsub("months", "", test_loan$term)
test_loan$term <- as.integer(test_loan$term)
test_loan$term[test_loan$term == 36 ] <- 1
test_loan$term[test_loan$term != 1] <- 0
test_loan$term <- as.numeric(test_loan$term)
str(test_loan$term)




#Assigning a number to each grade will make it easier to transform the variable into a numeric one
#and will be possible to check to correlation between variables
str(test_loan$grade)
test_loan$grade[test_loan$grade == "A"] <- 1
test_loan$grade[test_loan$grade == "B"] <- 2
test_loan$grade[test_loan$grade == "C"] <- 3
test_loan$grade[test_loan$grade == "D"] <- 4
test_loan$grade[test_loan$grade == "E"] <- 5
test_loan$grade[test_loan$grade == "F"] <- 6
test_loan$grade[test_loan$grade == "G"] <- 7
test_loan$grade <- as.integer(test_loan$grade)


test_loan$verification_status <- factor(test_loan$verification_status,
                                            levels = c("Verified",
                                                       "Source Verified",
                                                       "Not Verified"),
                                            ordered = TRUE) 
test_loan$verification_status <- as.numeric(test_loan$verification_status)


#I used the same logic like in the previous variable by assigning a level to each 
#but with the difference that here the levels are not ordered
test_loan$purpose <- factor(test_loan$purpose,
                                levels = c("car",
                                           "credit_card",
                                           "debt_consolidation",
                                           "educational",
                                           "home_improvement",
                                           "house",
                                           "major_purchase",
                                           "medical",
                                           "moving",
                                           "other",
                                           "renewable_energy",
                                           "small_business",
                                           "vacation",
                                           "wedding"),
                                ordered = FALSE)
test_loan$purpose <- as.numeric(test_loan$purpose)



#First droping the chararter part of the variable and transform it to an integer
unique(test_loan$emp_length)
test_loan$emp_length <- gsub("<", "", test_loan$emp_length)
test_loan$emp_length <- gsub("years", "", test_loan$emp_length)
test_loan$emp_length <- gsub("year", "", test_loan$emp_length)
test_loan$emp_length <- gsub("n/a", "", test_loan$emp_length)
test_loan$emp_length <- gsub(" ", "", test_loan$emp_length)
test_loan$emp_length <- gsub("\\+", "", test_loan$emp_length)
test_loan$emp_length <- ifelse(test_loan$emp_length =="", 10, test_loan$emp_length)
test_loan$emp_length <- as.integer(test_loan$emp_length)
unique(test_loan$emp_length)




test_loan$home_ownership[test_loan$home_ownership=="OWN" | test_loan$home_ownership=="MORTGAGE"  ] <- 1       
test_loan$home_ownership[test_loan$home_ownership!=1] <- 0
test_loan$home_ownership <- as.numeric(test_loan$home_ownership)
str(test_loan$home_ownership)




#Droping all the varibales with correlation higher than 75%
test_loan <-
  test_loan[, -which(names(test_loan) %in% c("bc_util","int_rate", "revol_util","open_acc","total_pymnt_inv",
                                                     "total_bc_limit","total_rec_prncp","total_pymnt",
                                                     "loan_amnt","funded_amnt","funded_amnt_inv","grade"))]



#############################################################################################################################################
#Prediction model is made to predict result of training model on test dataset 
#which has 30% of the amount of data.
#the pupose is to understand the fitting of the data 
pred.modl <- predict.glm(train_model, newdata = test_loan, type = "response")


#Create a column in test set with the result of prediction
test_loan$alpha <- pred.modl

#Used a threshold of 50% for my prediction which means that will see how the client will be classified with a prob of 50%
test_loan$alpha <- ifelse(test_loan$alpha >= 0.5, 1, 0 )

#Passing this data to test dataset
test_loan$alpha <- as.factor(test_loan$alpha)

test_loan$loan_status <- as.factor(test_loan$loan_status)

#Cheking the accuracy of prediction  #Accuracy : 87.1%
#means that we classified with 72.4% accuracy the client (Suitable, Default)
confusionMatrix(test_loan$alpha, test_loan$loan_status) 


#A receiver operating characteristic curve, or ROC curve, is a graphical plot that illustrates the diagnostic
#ability of a binary classifier system as its discrimination threshold is varied.

plot.roc(test_loan$loan_status , pred.modl , main = "ROC" , percent = TRUE , print.auc = TRUE,
         ci = TRUE , of = "thresholds" , thresholds = "best" , print.thres = "best" , col = 'blue')


################################################################################################################
# NAIVE BAYES 

#Bayes Theorem
# P(A|B) = P(A) * P(B|A)/P(B)
# A and B are independent 

training_loan$loan_status <- as.factor(training_loan$loan_status)

bayes <- naive_bayes(loan_status~., data = training_loan)

bayes

summary(bayes)


######################################################################################################################################
library(rattle)	

# DECISION TREE

# We are going to built the decision tree on the training and test model created for the Generalised Model above.
# Next, we create the tree on the basis of all meaningful predictors.
# The default splitting criterion is the Gini Index (CART algorithm)


tree <- rpart(train_model, # model formula
        data = training_loan, # data
        method = "class") # type of the tree: classification

# We can see all splitting conditions and the distribution of the dependent variable in each node. 
# Stars * denote terminal nodes. 

tree

rpart.plot(tree,
           extra = 106,
           type = 1,
           digits = 2)




tree2 <- rpart(train_model,
      data = training_loan,
      method = "class",
      minsplit = 50, #minimum number of observations in the given node for the considered split (default = 20)
      minbucket = 50, #minimum number of observations in the terminal nodes 
      maxdepth = 5, #maximum depth of the tree, assuming that first node (root node) is denoted by 0
      # we don't impose any restriction on the tree growth 
      cp = -1)

rpart.plot(tree2,
           extra = 106,
           type = 1,
           digits = 4)

printcp(tree2)

opt <- which.min(tree2$cptable[, "xerror"])

cp <- tree2$cptable[opt, "CP"]
cp

tree2p <- prune(tree2, cp = cp)

fancyRpartPlot(tree2p)


pred.tree43 <- predict(tree2p, training_loan)
head(pred.tree4)

pred.tree422 <- predict(tree2p,
                      training_loan,
                      type = "class")
head(pred.tree42)


confusionMatrix(data = pred.tree422, # predictions
                # actual values
                reference = as.factor(training_loan$loan_status),
                # definitions of the "success" label
                positive = "1") 

glimpse(training_loan)

levels(training_loan$loan_status)

rm(training_loan)

########################################################################################################################################
# KNN

#For the model creation was used the method of repeated cross validation "repeateadcv" and for recent interations we used 10 
#and the repeats which is complete set of tools to repeat cross validation we used number three.

knn_train <- sample_n(training_loan, nrow(training_loan) * 0.01)

knn_test <- sample_n(test_loan, nrow(test_loan) * 0.01)

trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3)

knn_model <- train(
  loan_status ~ .,
  data = knn_train,
  method = "knn",
  tuneLength = 20,
  trControl = trControl,
  preProc = c("center", "scale")
)


knn_prediction <- predict(knn_model, newdata = knn_test)

confusionMatrix(knn_prediction, knn_test$loan_status)


#########################################################################################################################

#SVM - Support Vector Machine
library(e1071)

#SVM model for classification
svm_model <- svm(formula = loan_status ~ .,
                 data = knn_train,
                 type = 'C-classification',
                 kernel = 'radial')

summary(svm_model)

# Number of Support Vectors:  3095
# ( 1697 1398 )


#Prediction of svm on test set
svm_predict <- predict(svm_model, knn_test)

svm_tab <- table(svm_predict, knn_test$loan_status)


#Miss classification table has a 13% ratio
1-sum(diag(svm_tab))/sum(svm_tab)


######################################################################################################################################
# This codes consist on some descriptive plots to get a first impression in the data we have
# we are going to use this in our presentation

#Distribution of loan amout
Desc(loan$loan_amnt, main = "Loan amount distribution", plotit = TRUE)

#Relation between grade according to interest rate 
ggplot(data=loan, aes(grade,int_rate,fill=grade))+geom_boxplot(outlier.color = "blue")+labs(title="Box plot of Interest rate")

#Relation between loan amount and purpose
ggplot(data=loan, aes(x=loan_amnt,col=purpose))+ geom_freqpoly(binwidth = 5000)

#Relation between home ownership and loan amount
ggplot(data = loan, aes(home_ownership, loan_amnt, fill = home_ownership)) +
  geom_boxplot(outlier.color = "blue") + labs(title = "Box plot of loan amount according to home ownership")

#Relation between loan status and loan amount
ggplot(data=loan, aes(loan_status, loan_amnt))+geom_boxplot(aes(fill=loan_status))+
  theme(axis.text.x = element_blank()) +
  labs(list(title = "Loan status",x = "Loan Status",y = "Loan Amount"))


#home ownership and annual income
ggplot(data = loan, aes(home_ownership, annual_inc, fill = home_ownership)) +
  geom_boxplot(outlier.color = "blue") + labs(title = "Box plot of home ownership according to annual income") +
  coord_cartesian( ylim = c(0,500000))



#loan amount and annual income 
ggplot(data = loan, aes(loan_amnt, annual_inc, fill = verification_status)) +
  geom_boxplot(outlier.color = "blue") + labs(title = "Box plot of loan amount according to annual income") +
  coord_cartesian(xlim = c(2500, 40000), ylim = c(0,500000))

 
#loan amount and employer length
ggplot(data = loan, aes(loan_amnt, emp_length, fill = home_ownership)) +
  geom_boxplot(outlier.color = "blue") + labs(title = "Box plot of loan amount according to employer length") +
  coord_cartesian(xlim = c(2500, 40000), ylim = c(9,11))


#loan amount and interest rate
ggplot(data = loan, aes(loan_amnt, int_rate, fill = verification_status)) +
  geom_boxplot(outlier.color = "blue") + labs(title = "Box plot of loan amount according to interest rate") 
  

#loan amount and term of loan
ggplot(data = loan, aes(loan_amnt, term , fill = home_ownership)) +
geom_boxplot(outlier.color = "blue") + labs(title = "Box plot of loan amount according to loan term") 


#loan amount and verification status
ggplot(data = loan, aes(loan_amnt, verification_status , fill = home_ownership)) +
  geom_boxplot(outlier.color = "blue") + labs(title = "Box plot of loan amount according verification status") 

##############################################################################################################################


#XG-BOOST
install.packages("xgboost")
library(xgboost)

results_logit <- list()


boostmodel <- xgboost(data = as.matrix(knn_train),
                      nrounds = 20,
                      verbose = 1)






