## KNN Algorithm

$~$
  
  For the model creation was used the method of repeated cross validation "repeateadcv" and for recent interations we used 10 and the repeats which is complete set of tools to repeat cross validation we used number three.

```{r echo=FALSE, message=FALSE, warning=FALSE}
library(caret)
library(pROC)
library(mlbench)

#Transforming the target variable into a factor 
training_loan$loan_status <- as.factor(training_loan$loan_status)

knn_train <- sample_n(training_loan, nrow(training_loan)*0.01)

knn_test <- sample_n(test_loan, nrow(test_loan)*0.01)

trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3)

knn_model <- train(loan_status ~., 
                   data = knn_train,
                   method = "knn",
                   tuneLength = 20, 
                   trControl = trControl,
                   preProc = c("center", "scale"))


knn_prediction <- predict(knn_model, newdata = knn_test)

confusionMatrix(knn_prediction, knn_test$loan_status)
```
$~$
  
  $~$
  
  ## DECISION TREE
  We are going to built the decision tree on the training and test model created for the Generalised Model above.
Next, we create the tree on the basis of all meaningful predictors.
The default splitting criterion is the Gini Index (CART algorithm)

```{r include=FALSE}
library(rpart)
library(rpart.plot)
library(data.tree)
library(caTools)
library(party)
library(naivebayes)
library(psych)
library(rattle)
library(rattle)
```

```{r include=FALSE}
tree <- rpart(knn_model, # model formula
              data = knn_train, # data
              method = "class") # type of the tree: classification

# We can see all splitting conditions and the distribution of the dependent variable in each node. 
# Stars * denote terminal nodes. 
```

```{r echo=FALSE}
tree
```


```{r echo=FALSE}
rpart.plot(tree,
           extra = 106,
           type = 1,
           digits = 2)
```

# STOPPING CRITERIA

```{r echo=FALSE}
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
```


```{r include=FALSE}

printcp(tree2)

opt <- which.min(tree2$cptable[, "xerror"])

cp <- tree2$cptable[opt, "CP"]
cp
```

# PRUNING 

```{r echo=FALSE}
tree2p <- prune(tree2, cp = cp)

fancyRpartPlot(tree2p)
```

```{r include=FALSE}
tree_predict <- predict(tree2p, training_loan)


tree_predict2 <- predict(tree2p,
                         training_loan,
                         type = "class")
head(tree_predict)
```

# PREDICTION 

```{r echo=FALSE}
confusionMatrix(data = tree_predict2, # predictions
                # actual values
                reference = as.factor(training_loan$loan_status),
                # definitions of the "success" label
                positive = "1") 


```

$~$
  
  $~$
  
  ## SVM - Support Vector Machine
  
  ```{r echo=TRUE}
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
```

## Conclusions

In this project 4 classification models were applied. (GLM, KNN, DECISION TREE, SVM)
After applying all the transformation and all the classification models as above we can say that this models predict with an accuracy which goes from
85% up to 87.1% in our Glm (generalized linear model). This are satisfying results for our research. 
Our depended variable was unbalanced so to eleminate the risk of a error in our prediction we applied the CBS (choice-based sampling) to create a subset with balanced values in the dependet variable and the result of the predicted model was 86.1%. 