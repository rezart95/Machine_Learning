setwd("C:/Users/xps/Desktop")
data<- read.csv("data.csv", header=T, stringsAsFactors=F)

#remove null data
data$X <- NULL

#reshape the dataset
data <- data[,-1]
data$Diagnosis<-factor(ifelse(data$Diagnosis=="B","Benign","Malignant"))

#inspect dataset
str(data)
head(data)

#Analyse the correlation between each variable
library(PerformanceAnalytics)
chart.Correlation(data[,c(2:11)],histogram = TRUE, col="grey10", pch=1, main="Cancer Mean")

#analyses between each variable (with diagnosis)
library(ggplot2)
library(GGally)
ggpairs(data[,c(2:11,1)], aes(color=Diagnosis, alpha=0.75), lower=list(continuous="smooth"))+ theme_bw()+
  labs(title="Cancer Mean")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))


#Principal Component Analysis

library(factoextra)
data_pca <- transform(data)

mean_pca <- prcomp(data_pca[,c(2:11)], scale = TRUE)
summary(mean_pca)

#Screeplot  Line lies at point PC4
fviz_eig(mean_pca, addlabels=TRUE, ylim=c(0,60), geom = c("bar", "line"), barfill = "pink", barcolor="grey",linecolor = "red", ncp=10)+
  labs(title = "Cancer Mean Variances - PCA",
       x = "Principal Components", y = "% of variances")

#PCA variables

mean_var <- get_pca_var(mean_pca)
mean_var

#Correlation between variables and PCA
library("corrplot")
corrplot(mean_var$cos2, is.corr=FALSE, type = "lower")

#Contribution of variables to PC1 and PC2
library(gridExtra)
p1 <- fviz_contrib(mean_pca, choice="var", axes=1, fill="pink", color="grey", top=10) #Dimension 1  
p2 <- fviz_contrib(mean_pca, choice="var", axes=2, fill="skyblue", color="grey", top=10) #Dimension 2
grid.arrange(p1,p2,ncol=2) #Together in one graph


#PCA Biplot of the mean
fviz_pca_biplot(mean_pca, col.ind = data$Diagnosis, col="black",
                palette = "jco", geom = "point", repel=TRUE,
                legend.title="Diagnosis", addEllipses = TRUE)


fviz_pca_ind(data, habillage = fit_km$cluster)


##########################################################################################
           #Doing SVM (Support Vector MAchines)


# A computer is not able to create true random numbers, but pseudo random numbers
# To optimize this process inside the computer there is actually a list of random numbers
# The random component is where in this list we start to generate numbers
# Forcing the seed to be the same will generate exactly the same list of random numbers
# Remember that the position of the seed is defined by you 
# Then we will be able to compare performances removing the random effects
 
# We start the list of random numbers at the position 218

nrows <- NROW(data)
set.seed(218)                           ## fix random value
index <- sample(1:nrows, 0.7 * nrows)   ## shuffle and divide

#train <- data                          ## 569 test data (100%)
train <- data[index,]                   ## 398 test data (70%)
test <- data[-index,]                   ## 171 test data (30%)

prop.table(table(train$Diagnosis))

library(caret)

install.packages('e1071') 
library(e1071) 

learn_svm <- svm(Diagnosis~., data=train)
pre_svm <- predict(learn_svm, test[,-1])
cm_svm <- confusionMatrix(pre_svm, test$Diagnosis)
cm_svm
