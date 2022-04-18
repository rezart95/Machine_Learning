#Installing and loading packages



requiredPackages = c("tidyverse","factoextra","stats","clustertend","flexclust",
                     
                     "fpc","cluster","ClusterR","knitr","kableExtra") 

for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)} 

for(i in requiredPackages){library(i,character.only = TRUE) } 

library(mlr)
library(tidyverse)
library(DataExplorer)
library(factoextra)
library(dendextend)
library(reshape2)
library(ggforce)
library(cluster)



# Loading data
setwd("C:/Users/ctiro/Desktop")
data_full <- read.csv("GENERAL.csv")

data_full$CUST_ID <- NULL


# Missing values

kable(summary(data_full),caption = "Summary statistics of the dataset")%>% 
  
  kable_styling(latex_options="scale_down")

# 313 observations are have NAs, we are dropping them

data_full <- data_full[complete.cases(data_full), ]

data_full

# Leaving only observations which have 12 months of tenure      

nrow(data_full)-length(which(data_full$TENURE<12))

data_full %>% select(filter.,TENURE==12))



###############################################################################################

#Tried some statistics to understand the distibution and denstity of all the data 

dim(data_full)
length(unique(data_full))
table(data_full$BALANCE)
summary(data_full)
table(data_full==0) #40 thousand zero values


t.test(data_full$BALANCE_FREQUENCY,var.equal=F)
wilcox.test(sqrt(data_full$BALANCE),sqrt(data_full$CASH_ADVANCE))

qqnorm(log(data_full$CASH_ADVANCE),ylim = c(0,50),xlim=c(0,7))



boxplot(data_full$BALANCE)
boxplot(data_full$PURCHASES)
boxplot(data_full$CASH_ADVANCE)
boxplot(data_full$TENURE)
boxplot(data_full$PURCHASES_FREQUENCY) #uh la la
boxplot(data_full$PAYMENTS)

#Trying the square root scale 
par(mfrow=c(1,3))
boxplot(sqrt(data_full$BALANCE))  
boxplot(sqrt(data_full$PURCHASES))
boxplot(sqrt(data_full$CASH_ADVANCE))
boxplot(sqrt(data_full$TENURE),ylim=c(0,4))
boxplot(sqrt(data_full$PAYMENTS))

#Trying the log scale 
boxplot(log(data_full$TENURE))
boxplot(log(data_full$PAYMENTS))


mean(data_full$TENURE)
sd(data_full$TENURE)
curve(dnorm(x,mean=11.53439,sd=1.310984),col=2,add=T)

mean(data_full$CASH_ADVANCE)
sd(data_full$CASH_ADVANCE)
curve(dnorm(x,mean=994.1755,sd=2121.458),col=2,add=T)

mean(data_full$PURCHASES_FREQUENCY)
sd(data_full$PURCHASES_FREQUENCY)
curve(dnorm(x,mean=0.496,sd=0.4012726),col=3,add=T)
lines(c(0.496,0.496),c(-1,dnorm(0.496,mean=0.496,sd=0.4012726)),lty=2)

mean(data_full$BALANCE_FREQUENCY)
sd(data_full$BALANCE_FREQUENCY)
curve(dnorm(x,mean=0.8950351,sd=0.2076969),col=3,add=T,main="Balance Frequency")
lines(c(0.8950351,0.8950351),c(-1,dnorm(0.8950351,mean=0.8950351,sd=0.2076969)),lty=2)

mean(data_full$PURCHASES_INSTALLMENTS_FREQUENCY)
sd(data_full$PURCHASES_INSTALLMENTS_FREQUENCY)
curve(dnorm(x,mean=0.3688203,sd=0.3980929),col=3,add=T,main="Purchases installments frequency")
lines(c(0.3688203,0.3688203),c(-1,dnorm(0.3688203,mean=0.3688203,sd=0.3980929)),lty=2)



plot(density(data_full$BALANCE))
plot(density(data_full$BALANCE_FREQUENCY))
plot(density(data_full$PURCHASES))
plot(density(data_full$ONEOFF_PURCHASES))
plot(density(data_full$INSTALLMENTS_PURCHASES))
plot(density(data_full$CASH_ADVANCE))
plot(density(data_full$PURCHASES_FREQUENCY))
plot(density(data_full$ONEOFF_PURCHASES_FREQUENCY))
plot(density(data_full$PURCHASES_INSTALLMENTS_FREQUENCY))
plot(density(data_full$CASH_ADVANCE_FREQUENCY))
plot(density(data_full$CASH_ADVANCE_TRX))
plot(density(data_full$PURCHASES_TRX))
plot(density(data_full$CREDIT_LIMIT)) 
plot(density(data_full$PAYMENTS))
plot(density(data_full$MINIMUM_PAYMENTS)) 
plot(density(data_full$PRC_FULL_PAYMENT))
plot(density(data_full$TENURE))

hist(data_full$BALANCE,
     main =" Balance amount left in their account",
     col= "red",
     xlab="Value",
     xlim= c(0,13000),
     ylim = c(0,5000))

hist(data_full$BALANCE_FREQUENCY,
     main ="How frequently the Balance is updated",
     col= "blue",
     xlab="Value",
     ylim = c(0,7000))

hist(data_full$PURCHASES,
     main ="Amount of purchases made from account",
     col= "red",
     xlab="Value",
     xlim= c(0,15000),
     ylim = c(0,10000))

hist(data_full$ONEOFF_PURCHASES,
     main ="Maximum purchase amount done in one-go",
     col= "red",
     xlab="Value",
     xlim= c(0,9000),
     ylim = c(0,9000))

hist(data_full$INSTALLMENTS_PURCHASES,
     main ="Amount of purchase done in installment",
     col= "red",
     xlab="Value",
     xlim = c(0,7000),
     ylim = c(0,10000))

hist(data_full$CASH_ADVANCE,
     main ="Cash in advance given by the user ",
     col= "red",
     xlab="Value",
     xlim= c(0,15000),
     ylim = c(0,10000))

hist(data_full$PURCHASES_FREQUENCY, 
     main ="How frequently the Purchases are being made ",
     col= "blue",
     xlab="Value",
     ylim = c(0,2500))

hist(data_full$ONEOFF_PURCHASES_FREQUENCY,
     main ="How frequently Purchases are happening in one-go",
     col= "blue",
     xlab="Value")

hist(data_full$PURCHASES_INSTALLMENTS_FREQUENCY,
     main ="How frequently purchases in installments are being done",
     col= "blue",
     xlab="Value")

hist(data_full$CASH_ADVANCE,
     main ="How frequently the cash in advance being paid ",
     col= "red",
     xlab="Value",
     xlim= c(0,15000),
     ylim = c(0,10000))

hist(data_full$CASH_ADVANCE_TRX,
     main ="Number of Transactions made with Cash in Advanced",
     col= "red",
     xlab="Value",
     xlim = c(0,45),
     ylim = c(0,10000))

hist(data_full$PURCHASES_TRX,
     main =" Number of purchase transactions made",
     col= "red",
     xlab="Value",
     xlim= c(0,150),
     ylim = c(0,8000))

hist(data_full$CREDIT_LIMIT,
     main ="Limit of Credit Card for user",
     col= "red",
     xlab="Value",
     xlim= c(0,20000),
     ylim = c(0,4000))

hist(data_full$PAYMENTS,
     main ="Amount of Payment done by user",
     col= "red",
     xlab="Value",
     xlim= c(0,15000),
     ylim = c(0,10000))

hist(data_full$MINIMUM_PAYMENTS,
     main ="Minimum amount of payments made by user",
     col= "red",
     xlab="Value",
     xlim= c(0,15000),
     ylim = c(0,10000))

hist(data_full$PRC_FULL_PAYMENT,
     main ="Percent of full payment paid by user ",
     col= "blue",
     xlab="Value")

hist(data_full$TENURE,
     main ="Tenure of credit card service for user ",
     col= "red",
     xlab="Value",
     ylim = c(0,10000))





# 313 observations are have NAs, we are dropping them

data_full <- data_full[complete.cases(data_full), ]



# Leaving only observations which have 12 months of tenure      

nrow(data_full)-length(which(data_full$TENURE<12))

data_full %>% select(filter(.,TENURE==12))



class(data_full)



# Outliers treatment 

columns <-c("BALANCE","PURCHASES","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES","CASH_ADVANCE","CREDIT_LIMIT",
            
            "PAYMENTS","MINIMUM_PAYMENTS")



t.test(data_full$BALANCE_FREQUENCY,var.equal=F)

