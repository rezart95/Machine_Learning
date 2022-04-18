library(arulesViz)
library(arules)
library(xlsx)

setwd("C:/Users/xps/Desktop")

shopping_dataset <- read.csv("dataset_group.csv", header = FALSE, sep = ',')

sapply(shopping_dataset, function(x)(sum(is.na(x))))
#V1 V2 V3 
#0  0  0 

nrow(shopping_dataset)
#[1] 289955


#trasaction data is 603
length(unique(shopping_dataset$V1)) #total
barplot(table(factor(shopping_dataset$V1)), main = "Dates",  col="blue", border="blue")


#transaction ID is 1139
length(unique(shopping_dataset$V2)) #total
barplot(table(factor(shopping_dataset$V2)), main = "Dates",  col="orange", border="pink")


#products of supermarket is 38
length(unique(shopping_dataset$V3)) #total
barplot(table(factor(shopping_dataset$V3)), main = "Buying Transactions",  col="orange", border="orange")


# Divide the data 
mba <- split(x=shopping_dataset[,"V3"],f=shopping_dataset$V2)
summary(mba)


# we look at the total records is 22343
sum(sapply(mba,length))


#Repeating Data
mba <- lapply(mba,unique)
sum(sapply(mba,length))
#[1] 16753

#Transform Data in Transactions 
mba <- as(mba, "transactions")
class(mba)


#we generate the rules, with a confidence factor of 80% and a support rank of 15%
rules <- apriori(mba, parameter = list(support = 0.15, confidence = 0.8))
#set of 22 rules 
optimal_value_rules <- rules[]
inspect(optimal_value_rules)

plot(optimal_value_rules[1:22], method = "graph")
#eggs and yogurt
plot(optimal_value_rules[16:16], method = "graph")

###########################################################################################à


names(itemFrequency(mba))

LIST(head(mba))


rules.trans1<-apriori(trans1, parameter=list(supp=0.1, conf=0.5))

image(mba)
plot(rules.mba, method="matrix", measure="lift")

plot(rules.trans1) 
plot(rules.trans1, measure=c("support","lift"), shading="confidence")

plot(rules.trans1, shading="order", control=list(main="Two-key plot"))

plot(rules.trans1, method="grouped") # this one has a great potential
plot(rules.trans1, method="graph") # this one too
plot(rules.trans1[1:22], method="graph", control=list(type="items"))
plot(rules.trans1, method="paracoord", control=list(reorder=TRUE))



chi2tab<-crossTable(mba, measure="chiSquared", sort=TRUE) #relations test 
# p-value of test, H0:independent rows and columns
chi2tab

itemFrequency(mba)
itemFrequency(mba, type="relative")
itemFrequency(mba, type="absolute")


#Cross table
ctab<-crossTable(mba, sort=TRUE) # defaut meausure count
ctab<-crossTable(mba, measure="count", sort=TRUE) # defaut meausure count
ctab

# eclat algorithm is used to mine frequent itemsets - to limit dataset
freq.items<-eclat(mba, parameter=list(supp=0.25, maxlen=15)) # basic eclat
inspect(freq.items)

# plot frequent items
# relative freq: total number of item to the number of transactions
# topN - how many top items we want to plot
itemFrequencyPlot(mba, topN=10, type="absolute", main="Item Frequency") 
itemFrequencyPlot(mba, topN=10, type="relative", main="Item Frequency")

names(itemFrequency(mba))

LIST(head(mba))

chi2tab<-crossTable(mba, measure="chiSquared", sort=TRUE) #relations test 
# p-value of test, H0:independent rows and columns
chi2tab

stab<-crossTable(mba, measure="support", sort=TRUE)
ptab<-crossTable(mba, measure="probability", sort=TRUE) # as support

ltab<-crossTable(mba, measure="lift", sort=TRUE)
stab
ptab

round(ptab,3)
round(chi2tab,3)
freq.items<-eclat(mba, parameter=list(supp=0.25, maxlen=15)) # basic eclat
inspect(freq.items)

