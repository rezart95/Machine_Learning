install.packages("MASS")
install.packages("dplyr")
install.packages("readr")
install.packages("ggplot2")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("class")
install.packages("caTools")
install.packages("caret")
install.packages("tidyverse")
install.packages("magrittr")
install.packages("e1071")


library(MASS)
library(dplyr)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(randomForest)
library(class)
library(caTools)
library(caret)
library(tidyverse)
library(magrittr)
library(ggpubr)
library("e1071")

#read dataset
grades<-read_csv('StudentsPerformance.csv')

#check readability of file
str(grades)


#check summary statistics
summary(grades)

#count how many missing in every column
colSums(is.na(grades)) %>% 
  sort()

#check if text
is.character(grades$gender)
table(grades$gender)

#convert gender from text to factors / it is nominal
grades$gender<-as.factor(grades$gender)

levels(grades$gender)

glimpse(grades$gender)

#convert lunch from text to factors / it is nominal
table(grades$lunch)

grades$lunch<-as.factor(grades$lunch)

levels(grades$lunch)

#convert 'test preperation' from text to factors / it is nominal
table(grades$`test preparation course`)

grades$`test preparation course`<-as.factor(grades$`test preparation course`)

levels(grades$lunch)


#convert 'test preperation' from text to factors / it is nominal
table(grades$`race/ethnicity`)

grades$`race/ethnicity`<-as.factor(grades$`race/ethnicity`)

levels(grades$`race/ethnicity`)

is.factor(grades$`race/ethnicity`)
############################

#convert ordinal varibale to factors/ 'parentel level education' is ordinal
is.character(grades$`parental level of education`)

table(grades$`parental level of education`)

levels(grades$`parental level of education`)

grades$`parental level of education`<- factor(grades$`parental level of education`,
                           # levels from lowest to highest
                           levels = c("some high school",
                                      "high school",
                                      "associate's degree",
                                      "some college",
                                      "bachelor's degree",
                                      "master's degree"),
                           ordered = TRUE) # ordinal

table(grades$`parental level of education`)
levels(grades$`parental level of education`)
glimpse(grades$`parental level of education`)

is.character(grades$`parental level of education`)




#Change Column names

colnames(grades)

names(grades)[2]<-'race'
names(grades)[3]<-'parental.level.of.education'
names(grades)[5]<-'test.preparation.course'
names(grades)[6]<-'math.score'
names(grades)[7]<-'reading.score'
names(grades)[8]<-'writing.score'
###############################################################
###############################################################


summary(grades)



# initial observations
#There are more females than males
#Group C has the largest number of members
#some college and associate's degree are the most frequently occuring parental levels of education
#most students have standard lunch 
#most students haven't completed test prep




#For the first visualization I want to see how the summary statistics for Math, Reading and Writing
#appear on a box plot and what these statistics look between genders



#make copy of original df to allow dplyr manipulations 
grades_copy=tbl_df(grades)

#verify data frame is 'tbl_df' class type
class(grades_copy)


#Math box and whiskers plot

math_box=
  grades_copy %>%
    group_by(gender)%>%
    select(gender, math.score) %>%
    ggplot(aes(x=gender, y=math.score, col=gender )) +
    geom_boxplot() +
    ggtitle("Math Scores By Gender")+
    theme_linedraw()


#reading box and whiskers
read_box= 
  grades_copy%>%
    group_by(gender) %>%
    select(gender, reading.score) %>%
    ggplot(aes(x=gender, y=reading.score, col=gender)) +
    geom_boxplot() +
    ggtitle("Reading Scores By Gender")
    theme_linedraw()
    

#Writing box and whiskers
write_box=
  grades_copy %>%
    group_by(gender) %>%
    select(gender, writing.score) %>%
    ggplot(aes(x=gender, y=writing.score, col=gender)) +
    geom_boxplot() +
    ggtitle('Writing Scores By Gender') +
    theme_linedraw()

#Put all visualisiations together

library(ggpubr)

ggarrange(math_box, read_box, write_box, common.legend = TRUE, legend = "bottom", nrow = 1, ncol = 3)

#table showing specific numbers in the graph below:

math_graph=
  grades_copy %>%
    group_by(gender) %>%
    select(gender, math.score) %>%
    summarise(count_gender = n(), avg_math = mean(math.score), median_math = median(math.score),
              min_math = min(math.score), max_math=max(math.score))

reading_graph=
  grades_copy %>%
  group_by(gender) %>%
  select(gender, reading.score) %>%
  summarise(count_gender = n(), avg_read = mean(reading.score), median_read = median(reading.score),
            min_read = min(reading.score), max_read=max(reading.score))
    

writing_graph=
  grades_copy %>%
  group_by(gender) %>%
  select(gender, writing.score) %>%
  summarise(count_gender = n(), avg_write = mean(writing.score), median_write = median(writing.score),
            min_write = min(writing.score), max_write=max(writing.score))

  
math_graph
reading_graph
writing_graph


#Here we can see that Males on average score higher than Females in math,
#but Females on average score higher in reading and writing than Males.
#We can also see that there are several outliers present,
#more of them specifically for Females.




#Visualisation 2

# Next I would like to explore with a similar graph as above if taking the preparation_course actually 
# had an impact on test scores and how that looks between males and females.

#Math with Test Preparation 

math_test_prep=
  grades_copy %>%
    group_by(gender) %>%
    select(gender, math.score, test.preparation.course) %>%
    ggplot(aes(x=gender, y=math.score, col=gender)) +
    geom_boxplot() +
    facet_wrap(~ test.preparation.course) +
    ggtitle("Math Scores by Gender w/o Test Prep") +
    theme_linedraw()

#Reading with test prep

read_test_prep = 
  grades_copy %>%
  group_by(gender) %>%
  select(gender, reading.score, test.preparation.course) %>%
  ggplot(aes(x=gender, y=reading.score, col=gender)) +
  geom_boxplot() +
  facet_wrap(~ test.preparation.course) +
  ggtitle("Reading Scores by Gender w/o Test Prep") +
  theme_linedraw()

#Writing with Test Preparation
write_test_prep=
  grades %>%
  group_by(gender) %>%
  select(gender,writing.score,test.preparation.course) %>%
  ggplot(aes(x= gender, y= writing.score, col= gender)) +
  geom_boxplot() +
  facet_wrap(~test.preparation.course)+
  ggtitle("Writing Scores by Gender w/o Test Prep")+
  theme_linedraw()



ggarrange(math_test_prep, read_test_prep, write_test_prep, common.legend = TRUE, legend = 'bottom', nrow = 1, ncol = 3)


# Will show the specific numbers behind each graph:
# Math numbers in graph:
grades %>%
  group_by(test.preparation.course, gender) %>%
  select(test.preparation.course, gender, math.score) %>%
  summarise(count_gender= n(), avg_math= mean(math.score), median_math=median(math.score),
            min_math= min(math.score),max_math= max(math.score))

# Reading numbers in graph:
grades %>%
  group_by(test.preparation.course, gender) %>%
  select(test.preparation.course, gender, reading.score) %>%
  summarise(count_genders= n(),avg_read= mean(reading.score), median_read= median(reading.score),
            min_read= min(reading.score),max_read= max(reading.score))

# Writing numbers in graph:
grades %>%
  group_by(test.preparation.course, gender) %>%
  select(test.preparation.course, gender, writing.score) %>%
  summarise(count_genders= n(),avg_write= mean(writing.score),median_write= median(writing.score), 
            min_write= min(writing.score),max_write= max(writing.score))

#By Looking at this graph we can see that the students who completed the test prepraration 
# did in fact get higher scores than those students who didn't take it. But interestingly enough
#we can see that for reading and writing scores, even when the males took the test prep course,
# they still performed almost equally as same as females who didnt take test prep course. 
#And even with test preparation being in the picture females still outperformed males 
#in both reading and writing. And in math males who didnt take test prep course almost did as well as 
# the females who took the course.





# Visualisation 3 

# This next visualization is going to show how the scores appear between students
# who completed/not completed the test prep course for Math, Reading and Writing
# by different Race/Ethnicity groups, in a histogram format.

# Each bar is representing a race(x-axis), and height of each bar shows the
# AVERAGE SCORE in Math, Reading and Writing.


avg_math_perRace = 
  grades_copy %>%
  group_by(race, test.preparation.course) %>%
  select(race, math.score, test.preparation.course ) %>%
  summarise(avg_math_score=mean(math.score)) %>%
  ggplot(aes(x=race, y=avg_math_score, fill=race)) +
  ylim(0,100) +
  geom_col() +
  facet_wrap(~test.preparation.course) +
  ggtitle("Average Math Scores by Race") +
  theme_bw()

avg_read_perRace=
  grades_copy %>%
  group_by(race,test.preparation.course) %>%
  select(race, reading.score, test.preparation.course) %>%
  summarise(avg_read_score= mean(reading.score))%>%
  ggplot(aes(x= race, y= avg_read_score, fill= race))+
  ylim(0,100)+
  geom_col()+
  facet_wrap(~test.preparation.course)+
  ggtitle("Average Reading Scores by Race")+
  theme_bw()
  
avg_write_perRace=
  grades_copy %>%
  group_by(race,test.preparation.course) %>%
  select(race, writing.score, test.preparation.course) %>%
  summarise(avg_write_score= mean(writing.score))%>%
  ggplot(aes(x= race, y= avg_write_score, fill= race))+
  ylim(0,100)+
  geom_col()+
  facet_wrap(~test.preparation.course)+
  ggtitle("Average Wrting Scores by Race/Ethinicity")+
  theme_bw()


ggarrange(avg_math_perRace,avg_read_perRace,avg_write_perRace,common.legend = TRUE,
          legend = "bottom",nrow = 1, ncol = 3)


# Will show the specific numbers behind each graph:

# Math numbers in graph:
grades_copy %>%
  group_by(test.preparation.course, race) %>%
  select(test.preparation.course, race, math.score) %>%
  summarise(count_race= n(), avg_math= mean(math.score)) 


grades_copy %>%
  group_by(test.preparation.course, race) %>%
  select(test.preparation.course, race, reading.score) %>%
  summarise(count_race= n(), avg_read= mean(reading.score)) 

grades_copy %>%
  group_by(test.preparation.course, race) %>%
  select(test.preparation.course, race, writing.score) %>%
  summarise(count_race= n(), avg_math= mean(writing.score)) 



#By looking at the graph above we can see that the same holds true,
#that taking the test prep course indeed increases scores and that 
#group E and D seem to be the ones with the highest scores across all races.



#Visualisation 4

#I will create a scatterplot that places a student on a grid based 
# on their score in reading and math. Then i will show for each parentel level
# of education, in acesding order, how each scatterplot changes 


some_high_school = 
  grades_copy %>%
  filter(parental.level.of.education == 'some high school') %>%
  ggplot(aes(x=reading.score, y=math.score, col=gender)) +
  geom_jitter() +
  ggtitle('Some High School')

high_school =
  grades_copy %>%
  filter(parental.level.of.education == 'high school') %>%
  ggplot(aes(x=reading.score, y= math.score, col=gender)) +
  geom_jitter() +
  ggtitle('High School')

some_college = 
  grades_copy %>%
  filter(parental.level.of.education == 'some college') %>%
  ggplot(aes(x=reading.score, y=math.score, col=gender)) +
  geom_jitter() +
  ggtitle('Some College')

college =
  grades_copy %>%
  filter(parental.level.of.education == 'college') %>%
  ggplot(aes(x=reading.score, y=math.score, col=gender)) +
  geom_jitter() +
  ggtitle('College')

associates_degree=
  grades_copy %>%
  filter(parental.level.of.education == "associate's degree") %>%
  ggplot(aes(x= reading.score, y= math.score, col= gender))+
  geom_jitter()+
  ggtitle("Associate's Degree")

bachelors_degree=
  grades_copy %>%
  filter(parental.level.of.education == "bachelor's degree") %>%
  ggplot(aes(x= reading.score, y= math.score, col= gender))+
  geom_jitter()+
  ggtitle("Bachelor's Degree")

masters_degree=
  grades_copy %>%
  filter(parental.level.of.education == "master's degree") %>%
  ggplot(aes(x= reading.score, y= math.score, col= gender)) +
  geom_jitter()+
  ggtitle("Master's Degree")

#Shows each level of education in ascending order, each dot is a student
# and is based on reading and math scores

ggarrange(some_high_school, high_school,some_college, college, associates_degree, 
          bachelors_degree, masters_degree, common.legend = TRUE, legend = 'bottom', nrow = 1, ncol = 7)


#table to show the number of students in each parentel level of education:

grades_copy %>%
  group_by(parental.level.of.education) %>%
  summarise(level_edu=n()) %>%
  arrange(desc(level_edu))


#Overall you can see that the blue dots (males) are closer to the math axis
#and the pink dots (females) are placed more to the right indicating higher reading scores.
# What is more interesting is seeing how the graphs change as parental level of education increases,
#observe the following:

#as parental level of education increases we see less students in each graph.
#Indicating that most parents of students in our data, don't have advanced degrees.
#we can also see that the scales for scores in Math and Reading start to increase as
#parental level of education increases. For example, starting in the Associates degree
#graph the minimum starts at 40 rather than 20 or 25. Indicating that perhaps,
#it could be argued that, parents with more advanced degrees might place more emphasis
#on academics than parents with lesser degrees.


#One Question is why have so many students who haven't taken the prep courses
#if it really does help to improve the score of the tests
# So lets dig deeper 'none' label students 
#the assumption is that maybe the course is offered at a high cost
#and maybe the less priviledged students
#might not be able to afford it. 


#Below is the breakdown in percentages of students who did/ did not take the test prep.

percent_of_students = round(prop.table(table(grades_copy$test.preparation.course))*100, digits = 1)

percent_of_students

#so 64.2% of students did not take the test prep course.
#Let's look at the 64% in detail and see how this number 
#breaks down by parental level of education


grades_copy %>%
  filter(test.preparation.course == 'completed') %>%
  group_by(parental.level.of.education) %>%
  summarise(students_count = n()) %>%
  arrange(desc(students_count))


grades_copy %>%
  filter(test.preparation.course == 'none') %>%
  group_by(parental.level.of.education) %>%
  summarise(students_count = n()) %>%
  arrange(desc(students_count))




# so we can still see some observations of student parents with advanced degrees

#Next lets look at how many of those students
# who have a free/reduced lunch in our 64% who did not take the course.

grades_copy %>%
  filter(test.preparation.course == 'none') %>%
  filter(lunch == 'free/reduced') %>%
  group_by(parental.level.of.education) %>%
  summarise(students_counts_free_lunch = n()) %>%
  arrange(desc(students_counts_free_lunch))

#let's see in percentage


grades_copy %>%
  filter(test.preparation.course == 'none') %>%
  summarise(n()) 

some_college_perc = round(((53/642)*100),digits = 1)
associate_perc = round(((48/642)*100),digits = 1)
high_school_perc = round(((46/642)*100),digits = 1)
some_high_school_perc = round(((38/642)*100),digits = 1)
bachelors_perc = round(((27/642)*100),digits = 1)
masters_perc = round(((12/642)*100),digits = 1)

lunches_percents= c(some_college_perc,associate_perc,high_school_perc,some_high_school_perc,
                    bachelors_perc,masters_perc)

print(paste("some college percent=",lunches_percents[1],"%"))
print(paste("assciate's  percent=",lunches_percents[2],"%"))
print(paste("high school percent =",lunches_percents[3],"%"))
print(paste("some high school percent =",lunches_percents[4],"%"))
print(paste("bachelor's percent =",lunches_percents[5],"%"))
print(paste("masters percent =",lunches_percents[6],"%"))



#With the assumption that income increases with level of education,
#(excluding bachelors = 4.2% and masters = 1.9%), approximately 28.9 % of students 
#who did not take the test prep course have a potential excuse for not taking the course.



##################    MODELLING   ###########################

# 1. Single Decision Tree: it will build a simple decision tree 

# 2. Random Forest Model: will create a Random Forest to see if this 
# boosting technique improves our single decision tree model.


# Model 1: Single Decision Tree 



set.seed(1234)

#partition data

index = createDataPartition(grades$test.preparation.course, p = 2/3, list=FALSE)

grades_train = grades[index, ]
grades_test = grades[-index, ]

print(paste("Training set observations: ", nrow(grades_train)))
print(paste("Test set observations: ", nrow(grades_test)))

single_decision_tree_model = rpart(test.preparation.course ~ ., method = "class", data = grades_train)
rpart.plot(single_decision_tree_model, type = 1, extra = 3, fallen.leaves = FALSE, clip.facs = FALSE,
           faclen=0, cex = 0.5, xlim=c(0,1), ylim=c(0,1))

# By the looks of this tree, we can see that, it does not seem like good model, too long and hard to understand
# Lets random forest



#Evaluation of single decision tree
evaluation <- function(single_decision_tree_model, grades_train, atype) {
  
  cat("\nConfusion matrix:\n")
  prediction = predict(single_decision_tree_model, grades_train, type = atype)
  xtab = table(prediction, grades_train$test.preparation.course)
  print(xtab)
  cat("\nEvaluation:\n")
  accuracy = sum(prediction == grades_train$test.preparation.course)/length(grades_train$test.preparation.course)
  precision = xtab[1,1]/sum(xtab[,1])
  recall = xtab[1,1]/sum(xtab[1,])
  f=2*(precision * recall) / (precision + recall)
  cat(paste("Accuracy:\t", format(accuracy, digits = 2), "\n", sep=""))
  cat(paste("Precision:\t", format(precision, digits = 2), "\n", sep=""))
  cat(paste("Recall:\t\t", format(recall, digits = 2), "\n", sep=""))
  cat(paste("F-measure:\t", format(f, digits = 2), "\n", sep=""))
}

evaluation(single_decision_tree_model, grades_test, atype = "class")



############### MODEL 2 = RANDOM FOREST ###################

# For this second model I will attempt to better the quality of our dataset
# with the goal of improving the accuracy of predicting whether a student
# took the prep course based on their testing scores. For this I will remove
# certain features (race.ethnicity, lunch, parental.level.of.education) and 
# create a new data frame that will only have 4 features and leave the class
# variable as test.preparation.course. 


# Feature Reduction 
# new data frame for random forest model

grades2 = grades

drop = c("race", "parental.level.of.education", "lunch")

grades3 = grades2[,!(names(grades2) %in% drop)]

str(grades3)

#Remove outlier observations

# here I will try to find a benchmark and remove any students from dataset that scored less than this benchmark

summary(grades3$math.score)
math_bench = 57 - 1.5 * IQR(grades3$math.score)

summary(grades3$reading.score)
read_bench = 59 - 1.5 * IQR(grades3$reading.score)

summary(grades$writing.score)
write_bench = 57.75 - 1.5 * IQR(grades3$writing.score)

print(paste("Math benchmark is: ", math_bench))
print(paste("Reading benchmark is: ", read_bench))
print(paste("Writing benchmark is: ", write_bench))

cleaned_grades = 
  grades3 %>%
  filter(reading.score >= 29 ) %>%
  filter(math.score >= 27) %>%
  filter(writing.score >= 25.875) %>%
  select(gender : writing.score)

names(cleaned_grades)  

summary(cleaned_grades$reading.score)
summary(cleaned_grades$writing.score)
summary(cleaned_grades$math.score)

print(paste("Data Frame with outliers: ",nrow(grades3)))
print(paste("Data Frame w/o outliers: ",nrow(cleaned_grades)))
print(paste("Number of students removed: ", nrow(grades3)- nrow(cleaned_grades)))

# will re-do the data partitions using the new 
# feature and outlier reduced dataframe "cleansed_grades"

index2= createDataPartition(cleaned_grades$test.preparation.course, p= 2/3, list= FALSE)

grades_train2= cleaned_grades[index2,]
grades_test2= cleaned_grades[-index2,]


str(grades_train2) 
str(grades_test2)

Controlparameters = trainControl(method = 'cv', number = 5, savePredictions = TRUE, classProbs = TRUE)

#specify mtry to build random forest --> will put this parameter in the model in the trControl argument

parameter_grid = expand.grid(mtry = c(2,3,4))

random_forest_model = train(test.preparation.course ~., data = grades_train2, method = 'rf',
                            trControl = Controlparameters, tuneGrid = parameter_grid)

# look at the model

random_forest_model

#************************* Evaluation of Random Forest

rand_forest_predictions = predict(random_forest_model, grades_test2)
rand_forest_predictions

rand_forest_table = table(predictions = rand_forest_predictions, actual = grades_test2$test.preparation.course)

rand_forest_table

rf_test_accuracy = sum(diag(rand_forest_table) / sum(rand_forest_table))

print(paste("Random Forest accuracy: ", round(rf_test_accuracy*100), "%"))

# By the looks of this analysis I can conclude that this dataset may not
# be fitting well to this type of classification algorithm. Maybe using
# another classification method would help improve our results.

# MODEL 3 = K-NEAREST NEIGHBOR MODEL

# Based on visualisation 4, where I plotted students in scotterplot based on their rading and math scores
# and by increasing parentel level of educaion. By looking at that graph I can see that, for the most part 
# both males and females appear to be in similar groups when plotted based on reading and math scores, therefore
# I would like to build a KNN model to classify a student as either male or female based on their reading and math scores.

#For this I will create a new data frame only containing gender,
#reading.score and math.score. And will remove these variable 
#from the data frame we removed outliers from... cleaned_grades

# create copy of original cleaned_grades data frame

cleaned_grades2 = cleaned_grades

# remove all features except gender , reading score and math score
  
drop = c("test.preparation.course","writing.score")

cleaned_grades3 = cleaned_grades2[,!(names(cleaned_grades2) %in% drop)] 

# check the structure

str(cleaned_grades3)

#check the class, if they are factors , continue
class(cleaned_grades3$gender)

# will re-do data partitions once again with reduced feature data frame
index3= createDataPartition(cleaned_grades3$gender, p= 2/3, list= FALSE)

grades_train3= cleaned_grades3[index3,]
grades_test3= cleaned_grades3[-index3,]

# check 
str(grades_train3) 
str(grades_test3)

# will create 2 subsets of data frames for both the training and testing sets
# 2 subsets for training, one containing only the class labels and the other containing
# only the variables without the class labels

grades_train3_wo_labels= subset(grades_train3, select= -gender)
grades_train3_w_labels= grades_train3$gender

#2 subsets for testing one containing only the class labels
#and the other containing only the variables without the class label

grades_test3_wo_labels= subset(grades_test3, select= -gender)
grades_test3_w_labels= grades_test3$gender

print(paste("Best value for K, based on sqrt root of training set observations = ",
            sqrt(nrow(grades_train3))))

# KNN model 1

knn_model = knn(train = grades_train3_wo_labels, test = grades_test3_wo_labels, cl = grades_train3_w_labels, k=25)

#view model

knn_model

# evaluate models
install.packages('gmodels')
library(gmodels)

knn_evaluation = CrossTable(x= grades_test3_w_labels, y= knn_model, prop.chisq= FALSE)

knn_model_accuracy= (141 + 130)/329

print(paste("KNN Model accuracy= ", round(knn_model_accuracy * 100), "%"))

#accuracy is 82 %




