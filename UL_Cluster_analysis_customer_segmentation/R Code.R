#Installing and loading packages

requiredPackages = c("tidyverse","factoextra","stats","clustertend","flexclust","ggforce"
                     ,"fpc","cluster","ClusterR","knitr","kableExtra","DataExplorer","reshape2") 
for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)} 
for(i in requiredPackages){library(i,character.only = TRUE) } 

## Steps of analysis
#1 Desriptive statistics 
#k-means clustering
# description of each cluster type characteristics
# ideas of how we can utilize insights from the analysis (e.g. marekting stratgies to apply to each group)

# Loading data
data_full <- read.csv("/Users/rafalelpassion/Unsupervised-Learning2019Z/Dataset/CC GENERAL.csv",
                      stringsAsFactors = F,
                      na.strings = c(" "))

# Missing values
plot_missing(data_full)
kable(summary(data_full),caption = "Summary statistics of the dataset")%>% 
        kable_styling(latex_options="scale_down")


# 313 observations are have NAs, we are dropping them
transformed_variables <- c("BALANCE", "PURCHASES", "ONEOFF_PURCHASES", "INSTALLMENTS_PURCHASES", 
                           "CASH_ADVANCE", "CASH_ADVANCE_TRX", "PURCHASES_TRX", "CREDIT_LIMIT", "PAYMENTS", "MINIMUM_PAYMENTS" )
data = data_full %>% 
        select(-CUST_ID) %>% 
        drop_na() %>% 
        filter(., TENURE==12)%>% 
        select(-TENURE) 
#%>% mutate_at(vars(transformed_variables), funs(log(1 + .))) 

# Leaving only observations which have 12 months of tenure      
nrow(data_full)-length(which(data_full$TENURE<12))
data_full <- data_full %>% 
        filter(., TENURE==12) 

# Outliers treatment 
columns <-c("BALANCE","PURCHASES","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES","CASH_ADVANCE","CREDIT_LIMIT",
        "PAYMENTS","MINIMUM_PAYMENTS")

as.data.frame(data_full) %>%
        gather() %>%                           
        ggplot(aes(value)) +                    
        facet_wrap(~ key, scales = "free") +  
        geom_density() +                      
        theme(strip.text = element_text(size=5))



any(is.na(scale(data)))
# K - means  - determining the optimal number of cluster
fviz_nbclust(scale(data), kmeans, method = "wss", k.max = 10)

km_fitted <- kmeans(scale(data), centers = 4)

prcomp(scale(data)) %>% 
fviz_pca_ind(., geom = "point",habillage = km_fitted$cluster)

silhouette(km_fitted$cluster, dist(scale(data), 
                                 method = "euclidean"), lable = FALSE) %>% 
        fviz_silhouette(., print.summary = FALSE) + theme_minimal()

data_clustered <- data
data_clustered$cluster = km_fitted$cluster


data_clustered %>% 
spread(.,cluster)

data_clustered_plot = melt(data_clustered, id.var = "cluster")
data_clustered_plot$cluster = as.factor(data_clustered$cluster)

data_clustered_plot %>% 
ggplot(aes(x = variable, y = value)) +
        geom_boxplot(aes(fill = cluster), outlier.size = 1) +
        facet_wrap_paginate( ~ variable, scales = "free", ncol = 3, nrow = 2, page = 1) +
        labs(x = NULL, y = NULL) +
        theme_minimal()

data_clustered_plot %>%
        ggplot(aes(x = variable, y = value)) +
        geom_boxplot(aes(fill = cluster), outlier.size = 1) +
        facet_wrap_paginate( ~ variable, scales = "free", ncol = 3, nrow = 2, page = 2) +
        labs(x = NULL, y = NULL) +
        theme_minimal()


data_clustered_plot %>% 
        ggplot(aes(x = variable, y = value)) +
        geom_boxplot(aes(fill = cluster), outlier.size = 1) +
        facet_wrap_paginate( ~ variable, scales = "free", ncol = 3, nrow = 2, page = 3) +
        labs(x = NULL, y = NULL) +
        theme_minimal()

