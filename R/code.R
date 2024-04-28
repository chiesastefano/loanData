library(tidyverse)
library(nortest)
library(cluster)
library(factoextra)
library(Rtsne)
library(NbClust)
library(dbscan)
library(gridExtra)
library(plotly)
library(ggcorrplot)
library(glmnet)
library(pROC)
library(MASS)
library(Metrics)
library(car)
library(randomForest)
library(rpart)



path <- "./data/loan_data.csv"

data <- read.csv(path)

data.1 <- data %>%
  mutate(
   annual.inc = exp(log.annual.inc),
   credit.policy = factor(credit.policy),
   not.fully.paid = factor(not.fully.paid),
   debt = (dti * annual.inc),
   total_interests = debt * int.rate
  )

data.2 <- data.1 %>%
  dplyr::select(-log.annual.inc)




# visualization==================================================================
# policy type: most of the clients respect the underwriting criteria of the website 

ggplot(data = data.2, aes(x = credit.policy)) +
  geom_bar(fill = "pink") +
  labs(x = "Credit Policy", y = "Frequency", title = "Number of Clients by Credit Policy")

# 19.5% don't respect the criteria. 80.5 respect them:
no_policy = policy.counts$count[1]
yes_policy = policy.counts$count[2]
print(no_policy/(no_policy+yes_policy))


# purpose: most of the clients got a loan for debt consolidation
ggplot(data = data.2, aes(x = purpose)) +
  geom_bar(fill = "lightblue") +
  labs(x = "Purpose", y = "Frequency", title = "Number of Clients by Purpose of the loan")

# int.rate: 75% clients < 0.14
ggplot(data.2, aes(x = int.rate)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Interest Rate",
       x = "Interest Rate",
       y = "Frequency") 
ad.test(data.2$int.rate) # not normal
quantile(data.2$int.rate)


ggplot(data.2, aes(x = factor(credit.policy), y = int.rate, fill = credit.policy)) +
  geom_boxplot() +
  labs(title = "Interest Rate of Clients that Respect vs. Don’t Respect the Company Credit Policy",
       x = "Credit Policy",
       y = "Interest Rate") +
  scale_x_discrete(labels = c("Respect Policy", "Don't Respect Policy")) +
  theme(legend.position = "none")


# installment: 75% of clients <432
ggplot(data.2, aes(x = installment)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Installment",
       x = "Installment",
       y = "Frequency") 
quantile(data.2$installment)


# dti: 75% less then 17.95
ggplot(data.2, aes(x = dti)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of DTI",
       x = "Debt-Income Ratio",
       y = "Frequency") 
quantile(data.2$dti)



# FICO: 75 less then 737 
ggplot(data.2, aes(x= fico)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of FICO",
       x = "FICO",
       y = "Frequency")
quantile(data.2$fico)

# clients that don't respect company credit policy have a higher credit score (weird)
ggplot(data.2, aes(x = factor(credit.policy), y = fico, fill = credit.policy)) +
  geom_boxplot() +
  labs(title = "FICO of Clients Who Respect vs. Don’t Respect the Company Credit Policy",
       x = "Credit Policy",
       y = "FICO") +
  scale_x_discrete(labels = c("Respect Policy", "Don't Respect Policy")) +
  theme(legend.position = "none")





#days of credit line: 75% less than 5730
ggplot(data.2, aes(x = days.with.cr.line)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Days having a Credit line",
         x = "Number of days",
         y = "Frequency")
quantile(data.2$days.with.cr.line)


# amount unpaid at the end of the credit card billing cycle
ggplot(data.2, aes(x = revol.bal))+
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Revolving Balance",
       x = "Revolving Balance",
       y = "Frequency")
quantile(data.2$revol.bal)

# box plot: more variance for clients that don't respect company policy, median almost identical
ggplot(data.2, aes(x = factor(credit.policy), y = log(revol.bal), fill = credit.policy)) +
  geom_boxplot() +
  labs(title = "Revolving Balance of Clients Who Respect vs. Don’t Respect the Company Credit Policy",
       x = "Credit Policy",
       y = "Log of Revolving Balance") +
  scale_x_discrete(labels = c("Respect Policy", "Don't Respect Policy")) +
  theme(legend.position = "none")

# how many 0 values in the category "No policy": 5.2%
indices <- which(data.2$revol.bal[data.2$credit.policy == 0] == 0)
no_policy_n = length(indices)
no_policy_ratio = no_policy_n/no_policy
no_policy_ratio

# how many 0 values in the category "Yes Policy" 2.8%
indices.2 <- which(data.2$revol.bal[data.2$credit.policy == 1] == 0)
yes_policy_n = length(indices.2)
yes_policy_ratio = yes_policy_n/yes_policy
yes_policy_ratio


# Credit line utilization rate: 75% less than 70.9
ggplot(data.2, aes(x = revol.util))+
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Clients' Utilization Rate",
       x = "Utilization Rate",
       y = "Frequency")
quantile(data.2$revol.util)

# box plot: higher utilization rate for clients that respect the policy (weird)
ggplot(data.2, aes(x = credit.policy, y = revol.util, fill = credit.policy)) +
  geom_boxplot() +
  labs(title = "Utilization Rate of Clients Who Respect vs. Don’t Respect the Company Credit Policy",
       x = "Credit Policy",
       y = "Utilization Rate") +
  scale_x_discrete(labels = c("Respect Policy", "Don't Respect Policy")) +
  theme(legend.position = "none")




# Inquiries last 6 month: 75% less than 2
ggplot(data.2, aes(x = inq.last.6mths))+
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Number of Inquiries Last 6 Month Distribution",
       x = "Number of inquiries",
       y = "Frequency")
quantile(data.2$inq.last.6mths)

# box plot: higher inquiries of Clients that respect the policy (weird)
ggplot(data.2, aes(x = credit.policy, y = inq.last.6mths, fill = credit.policy)) +
  geom_boxplot() +
  labs(title = "Number of Inquiries for Clients Who Respect vs. Don’t Respect the Company Credit Policy",
       x = "Credit Policy",
       y = "Number of Inquiries") +
  scale_x_discrete(labels = c("Respect Policy", "Don't Respect Policy")) +
  theme(legend.position = "none")




# Delinquent last 2 years: most of the distribution 0 times
ggplot(data.2, aes(x = delinq.2yrs))+
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Number of times a Client has been delinquent Distribution",
       x = "Number of times",
       y = "Frequency")
quantile(data.2$delinq.2yrs)

plot(data.2$delinq.2yrs)


# pub.rec: most of the clients with 0 derogatory public records
ggplot(data.2, aes(x = pub.rec))+
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Number of Derogatory Public Records Distribution",
       x = "Number of DPR",
       y = "Frequency")
quantile(data.2$pub.rec)



# not.fully.paid: Number most of the clients have fully paid the loan
ggplot(data = data.2, aes(x = not.fully.paid)) +
  geom_bar(fill = "red") +
  labs(x = "Client still owe money", y = "Frequency", title = "Clients whose loan is not fully paid")


#annual income: Median = 55764
ggplot(data.2, aes(x = annual.inc))+
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Annual Income Distribution",
       x = "Annual Income",
       y = "Frequency")
quantile(data.2$annual.inc)

ggplot(data.2, aes(x = credit.policy, y = log(annual.inc), fill = credit.policy)) +
  geom_boxplot() +
  labs(title = "Annual Income of Clients Who Respect vs. Don’t Respect the Company Credit Policy",
       x = "Credit Policy",
       y = "Log Annual Income") +
  scale_x_discrete(labels = c("Respect Policy", "Don't Respect Policy")) +
  theme(legend.position = "none")

#debt: Median = 669,795.7
ggplot(data.2, aes(x = debt))+
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Debt Distribution",
       x = "Debt",
       y = "Frequency")
quantile(data.2$debt)

#boxplot: not significant difference
ggplot(data.2, aes(x = credit.policy, y = log(debt), fill = credit.policy)) +
  geom_boxplot() +
  labs(title = "Debt of Clients Who Respect vs. Don’t Respect the Company Credit Policy",
       x = "Credit Policy",
       y = "Log Debt") +
  scale_x_discrete(labels = c("Respect Policy", "Don't Respect Policy")) +
  theme(legend.position = "none")

# scatterplot Annual Income VS Debt: the more the income, the more the debt (as a treshold)
ggplot(data = data.2, aes(x = log(annual.inc), y = log(debt))) +
  geom_point(alpha = 0.5) +
  labs(x = "Log Annual Income", y = "Log Debt", title = "Annual Income VS Debt")

data.2_sample <- data.2[sample(nrow(data.2), 1000), ]

# scatterplot 
ggplot(data = data.2_sample, aes(x = log(annual.inc), y = log(debt))) +
  geom_point(alpha = 0.5) +
  labs(x = "Log of Annual Income", y = "Log of Debt", title = "Scatterplot of Log Annual Income vs Log Debt")



# scatterplot Annual Income VS Interest Rates: not a clear pattern
ggplot(data = data.2, aes(x = log(annual.inc), y = int.rate)) +
  geom_point(alpha = 0.5) +
  labs(x = "Log Annual Income", y = "Interest Rate", title = "Annual Income VS Interest Rate")


#cluster_dimensions = cbind(as.factor(dbscan.1$cluster), as.factor(dbscan))



# data encoding===============================================================
data.3 <- data.2 %>%
  mutate(
    purpose = as.factor(case_when(
      purpose == "all_other" ~ "1",
      purpose == "credit_card" ~ "2",
      purpose == "debt_consolidation" ~ "3",
      purpose == "educational" ~ "4",
      purpose == "home_improvement" ~ "5",
      purpose == "major_purchase" ~ "6",
      purpose == "small_business" ~ "7"
    )))

encoded.data <- model.matrix(~ 0 + purpose, data.3)
data.4 <- cbind(data.3, encoded.data)

data.5 <- data.4 %>%
  select(-purpose) %>%
  rename(
    all_other = purpose1,
    credit_card = purpose2,
    debt_consolidation = purpose3,
    educational = purpose4,
    home_improvement = purpose5,
    major_purchase = purpose6,
    small_business = purpose7
  ) %>%
  mutate(
    all_other = as.factor(all_other),
    credit_card = as.factor(credit_card),
    debt_consolidation = as.factor(debt_consolidation),
    educational = as.factor(educational),
    home_improvement = as.factor(home_improvement),
    major_purchase = as.factor(major_purchase),
    small_business = as.factor(small_business)
  )




#clustering=====================================================================
# Let's start with clustering with clients related variables (excluding the one assigned by the company)
data.6 <- data.5 %>%
  select( -credit.policy, -int.rate, -installment, -not.fully.paid, -debt, -total_interests)
colnames(data.6)

# I don't want to scale binary variables
numeric_columns.1 <- sapply(data.6, is.numeric)
data.6s <- scale(data.6[, numeric_columns.1])


#compute the distance matrix with the scaled data using Gower
dist.1 <- as.matrix(daisy(data.6s))


# data reduction
set.seed(123)
tsne.1 <- Rtsne(dist.1, perplexity = 479, dims = 2, is_distance = TRUE) # perplexity equal to 5% of total records
tsne.coord.1 <- tsne.1$Y
colnames(tsne.coord.1) <- c("X1", "X2")
plot(tsne.coord.1, col = "blue", pch = 20, main = "t-SNE Visualization") # looks like 3 clusters

# 4 clusters or 6
fviz_nbclust(tsne.coord.1, kmeans, method = "wss")
fviz_nbclust(tsne.coord.1, kmeans, method = "silhouette")


# try NbClust function
#km.nbclust.1 <- NbClust(tsne.coord.1, max.nc = 6, method = "kmeans")

# ******************************************************************* 
#   * Among all indices:                                                
#   * 2 proposed 2 as the best number of clusters 
# * 8 proposed 3 as the best number of clusters 
# * 6 proposed 4 as the best number of clusters 
# * 7 proposed 6 as the best number of clusters 
# 
# ***** Conclusion *****                            
#   
#   * According to the majority rule, the best number of clusters is  3 
# 
# 
# ******************************************************************* 
#3 clusters
fviz_cluster(list(data = tsne.coord.1, cluster = km.nbclust.1$Best.partition),
                                  geom = "point", stand = FALSE, ellipse = FALSE)

fviz_cluster(list(data = tsne.coord.1, cluster = km.nbclust.1$Best.partition),
             geom = "point", stand = FALSE, ellipse = FALSE)

# 4 clusters
km.1 <- kmeans(tsne.coord.1, centers = 4, nstart = 10)
fviz_cluster(km.1, data = tsne.coord.1, geom = "point",
             stand = FALSE, ellipse = FALSE)

# 6 clusters
km.2 <- kmeans(tsne.coord.1, centers = 6, nstart = 10)
fviz_cluster(km.2, data = tsne.coord.1, geom = "point",
             stand = FALSE, ellipse = FALSE)



#hierarchical clustering
k.1 <- 3
dist.1_d <- as.dist(dist.1)
h.1_complete <- hclust(dist.1_d, method = "complete")
h.1.clusters_complete <- cutree(h.1_complete, k = k.1)
plot.h.1_complete <- fviz_cluster(list(data = data.6s, cluster = h.1.clusters_complete),
                                  geom = "point", stand = FALSE, ellipse = FALSE)

h.1_single <- hclust(dist.1_d, method = "single")
h.1.clusters_single <- cutree(h.1_single, k = k.1)
plot.h.1_single <- fviz_cluster(list(data = data.6s, cluster = h.1.clusters_single),
                                geom = "point", stand = FALSE, ellipse = FALSE)

h.1_average <- hclust(dist.1_d, method = "average")
h.1.clusters_average <- cutree(h.1_average, k = k.1)
plot.h.1_average <- fviz_cluster(list(data = data.6s, cluster = h.1.clusters_average),
                                 geom = "point", stand = FALSE, ellipse = FALSE)

grid.arrange(plot.h.1_complete, plot.h.1_single, plot.h.1_average, ncol = 3)


# we need something that considers the shape of the cluster (DBScan)
dbscan.1 <- dbscan(tsne.coord.1, eps = 2, minPts = 84)
fviz_cluster(dbscan.1, data = tsne.coord.1, geom = "point", stand = FALSE, ellipse = FALSE) #Good!




#try with 3 dimension data reduction
seed(1234)
tsne.2 <- Rtsne(dist.1, perplexity = 479, dims = 3, is_distance = TRUE) # perplexity equal to 5% of total records
tsne.coord.2 <- tsne.2$Y
dbscan.2 <- dbscan(tsne.coord.2, eps = 3, minPts = 84)


colnames(tsne.coord.2) <- c("X1", "X2", "X3")
p <- plot_ly(data = as.data.frame(tsne.coord.2),
            x = ~tsne.coord.2[,1],
            y = ~tsne.coord.2[,2],
            z = ~tsne.coord.2[,3],
            color = ~dbscan.2$cluster) %>%
 add_markers(size = 1.5)
print(p) # we obtained pretty much the same clusters


data.2$cluster_id <- as.factor(dbscan.1$cluster)

# cluster analysis==============================================================
# let's assign the cluster id to each record

sum(data.2$cluster_id == 0) # 90 outliers < 1% of the dataset

data.2 <- data.2 %>%
  filter(cluster_id != 0) # filter the outliers


# credit policy: not much difference
policy.counts_clusters <- data.2 %>%
  group_by(cluster_id, credit.policy) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(cluster_id) %>%
  mutate(freq = count/sum(count))

ggplot(policy.counts_clusters, aes(x = cluster_id, y = freq, fill = credit.policy)) +
  geom_bar(stat = "identity") +
  labs(title = "Credit Policy Type Across Clusters ",
       x = "Cluster", y = "Percentage of Claims")

#purpose: not much difference
purpose.counts_clusters <- data.2 %>%
  group_by(cluster_id, purpose) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(cluster_id) %>%
  mutate(freq = count/sum(count))

ggplot(purpose.counts_clusters, aes(x = cluster_id, y = freq, fill = purpose)) +
  geom_bar(stat = "identity") +
  labs(title = "Purpose Across Clusters ",
       x = "Cluster", y = "Percentage of Purpose")

#int.rate: lower interest rate for cluster 1
ggplot(data.2, aes(x = cluster_id, y = int.rate, fill = cluster_id)) +
  geom_boxplot() +
  labs(title = "Interest Rate Distribution Across Clusters",
       x = "Cluster",
       y = "Interest Rate")


#installment: not much difference
ggplot(data.2, aes(x = cluster_id, y = installment, fill = cluster_id)) +
  geom_boxplot() +
  labs(title = "Installment Distribution Across Clusters",
       x = "Cluster",
       y = "Installment")

#dti: not much difference
ggplot(data.2, aes(x = cluster_id, y = dti, fill = cluster_id)) +
  geom_boxplot() +
  labs(title = "Debt-to-income ratio Across Clusters",
       x = "Cluster",
       y = "DTI")

#fico: higher FICO for cluster 1 (that also has lower interest rate)
ggplot(data.2, aes(x = cluster_id, y = fico, fill = cluster_id)) +
  geom_boxplot() +
  labs(title = "Fico ratio Across Clusters",
       x = "Cluster",
       y = "DTI")


#days.with.cr.line: increasing between clusters
ggplot(data.2, aes(x = cluster_id, y = days.with.cr.line, fill = cluster_id)) +
  geom_boxplot() +
  labs(title = "Days with a Credit Line Across Clusters",
       x = "Cluster",
       y = "Days with a Credit Line")


#revol.bal: not much difference
ggplot(data.2, aes(x = cluster_id, y = log(revol.bal), fill = cluster_id)) +
  geom_boxplot() +
  labs(title = "Revolving Balance Across Clusters",
       x = "Cluster",
       y = "Log Revolving Balance")


#revol.util: higher for the third one
ggplot(data.2, aes(x = cluster_id, y = revol.util, fill = cluster_id)) +
  geom_boxplot() +
  labs(title = "Revolving Line Utilization rate Across Clusters",
       x = "Cluster",
       y = "Utilization Rate")



#inq.last.6mths: the 3d cluster has an higher percentage of inquiries
inq_sum <- data.2 %>%
  group_by(cluster_id) %>%
  summarise(sum_inq = sum(inq.last.6mths), n = n()) %>%
  mutate(relative_sum = sum_inq / n)

ggplot(inq_sum, aes(x = as.factor(cluster_id), y = relative_sum, fill = as.factor(cluster_id))) +
  geom_bar(stat = "identity") +
  labs(title = "Relative Sum of Inquiries in the last 6 months Across Clusters",
       x = "Cluster",
       y = "Inquiries %")


#delinq.2yrs: cluster two and three have an higher percentage of delinquent
delinq.2yrs_sum <- data.2 %>%
  group_by(cluster_id) %>%
  summarise(sum_del = sum(delinq.2yrs), n = n()) %>%
  mutate(relative_sum = sum_del / n)

ggplot(delinq.2yrs_sum, aes(x = as.factor(cluster_id), y = relative_sum, fill = as.factor(cluster_id))) +
  geom_bar(stat = "identity") +
  labs(title = "Relative Sum of Delinquents in the last 2 years Within Clusters",
       x = "Cluster",
       y = "Delinquents %")



#pub.rec: almost all the derogatory public records are in the 3d cluster
pub.rec_sum <- data.2 %>%
  group_by(cluster_id) %>%
  summarise(sum_pub_rec = sum(pub.rec), n = n()) %>%
  mutate(relative_sum = sum_pub_rec / n)

ggplot(pub.rec_sum, aes(x = as.factor(cluster_id), y = relative_sum, fill = as.factor(cluster_id))) +
  geom_bar(stat = "identity") +
  labs(title = "Relative Sum of Derogatory Public Records Within Clusters",
       x = "Cluster",
       y = "Derogatory Public Records %")



# not.fully.paid: sightly increasing (but similar)
not.fully.paid.counts_clusters <- data.2 %>%
  group_by(cluster_id, not.fully.paid) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(cluster_id) %>%
  mutate(freq = count/sum(count))

ggplot(not.fully.paid.counts_clusters, aes(x = cluster_id, y = freq, fill = not.fully.paid)) +
  geom_bar(stat = "identity") +
  labs(title = "Not fully paid Distribution Accross Clusters",
       x = "Cluster", y = "Percentage of Claims")




#annual.inc: similar
ggplot(data.2, aes(x = cluster_id, y = annual.inc, fill = cluster_id)) +
  geom_boxplot() +
  labs(title = "Annual Income Across Clusters",
       x = "Cluster",
       y = "Annual Income")


#debt: similar
ggplot(data.2, aes(x = cluster_id, y = debt, fill = cluster_id)) +
  geom_boxplot() +
  labs(title = "Total Debt Across Clusters",
       x = "Cluster",
       y = "Debt")


#total_interests: similar, but increasing between clusters
ggplot(data.2, aes(x = cluster_id, y = log(total_interests), fill = cluster_id)) +
  geom_boxplot() +
  labs(title = "Total Interests Across Clusters",
       x = "Cluster",
       y = "Interests")










#Which are the variables that make a client respecting the company policy?=========
data.7 <- data.2 %>%
  dplyr::select(-int.rate, -not.fully.paid, -total_interests, -installment)

corr.matrix.1 <- cor(data.7 %>% select_if(.predicate = is.numeric))
ggcorrplot(corr.matrix.1, type = "lower", outline.color = "white", lab = TRUE) +
  ggtitle("Correlation Heatmap")





# split the Data into Training and Test Set
ind <- sample(nrow(data.7), size = 0.8*nrow(data.7))
data.train.1 <- data.7[ind, ]
data.test.1 <- data.7[-ind, ]

# we use glm() with specifying a binomial family because we are estimating a binary variable 
model.1 <- glm(credit.policy ~ ., family = binomial(), data = data.train.1)
summary(model.1)

prediction.1 <- round(predict(model.1, newdata = data.test.1, type = "response"))
vif(model.1)

# compute some indices for evaluation 
model.1.auc <- pROC::auc(pROC::roc(data.test.1$credit.policy, prediction.1))

# plot the ROC curve
roc_curve <- pROC::roc(data.test.1$credit.policy, prediction.1)
#plot(roc_curve, main="ROC Curve", col = "blue")
#legend("bottomright", legend = paste("AUC = ", round(auc(roc_curve), 2)), col = "blue", lwd = 2, bty = "n")


model.1.conf.matrix <- caret::confusionMatrix(data = factor(prediction.1, levels = c("0", "1")),
                                              reference = data.test.1$credit.policy,
                                              positive = "1")
model.1.accuracy <- model.1.conf.matrix$overall["Accuracy"]
model.1.precision <- model.1.conf.matrix$byClass["Precision"]
model.1.recall <- model.1.conf.matrix$byClass["Recall"]
model.1.f1 <- model.1.conf.matrix$byClass["F1"]



model.1.conf.matrix$table
model.1.auc #0.8005
model.1.accuracy #TP+TN/ALL 0.9044885 
model.1.precision #TP/TP+FP 0.9168704
model.1.recall #TP/TP+FN 0.9696186 
model.1.f1 #Harmonic mean between precision and recall 0.9425071 

#Probably very good because the company assigns the credit.policy based on this data





# th model has a variable that, after a certain threshold, predict 100% of 0 or 1 

# let's check the variables one by one
table(data.train.1$purpose, data.train.1$credit.policy)
for(predictor in names(data.train.1)) {
  if(is.numeric(data.train.1[[predictor]])) {
    plot(density(data.train.1[data.train.1$credit.policy == 0 & !is.na(data.train.1[[predictor]]), predictor], na.rm=TRUE), 
         col='red', main=paste('Density Plot of', predictor, 'by Credit Policy'), xlab=predictor)
    lines(density(data.train.1[data.train.1$credit.policy == 1 & !is.na(data.train.1[[predictor]]), predictor], na.rm=TRUE), 
          col='blue')
    legend("topright", c("credit.policy=0", "credit.policy=1"), fill=c("red", "blue"))
  }
}

#inq.last.6mths, after 9 times, predict 100% of the records being 0.
# same problem with revol.bal
# The coefficient of this variable is also very high compared to the others.

#I could remove them




# let's try with Ridge regression (I'm using Ridge instead of Lasso because I believe that most of the variables are relevant)
model.matrix.1 <- model.matrix(credit.policy ~ . - 1, data = data.train.1)  # predictors
model.vector.1 <- data.train.1$credit.policy  # outcome


model.2 <- glmnet(model.matrix.1, model.vector.1, family = "binomial", alpha = 0)
summary(model.2)

model.2.fit <- cv.glmnet(model.matrix.1, model.vector.1, family = "binomial")
optimal_lambda <- model.2.fit$lambda.min
model.2.coefficients <- coef(model.2, s = optimal_lambda)

model.matrix.test.1 <- model.matrix(credit.policy ~ . - 1, data = data.test.1)
predictions.2 <- round(predict(model.2, newx = model.matrix.test.1, type = "response", s = optimal_lambda))




# compute some indices for evaluation 
model.2.auc <- pROC::auc(pROC::roc(data.test.1$credit.policy, predictions.2))
model.2.conf.matrix <- caret::confusionMatrix(data = factor(predictions.2, levels = c("0", "1")),
                                              reference = data.test.1$credit.policy,
                                              positive = "1")
model.2.accuracy <- model.2.conf.matrix$overall["Accuracy"]
model.2.precision <- model.2.conf.matrix$byClass["Precision"]
model.2.recall <- model.2.conf.matrix$byClass["Recall"]
model.2.f1 <- model.2.conf.matrix$byClass["F1"]


model.2.conf.matrix$table
model.2.auc #0.7611 lower
model.2.accuracy #TP+TN/ALL 0.9008351 same
model.2.precision #TP/TP+FP 0.8988 lower
model.2.recall #TP/TP+FN 0.9883 higher 
model.2.f1 #harmonic mean between precision and recall 0.9415 lower
# the two model performance are not much different

model.1.auc #0.8005
model.1.accuracy #TP+TN/ALL 0.9044885 
model.1.precision #TP/TP+FP 0.9168704
model.1.recall #TP/TP+FN 0.9696186 
model.1.f1 #Harmonic mean between precision and recall 0.9425071 




# step-wise selection (bi-directional)
data.train.1 <- data.7[ind, ]
data.test.1 <- data.7[-ind, ]
model.3 <- stepAIC(model.1, direction = "both", trace = 0)
summary(model.3) #no dit and pub.rec (both not statistically significant)

prediction.3 <- round(predict(model.3, newdata = data.test.1, type = "response"))


# compute some indices for evaluation 
model.3.auc <- pROC::auc(pROC::roc(data.test.1$credit.policy, prediction.3))
model.3.conf.matrix <- caret::confusionMatrix(data = factor(prediction.3, levels = c("0", "1")),
                                              reference = data.test.1$credit.policy,
                                              positive = "1")
model.3.accuracy <- model.3.conf.matrix$overall["Accuracy"]
model.3.precision <- model.3.conf.matrix$byClass["Precision"]
model.3.recall <- model.3.conf.matrix$byClass["Recall"]
model.3.f1 <- model.3.conf.matrix$byClass["F1"]


model.3.conf.matrix$table
model.3.auc #0.7992 lower
model.3.accuracy #TP+TN/ALL 0.90396 lower 
model.3.precision #TP/TP+FP 0.916310 lower
model.3.recall #TP/TP+FN 0.9696186 same
model.3.f1 #Harmonic mean between precision and recall 0.94221 lower
#not much difference





# random forest
ind <- sample(nrow(data.7), size = 0.8*nrow(data.7))
data.train.5 <- data.7[ind, ]
data.test.5 <- data.7[-ind, ]

rf.1=randomForest(credit.policy~.,data=data.train.5)
rf.1   # it can't predict fraudulent
predictions.5 <- round(predict(model.1, newdata = data.test.1, type = "response"))

table(predictions.5, data.test.5$credit.policy)



model.5.auc <- pROC::auc(pROC::roc(data.test.5$credit.policy, predictions.5))
model.5.conf.matrix <- caret::confusionMatrix(data = factor(predictions.5, levels = c("0", "1")),
                                              reference = data.test.5$credit.policy,
                                              positive = "1")
model.5.accuracy <- model.5.conf.matrix$overall["Accuracy"]
model.5.precision <- model.5.conf.matrix$byClass["Precision"]
model.5.recall <- model.5.conf.matrix$byClass["Recall"]
model.5.f1 <- model.5.conf.matrix$byClass["F1"]


model.5.conf.matrix$table
model.5.auc #0.8193 higher
model.5.accuracy #TP+TN/ALL 0.9008351 same
model.5.precision #TP/TP+FP 0.9178255 higher
model.5.recall #TP/TP+FN 0.9603175  lower 
model.5.f1 #harmonic mean between precision and recall 0.9385908  lower













#tree
library(rpart.plot)

ind <- sample(nrow(data.7), size = 0.8*nrow(data.7))
data.train.6 <- data.7[ind, ]
data.test.6 <- data.7[-ind, ]

model.6 <- rpart(credit.policy ~ ., data = data.train.6)
par(mar=c(1,1,1,1))
prp(model.6, extra = 1, box.palette = c("lightblue", "lightgreen"),
    branch.lty = 3, branch = 1, varlen = 0, yesno = 2, shadow.col = "gray", faclen = 0)
predictions.6 <- predict(model.6, newdata = data.test.6, type = "class")

model.6.conf.matrix <- caret::confusionMatrix(data = factor(data.test.6$credit.policy, levels = c("0", "1")),
                                              reference = factor(predictions.6, levels = c("0", "1")),
                                              positive = "1")
model.6.accuracy <- model.6.conf.matrix$overall["Accuracy"]
model.6.precision <- model.6.conf.matrix$byClass["Precision"] # Precision is calculated as Sensitivity
model.6.recall <- model.6.conf.matrix$byClass["Recall"] # Recall is calculated as Precision
model.6.f1 <- model.6.conf.matrix$byClass["F1"]

summary(model.6)
model.6.conf.matrix$table 
model.6.accuracy #0.9822547   
model.6.precision #0.9961735  
model.6.recall #0.9823899 
model.6.f1 #0.9892337 














#Which are the variables that affect more the decision of the interest rate?====


data.8 <- data.2 %>%
  mutate(
    iti = installment / (annual.inc / 12) # compute the installment to income ratio
  ) %>%
  dplyr::select(-cluster_id, -not.fully.paid, -total_interests, -installment)


corr.matrix.3 <- cor(data.8 %>% select_if(.predicate = is.numeric))
ggcorrplot(corr.matrix.3, type = "lower", outline.color = "white", lab = TRUE) +
  ggtitle("Correlation Heatmap")




ind2 <- sample(nrow(data.8), size = 0.8*nrow(data.8))
data.train.2 <- data.8[ind2, ]
data.test.2 <- data.8[-ind2, ]

model.4 <- glm(int.rate ~ ., family = Gamma("log"), data = data.train.2)
summary(model.4) #iti significant 5% confidence
vif(model.4)  #annual.inc and debt could cause multicollinearity


prediction.4 <- predict(model.4, newdata = data.test.2, type = "response")


# compute R-squared
rsq <- cor(prediction.4, data.test.2$int.rate)^2
rsq #0.6093039



# compute McFadden's R-squared
model.null <- glm(int.rate ~ 1, family = Gamma("log"), data = data.train.2)
rsq.mcfadden <- 1 - logLik(model.4)/logLik(model.null)
rsq.mcfadden #'log Lik.' -0.2305983 (df=20)


# compute RMSE
rmse <- rmse(data.test.2$int.rate, prediction.4)
rmse # 0.01695035
mean(data.test.2$int.rate) #0.1230635 

# residuals
res.1 <- data.test.2$int.rate - prediction.4
plot(res.1)

data.test.2$res1 <- res.1
corr.matrix.2 <- cor(data.test.2 %>% select_if(.predicate = is.numeric))
ggcorrplot(corr.matrix.2, type = "lower", outline.color = "white", lab = TRUE) +
  ggtitle("Correlation Heatmap")
#probably the error is higher when the interest rate is higher because:
# The distribution is right skewed
# There are other factors influencing the int.rate that are not included in the model (eg. inflation rate)

data.test.2$res1 <- NULL

#try with step-wise selection
model.5 <- stepAIC(model.4, direction = "both", trace = 0)
summary(model.5) # removed(days.with.cr.line, delinq.2yrs, annual.inc, pub.rec, debt) (non of them where statistically significant)
vif(model.5) # better

prediction.5 <- predict(model.5, newdata = data.test.2, type = "response")

# compute R-squared
rsq <- cor(prediction.5, data.test.2$int.rate)^2
rsq #0.6088126 # higher R^2 even with less variables


model.null2 <- glm(int.rate ~ 1, family = Gamma("log"), data = data.train.2)
rsq.mcfadden2 <- 1 - logLik(model.5)/logLik(model.null2)
rsq.mcfadden2 #'log Lik.' -0.2305437 (df=18): similar to the previous, but with less predictors

# compute RMSE
rmse <- rmse(data.test.2$int.rate, prediction.5)
rmse # 0.01695035
mean(data.test.2$int.rate) #0.1226729 


# residuals
res.2 <- data.test.2$int.rate - prediction.5
plot(res.2)

data.test.2$res2 <- res.2
corr.matrix.3 <- cor(data.test.2 %>% select_if(.predicate = is.numeric))
ggcorrplot(corr.matrix.3, type = "lower", outline.color = "white", lab = TRUE) +
  ggtitle("Correlation Heatmap") #almost identical



plot(data.test.2$int.rate, res.2, xlab = "Interest Rate", ylab = "res.2")
abline(lm(res.2 ~ data.test.2$int.rate), col = "red")