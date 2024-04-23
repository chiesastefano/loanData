library(tidyverse)
library(nortest)
library(cluster)
library(factoextra)
library(Rtsne)
library(NbClust)
library(dbscan)

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
  select(-log.annual.inc)




# visualization==================================================================
# policy type: most of the clients respect the underwriting criteria of the website 

ggplot(data = data.2, aes(x = credit.policy)) +
  geom_bar(fill = "pink") +
  labs(x = "Policy Type", y = "Frequency", title = "Number of Clients by Policy Type")

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
  labs(title = "Distribution of Claim Amount",
       x = "Interest Rate",
       y = "Frequency") 
ad.test(data.2$int.rate) # not normal
quantile(data.2$int.rate)

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
       y = "Number of Clients") +
  scale_x_discrete(labels = c("Respect Policy", "Don't Respect Policy")) +
  theme(legend.position = "none")




# Delinquent last 2 years: most of the distribution 0 times
ggplot(data.2, aes(x = delinq.2yrs))+
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Number of times a Client has been delinquent Distribution",
       x = "Number of times",
       y = "Frequency")
quantile(data.2$delinq.2yrs)




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
       y = "Debt") +
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

# we need something that considers the shape of the cluster (DBScan)
dbscan.1 <- dbscan(tsne.coord.1, eps = 2, minPts = 84)
fviz_cluster(dbscan.1, data = tsne.coord.1, geom = "point", stand = FALSE, ellipse = FALSE) #Good!



# cluster analysis==============================================================
# let's assign the cluster id to each record
data.2$cluster_id <- as.factor(dbscan.1$cluster)
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





