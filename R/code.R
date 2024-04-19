library(tidyverse)
library(nortest)

path <- "./data/loan_data.csv"

data <- read.csv(path)

data.1 <- data %>%
  mutate(
   annual.inc = exp(log.annual.inc)
  )

data.2 <- data.1 %>%
  select(-log.annual.inc)




# visualization==================================================================
# policy type: most of the clients respect the underwriting criteria of the website 
policy.counts <- data.2 %>%
  group_by(credit.policy)%>%
  summarise(count = n())

ggplot(policy.counts, aes(x = credit.policy, y = count)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(title = "Number of Clients by Policy Type",
       x = "Policy Type", y = "Number of Clients")

# 19.5% don't respect the criteria. 80.5 respect them:
print(policy.counts$count[1]/(policy.counts$count[1]+policy.counts$count[2]))


# purpose: most of the clients got a loan for debt consolidation
purpose.counts <- data.2 %>%
  group_by(purpose)%>%
  summarise(count = n())

ggplot(purpose.counts, aes(x = purpose, y = count)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Number of Clients by Purpose of the loan",
       x = "Purpose", y = "Number of Clients")


#int.rate: 75% clients < 0.14
ggplot(data.2, aes(x = int.rate)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Claim Amount",
       x = "Interest Rate",
       y = "Frequency") 
ad.test(data.2$int.rate) # not normal
quantile(data.2$int.rate)

#installment: 75% of clients <432
ggplot(data.2, aes(x = installment)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Installment",
       x = "Installment",
       y = "Frequency") 
quantile(data.2$installment)


#dti
#installment: 75% of clients <17.95
ggplot(data.2, aes(x = dti)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of DTI",
       x = "Debt-Income Ratio",
       y = "Frequency") 
quantile(data.2$dti)





