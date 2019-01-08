library('dplyr')
library('ggplot2')
library('tidyverse')
library(digorig)




full_dataset <- read.csv2('candidates_toolkit/data/full_dataset.csv')
hist_data_train <- read.csv2('candidates_toolkit/data/ml_case_training_hist_data.csv', sep = ',')
hist_data_test <- read.csv2('candidates_toolkit/data/ml_case_test_hist_data.csv', sep = ',')

hist_data <- rbind(hist_data_train, hist_data_test)

full_dataset %>% head

full_dataset

hist_data %>% head

hist_data$price_date <- as.Date(hist_data$price_date)
hist_data$id <- as.character(hist_data$id)
hist_data$price_p1_var <- as.numeric(hist_data$price_p1_var)
hist_data$price_p2_var <- as.numeric(hist_data$price_p2_var)
hist_data$price_p3_var <- as.numeric(hist_data$price_p3_var)
hist_data$price_p1_fix <- as.numeric(hist_data$price_p1_fix)
hist_data$price_p2_fix <- as.numeric(hist_data$price_p2_fix)
hist_data$price_p3_fix <- as.numeric(hist_data$price_p3_fix)

hist_data_features <- hist_data %>% 
  group_by(id) %>% 
  summarise(
    min_price_date = min(price_date),
    price_p1_max = max(price_p1_var, na.rm = T),
    price_p2_max = max(price_p2_var, na.rm = T),
    price_p3_max = max(price_p3_var, na.rm = T),
    price_p1_fix_max = max(price_p1_fix, na.rm = T),
    price_p2_fix_max = max(price_p2_fix, na.rm = T),
    price_p3_fix_max = max(price_p3_fix, na.rm = T),
    price_p1_min = min(price_p1_var, na.rm = T),
    price_p2_min = min(price_p2_var, na.rm = T),
    price_p3_min = min(price_p3_var, na.rm = T),
    price_p1_fix_min = min(price_p1_fix, na.rm = T),
    price_p2_fix_min = min(price_p2_fix, na.rm = T),
    price_p3_fix_min = min(price_p3_fix, na.rm = T),
    price_p1_median = median(price_p1_var, na.rm = T),
    price_p2_median = median(price_p2_var, na.rm = T),
    price_p3_median = median(price_p3_var, na.rm = T),
    price_p1_fix_median = median(price_p1_fix, na.rm = T),
    price_p2_fix_median = median(price_p2_fix, na.rm = T),
    price_p3_fix_median = median(price_p3_fix, na.rm = T),
    price_p1_median = median(price_p1_var, na.rm = T),
    price_p2_median = median(price_p2_var, na.rm = T),
    price_p3_median = median(price_p3_var, na.rm = T),
    price_p1_fix_median = median(price_p1_fix, na.rm = T),
    price_p2_fix_median = median(price_p2_fix, na.rm = T),
    price_p3_fix_median = median(price_p3_fix, na.rm = T),
    price_p1_mean = mean(price_p1_var, na.rm = T),
    price_p2_mean = mean(price_p2_var, na.rm = T),
    price_p3_mean = mean(price_p3_var, na.rm = T),
    price_p1_fix_mean = mean(price_p1_fix, na.rm = T),
    price_p2_fix_mean = mean(price_p2_fix, na.rm = T),
    price_p3_fix_mean = mean(price_p3_fix, na.rm = T)
  )


hist_data_evolution_features <- hist_data %>% 
  arrange(id, price_date) %>% 
  group_by(id) %>% 
  summarise(
    increased_p1_fix = (last(price_p1_fix) - first(price_p1_fix)) > 0,
    increased_p2_fix = (last(price_p2_fix) - first(price_p2_fix)) > 0,
    increased_p3_fix = (last(price_p3_fix) - first(price_p3_fix)) > 0,
    increased_p1_var = (last(price_p1_var) - first(price_p1_var)) > 0,
    increased_p2_var = (last(price_p2_var) - first(price_p2_var)) > 0,
    increased_p3_var = (last(price_p3_var) - first(price_p3_var)) > 0,
    increment_p1_fix = (last(price_p1_fix) - first(price_p1_fix)),
    increment_p2_fix = (last(price_p2_fix) - first(price_p2_fix)),
    increment_p3_fix = (last(price_p3_fix) - first(price_p3_fix)),
    increment_p1_var = (last(price_p1_var) - first(price_p1_var)),
    increment_p2_var = (last(price_p2_var) - first(price_p2_var)),
    increment_p3_var = (last(price_p3_var) - first(price_p3_var))
  )


full_dataset <- left_join(full_dataset, hist_data_evolution_features)
full_dataset <- left_join(full_dataset, hist_data_features)

full_dataset %>% filter(!is.na(churn)) %>% count(churn) %>% mutate(n/sum(n))
full_dataset %>% filter(!is.na(churn)) %>% count(increased_p1_fix, churn) %>% group_by(churn) %>% mutate(n/sum(n))
full_dataset %>% filter(!is.na(churn)) %>% count(increased_p2_fix, churn) %>% group_by(churn) %>% mutate(n/sum(n))
full_dataset %>% filter(!is.na(churn)) %>% count(increased_p3_fix, churn) %>% group_by(churn) %>% mutate(n/sum(n))
full_dataset %>% filter(!is.na(churn)) %>% count(increased_p1_var, churn) %>% group_by(churn) %>% mutate(n/sum(n))
full_dataset %>% filter(!is.na(churn)) %>% count(increased_p2_var, churn) %>% group_by(churn) %>% mutate(n/sum(n))
full_dataset %>% filter(!is.na(churn)) %>% count(increased_p3_var, churn) %>% group_by(churn) %>% mutate(n/sum(n))



full_dataset %>% 
  filter(!is.na(churn)) %>% 
  ggplot(aes(x = increased_p1, color = as.factor(churn), fill = as.factor(churn))) + 
  geom_density(alpha = 0.3) +
  geom_rug()


full_dataset %>% count(has_gas, churn) %>% group_by(churn) %>% mutate(n/sum(n))

full_dataset %>% select(net_margin) %>% summary

full_dataset$net_margin <- as.numeric(as.character(full_dataset$net_margin))

full_dataset %>% 
  filter(!is.na(churn), !is.na(net_margin)) %>% 
  ggplot(aes(x = net_margin, color = as.factor(churn), fill = as.factor(churn))) + 
  geom_density(alpha = 0.3) +
  geom_rug()

hist_features <- full_dataset

write.csv2(hist_features, 'candidates_toolkit/data/hist_features.csv', row.names = F)

scores_churn <- read.csv2('candidates_toolkit/data/scores_churn_test.csv', sep = ',')

scores_churn %>% count(churn)

scores_churn %>% 
  mutate(quantile = ntile(scores, 10)) %>% 
  group_by(quantile) %>% 
  count(churn) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = quantile, y = prop, fill = as.factor(churn))) +
  geom_bar(stat = 'identity')
  
quantile(as.numeric(as.character(scores_churn$scores)), 0.9)


full_dataset$margin_gross_pow_ele

full_dataset %>% 
  filter(!is.na(churn)) %>% 
  ggplot(aes(x = as.numeric(as.character(margin_net_pow_ele)),
             color = as.factor(churn), fill = as.factor(churn))) +
  geom_density(alpha = 0.3)
