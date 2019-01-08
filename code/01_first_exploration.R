library('dplyr')

train_data <- read.csv('candidates_toolkit/data/ml_case_training_data_v2.csv', sep = ',')
test_data <- read.csv2('candidates_toolkit/data/ml_case_test_data_v2.csv', sep = ',')
output_train <- read.csv2('candidates_toolkit/data/ml_case_training_output.csv', sep = ',')
test_output <- read.csv2('candidates_toolkit/data/ml_case_test_output_template.csv', sep = ',')

train_data$X <- NULL
test_data$X <- NULL
output_train$X <- NULL
test_output$X <- NULL


train_data <- left_join(train_data, output_train)
test_data <- left_join(test_data, test_output)

test_data %>% dim
train_data %>% dim

test_data$churn <- test_data$Churn_prediction
test_data$Churn_prediction <- NULL

setdiff(test_data %>% names, train_data %>% names)

train_data$Churn_probability <- train_data$churn

full_dataset <- rbind(train_data, test_data)


write.csv2(full_dataset, 'candidates_toolkit/data/full_dataset.csv', row.names = F)

read.csv2('candidates_toolkit/data/full_dataset.csv') %>% head


full_dataset %>% dim


train_data %>% count(churn)



# Dates --------------------------------------------------------------------------------------


full_dataset %>% select(date_activ) %>% count(is.na((date_activ)))

full_dataset$date_activ <- as.Date(full_dataset$date_activ)
full_dataset$date_end <- as.Date(full_dataset$date_end)

full_dataset$date_diff_activ = difftime(as.Date('2016-01-15'), full_dataset$date_activ)
full_dataset$date_diff_end = difftime(as.Date('2016-01-15'), full_dataset$date_end)

write.csv2(full_dataset, 'candidates_toolkit/data/full_dataset.csv', row.names = F)
