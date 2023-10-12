source("provided_functions_reentrega.R")

data <- load_df("./data/heart.csv", "Heart", "HeartDisease")

data_df <- data$data_df
dataset_actual <- data$dataset_name
var_to_predict <- data$var_to_predict
prop_val <- 0.25

val_index <- sample(1:nrow(data_df), ceiling(nrow(data_df) * prop_val))
train_df <- data_df[-val_index,]
train_df_anterior <- data_df[-val_index,]
val_df <- data_df[val_index,]

if ("obs_id_value" %in% colnames(train_df)) {
  stop("obs_id_value is already a variable name in the data")
}
train_df$obs_id_value <- 1:nrow(train_df)
val_df$obs_id_value <- 1:nrow(val_df)

X_train <- train_df[, setdiff(colnames(train_df), var_to_predict)]
y_train <- train_df[, c("obs_id_value", var_to_predict)]
X_val <- val_df[, setdiff(colnames(train_df), var_to_predict)]
y_val <- val_df[, c("obs_id_value", var_to_predict)]
rm(train_df, val_df)

prop_noise_x <- 1
noise_level <- 30
q_noisy_var <- 5

if (prop_noise_x > 0) {
  num_rows_to_add_noise <- ceiling(nrow(X_train) * prop_noise_x)
  noise_level <- noise_level
  if (dataset_actual == "Heart"){
    df <- read.csv("./data/heart.csv")
    tree <- rpart(HeartDisease~., data = df, control=rpart.control(minsplit=2, minbucket=1, maxdepth=10, cp=0, xval=0))
    importance_scores <- tree$variable.importance
    top_variables <- names(importance_scores[order(-importance_scores)])[1:q_noisy_var]
  }
  for (var_name in top_variables) {
    if (is.numeric(X_train[[var_name]])) {
      noise <- rnorm(num_rows_to_add_noise, mean = 0, sd = noise_level * sd(X_train[[var_name]]))
      X_train[sample(nrow(X_train), num_rows_to_add_noise), var_name] <- X_train[sample(nrow(X_train), num_rows_to_add_noise), var_name] + noise
    } else {
      var_to_switch <- var_name
      
      unique_levels <- unique(X_train[[var_to_switch]])
      
      for (i in seq_len(num_rows_to_add_noise)) {
        random_row <- sample(seq_len(nrow(X_train)), 1)
        
        current_value <- X_train[random_row, var_to_switch]
        
        replacement_value <- sample(setdiff(unique_levels, current_value), 1)
        
        X_train[random_row, var_to_switch] <- replacement_value
      }
    }
  }    
}