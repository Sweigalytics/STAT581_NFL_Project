library(bestglm)
library(caret) # Used for confusionMatrix
library(dplyr)
library(DBI)
library(glmnet)
library(ggpubr)
library(psych) # For EDA
library(randomForest)
library(tidyverse)
library(xgboost)

db <- 'nfl'
host_db <- Sys.getenv(x = 'POSTGRES_HOST')
db_port <- Sys.getenv(x = 'POSTGRES_PORT')
db_user <- Sys.getenv(x = 'POSTGRES_USER')
db_pass <- Sys.getenv(x = 'POSTGRES_PASS')

con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_pass)

## Pull the primary passer stats with a limit on Hurts minimum/maximum
df_prim <- dbGetQuery(con, "select * from public.primary_passer_stats_hurts_maximum")

dbDisconnect(con)

## Find the median games played for primary QBs with Jalen Hurts' number of games or greater.
median_games_played <- median(df_prim$primary_passing_games)

## We only want to consider QBs who first played in 2018 or sooner. 
## QBs who first played later would not have had the opportunity to play the median 64 games yet.
df_qbs <- df_prim %>% filter(player_first_season <= 2018)

rownames(df_qbs) <- df_qbs$passer_player_id

## Need to convert the point differential into numeric
df_qbs <- df_qbs %>% mutate(net_point_differential_change = as.numeric(net_point_differential_change))

## Define a `franchise_qb` label if the QB played greater than or equal to the median. 
df_qbs <- df_qbs %>% mutate(franchise_qb = if_else(primary_passing_games >= median_games_played, 1, 0))
df_qbs <- df_qbs %>% mutate(across(franchise_qb, as.factor))


# EDA

### Only including the variables we want to consider for prediction.
pred_vars <- c("fumbles_per_attempt", "interceptions_per_attempt", "passing_yards_per_attempt", "passing_completion_percentage", "mean_cpoe", "rushing_yards_per_attempt", "primary_passing_tds_per_game", "primary_rushing_tds_per_game", "sacks_per_play", "interceptions_per_attempt", "fumbles_per_attempt", "turnovers_per_attempt", "epa_per_play", "net_win_percentage_change", "net_point_differential_change", "franchise_qb")

## Pulling summary statistics and writing to a CSV file to combine with the Word report.
eda_df_qbs <- describe(df_qbs[, names(df_qbs) %in% pred_vars], fast=TRUE)
write.csv(eda_df_qbs[order(row.names(eda_df_qbs)), ], "..\\..\\Report\\franchise_qb_eda.csv")

## Histograms of QB Features
num_cols <- colnames(select_if(df_qbs, is.numeric))

nbins = 10

for(i in num_cols){
  
  assign(paste('plot_',i,sep=""), 
         ggplot(data=df_qbs, aes_string(x=i)) + geom_histogram(bins = nbins) + xlab(str_to_title(gsub("_"," ",i))) + theme(text = element_text(size = 8))
  )
}



ggarrange(
  plot_fumbles_per_attempt,
  plot_interceptions_per_attempt,
  plot_passing_yards_per_attempt,
  plot_passing_completion_percentage,
  plot_mean_cpoe,
  plot_rushing_yards_per_attempt,
  plot_primary_passing_tds_per_game,
  plot_primary_rushing_tds_per_game,
  plot_sacks_per_play,
  plot_interceptions_per_attempt,
  plot_fumbles_per_attempt,
  plot_turnovers_per_attempt,
  plot_epa_per_play,
  plot_net_win_percentage_change,
  plot_net_point_differential_change
)


# Train/Test Split

set.seed(581)
spec = c(train = .8, test = .2)

g = sample(cut(
  seq(nrow(df_qbs)), 
  nrow(df_qbs)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(df_qbs, g)

df_qbs_train <- res$train[ , pred_vars]
df_qbs_train <- df_qbs_train[, names(df_qbs_train) %in% pred_vars] # Removes some column names that appear ending in ".1"

df_qbs_test <- res$test[ , pred_vars]
df_qbs_test <- df_qbs_test[, names(df_qbs_test) %in% pred_vars] # Removes some column names that appear ending in ".1"


## Validating Training data is still normally distributed.
for(i in num_cols){
  
  assign(paste('plot_train_',i,sep=""), 
         ggplot(data=df_qbs_train, aes_string(x=i)) + geom_histogram(bins = nbins) + xlab(str_to_title(gsub("_"," ",i))) + theme(text = element_text(size = 8))
  )
}


ggarrange(
  plot_train_fumbles_per_attempt,
  plot_train_interceptions_per_attempt,
  plot_train_passing_yards_per_attempt,
  plot_train_passing_completion_percentage,
  plot_train_mean_cpoe,
  plot_train_rushing_yards_per_attempt,
  plot_train_primary_passing_tds_per_game,
  plot_train_primary_rushing_tds_per_game,
  plot_train_sacks_per_play,
  plot_train_interceptions_per_attempt,
  plot_train_fumbles_per_attempt,
  plot_train_turnovers_per_attempt,
  plot_train_epa_per_play,
  plot_train_net_win_percentage_change,
  plot_train_net_point_differential_change
)

# Training and Evaluating Predictive Models

## Logistic Regression

## Scaled versions of training and test data.
normParam <- preProcess(df_qbs_train)
df_qbs_train_scaled <- predict(normParam, df_qbs_train)
df_qbs_test_scaled <- predict(normParam, df_qbs_test)

x.train <- data.matrix(subset(df_qbs_train_scaled, select = -c(franchise_qb)))
y.train <- data.matrix(df_qbs_train_scaled[, c('franchise_qb')])

### Storing the number of rows into a variable
x.train.rows <- dim(x.train)[1]

## Best Subsets (example here: https://bookdown.org/tpinto_home/Regularisation/best-subset-selection.html)
df_qbs_train.bglm <- rename(df_qbs_train, y=franchise_qb)

best.logit <- bestglm(df_qbs_train.bglm, IC = "BIC", family=binomial, method="exhaustive")

summary(best.logit$BestModel)

df_qbs_train.bglm <- rename(df_qbs_test, y=franchise_qb)

predict(best.logit$BestModel, df_qbs_train.bglm)


## Ridge Regression/Lasso

### Fit with a LOOCV by defining the folds as the number of rows in the training data.

### Lasso
grid <- 10^seq (-1, -2, length = 500)

logistic.fit.lasso <- cv.glmnet(x.train, y.train, family = "binomial", alpha = 1, lambda = grid, nfolds = x.train.rows)

plot(logistic.fit.lasso, xvar="lambda", label=TRUE)

bestlam.lasso <- logistic.fit.lasso$lambda.min

x.test <- data.matrix(subset(df_qbs_test_scaled, select = -c(franchise_qb)))
y.test <- data.matrix(df_qbs_test_scaled[, c('franchise_qb')])

logistic.lasso.preds <- predict(logistic.fit.lasso, s = bestlam.lasso, newx = x.test, type = "class")
logistic.lasso.preds

logistic.lasso.confusion <- confusionMatrix(as.factor(logistic.lasso.preds), as.factor(y.test), mode="everything", positive = "1")
logistic.lasso.confusion

accuracy.logistic.lasso <- c("Logistic Regression LASSO", logistic.lasso.confusion$overall[["Accuracy"]])

coef(logistic.fit.lasso, s = bestlam.lasso)


### Ridge Regression
grid <- 10^seq (10, -10, length = 500)

logistic.fit.ridge <- cv.glmnet(x.train, y.train, family = "binomial", alpha = 0, lambda = grid, nfolds = x.train.rows)

plot(logistic.fit.ridge, xvar="lambda", label=TRUE)

bestlam.ridge <- logistic.fit.ridge$lambda.min

x.test <- data.matrix(subset(df_qbs_test_scaled, select = -c(franchise_qb)))
y.test <- data.matrix(df_qbs_test_scaled[, c('franchise_qb')])

logistic.ridge.preds <- predict(logistic.fit.ridge, s = bestlam.ridge, newx = x.test, type = "class")
logistic.ridge.preds

logistic.ridge.confusion <- confusionMatrix(as.factor(logistic.ridge.preds), as.factor(y.test), mode="everything", positive = "1")
logistic.ridge.confusion

accuracy.logistic.ridge <- c("Logistic Regression Ridge Regression", logistic.ridge.confusion$overall[["Accuracy"]])

coef(logistic.fit.ridge, s = bestlam.ridge)


### Elastic Net
grid <- 10^seq (-0.75, -3, length = 500)

logistic.fit.elastic <- cv.glmnet(x.train, y.train, family = "binomial", alpha = 0.5, lambda = grid, nfolds = x.train.rows)

plot(logistic.fit.elastic, xvar="lambda", label=TRUE)

bestlam.elastic <- logistic.fit.elastic$lambda.min

x.test <- data.matrix(subset(df_qbs_test_scaled, select = -c(franchise_qb)))
y.test <- data.matrix(df_qbs_test_scaled[, c('franchise_qb')])

logistic.elastic.preds <- predict(logistic.fit.elastic, s = bestlam.elastic, newx = x.test, type = "class")
logistic.elastic.preds

logistic.elastic.confusion <- confusionMatrix(as.factor(logistic.elastic.preds), as.factor(y.test), mode="everything", positive = "1")
logistic.elastic.confusion

accuracy.logistic.elastic <- c("Logistic Regression Elastic Net", logistic.elastic.confusion$overall[["Accuracy"]])

coef(logistic.fit.elastic, s = bestlam.ridge)


## Random Forest

### Need to remove NA rows. Unfortunately, the CPOE data only goes back to 2006 and removes some players that are still playing today.
### Jalen Hurts is more recent and has full CPOE stats.
df_qbs_train_nas_rm <- na.omit(df_qbs_train)

x.train <- data.matrix(subset(df_qbs_train_nas_rm, select = -c(franchise_qb)))
y.train <- data.matrix(df_qbs_train_nas_rm[, c('franchise_qb')])

df_qbs_test_na_rm <- na.omit(df_qbs_test[, pred_vars])

x.test <- data.matrix(subset(df_qbs_test_na_rm, select = -c(franchise_qb)))
y.test <- data.matrix(df_qbs_test_na_rm[, c('franchise_qb')])

set.seed(581)

rf.fit <- randomForest(franchise_qb ~ ., data = df_qbs_train_nas_rm[, pred_vars], importance=TRUE, ntree=1000)
rf.pred <- predict(rf.fit, newdata = df_qbs_test_na_rm)

rf.confusion <- confusionMatrix(rf.pred, as.factor(y.test), mode = "everything", positive = "1")
rf.confusion

accuracy.rf <- c("Random Forest", rf.confusion$overall[["Accuracy"]])

importance(rf.fit)
varImpPlot(rf.fit)


## XGBoost
x.train <- data.matrix(subset(df_qbs_train, select = -c(franchise_qb)))
y.train <- data.matrix(df_qbs_train[, c('franchise_qb')])

x.test <- data.matrix(subset(df_qbs_test, select = -c(franchise_qb)))
y.test <- data.matrix(df_qbs_test[, c('franchise_qb')])

dtrain <- xgb.DMatrix(data = x.train,label = y.train)
dtest <- xgb.DMatrix(data = x.test, label = y.test)

hyper_grid <- expand.grid(max_depth = seq(2, 6, 1), eta = seq(.1, .4, .005))  

xgb_train_logloss <- NULL
xgb_test_logloss <- NULL

for (j in 1:nrow(hyper_grid)) {
  set.seed(581)
  m_xgb_untuned <- xgb.cv(
    data = dtrain,
    nrounds = 1000,
    objective = "binary:logistic",
    early_stopping_rounds = 3,
    nfold = x.train.rows,
    max_depth = hyper_grid$max_depth[j],
    eta = hyper_grid$eta[j]
  )
  
  xgb_train_logloss[j] <- m_xgb_untuned$evaluation_log$train_logloss_mean[m_xgb_untuned$best_iteration]
  xgb_test_logloss[j] <- m_xgb_untuned$evaluation_log$test_logloss_mean[m_xgb_untuned$best_iteration]
  
  cat(j, "\n")
} 

## Pulling the tuned parameters.
best_max_depth <- hyper_grid[which.min(xgb_test_logloss), ]$max_depth
best_eta <- hyper_grid[which.min(xgb_test_logloss), ]$eta

set.seed(581)

m1_xgb <-
  xgboost(
    data = dtrain,
    nrounds = 1000,
    objective = "binary:logistic",
    early_stopping_rounds = 3,
    max_depth = best_max_depth,
    eta = best_eta
  )   

xg.pred <- predict(m1_xgb, dtest)
xg.pred.labels <- as.factor(ifelse(xg.pred > 0.5,"1","0"))

xgb.confusion <- confusionMatrix(xg.pred.labels, as.factor(y.test), mode = "everything", positive = "1")
xgb.confusion

accuracy.xgb <- c("XGBoost", xgb.confusion$overall[["Accuracy"]])

importance_matrix = xgb.importance(colnames(dtrain), model = m1_xgb)
xgb.plot.importance(importance_matrix)


# Model Selection
df_accuracy <- data.frame(rbind(accuracy.logistic.elastic,
                                accuracy.logistic.lasso,
                                accuracy.logistic.ridge,
                                accuracy.rf,
                                accuracy.xgb))


colnames(df_accuracy) <- c("Model", "Overall Accuracy")

df_accuracy$`Overall Accuracy` <- as.numeric(df_accuracy$`Overall Accuracy`)

df_accuracy %>% arrange(desc(`Overall Accuracy`))

ggplot(df_accuracy, aes(x = `Overall Accuracy`, y = reorder(Model, `Overall Accuracy`))) + geom_bar(stat = "identity") +
  ylab("Predictive Model")

# Predictions
df_qbs_hurts <- df_prim %>% filter(passer_player_id == '00-0036389')
rownames(df_qbs_hurts) <- df_qbs_hurts$passer_player_id

df_qbs_hurts[, colnames(df_qbs_hurts) %in% pred_vars]

df_qbs_hurts_scaled <- predict(normParam, df_qbs_hurts[, colnames(df_qbs_hurts) %in% pred_vars])

x.hurts <- data.matrix(df_qbs_hurts_scaled)

predict(logistic.fit.lasso, s = bestlam.lasso, newx = x.hurts, type = "class")
predict(logistic.fit.lasso, s = bestlam.lasso, newx = x.hurts, type = "response")
