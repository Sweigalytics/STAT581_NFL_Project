library(bestglm)
library(caret)
library(dplyr)
library(leaps)
library(glmnet)
library(ggpubr)
library(tidyverse)


db <- 'nfl'
host_db <- Sys.getenv(x = 'POSTGRES_HOST')
db_port <- Sys.getenv(x = 'POSTGRES_PORT')
db_user <- Sys.getenv(x = 'POSTGRES_USER')
db_pass <- Sys.getenv(x = 'POSTGRES_PASS')

con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_pass)

## Pull the primary passer stats with a limit on Hurts minimum/maximum
df_qb_contracts <- dbGetQuery(con, "select * from public.primary_passer_stats_hurts_maximum_contracts")

dbDisconnect(con)

## Need to convert the point differential into numeric
df_qb_contracts <- df_qb_contracts %>% mutate(net_point_differential_change = as.numeric(net_point_differential_change))

## Pull Hurts into his own dataframe, then filter him out of the primary dataset.
df_qb_contracts_hurts <- df_qb_contracts %>% filter(passer_player_id == '00-0036389')
df_qb_contracts <- df_qb_contracts %>% filter(passer_player_id != '00-0036389')

EDA

## Histograms of QB Features
num_cols <- colnames(select_if(df_qb_contracts, is.numeric))

nbins = 10

for(i in num_cols){
  
  assign(paste('plot_',i,sep=""), 
         ggplot(data=df_qb_contracts, aes_string(x=i)) + geom_histogram(bins = nbins) + xlab(str_to_title(gsub("_"," ",i))) + theme(text = element_text(size = 8))
  )
}

ggarrange(
  plot_days_to_hurts_game,
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


### Found that including QBs with `days_to_hurts_game` > 2000 brings in veteran QBs. Filtering them out and attempting histograms again.
df_qb_contracts_nonvets <- df_qb_contracts %>% filter(days_to_hurts_game < 2000)

for(i in num_cols){
  
  assign(paste('plot_',i,sep=""), 
         ggplot(data=df_qb_contracts_nonvets, aes_string(x=i)) + geom_histogram(bins = nbins) + xlab(str_to_title(gsub("_"," ",i))) + theme(text = element_text(size = 8))
  )
}

ggarrange(
  plot_days_to_hurts_game,
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

### Only including the variables we want to consider for prediction.
pred_vars <- c("fumbles_per_attempt", "interceptions_per_attempt", "passing_yards_per_attempt", "passing_completion_percentage", "mean_cpoe", "rushing_yards_per_attempt", "primary_passing_tds_per_game", "primary_rushing_tds_per_game", "sacks_per_play", "interceptions_per_attempt", "fumbles_per_attempt", "turnovers_per_attempt", "epa_per_play", "net_win_percentage_change", "net_point_differential_change", "days_to_hurts_game")
response_vars <- c("years", "inflated_value", "inflated_apy", "inflated_guaranteed")
all_vars <- c(pred_vars, response_vars)

set.seed(581)
spec = c(train = .8, test = .2)

g = sample(cut(
  seq(nrow(df_qb_contracts_nonvets)), 
  nrow(df_qb_contracts_nonvets)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(df_qb_contracts_nonvets, g)

df_qb_contracts_train <- res$train[ , all_vars]
df_qb_contracts_test <- res$test[ , all_vars]



# Training and Evaluating Predictive Models


## Best Subsets (example here: https://bookdown.org/tpinto_home/Regularisation/best-subset-selection.html)

### Remove NAs
df_qb_contracts_train_na_rm <- drop_na(df_qb_contracts_train)

df_qbs_contracts_train.bglm <- df_qb_contracts_train_na_rm[ , names(df_qb_contracts_train_na_rm) %in% c(pred_vars, "inflated_apy")]

regfit.full <- regsubsets(inflated_apy ~ ., data = df_qbs_contracts_train.bglm, method = "exhaustive", nvmax = length(pred_vars))

reg.summary <- summary(regfit.full)

par(mfrow = c(2 , 2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")

plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
max.adjr2 = which.max(reg.summary$adjr2)
points(max.adjr2, reg.summary$adjr2[max.adjr2], col = "red", cex = 2, pch = 20)

plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
min.cp = which.min(reg.summary$cp)
points(min.cp, reg.summary$cp[min.cp], col = "red", cex = 2, pch = 20)

plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
min.bic = which.min(reg.summary$bic)
points(min.bic, reg.summary$bic[min.bic], col = "red", cex = 2, pch = 20)

xvars <- dimnames(reg.summary$which)[[2]][-1]
responsevar <- "inflated_apy"

models_to_eval <- sort(unique(c(max.adjr2, min.cp, min.bic)))

lst.apy <- vector("list", dim(reg.summary$which)[1])

for (i in models_to_eval){
  print(i)
  id <- reg.summary$which[i, ]
  form <- reformulate(xvars[which(id[-1])], responsevar, id[1])
  lst.apy[[i]] <- lm(form, df_qbs_contracts_train.bglm)
}

### Predict 
apy.adjr2.preds <- predict(lst.apy[[max.adjr2]], df_qb_contracts_test)
apy.cp.preds <- predict(lst.apy[[min.cp]], df_qb_contracts_test)
apy.bic.preds <- predict(lst.apy[[min.bic]], df_qb_contracts_test)

apy.adjr2.mse <- mean((apy.adjr2.preds - df_qb_contracts_test$inflated_apy)^2)
apy.cp.mse <- mean((apy.cp.preds - df_qb_contracts_test$inflated_apy)^2)
apy.bic.mse <- mean((apy.bic.preds - df_qb_contracts_test$inflated_apy)^2)

c(apy.adjr2.mse, apy.cp.mse, apy.bic.mse)

cbind(apy.adjr2.preds, apy.cp.preds, apy.bic.preds, df_qb_contracts_test$inflated_apy)


best.apy.model <- best.apy$BestModel

best.preds.apy <- as.numeric(predict(best.apy.model, df_qb_contracts_test))

mean((best.preds.apy - df_qb_contracts_test$inflated_apy)^2)


df_qbs_contracts_train.bglm <- rename(df_qb_contracts_train_na_rm[ , names(df_qb_contracts_train_na_rm) %in% c(pred_vars, "years")], y=years)
best.years <- bestglm(df_qbs_contracts_train.bglm, IC = "BIC", family=gaussian, method="exhaustive")

summary(best.years$BestModel)

df_qb_contracts_test_na_rm <- drop_na(df_qb_contracts_test)

best.preds.apy <- predict(best.apy$BestModel, newx = df_qb_contracts_test_na_rm)

mean((best.preds.apy - df_qb_contracts_test_na_rm$inflated_apy)^2)

predict(best.years$BestModel, df_qb_contracts_hurts)

## Ridge Regression/Lasso

### Scaled versions of training and test data.
normParam <- preProcess(df_qb_contracts_train[ , names(df_qb_contracts_train) %in% pred_vars])
df_qbs_contracts_train_scaled <- predict(normParam, df_qb_contracts_train)
df_qbs_contracts_test_scaled <- predict(normParam, df_qb_contracts_test)

x.train <- data.matrix(subset(df_qbs_contracts_train_scaled))
y.train <- data.matrix(df_qb_contracts_train[, c('inflated_apy')])

### Storing the number of rows into a variable
x.train.rows <- dim(x.train)[1]

### Fit with a LOOCV by defining the folds as the number of rows in the training data.
grid <- 10^seq (10, -2, length = 100)

lasso.fit <- cv.glmnet(x.train, y.train, family = "gaussian", alpha = 1, lambda = grid, nfolds = x.train.rows)

plot(lasso.fit, xvar="lambda", label=TRUE)

bestlam <- lasso.fit$lambda.min

x.test <- data.matrix(subset(df_qbs_contracts_test_scaled, select = -c(inflated_apy)))
y.test <- data.matrix(df_qbs_contracts_test_scaled[, c('inflated_apy')])

lasso.preds <- predict(lasso.fit, s = bestlam, newx = x.test)

mean((lasso.preds - df_qb_contracts_test$inflated_apy)^2)




## XGBoost
x.train <- data.matrix(subset(df_qbs_contracts_train, select = -c(apy)))
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