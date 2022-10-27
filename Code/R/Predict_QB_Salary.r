library(bestglm)
library(caret)
library(dplyr)
library(leaps)
library(glmnet)
library(ggpubr)
library(psych) # For EDA
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

# EDA

### Only including the variables we want to consider for prediction.
pred_vars <- c("fumbles_per_attempt", "interceptions_per_attempt", "passing_yards_per_attempt", "passing_completion_percentage", "mean_cpoe", "rushing_yards_per_attempt", "primary_passing_tds_per_game", "primary_rushing_tds_per_game", "sacks_per_play", "interceptions_per_attempt", "fumbles_per_attempt", "turnovers_per_attempt", "epa_per_play", "net_win_percentage_change", "net_point_differential_change", "days_to_hurts_game")
response_vars <- c("years", "inflated_value", "inflated_apy", "inflated_guaranteed")
all_vars <- c(pred_vars, response_vars)

## Pulling summary statistics and writing to a CSV file to combine with the Word report.
eda_df_qb_contracts <- describe(df_qb_contracts[, names(df_qb_contracts) %in% pred_vars], fast=TRUE)
write.csv(eda_df_qb_contracts[order(row.names(eda_df_qbs)), ], "..\\..\\Report\\qb_contracts_eda.csv")

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

### Remove NAs
df_qb_contracts_train_na_rm <- drop_na(df_qb_contracts_train)

df_qbs_contracts_train.bglm <- df_qb_contracts_train_na_rm[ , names(df_qb_contracts_train_na_rm) %in% c(pred_vars, "inflated_apy")]

num_vars <- length(pred_vars)


## Fit Best Subsets
regfit.full <- regsubsets(inflated_apy ~ ., data = df_qbs_contracts_train.bglm, method = "exhaustive", nvmax = num_vars)

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

### Predict Best Subsets
apy.bs.adjr2.preds <- predict(lst.apy[[max.adjr2]], df_qb_contracts_test)
apy.bs.cp.preds <- predict(lst.apy[[min.cp]], df_qb_contracts_test)
apy.bs.bic.preds <- predict(lst.apy[[min.bic]], df_qb_contracts_test)

mse.bs.adjr2 <- mean((df_qb_contracts_test$inflated_apy - apy.bs.adjr2.preds)^2)
mse.bs.cp <- mean((df_qb_contracts_test$inflated_apy - apy.bs.cp.preds)^2)
mse.bs.bic <- mean((df_qb_contracts_test$inflated_apy - apy.bs.bic.preds)^2)


## Fit Forward Stepwise
regfit.fwd <- regsubsets(inflated_apy ~ ., df_qbs_contracts_train.bglm, method = "forward", nvmax = num_vars)
reg.fwd.summary <- summary(regfit.fwd)

par(mfrow = c(2 , 2))
plot(reg.fwd.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")

plot(reg.fwd.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
max.adjr2 = which.max(reg.fwd.summary$adjr2)
points(max.adjr2, reg.fwd.summary$adjr2[max.adjr2], col = "red", cex = 2, pch = 20)

plot(reg.fwd.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
min.cp = which.min(reg.fwd.summary$cp)
points(min.cp, reg.fwd.summary$cp[min.cp], col = "red", cex = 2, pch = 20)

plot(reg.fwd.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
min.bic = which.min(reg.fwd.summary$bic)
points(min.bic, reg.fwd.summary$bic[min.bic], col = "red", cex = 2, pch = 20)

xvars <- dimnames(reg.fwd.summary$which)[[2]][-1]
responsevar <- "inflated_apy"

models_to_eval <- sort(unique(c(max.adjr2, min.cp, min.bic)))

lst.fwd.apy <- vector("list", dim(reg.fwd.summary$which)[1])

for (i in models_to_eval){
  print(i)
  id <- reg.fwd.summary$which[i, ]
  form <- reformulate(xvars[which(id[-1])], responsevar, id[1])
  lst.fwd.apy[[i]] <- lm(form, df_qbs_contracts_train.bglm)
}

### Predict Forward Stepwise
apy.fwd.adjr2.preds <- predict(lst.fwd.apy[[max.adjr2]], df_qb_contracts_test)
apy.fwd.cp.preds <- predict(lst.fwd.apy[[min.cp]], df_qb_contracts_test)
apy.fwd.bic.preds <- predict(lst.fwd.apy[[min.bic]], df_qb_contracts_test)

mse.fwd.adjr2 <- mean((df_qb_contracts_test$inflated_apy - apy.fwd.adjr2.preds)^2)
mse.fwd.cp <- mean((df_qb_contracts_test$inflated_apy - apy.fwd.cp.preds)^2)
mse.fwd.bic <- mean((df_qb_contracts_test$inflated_apy - apy.fwd.bic.preds)^2)


## Backward Stepwise
regfit.bwd <- regsubsets(inflated_apy ~ ., df_qbs_contracts_train.bglm, method = "backward", nvmax = num_vars)
reg.bwd.summary <- summary(regfit.bwd)

par(mfrow = c(2 , 2))
plot(reg.bwd.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")

plot(reg.bwd.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
max.adjr2 = which.max(reg.bwd.summary$adjr2)
points(max.adjr2, reg.bwd.summary$adjr2[max.adjr2], col = "red", cex = 2, pch = 20)

plot(reg.bwd.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
min.cp = which.min(reg.bwd.summary$cp)
points(min.cp, reg.bwd.summary$cp[min.cp], col = "red", cex = 2, pch = 20)

plot(reg.bwd.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
min.bic = which.min(reg.bwd.summary$bic)
points(min.bic, reg.bwd.summary$bic[min.bic], col = "red", cex = 2, pch = 20)

xvars <- dimnames(reg.bwd.summary$which)[[2]][-1]
responsevar <- "inflated_apy"

models_to_eval <- sort(unique(c(max.adjr2, min.cp, min.bic)))

lst.bwd.apy <- vector("list", dim(reg.bwd.summary$which)[1])

for (i in models_to_eval){
  print(i)
  id <- reg.bwd.summary$which[i, ]
  form <- reformulate(xvars[which(id[-1])], responsevar, id[1])
  lst.bwd.apy[[i]] <- lm(form, df_qbs_contracts_train.bglm)
}

### Predict Backward Stepwise
apy.bwd.adjr2.preds <- predict(lst.bwd.apy[[max.adjr2]], df_qb_contracts_test)
apy.bwd.cp.preds <- predict(lst.bwd.apy[[min.cp]], df_qb_contracts_test)
apy.bwd.bic.preds <- predict(lst.bwd.apy[[min.bic]], df_qb_contracts_test)

mse.bwd.adjr2 <- mean((df_qb_contracts_test$inflated_apy - apy.bwd.adjr2.preds)^2)
mse.bwd.cp <- mean((df_qb_contracts_test$inflated_apy - apy.bwd.cp.preds)^2)
mse.bwd.bic <- mean((df_qb_contracts_test$inflated_apy - apy.bwd.bic.preds)^2)


## Forward Stepwise Fitting Years

df_qbs_contracts_train.bglm <- df_qb_contracts_train_na_rm[ , names(df_qb_contracts_train_na_rm) %in% c(pred_vars, "years")]

regfit.fwd.years <- regsubsets(years ~ ., df_qbs_contracts_train.bglm, method = "forward", nvmax = num_vars)
reg.fwd.years.summary <- summary(regfit.fwd.years)

par(mfrow = c(2 , 2))
plot(reg.fwd.years.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")

plot(reg.fwd.years.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
max.adjr2 = which.max(reg.fwd.years.summary$adjr2)
points(max.adjr2, reg.fwd.years.summary$adjr2[max.adjr2], col = "red", cex = 2, pch = 20)

plot(reg.fwd.years.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
min.cp = which.min(reg.fwd.years.summary$cp)
points(min.cp, reg.fwd.years.summary$cp[min.cp], col = "red", cex = 2, pch = 20)

plot(reg.fwd.years.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
min.bic = which.min(reg.fwd.years.summary$bic)
points(min.bic, reg.fwd.years.summary$bic[min.bic], col = "red", cex = 2, pch = 20)

xvars <- dimnames(reg.fwd.years.summary$which)[[2]][-1]
responsevar <- "years"

models_to_eval <- sort(unique(c(max.adjr2, min.cp, min.bic)))

lst.fwd.years <- vector("list", dim(reg.fwd.years.summary$which)[1])

for (i in models_to_eval){
  print(i)
  id <- reg.fwd.years.summary$which[i, ]
  form <- reformulate(xvars[which(id[-1])], responsevar, id[1])
  lst.fwd.years[[i]] <- lm(form, df_qbs_contracts_train.bglm)
}

### Predict Forward Stepwise Years

years.fwd.adjr2.preds <- predict(lst.fwd.years[[max.adjr2]], df_qb_contracts_test)
years.fwd.cp.preds <- predict(lst.fwd.years[[min.cp]], df_qb_contracts_test)
years.fwd.bic.preds <- predict(lst.fwd.years[[min.bic]], df_qb_contracts_test)

mse.years.fwd.adjr2 <- mean((df_qb_contracts_test$years - years.fwd.adjr2.preds)^2)
mse.years.fwd.cp <- mean((df_qb_contracts_test$years - years.fwd.cp.preds)^2)
mse.years.fwd.bic <- mean((df_qb_contracts_test$years - years.fwd.bic.preds)^2)


## Lasso

### Log transforming data because of skewness/curvature.
df_qb_contracts_train_log <- cbind(as.data.frame(lapply(df_qb_contracts_train[, names(df_qb_contracts_train) %in% pred_vars], function(x) log(x - min(x, na.rm = TRUE) + 1))), df_qb_contracts_train$inflated_apy) %>% rename("inflated_apy" = "df_qb_contracts_train$inflated_apy")
rownames(df_qb_contracts_train_log) <- rownames(df_qb_contracts_train)

df_qb_contracts_test_log <- cbind(as.data.frame(lapply(df_qb_contracts_test[, names(df_qb_contracts_test) %in% pred_vars], function(x) log(x - min(x, na.rm = TRUE) + 1))), df_qb_contracts_test$inflated_apy) %>% rename("inflated_apy" = "df_qb_contracts_test$inflated_apy")
rownames(df_qb_contracts_test_log) <- rownames(df_qb_contracts_test)

par(mfrow = c(2, 2))
plot(df_qb_contracts_train$primary_passing_tds_per_game, df_qb_contracts_train$inflated_apy)
plot(df_qb_contracts_train$primary_rushing_tds_per_game, df_qb_contracts_train$inflated_apy)
plot(df_qb_contracts_train$turnovers_per_attempt, df_qb_contracts_train$inflated_apy)
plot(df_qb_contracts_train$days_to_hurts_game, df_qb_contracts_train$inflated_apy)

plot(df_qb_contracts_train_log$primary_passing_tds_per_game, df_qb_contracts_train$inflated_apy)
plot(df_qb_contracts_train_log$primary_rushing_tds_per_game, df_qb_contracts_train$inflated_apy)
plot(df_qb_contracts_train_log$turnovers_per_attempt, df_qb_contracts_train$inflated_apy)
plot(df_qb_contracts_train_log$days_to_hurts_game, df_qb_contracts_train$inflated_apy)

### Scaled versions of training and test data.

normParam <- preProcess(df_qb_contracts_train[ , names(df_qb_contracts_train) %in% pred_vars])
df_qbs_contracts_train_scaled <- predict(normParam, df_qb_contracts_train)
df_qbs_contracts_test_scaled <- predict(normParam, df_qb_contracts_test)

x.train <- data.matrix(df_qbs_contracts_train_scaled[, names(df_qbs_contracts_train_scaled) %in% pred_vars])
y.train <- data.matrix(df_qbs_contracts_train_scaled[, c('inflated_apy')])

### Storing the number of rows into a variable
x.train.rows <- dim(x.train)[1]

### Fit with a LOOCV by defining the folds as the number of rows in the training data.
grid <- 10^seq (10, -2, length = 1000)

lasso.fit <- cv.glmnet(x.train, y.train, family = "gaussian", alpha = 1, lambda = grid, nfolds = x.train.rows)

par(mfrow = c(1 , 1))
plot(lasso.fit, xvar="lambda", label=TRUE)

plot(lasso.fit$glmnet.fit, "norm", label=TRUE)
plot(lasso.fit$glmnet.fit, "lambda", label=TRUE)

bestlam.lasso.apy <- lasso.fit$lambda.min

x.test <- data.matrix(df_qbs_contracts_test_scaled[ , names(df_qbs_contracts_train_scaled) %in% pred_vars])
y.test <- data.matrix(df_qbs_contracts_test_scaled[, c('inflated_apy')])

lasso.preds <- predict(lasso.fit, s = bestlam.lasso.apy, newx = x.test)

mse.lasso.apy <- mean((lasso.preds - df_qb_contracts_test$inflated_apy)^2)

coef(lasso.fit, s = bestlam.lasso.apy)


#### Fitting and Predicting number of years on the contract (Lasso)
x.train.years <- data.matrix(df_qbs_contracts_train_scaled[, names(df_qbs_contracts_train_scaled) %in% pred_vars])
y.train.years <- data.matrix(df_qbs_contracts_train_scaled[, c('years')])

x.train.rows <- dim(x.train.years)[1]

grid <- 10^seq (10, -2, length = 1000)

lasso.fit.years <- cv.glmnet(x.train.years, y.train.years, family = "gaussian", alpha = 1, lambda = grid, nfolds = x.train.rows)

par(mfrow = c(1 , 1))
plot(lasso.fit.years, xvar="lambda", label=TRUE)

plot(lasso.fit.years$glmnet.fit, "norm", label=TRUE)
plot(lasso.fit.years$glmnet.fit, "lambda", label=TRUE)

bestlam <- lasso.fit.years$lambda.min

x.test.years <- data.matrix(df_qbs_contracts_test_scaled[, names(df_qbs_contracts_test_scaled) %in% pred_vars])
y.test.years <- data.matrix(df_qbs_contracts_test_scaled[, c('years')])

lasso.preds.years <- predict(lasso.fit.years, s = bestlam, newx = x.test.years)

mse.lasso.years <- mean((lasso.preds.years - df_qb_contracts_test$years)^2)

coef(lasso.fit.years, s = bestlam)


## Ridge Regression
grid <- 10^seq (10, -2, length = 1000)
rr.fit <- cv.glmnet(x.train, y.train, family = "gaussian", alpha = 0, lambda = grid, nfolds = x.train.rows)

par(mfrow = c(1 , 1))
plot(rr.fit, xvar="lambda", label=TRUE)

plot(rr.fit$glmnet.fit, "norm", label=TRUE)
plot(rr.fit$glmnet.fit, "lambda", label=TRUE)

bestlam <- rr.fit$lambda.min

x.test <- data.matrix(subset(df_qbs_contracts_test_scaled, select = -c(inflated_apy)))
y.test <- data.matrix(df_qbs_contracts_test_scaled[, c('inflated_apy')])

rr.preds <- predict(rr.fit, s = bestlam, newx = x.test)

mse.rr <- mean((rr.preds - df_qb_contracts_test$inflated_apy)^2)

coef(rr.fit, s = bestlam)


#### Fitting and Predicting Number of Years on Contract (Ridge Regression)
x.train.years <- data.matrix(df_qbs_contracts_train_scaled[, names(df_qbs_contracts_train_scaled) %in% pred_vars])
y.train.years <- data.matrix(df_qbs_contracts_train_scaled[, c('years')])

x.train.rows <- dim(x.train.years)[1]

grid <- 10^seq (10, -2, length = 1000)

rr.fit.years <- cv.glmnet(x.train.years, y.train.years, family = "gaussian", alpha = 0, lambda = grid, nfolds = x.train.rows)

par(mfrow = c(1 , 1))
plot(rr.fit.years, xvar="lambda", label=TRUE)

plot(rr.fit.years$glmnet.fit, "norm", label=TRUE)
plot(rr.fit.years$glmnet.fit, "lambda", label=TRUE)

bestlam <- rr.fit.years$lambda.min

x.test.years <- data.matrix(df_qbs_contracts_test_scaled[, names(df_qbs_contracts_test_scaled) %in% pred_vars])
y.test.years <- data.matrix(df_qbs_contracts_test_scaled[, c('years')])

rr.preds.years <- predict(rr.fit.years, s = bestlam, newx = x.test.years)

mse.rr.years <- mean((rr.preds.years - df_qb_contracts_test$years)^2)

coef(lasso.fit.years, s = bestlam)



# Check Winning Model's Assumptions
lasso.fit$lambda[bestlam]

cbind(apy.fwd.bic.preds,rr.preds, lasso.preds, df_qb_contracts_test$inflated_apy)

df_qbs_contracts_hurts_scaled <- predict(normParam, df_qb_contracts_hurts)

x.test.hurts <- data.matrix(df_qbs_contracts_hurts_scaled[, names(df_qbs_contracts_hurts_scaled) %in% pred_vars])

predict(lasso.fit, s = bestlam.lasso.apy, newx = x.test.hurts)
