library(caret) # Used for confusionMatrix
library(dplyr)
library(DBI)
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
df_prim <- dbGetQuery(con, "select * from public.primary_passer_stats_hurts_maximum")

dbDisconnect(con)

## Find the median games played for primary QBs with Jalen Hurts' number of games or greater.
median_games_played <- median(df_prim$primary_passing_games)

## We only want to consider QBs who first played in 2018 or sooner. 
## QBs who first played later would not have had the opportunity to play the median 64 games yet.
df_qbs <- df_prim %>% filter(player_first_season <= 2018)

rownames(df_qbs) <- df_qbs$passer_player_id

## Define a `franchise_qb` label if the QB played greater than or equal to the median. 
df_qbs <- df_qbs %>% mutate(franchise_qb = if_else(primary_passing_games >= median_games_played, 1, 0))
df_qbs <- df_qbs %>% mutate(across(franchise_qb, as.factor))


# EDA

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
  plot_rushing_yards_per_attempt,
  plot_primary_passing_tds_per_game,
  plot_primary_rushing_tds_per_game,
  plot_sacks_per_play,
  plot_interceptions_per_attempt,
  plot_fumbles_per_attempt,
  plot_turnovers_per_attempt
)


# Train/Test Split

### Only including the variables we want to consider for prediction.
pred_vars <- c("fumbles_per_attempt", "interceptions_per_attempt", "passing_yards_per_attempt", "passing_completion_percentage", "rushing_yards_per_attempt", "primary_passing_tds_per_game", "primary_rushing_tds_per_game", "sacks_per_play", "interceptions_per_attempt", "fumbles_per_attempt", "turnovers_per_attempt", "franchise_qb")

set.seed(581)
spec = c(train = .8, test = .2)

g = sample(cut(
  seq(nrow(df_qbs)), 
  nrow(df_qbs)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(df_qbs, g)

df_qbs_train <- res$train[ , pred_vars]
df_qbs_test <- res$test[ , pred_vars]


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
  plot_train_rushing_yards_per_attempt,
  plot_train_primary_passing_tds_per_game,
  plot_train_primary_rushing_tds_per_game,
  plot_train_sacks_per_play,
  plot_train_interceptions_per_attempt,
  plot_train_fumbles_per_attempt,
  plot_train_turnovers_per_attempt
)

# Training and Evaluating Predictive Models

## Logistic Regression

## Scaled versions of training and test data.
normParam <- preProcess(df_qbs_train)
df_qbs_train_scaled <- predict(normParam, df_qbs_train)
df_qbs_test_scaled <- predict(normParam, df_qbs_test)

x.train <- data.matrix(subset(df_qbs_train_scaled, select = -c(franchise_qb)))
y.train <- data.matrix(df_qbs_train_scaled[, c('franchise_qb')])

logistic.fit <- cv.glmnet(x.train, y.train, family = "binomial")

bestlam <- logistic.fit$lambda.min

x.test <- data.matrix(subset(df_qbs_test_scaled, select = -c(franchise_qb)))
y.test <- data.matrix(df_qbs_test_scaled[, c('franchise_qb')])

logistic.preds <- predict(logistic.fit, s = bestlam, newx = x.test, type = "class")
logistic.preds

logistic.confusion <- confusionMatrix(as.factor(logistic.preds), as.factor(y.test), mode="everything", positive = "1")
logistic.confusion


## XGBoost
