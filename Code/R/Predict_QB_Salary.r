library(dplyr)
library(glmnet)
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

