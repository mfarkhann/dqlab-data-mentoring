suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})


set.seed(26)
options(lubridate.week.start = 1)

n_trx <- 200
base_date <- floor_date(today('Asia/Jakarta'), 'week') - weeks(2)

investor_id <- round(runif(n_trx, 1,10))
tanggal <- base_date + days(round(runif(n_trx, 1,14)) )
tipe <- c('TOPUP','CASHOUT')[round(runif(n_trx, 1,2))]
amount <- 10^3* round(runif(n_trx, 1,1000))

df_transaksi <- tibble(tanggal, investor_id, tipe, amount) %>% 
  mutate(kategori_investor = case_when(investor_id<5 ~ 'Kategori A',
                                       investor_id<8 ~ 'Kategori B',
                                       TRUE ~ 'Kategori C'))


df_transaksi %>% 
  saveRDS(here::here('email','data','transaksi.rds'))
