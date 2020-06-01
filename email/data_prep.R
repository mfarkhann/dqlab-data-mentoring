suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})

options(scipen = 99)
Sys.setlocale(category = "LC_ALL", locale = "id_ID.UTF-8")
options(lubridate.week.start = 1)

#Variabel Tanggal
hari_ini <- today(tzone = 'Asia/Jakarta')
awal_minggu_ini <- floor_date(hari_ini, unit = "week")
awal_minggu_lalu <- awal_minggu_ini -  weeks(1)
akhir_minggu_lalu <- awal_minggu_ini -  days(1)
awal_dua_minggu_lalu <- awal_minggu_ini -  weeks(2)

tanggal_awal_minggu_lalu <- strftime(awal_minggu_lalu, format = '%d %B %Y')
tanggal_akhir_minggu_lalu <- strftime(akhir_minggu_lalu, format = '%d %B %Y')
tanggal_awal_dua_minggu_lalu <- strftime(awal_dua_minggu_lalu, format = '%d %B %Y')


df_transaksi <- readRDS(here::here('email','data','transaksi.rds'))

df_transaksi_2_minggu <- df_transaksi %>% 
  filter(tanggal>= awal_dua_minggu_lalu,
         tanggal < awal_minggu_ini)

df_summary <- df_transaksi_2_minggu %>% 
  group_by(tipe) %>% 
  summarise(jumlah_last_week = sum(ifelse(tanggal>=awal_minggu_lalu, 0, amount)),
            jumlah_last_2_weeks = sum(ifelse(tanggal>=awal_minggu_lalu, amount, 0))) %>% 
  mutate(growth = round((jumlah_last_week/jumlah_last_2_weeks-1),4)) %>% 
  ungroup()


df_summary_kategori <- df_transaksi_2_minggu %>% 
  group_by(tipe, kategori_investor) %>% 
  summarise(jumlah_last_week = sum(ifelse(tanggal>=awal_minggu_lalu, 0, amount)),
            jumlah_last_2_weeks = sum(ifelse(tanggal>=awal_minggu_lalu, amount, 0))) %>% 
  mutate(growth = round((jumlah_last_week/jumlah_last_2_weeks-1),4)) %>% 
  ungroup()

