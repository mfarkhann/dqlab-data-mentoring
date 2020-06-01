suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(blastula)
  library(formattable)
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


#Formatter
customGreen = "#57c750"
customRed = "#ff7f7f"
positive_growth_formatter <- formatter("span", 
                                       style = x ~ style(font.weight = "bold", 
                                                         color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))), 
                                       x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"), scales::percent(x)))

negative_growth_formatter <- formatter("span", 
                                       style = x ~ style(font.weight = "bold", 
                                                         color = ifelse(x < 0, customGreen, ifelse(x > 0, customRed, "black"))), 
                                       x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"), scales::percent(x)))

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

summary_topup <- df_summary %>%
  filter(tipe == 'TOPUP') %>% 
  mutate(jumlah_last_week = scales::comma(jumlah_last_week),
         jumlah_last_2_weeks = scales::comma(jumlah_last_2_weeks)
  )

#Summary Cashout
summary_cashout <- df_summary %>%
  filter(tipe == 'CASHOUT')  %>%
  mutate(jumlah_last_week = scales::comma(jumlah_last_week),
         jumlah_last_2_weeks = scales::comma(jumlah_last_2_weeks)
  )


breakdown_topup <- df_summary_kategori %>%
  filter(tipe == 'TOPUP') %>% 
  select(-tipe) %>% 
  mutate(jumlah_last_week = scales::comma(jumlah_last_week), 
         jumlah_last_2_weeks = scales::comma(jumlah_last_2_weeks)) %>%
  rename("Source" = kategori_investor, !!tanggal_awal_minggu_lalu := jumlah_last_week, 
         !!tanggal_awal_dua_minggu_lalu := jumlah_last_2_weeks, "Growth" = growth) %>%
  formattable(align = c("l","c","c","c"),
              list(`Growth` = positive_growth_formatter))


breakdown_cashout <- df_summary_kategori %>%
  filter(tipe == 'CASHOUT') %>% 
  select(-tipe) %>% 
  mutate(jumlah_last_week = scales::comma(jumlah_last_week), 
         jumlah_last_2_weeks = scales::comma(jumlah_last_2_weeks)) %>%
  rename("Source" = kategori_investor, !!tanggal_awal_minggu_lalu := jumlah_last_week, 
         !!tanggal_awal_dua_minggu_lalu := jumlah_last_2_weeks, "Growth" = growth) %>%
  formattable(align = c("l","c","c","c"),
              list(`Growth` = positive_growth_formatter))

library(ggplot2)

plot <- df_transaksi_2_minggu %>% 
  group_by(tanggal, tipe) %>% 
  summarise_if(is.numeric, list(sum)) %>% 
  ggplot(aes(tanggal, amount, colour = tipe)) +
  geom_line() +
  scale_y_continuous(labels=scales::comma_format()) +
  scale_x_date(labels = scales::date_format("%d %b")) +
  labs(title = "Daily Topup and Cashout",
       subtitle = paste(tanggal_awal_dua_minggu_lalu, " - ", tanggal_akhir_minggu_lalu),
       colour = "",
       x = "",
       y = "") +
  theme_minimal()

plot_email <- add_ggplot(plot)

email <- compose_email(
  body = md(glue::glue("
     Dear Leaders,
     
     Berikut adalah Weekly Report untuk periode {tanggal_awal_minggu_lalu} - {tanggal_akhir_minggu_lalu}
     
  ### Last week topup
  **{summary_topup$jumlah_last_week}** \\
  ( {positive_growth_formatter(summary_topup$growth)} dari  {summary_topup$jumlah_last_2_weeks})
  
  {block_spacer()}
     

  ### Last week cashout
  **{summary_cashout$jumlah_last_week}** \\
  ( {negative_growth_formatter(summary_cashout$growth)}  dari  {summary_cashout$jumlah_last_2_weeks})  
  
  {block_spacer()}
  

  ####   Breakdown Topup
  {breakdown_topup}  
     
  ####   Breakdown Cashout
  {breakdown_cashout}

  {block_spacer()}

  {plot_email}

  ")),
  footer = "This is an automatically generated email. For any inquiry please email to data@gmail.com")

email

