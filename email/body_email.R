suppressPackageStartupMessages({
  library(blastula)
  library(formattable)
  library(ggplot2)
})

#Formatter
customGreen = "#57c750"
customRed = "#ff7f7f"
positive_growth_formatter <- formatter("span", 
                                       style = x ~ style(font.weight = "bold", 
                                                         color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))), 
                                       x ~ scales::percent(x))

negative_growth_formatter <- formatter("span", 
                                       style = x ~ style(font.weight = "bold", 
                                                         color = ifelse(x < 0, customGreen, ifelse(x > 0, customRed, "black"))), 
                                       x ~ scales::percent(x))



summary_topup <- df_summary %>%
  filter(tipe == 'TOPUP') %>% 
  mutate(jumlah_last_week = scales::comma(jumlah_last_week),
         jumlah_last_2_weeks = scales::comma(jumlah_last_2_weeks)
  )

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

