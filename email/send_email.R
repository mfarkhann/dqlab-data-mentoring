suppressPackageStartupMessages({
  library(gmailr)
})

gm_auth_configure(path = "client_id.json")
# gm_auth()

tanggal_judul <- strftime(awal_minggu_lalu, format = '%d %B %Y')
email_stage = 'LIVE'

email_to <- "kna.data.test@gmail.com"
judul <- paste("Weekly Report", tanggal_judul)

email_message <- gm_mime() %>% 
  gm_to(email_to) %>% 
  gm_subject(judul) %>% 
  gm_html_body(body_email$html_html) 


if(email_stage == 'LIVE') {
  gm_send_message(email_message)
} else {
  gm_create_draft(email_message)
}
