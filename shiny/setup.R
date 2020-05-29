library(dplyr)

# Data user yang digunakan untuk sampel
df_user <- tibble(
  name = c("BM Setosa", "BM Versicolor", "BM Virginica", "Supervisor BM"),
  email = c("setosa@gmail.com",'versicolor@gmail.com','virginica@gmail.com','supervisor@gmail.com'),
  species = c('setosa','versicolor','virginica','supervisor'))

df_user %>% 
  mutate(session = "") %>% 
  saveRDS(here::here('shiny','data','credentials.rds'))

shiny::runApp('shiny',display.mode = 'showcase')
