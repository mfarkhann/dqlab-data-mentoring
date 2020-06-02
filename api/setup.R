library(dplyr)

irisData <- iris %>% 
  mutate(id = rownames(iris)) %>% 
  select(id, everything())
irisData$Species = NULL

saveRDS(irisData, here::here('api','data','iris_data.rds'))
