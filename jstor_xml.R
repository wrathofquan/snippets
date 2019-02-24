library(dplyr)
library(jstor)

files <- list.files(pattern = ".xml")

for (file in files){
temp <- jst_get_article(file) 
jstor_df <- rbind(jstor_df, temp)
}
