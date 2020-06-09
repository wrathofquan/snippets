library(tidyverse)
library(readxl)
library(httr)
library(rvest)

data <- read_excel("MAKdata.xlsx")

data$`book subtitle`[is.na(data$`book subtitle`)] <- " "

data <- data %>% unite("author_title", Author:`book subtitle`, remove = TRUE, sep = "+") 

search_string <- gsub(" ", "+", data$author_title)

base_url <- "https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q="

my_proxy <- use_proxy(port, auth = "basic")


results <- map_dfr(search_string, function(i){
  search_url <- paste0(base_url,i)
  content <- read_html(GET(search_url,my_proxy))
  citation_node <- content %>% html_nodes("#gs_res_ccl_mid :nth-child(1) .gs_ri a:nth-child(3)") 
  citation_count <- html_text(citation_node) %>% gsub("Cited by ","", .)
  print(citation_count)
  tibble(citation_count)
  Sys.sleep(sample(20, 1)) ##tweak this or change proxies if getting 429 errors
})