library(tidyverse)
library(tidytext)
library(wordcloud)
library(mboxr)

setwd("~/Takeout/Mail")
data <- read_mbox(mbox = "All mail Including Spam and Trash.mbox", file = "output.rds")

cleaned_data <- data %>%
         filter(str_detect(content, "[A-Za-z]"))

parse_sent_message <- function(email){
  substr(
    gsub("-top:|-bottom:|break-word","",
    sub("Content-Type: application/pdf|Mime-Version: 1.0.*","",
    sub(".*charset ISO|charset  UTF-8|charset us-ascii","",
    sub(".*Content-Transfer-Encoding: 7bit", "", 
    sub("orwarded message.*", "", 
    gsub("=|\"", " ", 
    gsub("  ", " ", 
    gsub("= ", "", 
    sub(".*Content-Transfer-Encoding: quoted-printable", "", 
    sub(".*charset=UTF-8", "", 
    gsub("=E2=80=99|&#39;", "'", 
    gsub(">|<", "", 
    sub("On [A-Z][a-z]{2}.*", "",
    gsub("\n|\t|<div|</div>|<br>", " ", 
    email)))))))))))))), 1, 10000)
}

cleaned_data$content <- parse_sent_message(cleaned_data$content)


email_stop_words <- stop_words %>% 
  rbind(data_frame("word" = c(seq(0,9),"icagicagicagicagicagicagicagicagicagicagicagicagicagicagicagicagicagicagicag",
                           "c2","a0", "sans","color:rgb", "target", "3d", "8a", "mail.gmail.com", "wa", "aa", "content", "dir",
                          "ad", "af", "font", "type", "auto", "zz", "ae", "zx", "id", "ai", "helvetica", "berkeley.edu",
                          "style", "nbsp", "class", "span", "http", "text", "gmail.com", "\r", "18px", "space:pre",
                          "joshua.quan", "github", "quan", "librarian", "wrathofquan", "3ds0", "alt", "src", "1.38",
                          "17,85,204", "align:baseline", "gmail", "11pt", "data", "east", "width",
                          "table", "border", "100", "itemprop", "mso", "10px", "align", "amp", "13px", "family", "top",
                          "email", "center", "cellpadding", "222", "max", "zwnj", "tdtd", "schema.org", "1px","meta",
                          "bgcolor", "solid", "itemscope", "itemtype","list", "collapse", "body", "0pt", "html",
                          "20px", "tbody", "display", "null", "msonormal",
                          "plain", "0px", "size", "color", "quot", "8859", "href", "margin", "ltr", "asian:normal", "family:arial",
                          "margin0pt", "mailto:joshua.quan", "numeric:normal", "136,136,136", "family:open_sanssemibold", "berkeley", "library", "height",
                          "gmail_signature", "_blank", "variant", "12.668px", "94720", "0,0,0", "arial", "smartmail", "vertical", "background", "line", 
                          "ihe", "12px", "12", "4px", "family:helvetica", "img", "color:transparent", "white", "wrap", "joshua", "josh", "services",
                          "left", "github.com","memorial", "doe","disposition", "attachment", "padding", "rgba", "webkit", "https", "div", "br", "serif"),
               "lexicon" = "sent_email"))  



#
cleaned_data_token <- cleaned_data %>%
  unnest_tokens(word, content)


## Stop Words
cleaned_data_token <- cleaned_data_token %>% anti_join(email_stop_words)

##quick count of words


##

cleaned_data_token %>%
  filter(from == "Josh Quan <joshua.quan@berkeley.edu>") %>% 
  count(word, sort = TRUE) %>%
  top_n(n = 50, wt = n) %>% 
  filter(n > 50) %>%
  filter(nchar(word) >= 3) %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


#sentiment analysis
afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")



##sentiment analysis

email_sentiment <- 
  cleaned_data_token %>%
  inner_join(get_sentiments("bing")) %>%
  filter(date >  2018-07-01) %>% 
  count(date, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(email_sentiment, aes(date, sentiment)) + geom_point(aes(alpha = 0.3)) + geom_smooth() 

