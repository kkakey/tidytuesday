library(tidyverse)
library(wordcloud)
library(colorfindr)
library(rvest)
library(RSelenium)
library(randomcoloR)
set.seed(2024)
data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-10/parfumo_data_clean.csv')

# filter to only well-rated candles
data <- 
  data |> 
  filter(Rating_Count >= 100)

# top 100 rated candles
output <- 
  data |> 
  slice_max(n=100, order_by = Rating_Value)

# main scent counts of top rated candles
words <- 
  unlist(str_split(string = output$Main_Accords, pattern = ', ')) |>
  as_tibble() |> 
  count(value, sort=TRUE)


#################################
words$color <- NA
for (num in seq_along(words$value)) {
  
  print(words$value[num])
  base_page <-
    "https://www.google.com/search?" %>%
    paste0("q=", words$value[num], "%scent", 
           "image&tbm=isch&tbs=isz:lt,islt:0.5") %>%
    read_html() %>%
    html_nodes(xpath = "//td/a") %>% 
    html_attr("href") %>%
    `[`(str_detect(., "/url\\?")) %>%
    strsplit("=|\\&") %>%
    sapply(`[`, 2) %>% 
    .[!(grepl('etsy|instagram|flickr', .))] %>%
    .[1]
  
  if (num==1) {
    rD <- RSelenium::rsDriver(browser = "firefox", port = 4444L, verbose = T)
    remDr <- rD[["client"]]
  }
  
  Sys.sleep(3)
  remDr$navigate(base_page)
  Sys.sleep(10)
  html <- remDr$getPageSource()[[1]]
  
  img_links <-
    read_html(html) %>%
    html_nodes("img") %>%
    html_attr('src') %>%
    .[grepl('bmp$|jpg$|jpeg$|png$|tiff$|svg$', .) & grepl('^http', .)]
    
    
  if (length(img_links) > 1) {
    img_link <- img_links[2]
  } else {
    img_link <- img_links[1]
  }
  
  if (is.na(img_link) | nrow(get_colors(img_link))==0) {
    color <- randomColor(count = 1, hue = "purple")
    words[words$value == words$value[num],]$color <- color 
  } else {
    color <- 
      get_colors(img_link) %>%
      filter(!str_detect(col_hex, '^#0')) %>%
      slice(1)
    words[words$value == words$value[num],]$color <- color$col_hex 
  }
  
}

# close webpage
rD$server$stop()

###
wordcloud(words = words$value, freq = words$n, scale=c(2,.5), 
          min.freq = 1, random.order=FALSE, colors=brewer.pal(8, "Dark2"),
          family = "serif", font = 3)

