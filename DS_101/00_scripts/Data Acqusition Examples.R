# get the URL for the wikipedia page with all S&P 500 symbols
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
# use that URL to scrape the S&P 500 table using rvest
library(rvest)
sp_500 <- url %>%
  # read the HTML from the webpage
  read_html() %>%
  # Get the nodes with the id
  html_nodes(css = "#constituents") %>%
  # html_nodes(xpath = "//*[@id='constituents']"") %>% 
  # Extract the table and turn the list into a tibble
  html_table() %>% 
  .[[1]] %>% 
  as_tibble()


# IMDB 250 movies
url  <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"
html <- url %>% 
  read_html()

rank <-  html %>% 
  html_nodes(css = ".titleColumn") %>% 
  html_text() %>% 
  # Extrag all digits between " " and ".\n" The "\" have to be escaped
  # You can use Look ahead "<=" and Look behind "?=" for this
  stringr::str_extract("(?<= )[0-9]*(?=\\.\\n)")%>% 
  # Make all values numeric
  as.numeric()

title <- html %>% 
  html_nodes(".titleColumn > a") %>% 
  html_text()


######
library(jsonlite)
bike_data_lst <- fromJSON("bike_data.json")
# Open the data by clicking on it in the environment or by running View()
View(bike_data_lst)