
library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing
library(httr)

# Challenge 1

tesla_forecast <- GET("https://money.cnn.com/quote/forecast/forecast.html?symb=tsla")

stock_forecast <- function(stock_code) {
  url <- modify_url(url = "https://money.cnn.com/quote/forecast/forecast.html", path = glue("?symb={stock_code}"))
  resp <- GET(url)
  stop_for_status(resp) # automatically throws an error if a request did not succeed
}

tesla_forecast <- stock_forecast("tsla")
tesla_forecast

apple_forecast <- stock_forecast("AAPL")
apple_forecast

amazon_forecast <- stock_forecast("AMZN")
amazon_forecast


# Challenge 2

# 1.1 COLLECT PRODUCT FAMILIES ----

rose_url_home          <- "https://www.rosebikes.com"

# Read in the HTML for the entire webpage
rose_html_home         <- read_html(rose_url_home)

# Web scrape the ids for the families
rose_model_tbl <- rose_html_home %>%
  
  # Get the nodes for the model name ...
  html_nodes(css = ".main-navigation-category-with-tiles__link ") %>%
  # ...and extract the information of the href attribute
  html_attr('href') %>%
  
  discard(.p = ~stringr::str_detect(.x,"sale")) %>%
  str_remove(pattern = "/bikes/") %>%
  # Convert vector to tibble
  enframe(name = "position", value = "model") 



# 1.2 COLLECT PRODUCT CATEGORIES ----

# Extract the urls from the href attribute
rose_category_tbl <- rose_model_tbl %>%
  transmute(
    url = glue("https://www.rosebikes.com{model}")
  ) %>%
  distinct(url)

# 2.1 Get URL for each bike of the Product model

# select first bike model url
rose_category_url <- rose_category_tbl$url[1]
#xopen(rose_category_url)

# Get the URLs for the bikes of the first model
html_rose_category  <- read_html(rose_category_url)

rose_url_tbl  <- html_rose_category %>%
  html_nodes(css = ".catalog-category-bikes__button") %>%
  html_attr("href") %>%
  # Convert vector to tibble
  enframe(name = "position", value = "url") %>%
  mutate(
    url = glue("https://www.rosebikes.com{url}")
  )

rose_bike_type_tbl <- html_rose_category %>%
  html_nodes(css = ".catalog-category-bikes__title-text") %>%
  html_text() %>%
  # Convert vector to tibble
  enframe(name = "position", value = "type")

rose_bike_price_tbl <- html_rose_category %>%
  html_nodes(css = ".catalog-category-bikes__price-title") %>%
  html_text() %>%
  enframe(name = "position", value = "price") %>%
  na_if("")


rose_mtb_tbl <- rose_bike_type_tbl %>%
  left_join(rose_bike_price_tbl) %>%
  left_join(rose_url_tbl)


