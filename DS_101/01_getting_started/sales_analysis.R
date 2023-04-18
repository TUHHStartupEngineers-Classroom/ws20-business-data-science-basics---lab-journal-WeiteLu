# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----

library(tidyverse)
#  library(tibble)    --> is a modern re-imagining of the data frame
#  library(readr)     --> provides a fast and friendly way to read rectangular data like csv
#  library(dplyr)     --> provides a grammar of data manipulation
#  library(magrittr)  --> offers a set of operators which make your code more readable (pipe operator)
#  library(tidyr)     --> provides a set of functions that help you get to tidy data
#  library(stringr)   --> provides a cohesive set of functions designed to make working with strings as easy as possible
#  library(ggplot2)   --> graphics

## ── Attaching packages ──────────────────────────────── tidyverse 1.3.0 ──
## ✓ ggplot2 3.3.0     ✓ purrr   0.3.4
## ✓ tibble  3.0.1     ✓ dplyr   0.8.5
## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
## ✓ readr   1.3.1     ✓ forcats 0.5.0
## ── Conflicts ───────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()

# Excel Files
library(readxl)


# 2.0 Importing Files ----

# A good convention is to use the file name and suffix it with tbl for the data structure tibble
bikes_tbl      <- read_excel(path = "DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("DS_101/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")

# Not necessary for this analysis, but for the sake of completeness
bikeshops_tbl  <- read_excel("DS_101/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

# 3.0 Examining Data ----

# Method 1: Print it to the console
####orderlines_tbl
# Method 2: Clicking on the file in the environment tab (or run View(orderlines_tbl)) There you can play around with the filter.
# Method 3: glimpse() function. Especially helpful for wide data (data with many columns)
####glimpse(orderlines_tbl)

# 4.0 Joining Data ----

# by automatically detecting a common column, if any ...
left_join(orderlines_tbl, bikes_tbl)
## Error: `by` must be supplied when `x` and `y` have no common variables.

# If the data has no common column name, you can provide each column name in the "by" argument. For example, by = c("a" = "b") will match x.a to y.b. The order of the columns has to match the order of the tibbles).
left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))

# Chaining commands with the pipe and assigning it to order_items_joined_tbl
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

# Examine the results with glimpse()
bike_orderlines_joined_tbl %>% glimpse()


# 5.0 Wrangling Data ----

# All actions are chained with the pipe already. You can perform each step separately and use glimpse() or View() to validate your code. Store the result in a variable at the end of the steps.
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  # 5.1 Separate category name
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>%
  
  # 5.2 Add the total price (price * quantity) 
  # Add a column to a tibble that uses a formula-style calculation of other columns
  mutate(total.price = price * quantity) %>%
  
  # 5.3 Optional: Reorganize. Using select to grab or remove unnecessary columns
  # 5.3.1 by exact column name
  select(-...1, -gender) %>%
  
  # 5.3.2 by a pattern
  # You can use the select_helpers to define patterns. 
  # Type ?ends_with and click on Select helpers in the documentation
  select(-ends_with(".id")) %>%
  
  # 5.3.3 Actually we need the column "order.id". Let's bind it back to the data
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  
  # 5.3.4 You can reorder the data by selecting the columns in your desired order.
  # You can use select_helpers like contains() or everything()
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  
  # 5.4 Rename columns because we actually wanted underscores instead of the dots
  # (one at the time vs. multiple at once)
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# 6.0 Business Insights ----
# 6.1 Sales by Year ----

# Step 1 - Manipulate

library(lubridate)
# Step 1 - Manipulate
sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>%
  
  # Select columns
  select(order_date, total_price) %>%
  
  # Add year column
  mutate(year = year(order_date)) %>%
  
  # Grouping by year and summarizing sales
  group_by(year) %>% 
  summarize(sales = sum(total_price)) %>%
  
  # Optional: Add a column that turns the numbers into a currency format 
  # (makes it in the plot optically more appealing)
  # mutate(sales_text = scales::dollar(sales)) <- Works for dollar values
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_year_tbl

# Step 2 - Visualize

sales_by_year_tbl %>%
  
  # Setup canvas with the columns year (x-axis) and sales (y-axis)
  ggplot(aes(x = year, y = sales)) +
  
  # Geometries
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_label(aes(label = sales_text)) + # Adding labels to the bars
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  
  # Formatting
  # scale_y_continuous(labels = scales::dollar) + # Change the y-axis. 
  # Again, we have to adjust it for euro values
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by year",
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )

# 6.2 Sales by Year and Category 2 ----

# Step 1 - Manipulate

sales_by_year_cat_1_tbl <- bike_orderlines_wrangled_tbl %>%
  
  # Select columns and add a year
  select(order_date, total_price, category_1) %>%
  mutate(year = year(order_date)) %>%
  
  # Group by and summarize year and main catgegory
  group_by(year, category_1) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  # Format $ Text
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_year_cat_1_tbl 

# Step 2 - Visualize

sales_by_year_cat_1_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = category_1)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~ category_1) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" # Changes the legend name
  )



# 7.0 Writing Files ----

# 7.1 Excel ----
install.packages("writexl")
library("writexl")
bike_orderlines_wrangled_tbl %>%
  write_xlsx("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")

# 7.2 CSV ----
bike_orderlines_wrangled_tbl %>% 
  write_csv("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")

# 7.3 RDS ----
bike_orderlines_wrangled_tbl %>% 
  write_rds("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")


# 8 Challenge ----

bike_orderlines_wrangled_state_tbl <- bike_orderlines_wrangled_tbl %>%
  # Separate location 
  separate(col    = location,
           into   = c("state", "city"),
           sep    = ", ")

sales_by_year_state_tbl <- bike_orderlines_wrangled_state_tbl %>%
  
  # Select columns and add a year
  select(order_date, total_price, state) %>%
  mutate(year = year(order_date)) %>%
  
  # Group by and summarize year and main catgegory
  group_by(year, state) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  # Format $ Text
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_year_state_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = state)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~ state) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and state",
    fill = "State" # Changes the legend name
  )+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Sales by bikeshop
sales_by_year_bikeshop_tbl <- bike_orderlines_wrangled_state_tbl %>%
  
  # Select columns and add a year
  select(order_date, total_price, bikeshop) %>%
  mutate(year = year(order_date)) %>%
  
  # Group by and summarize year and main catgegory
  group_by(year, bikeshop) %>%
  summarise(sales = sum(total_price)) %>%
  #ungroup() %>%
  
  # Format $ Text
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_year_bikeshop_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = bikeshop)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~ bikeshop) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and bikeshop",
    fill = "bikeshop" # Changes the legend name
  )+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


  
  