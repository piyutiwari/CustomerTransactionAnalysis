library(tidyverse)
library(readxl)
library(varhandle)
library(lubridate)
library(reshape2)
library (rfm)
library(devtools)
library(caret)
library(dplyr)

ol_sheet1 <-
  read_excel("data/input/online_retail_II.xlsx", sheet = "Year 2009-2010") %>%
  mutate(data_year = "Year 2009-2010") %>%
  rename(id = `Customer ID`)

ol_sheet2 <-
  read_excel("data/input/online_retail_II.xlsx", sheet = "Year 2010-2011") %>%
  mutate(data_year = "Year 2010-2011") %>%
  rename(id = `Customer ID`)

combined_df <- bind_rows(ol_sheet1, ol_sheet2)

combined_df <- combined_df %>%
  filter((is.na(id) == FALSE) &  # Keep not null id
           (Quantity > 0) &  # Keep Valid Quantity
           (Price > 0) & # Keep valid price
           (substr(
             toupper(Invoice), nchar(Invoice), nchar(Invoice)
           ) != 'C') & # Keep invoice without C as last character
           (check.numeric(StockCode) == TRUE))
## 713,989 rows

#### Average quantity per invoice
combined_df <- combined_df %>%
  mutate(
    clean_date = date(InvoiceDate),
    purchase_year = year(InvoiceDate),
    purchase_month = month(InvoiceDate),
    purchase_wday = wday(InvoiceDate),
    purchase_year_month = paste(purchase_year , purchase_month, sep = '-'),
    total_cost = Price * Quantity                     # for a particular stock code, and an invoice
  )


### are there any interesting association rules
library("tidyverse")
library("arules")


### Step 1 - Convert the file into transactions
### Currently a row in data frame represents one product per invoice.
### Invoice is a transaction - Converting to transactions mean that one row will have all
### the products in the invoice.

### Writing dataframe to temp file. This method is a faster way to convert df to transactions.
write.table(combined_df, file = tmp <-
              file(), row.names = FALSE)

### Reading file as transactions.
### Cols - cols = c("Invoice", "Description") -define transaction id (i.e., Incoive in our case) and Products (i.e., Description)
### Examples
### Dataframe will look like this
# head(combined_df[c("Invoice", "Description")])
#
# # A tibble: 6 x 2
#   Invoice Description
#   <chr>   <chr>
# 1 489434  "15CM CHRISTMAS GLASS BALL 20 LIGHTS"
# 2 489434  "RECORD FRAME 7\" SINGLE SIZE"
# 3 489434  "STRAWBERRY CERAMIC TRINKET BOX"
# 4 489434  "PINK DOUGHNUT TRINKET POT"
# 5 489434  "SAVE THE PLANET MUG"
# 6 489434  "FANCY FONT HOME SWEET HOME DOORMAT"

### Transaction will be like
# 489434 {"15CM CHRISTMAS GLASS BALL 20 LIGHTS", "RECORD FRAME 7\" SINGLE SIZE" ...}

trans4 <- read.transactions(
  tmp,
  format = "single",
  header = TRUE,
  cols = c("Invoice", "Description")
) # create transaction
close(tmp) # close temp file

inspect(head(trans4, 4))


# Generating Rule

# Support is an indication of how frequently the itemset appears in the dataset.
# Consider only the two transactions from the above output. The support of the item citrus
# fruit is 1/2 as it appears in only 1 out of the two transactions.
#
# Confidence is an indication of how often the rule has been found to be true.
# We will discuss more about confidence after generating the rules.

grocery_rules <-
  apriori(trans4, parameter = list(
    support = 0.01,
    confidence = 0.1,
    minlen = 3
  ))

inspect(head(sort(grocery_rules, by = "lift"), 90))

## we could see that people are buying in bulk, and this data is more of bulk shopping. for example, people who are re-salers.
## from the rules, we can infer that similar type of items are bought together.
## there isn't much variety in the cart.


### Cohort Analysis

min(combined_df$InvoiceDate)

max(combined_df$InvoiceDate)

### We are trying to get an idea about churn or retention.
### Assumptions
### 1. First shopping month is considered the start of life of customer. Called Cohort Month. All the
### Customesrs with first shopping in same month belong to same cohort.


### Finding Cohort month

### Floor date function from lubridate returns the 1 day of the month.
cohort_df <- combined_df %>%
  select(c(id, InvoiceDate, Invoice)) %>%
  mutate(first_day_of_invoice_month = floor_date(date(InvoiceDate), 'month'))


### Get the first invoice month of any consumer (i.e., cohort month)
## We are going to assume that this the first month he joined/started shopping with us
cohort_grp <- cohort_df %>%
  group_by(id) %>%
  summarise(cohort_month = min(first_day_of_invoice_month))

## This finds months between invoice date and cohort_month/join month
cohort_df <- cohort_df %>%
  inner_join(cohort_grp, by = 'id') %>%
  mutate(months_between = interval(cohort_month, first_day_of_invoice_month) %/% months(1))


### calculate unique custoemrs for each group of cohort_month and months_between
cohort_analysis_df <- cohort_df %>%
  group_by(cohort_month, months_between) %>%
  summarise(distinct_customer = n_distinct(id))

ggplot(
  data = cohort_analysis_df %>% filter(cohort_month == '2010-04-01'),
  mapping = aes(y = distinct_customer,
                x = months_between)
) +
  geom_line(color = "blue") +
  theme_minimal() +
  labs(x='Cohort Index', y="# New Customer Joined", title = "Customer Returning for Cohort Month 2010-04-01")

ggsave("Cohort_Month_Customer_Number.png",
       plot = last_plot(),
       path = "output/viz")


#### cross tab for easier viewing
cohort_cross_tab <- cohort_analysis_df %>%
  spread(months_between, distinct_customer)


ggplot(
  data = cohort_cross_tab,
  mapping = aes(y = `0`,
                x = cohort_month)
) +
  geom_line(color = "red") +
  theme_minimal() +
  labs(x='Cohort Month', y="# New Customer Joined", title = "# New Customer Joined Per Cohort Month")

ggsave("New_Customer_Number.png",
       plot = last_plot(),
       path = "output/viz")

red <- cohort_cross_tab %>% ungroup() %>% mutate(cumsum_0 = cumsum(`0`), persent_growth=`0`*100/lag(cumsum_0))
ggplot(
  data = cohort_cross_tab %>% ungroup() %>% mutate(cumsum_0 = cumsum(`0`), persent_growth=`0`/lag(cumsum_0)),
 
) +
  geom_line(color = "blue",  mapping = aes(y = cumsum_0,
                                          x = cohort_month)) +
  geom_bar(aes(y=`0`, x = cohort_month, fill=factor(cohort_month)), stat = "identity", show.legend = FALSE) +
  theme_minimal() +
  labs(x='Cohort Month', y="# New Customer Joined", title = "# New Customer Joined Per Cohort Month")


ggplot(
  data = cohort_cross_tab %>% ungroup() %>% mutate(cumsum_0 = cumsum(`0`), percent_growth=`0`/lag(cumsum_0)),
  
) +
  geom_line(color = "blue",  mapping = aes(y = percent_growth,
                                           x = cohort_month)) +
  theme_minimal() +
  labs(x='Cohort Month', y="Growth Rate", title = "# New Customer Joined Growth rate")

ggsave("Growth_rate.png",
       plot = last_plot(),
       path = "output/viz")


#######
# CohortMonth is the first ever transaction/invoice month of a customer in date
# We can proxy it for their joining date
# Consider CohortMonth 2009-12-01: For CohortIndex/months_between 0,
# this tells us that 937 unique customers made transactions during CohortMonth 2010-12-01.
# For CohortIndex 1,
# this tells that there are 327 customers out of 937 who made their first transaction during
# CohortMonth 2009-12-01 and they also made transactions during the next month. That is, they remained active.
#
# For CohortIndex 2, this tells that there are 315 customers out of 937
# who made their first transaction during CohortMonth 2009-12-01 and
# they also made transactions during the second-next month. And so on for higher CohortIndices.



# Let us now calculate the Retention Rate.
# It is defined as the percentage of active customers out of total customers.
# Since the number of active customers in each cohort corresponds to the CohortIndex 0 values,
# we take the first column of the data as the cohort sizes.


retention_df <- cohort_cross_tab %>%
  ungroup() %>%
  mutate(across(`0`:`24`, ~ .x / `0`))  # divide all columns except cohort month by `0` column values

retention_pivoted <- pivot_longer(
  data = retention_df,
  cols = -c(1),
  names_to = "cohort_index",
  values_to = "retention_rate"
)

retention_pivoted$cohort_index <-
  as.numeric(retention_pivoted$cohort_index)

ggplot(
  data = retention_pivoted,
  mapping = aes(y = cohort_month,
                x = cohort_index,
                fill = retention_rate)
) +
  geom_tile(color = "black", alpha=0.7) +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"), na.value = NA, name="Retention Rate",) +
  labs(x = "Cohort Index", y="Cohort Month", title = "Retention Rate Per Cohort")

ggsave("Cohort_Retention_Rate.png",
       plot = last_plot(),
       path = "output/viz")

### Looks like customer retention is not all the great

ggplot(
  data = retention_pivoted %>% filter(cohort_month == '2010-04-01'),
  mapping = aes(y = retention_rate * 100,
                x = cohort_index)
) +
  geom_bar(aes(
               fill = factor(cohort_index)) ,stat = "identity", show.legend = FALSE) +
  geom_line(color='blue') +
  geom_point(color='red', size=4) +
  theme_minimal() +
  labs(x='Cohort Index', y="% of Cohort Customer Purchasing", title = "Customer Returning for Cohort Month 2010-04-01")

ggsave("Cohort_Month_Customer_Number.png",
       plot = last_plot(),
       path = "output/viz")





ggplot(
  data = retention_pivoted %>% group_by(cohort_index) %>% summarise(mean_cust_percent = mean(retention_rate, na.rm = TRUE)),
  mapping = aes(y = mean_cust_percent * 100,
                x = cohort_index)
) +
  geom_bar(aes(
    fill = factor(cohort_index)) ,stat = "identity", show.legend = FALSE) +
  geom_line(color='blue') +
  geom_point(color='red', size=4) +
  theme_minimal() +
  labs(x='Cohort Index', y="Mean % of Cohort Customer Purchasing", title = "Customer Returning for Cohort Month")

ggsave("Mean_Cohort_Month_Customer_Number.png",
       plot = last_plot(),
       path = "output/viz")
