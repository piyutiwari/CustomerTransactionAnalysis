library(tidyverse)
library(readxl)
library(varhandle)
library(lubridate)
library(reshape2)
library (rfm)
library(devtools)
library(caret)
library(glinternet)
set.seed(1001)

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

count(combined_df, purchase_year)

get_train_test <-
  function(combined_df,
           start_year = 0,
           end_year = 9999,
           date_of_analysis = "2011-12-10") {
    combined_df <-
      combined_df %>% filter(purchase_year > start_year &
                               purchase_year <= end_year)
    #### combined_df is the final cleaned data set with rows 713,979. This should be used further to add all of the requred features.
    #Train - 2009/12 to 2010/12 TEST 2011
    ## assumption is that one invoice = 1 order
    ## calculating avg order value =>  a cust Piyu has 10 invoices - calc total cost of each of the 10 invc - then avg of each tot cost of each invc for Piyu
    ## avg order value per customer. --> dependent variable
    cust_df_aov_bv <- combined_df %>%
      group_by(id) %>% summarise(
        total_order_value = sum(total_cost),
        total_orders = n_distinct(Invoice),
        total_basket_size = sum(Quantity),
      ) %>% mutate(
        avg_basket_size = total_basket_size / total_orders,
        avg_order_value = total_order_value / total_orders
      )
    
    last3_df <- combined_df %>%
      group_by(id, Invoice, InvoiceDate) %>%
      summarise(invoice_cost = sum(total_cost)) %>%
      ungroup() %>%
      group_by(id) %>%
      top_n(3, desc(InvoiceDate)) %>%
      mutate(row_n = row_number()) %>%
      select(c(id, row_n, invoice_cost)) %>%
      spread(row_n, invoice_cost) %>%
      select(id, `1`, `2`, `3`)
    
    #Order Month Qty
    cust_month <-
      combined_df %>% group_by(id, purchase_month) %>% summarise(orders = n_distinct(Invoice))
    
    cust_month <-
      dcast(cust_month,
            id ~ purchase_month,
            value.var = "orders",
            fill = 0)
    names(cust_month) <-
      c(
        'id',
        'jan_ord',
        'feb_ord',
        'mar_ord',
        'apr_ord',
        'may_ord',
        'jun_ord',
        'july_ord',
        'aug_ord',
        'sep_ord',
        'oct_ord',
        'nov_ord',
        'dec_ord'
      )
    
    #Order Weekday Qty
    cust_wday <-
      combined_df %>% group_by(id, purchase_wday) %>% summarise(orders = n_distinct(Invoice))
    cust_wday <-
      dcast(cust_wday,
            id ~ purchase_wday,
            value.var = "orders",
            fill = 0)
    # names(cust_wday) <-
    #   c('id',
    #     'sun_ord',
    #     'mon_ord',
    #     'tue_ord',
    #     'wed_ord',
    #     'thu_ord',
    #     'fri_ord',
    #     'sat_ord')
    
    #RFM
    analysis_date <- as.Date(date_of_analysis) #last date + 1
    str(analysis_date)
    rfm_df <-
      group_by(combined_df, id) %>% summarise(
        Recency = as.numeric(analysis_date - max(clean_date)),
        Frequency = n(),
        Monetary = sum(total_cost)
      )
    
    n_distinct(combined_df$id)
    n_distinct(cust_df_aov_bv$id)
    
    #Normalizing these scores
    rfm_df$R_score <- 0
    rfm_df$R_score[rfm_df$Recency >= 402.0] <- 1
    rfm_df$R_score[rfm_df$Recency >= 118.0 &
                     rfm_df$Recency < 402.0] <-
      2
    rfm_df$R_score[rfm_df$Recency >= 48.0 &
                     rfm_df$Recency < 118.0] <- 3
    rfm_df$R_score[rfm_df$Recency < 48.00] <- 4
    
    rfm_df$F_score <- 0
    rfm_df$F_score[rfm_df$Frequency >= 142] <- 4
    rfm_df$F_score[rfm_df$Frequency < 142 &
                     rfm_df$Frequency >= 53] <- 3
    rfm_df$F_score[rfm_df$Frequency < 53 &
                     rfm_df$Frequency >= 21] <- 2
    rfm_df$F_score[rfm_df$Frequency < 21] <- 1
    
    rfm_df$M_score <- 0
    rfm_df$M_score[rfm_df$Monetary >= 2307.1] <- 4
    rfm_df$M_score[rfm_df$Monetary < 2307.1 &
                     rfm_df$Monetary >= 898.9] <- 3
    rfm_df$M_score[rfm_df$Monetary < 898.9 &
                     rfm_df$Monetary >= 348.8] <- 2
    rfm_df$M_score[rfm_df$Monetary < 348.8] <- 1
    
    rfm_df <-
      rfm_df %>% mutate(RFM_score = 100 * R_score + 10 * F_score + M_score)
    
    #RFM Segment Tags
    rfm_df$Segment <- "0"
    rfm_df$Segment[which(rfm_df$RFM_score %in% c(444, 434, 443, 344, 442, 244, 424, 441))] <-
      "Loyalists"
    rfm_df$Segment[which(
      rfm_df$RFM_score %in% c(
        332,
        333,
        342,
        343,
        334,
        412,
        413,
        414,
        431,
        432,
        441,
        421,
        422,
        423,
        424,
        433
      )
    )] <- "Potential Loyalists"
    rfm_df$Segment[which(rfm_df$RFM_score %in% c(233, 234, 241, 311, 312, 313, 314, 321, 322, 323, 324, 331,  341))] <-
      "Promising"
    rfm_df$Segment[which(rfm_df$RFM_score %in% c(124, 133, 134, 142, 143, 144, 214, 224, 234, 242, 243, 232))] <-
      "Hesitant"
    rfm_df$Segment[which(rfm_df$RFM_score %in% c(122, 123, 131 , 132, 141, 212, 213, 221, 222, 223, 231))] <-
      "Need attention"
    rfm_df$Segment[which(rfm_df$RFM_score %in% c(111, 112, 113, 114, 121, 131, 211, 311, 411))] <-
      "Detractors"
    
    #Combine All Customer Data for TRAIN
    train <- inner_join(cust_month, cust_wday, by = c("id" = "id"))
    train <- inner_join(train, rfm_df, by = c("id" = "id"))
    train <- inner_join(train, cust_df_aov_bv, by = c("id" = "id"))
    cust_with_atleast_3_txn <-
      last3_df %>% rowwise() %>% mutate(mean_trx = mean(c(`1`, `2`, `3`)),
                                        stddev_trx = sd(c(`1`, `2`, `3`)))
    train <-
      inner_join(train, cust_with_atleast_3_txn, by = c("id" = "id"))
    #drop unneccessary columns
    train$total_order_value <- NULL
    train$total_orders <- NULL
    train$Segment <- factor(train$Segment)
    train$RFM_score <- factor(train$RFM_score)
    train$R_score <- factor(train$R_score)
    train$F_score <- factor(train$F_score)
    train$M_score <- factor(train$M_score)
    return(train)
  }

train <-
  get_train_test(
    combined_df = combined_df,
    date_of_analysis = "2011-01-01",
    end_year = 2010
  )
train[is.na(train)] <- 0
train$`7` <- NULL

colnames(train)

test <-
  get_train_test(
    combined_df = combined_df,
    date_of_analysis = "2011-12-10",
    start_year = 2010
  )

y <- train$avg_order_value
df <- train %>% select(-c(id, avg_order_value, RFM_score))

numLevels <- df %>% sapply(nlevels)
numLevels[numLevels == 0] <- 1
i_num <- sapply(df, is.numeric)
df$Segment <- as.integer(df$Segment) - 1
df$R_score <- as.integer(df$R_score) - 1
df$F_score <- as.integer(df$F_score) - 1
df$M_score <- as.integer(df$M_score) - 1
X <- df


cv_fit <- glinternet.cv(X, y, numLevels, nFolds = 3)

plot(cv_fit)

i_1Std <- which(cv_fit$lambdaHat == cv_fit$lambda)
coefs <- coef(cv_fit$glinternetFit)[[i_1Std]]
coefs$mainEffects

coefs$mainEffectsCoef

# Now for the interaction pairs:
coefs$interactions


idx_num <- (1:length(i_num))[i_num]
idx_cat <- (1:length(i_num))[!i_num]
names(numLevels)[idx_cat[coefs$mainEffects$cat]]
names(numLevels)[idx_num[coefs$mainEffects$cont]]
coefs$mainEffectsCoef

# The model has a root mean squared error (RMSE) on validation data of
sqrt(cv_fit$cvErr[[i_1Std]])


test <- get_train_test(combined_df = combined_df, start_year = 2010)
test[is.na(test)] <- 0
y_test <- test$avg_order_value
df_test <- test %>% select(-c(id, avg_order_value, RFM_score))

numLevels <- df_test %>% sapply(nlevels)
numLevels[numLevels == 0] <- 1
i_num <- sapply(df_test, is.numeric)
df_test$Segment <- as.integer(df_test$Segment) - 1
df_test$R_score <- as.integer(df_test$R_score) - 1
df_test$F_score <- as.integer(df_test$F_score) - 1
df_test$M_score <- as.integer(df_test$M_score) - 1

pred_rows <- predict(cv_fit, df_test)
test['pred_glint'] <- predict(cv_fit, df_test)

library(Metrics)
mape(y_test, pred_rows)

rmse(y_test, pred_rows)

write.csv(test, "output/data/glinternet_cv_test_results.csv")

ggplot(test[test$avg_order_value < 2000,],                                     # Draw plot using ggplot2 package
       aes(x = avg_order_value,
           y = pred_glint)) +
  geom_point() +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "red",
    size = 2
  ) +
  labs(x = "average order value", y = "predicted value", title = "GL Internet Actual Vs Predicted")

ggsave("GL_Internet_Actual_Vs_Predicted.png",
       plot = last_plot(),
       path = "output/viz")
