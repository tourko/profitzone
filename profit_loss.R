library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)

profit_loss <- function(transactions) {
  # Merge split options symbols with there "main" symbol, e.g. UNG1 -> UNG
  transactions <- transactions %>% 
    mutate(symbol = stringr::str_replace(symbol, pattern = capture(one_or_more(UPPER)) %R% optional(DGT), replacement = "\\1"))
  
  # Get open and close quantities by stock and cusip.
  #   symbol   cusip open_qty close_qty
  # *  <chr>   <chr>    <dbl>     <dbl>
  # 1   AAPL 8BRTKX9        1        -1
  # 2   AAPL 8BRTNB5        1        -1
  # 3   AAPL 8BRTNF4        1        -1
  # 4   AAPL 8KLVSQ0        1        -1
  # 5   AAPL 8KLVVP9        1        -1
  quantity <- transactions %>% 
    mutate(quantity = quantity * if_else(action == "SELL", -1, 1)) %>%
    select(symbol, cusip, position, quantity) %>% 
    group_by(symbol, cusip, position) %>% 
    summarise(quantity = sum(quantity)) %>% 
    spread(position, quantity, fill = 0) %>% 
    rename(open_qty = OPEN, close_qty = CLOSE)
  
  # Get open and close credit/debit by stock and cusip.
  #   symbol   cusip open_cr_db close_cr_db
  # *  <chr>   <chr>      <dbl>       <dbl>
  # 1   AAPL 8BRTKX9       -134          30
  # 2   AAPL 8BRTNB5        -37          13
  # 3   AAPL 8BRTNF4        -62          11
  # 4   AAPL 8KLVSQ0       -112          46
  # 5   AAPL 8KLVVP9        -66          10
  credit_debit <- transactions %>% 
    mutate(credit_debit = principal * if_else(action == "BUY", -1, 1)) %>%
    select(symbol, cusip, position, credit_debit) %>% 
    group_by(symbol, cusip, position) %>% 
    summarise(credit_debit = sum(credit_debit)) %>% 
    spread(position, credit_debit, fill = 0) %>% 
    rename(open_cr_db = OPEN, close_cr_db = CLOSE)
  
  # Sum up quantities and credit/debit and merge into one data frame.
  #   symbol   cusip quantity profit_loss
  #    <chr>   <chr>    <dbl>       <dbl>
  # 1   AAPL 8BRTKX9        0        -104
  # 2   AAPL 8BRTNB5        0         -24
  # 3   AAPL 8BRTNF4        0         -51
  # 4   AAPL 8KLVSQ0        0         -66
  # 5   AAPL 8KLVVP9        0         -56
  all <- inner_join(quantity, credit_debit, by = c("symbol","cusip")) %>% 
    mutate(quantity = open_qty + close_qty,
           profit_loss = open_cr_db + close_cr_db) %>% 
    select(symbol, cusip, quantity, profit_loss)
  
  # Calculate P/L for the closed positions.
  closed <- all %>%
    filter(quantity == 0) %>% 
    group_by(symbol) %>% 
    summarise(profit_loss = sum(profit_loss))
  
  # Calculate P/L for the non-closed positions.
  # It could still be opened positions or expired positions.
  non_closed <- all %>%
    filter(quantity != 0)
  
  # Out of non-closed positions find cusips of the expired options
  expired_options <- transactions %>% 
    filter(cusip %in% non_closed$cusip, position == "OPEN", expiration_date < today()) %>%
    select(cusip)
  
  # Calculate P/L for the expired options.
  expired <- non_closed %>% 
    semi_join(expired_options, by = "cusip") %>% 
    group_by(symbol) %>% 
    summarise(profit_loss = sum(profit_loss))
  
  # Merge closed and expired to get realized P/L
  realized <- bind_rows(closed, expired) %>% 
    group_by(symbol) %>% 
    summarise(realized = sum(profit_loss))
  
  # Calculate unrealized P/L, which is P/L of opened positions
  unrealized <-  anti_join(non_closed, expired_options, by = "cusip") %>% 
    group_by(symbol) %>% 
    summarise(unrealized = sum(profit_loss))
  
  # Calculate commissions and fees
  commissions_and_fees <- transactions %>% 
    select(symbol, commission, transaction_fee, additional_fee) %>% 
    group_by(symbol) %>% 
    summarise(commissions = sum(commission), fees = sum(transaction_fee) + sum(additional_fee))
  
  # Merge realized, unrealized and commissions_and_fees into one data frame
  profit_loss <- reduce(list(realized, unrealized, commissions_and_fees), full_join, by = "symbol")
}

transactions <- readRDS("transactions.rds")
p_and_l <- profit_loss(transactions)
print(p_and_l)
