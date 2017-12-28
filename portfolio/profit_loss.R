library(dplyr)

transactions <- readRDS("transactions.rds")

# Get open and close quantities by stock and cusip.
#   symbol   cusip open_qty close_qty
# *  <chr>   <chr>    <dbl>     <dbl>
# 1   AAPL 8BRTKX9        1        -1
# 2   AAPL 8BRTNB5        1        -1
# 3   AAPL 8BRTNF4        1        -1
# 4   AAPL 8KLVSQ0        1        -1
# 5   AAPL 8KLVVP9        1        -1
quantity <- transactions %>% 
  mutate(quantity = quantity * if_else(buy_sell == "SELL", -1, 1)) %>%
  select(symbol, cusip, open_close, quantity) %>% 
  group_by(symbol, cusip, open_close) %>% 
  summarise(quantity = sum(quantity)) %>% 
  spread(open_close, quantity, fill = 0) %>% 
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
  mutate(credit_debit = principal * if_else(buy_sell == "BUY", -1, 1)) %>%
  select(symbol, cusip, open_close, credit_debit) %>% 
  group_by(symbol, cusip, open_close) %>% 
  summarise(credit_debit = sum(credit_debit)) %>% 
  spread(open_close, credit_debit, fill = 0) %>% 
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
  filter(cusip %in% non_closed$cusip, open_close == "OPEN", expiration_date < today()) %>%
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

# Calculate unrealized P/L
unrealized <- non_closed %>%
  anti_join(expired_options, by = "cusip") %>% 
  group_by(symbol) %>% 
  summarise(unrealized = sum(profit_loss))

# Calculate commissions and fees
commissions_and_fees <- transactions %>% 
  select(symbol, commission, transaction_fee, additional_fee) %>% 
  group_by(symbol) %>% 
  summarise(commissions = sum(commission), fees = sum(transaction_fee) + sum(additional_fee))

# Merge realized, unrealized and commissions_and_fees into one data frame
profit_loss <- reduce(list(realized, unrealized, commissions_and_fees), full_join, by = "symbol")

print(profit_loss)
