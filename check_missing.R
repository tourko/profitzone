#
# Check if there are any missing transactions that should have been there
#
library(dplyr)
library(tidyr)

transactions <- readRDS("transactions.rds")

# Convert to a data frame with the OPENING and CLOSING quantities for each CUSIP
#      cusip  OPEN CLOSE
# *   <chr> <dbl> <dbl>
# 1 8BKQXT2     1     1
# 2 8BRRSQ6     1     1
# 3 8BRTKX9     1     1
# 4 8BRTNB5    NA     1
# .     ...     .     .
close_open <- transactions %>% 
  select(cusip, open_close, quantity) %>% 
  group_by(cusip, open_close) %>% 
  summarise(quantity = sum(quantity)) %>% 
  spread(open_close, quantity)

# Find CUSIPs with CLOSING transactions, but with no OPENING transactions
close_with_no_open <- close_open %>%
  filter(is.na(OPEN), CLOSE > 0)

if (nrow(close_with_no_open) > 0) {
  message("CLOSING transactions that have no matching OPENING transactions:")
  print(transactions %>% filter(cusip %in% close_with_no_open$cusip))
}

# Find CUSIPs that have greater CLOSING quantity than OPENING quantity
close_gt_open <- close_open %>%
  na.omit() %>% 
  filter(CLOSE > OPEN)

if (nrow(close_gt_open) > 0) {
  message("CLOSING transactions that have greater quantity than OPENING transactions:")
  print(transactions %>% filter(cusip %in% close_gt_open$cusip))
}

