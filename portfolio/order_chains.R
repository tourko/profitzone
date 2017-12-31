library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

# Read transactions
transactions <- readRDS("transactions.rds")

#
# Group transactions into orders and add "order_id".
#
# Order is a collection of transactions executed on the same date,
# for the same symbol and with the same first letter for the tag_number.
#
orders <- transactions %>% 
  # Get the first letter of the tag_number
  mutate(tag_letter = str_sub(tag_number, 1, 1)) %>% 
  # "Wrap" transactions by trade_date, symbol and the first letter of the tag_number
  nest(-trade_date, -symbol, -tag_letter, .key = "transactions") %>% 
  # Add order_id
  mutate(order_id = str_c("R", str_pad(row_number(), 5, pad = "0"))) %>% 
  # "Unwrap" transactions
  unnest() %>% 
  # Put "order_id" in front folowed by transactions columns
  select(order_id, one_of(colnames(transactions)))

#
# Classify orders as either OPEN, CLOSE or ROLL.
#
# If all transactions in an order are OPEN, then it is an OPEN order.
# If all transactions in an order are CLOSE, then it is a CLOSE order.
# If there is a mix of OPEN and CLOSE transactions in an order, then it is a ROLL order.

orders <-
  # Create a contigency matrix that shows relations between orders and open_close field of the transactions.
  # Rows contain order_id, columns contain open_close:
  #          open_close
  # order_id OPEN CLOSE
  # R00001    1     0
  # R00002    4     0
  # R00003    4     0
  # R00004    0     1
  # R00005    2     0
  # ......   ...   ...
  xtabs(~ order_id + open_close, data = orders) %>%
  # Convert to a tibble:
  # A tibble: 332 x 3
  #   order_id open_close n_legs
  #      <chr>      <chr>  <int>
  # 1   R00001       OPEN      1
  # 2   R00002       OPEN      4
  # 3   R00003       OPEN      4
  # 4   R00004       OPEN      0
  # 5   R00005       OPEN      2
  # .   ......       ....    ...
  as_tibble(n = "n_legs") %>%
  # Spread "open_close" into two columns: CLOSE and OPEN
  # A tibble: 166 x 3
  #   order_id CLOSE  OPEN
  # *    <chr> <int> <int>
  # 1   R00001     0     1
  # 2   R00002     0     4
  # 3   R00003     0     4
  # 4   R00004     1     0
  # 5   R00005     0     2
  # .   ......   ...   ...
  spread(open_close, n_legs) %>%
  # Convert OPEN and CLOSE to logical: 0 - FALSE; non-0 - TRUE
  # A tibble: 166 x 3
  #   order_id CLOSE  OPEN
  # <chr> <lgl> <lgl>
  # 1   R00001 FALSE  TRUE
  # 2   R00002 FALSE  TRUE
  # 3   R00003 FALSE  TRUE
  # 4   R00004  TRUE FALSE
  # 5   R00005 FALSE  TRUE
  # .   ......   ...   ...
  mutate(CLOSE = as.logical(CLOSE), OPEN = as.logical(OPEN)) %>%
  # Add ROLL column, which is TRUE if both CLOSE and OPEN, otherwise FALSE
  # Also set OPEN and CLOSE to FALSE, where ROLL is TRUE: xor(..., ROLL)
  # A tibble: 166 x 4
  #   order_id CLOSE  OPEN  ROLL
  #     <chr>  <lgl> <lgl> <lgl>
  # 1   R00001 FALSE  TRUE FALSE
  # 2   R00002 FALSE  TRUE FALSE
  # 3   R00003 FALSE  TRUE FALSE
  # 4   R00004  TRUE FALSE FALSE
  # 5   R00005 FALSE  TRUE FALSE
  # .   ......   ...   ...   ...
mutate(ROLL = CLOSE & OPEN, OPEN = xor(OPEN, ROLL), CLOSE = xor(CLOSE, ROLL)) %>%
  # Gather OPEN, CLOSE and ROLL colums into key "order_type" and value "v".
  # Filter out rows that have FALSE in "v" and drop "v" column.
  gather(key = order_type, value = v, -order_id) %>% filter(v) %>% select(-v) %>% 
  # Finally, convert "order_type" to a factor and sort by "order_id":
  # A tibble: 166 x 2
  #   order_id order_type
  #      <chr>      <chr>
  # 1   R00001       OPEN
  # 2   R00002       OPEN
  # 3   R00003       OPEN
  # 4   R00004      CLOSE
  # 5   R00005       OPEN
  # .   ......        ...
  mutate(order_type = factor(order_type, levels = c("OPEN", "CLOSE", "ROLL"))) %>% arrange(order_id) %>% 
  # Add "order_type" to the "orders" dataframe by joining the two
  inner_join(orders, by = "order_id") %>% 
  # Put "order_type" column after "trade_date"
  select(order_id, trade_date, order_type, everything())

#
# Chain orders by CUSIPs. If an instrument with a given CUSIP is found in diffrent orders,
# then these orders must be related and hence has to be chained together.
#

# Create a contigency matrix that shows relations between orders and CUSIPs.
# Rows contain order_id, columns contain cusip:
#          cusip
# order_id 369604103 8BKQXT2 8BRRSQ6 8BRTKX9 8BRTNB5 ...
# R00001       0       0       0       0       0     ...
# R00002       0       0       0       0       0     ...
# R00003       0       0       0       0       0     ...
# R00004       0       0       0       0       0     ...
# R00005       0       0       0       0       0     ...
# ......      ...     ...     ...     ...     ...    ...  
m <- xtabs(~ order_id + cusip, data = orders)

order_chain <-
  # Build a list of matrixes for the related orders:
  #
  # [[1]]
  #          cusip
  # order_id 9H82162
  # R00001      1
  # R00004      1
  #
  # [[2]]
  #          cusip
  # order_id 8BWGYG3 8BWGYH3 8BWGYJ1 9BWGYG5 9BWGYG6 9BWGYH8
  # R00002      1       0       1       1       0       1
  # R00040      0       1       1       0       1       1
  # R00057      1       0       0       1       0       0
  # R00088      0       1       0       0       1       0
  #
  # [[3]]
  #          cusip
  # order_id 8H54976 8H55643 9H55256 9H55257
  # R00003      1       1       1       1
  # R00043      0       0       1       1
  # R00066      0       1       0       0
  #
  # [[4]]
  #          cusip
  # order_id 9H82162
  # R00001      1
  # R00004      1
  #
  # Itterate through the rows of the contigency matrix.
  # Use index for the rows, so that we can get a row as matrix rather than as a vector.
  1:nrow(m) %>% map(function(i) {
    # Get a row as a matrix
    r <- m[              i,                , drop = FALSE]
    
    # Initial number of rows
    n = 1
    repeat {
      # Get columns (in the selected row(s)), which contain at least one non-0 value
      r <- m[               , colSums(r) != 0, drop = FALSE]
      # Get rows (in the selected column(s)), which contain at least one non-0 value 
      r <- m[rowSums(r) != 0,                , drop = FALSE]
      # Get number of rows
      k = nrow(r)
      # If number of rows increased compared to the previous itteration,
      # then we found new relations.
      if (k > n) {
        # Repeat the loop again and see if we find more rows (orders).
        n = k
      } else {
        # No more rows found, hence we already have all related rows (orders)
        break
      }
    }
    
    r <- r[rowSums(r) != 0, colSums(r) != 0, drop = FALSE]
  }) %>%
  # The list may contain duplicated matrixes. From the example above, the orders "R00001" and "R00004" are related,
  # So we'll get identical matrixes, for rows "R00001" and "R00004" in the contigency matrix.
  # Leave only unique matrixes in the list.
  unique() %>%
  # Replace each matrix in the list with a tibble with 2 columns: order_id and chain_id.
  # [[1]]
  # # A tibble: 2 x 2
  #   order_id chain_id
  #      <chr>    <chr>
  # 1   R00001   C00001
  # 2   R00004   C00001
  # 
  # [[2]]
  # # A tibble: 4 x 2
  #   order_id chain_id
  #      <chr>    <chr>
  # 1   R00002   C00002
  # 2   R00040   C00002
  # 3   R00057   C00002
  # 4   R00088   C00002
  # 
  # order_id comes from the row names of the matrixes and
  # chain_id is based on the index of the element in the list.
  #
  # Finally, merge all elements in the list into a single tibble by binding the rows.
  map2_dfr(seq_along(.), ~ tibble(order_id = rownames(.x), chain_id = str_c("C", str_pad(.y, 5, pad = "0"))))

# Add chain_id to the orders data frame
chained_orders <- orders %>% 
  # Join orders with order_chains
  inner_join(order_chain, by = "order_id") %>% 
  # Put chain_id column in the front
  select(chain_id, everything()) %>% 
  # Sort by chain_id
  arrange(chain_id)
