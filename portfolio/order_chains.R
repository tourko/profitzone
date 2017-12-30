library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

# Read transactions
transactions <- readRDS("transactions.rds")

# Order is a collection of transactions executed on the same date,
# for the same symbol and with the same first letter for the tag_number.
# We would like to add an order_id for each order.
orders <- transactions %>% 
  # Get the first letter of the tag_number
  mutate(tag_letter = str_sub(tag_number, 1, 1)) %>% 
  # Group by trade_date, symbol and the first letter of the tag_number
  nest(-trade_date, -symbol, -tag_letter, .key = "transactions") %>% 
  # Add order_id
  mutate(order_id = str_c("R", str_pad(row_number(), 5, pad = "0"))) %>% 
  unnest() %>% 
  select(order_id, one_of(colnames(transactions)))

# Chain orders by CUSIPs. If an instrument with a given CUSIP is present
# in diffrent orders, then these orders got to be related and hence has to be chained.
cusip_links <- orders %>% 
  # Add link_id column. We can use cusip as a unique identifier for links.
  mutate(link_id = cusip) %>% 
  select(order_id, link_id)

# Create contigency matrix that shows relations between orders and cusip links.
# Rows contain order_id, columns contain link_id:
#          link_id
# order_id 369604103 8BKQXT2 8BRRSQ6 8BRTKX9 8BRTNB5 ...
# R00001       0       0       0       0       0     ...
# R00002       0       0       0       0       0     ...
# R00003       0       0       0       0       0     ...
# R00004       0       0       0       0       0     ...
# R00005       0       0       0       0       0     ...
#  ...        ...     ...     ...     ...     ...    ...  
m <- xtabs(~ order_id + link_id, cusip_links)

order_chain <-
  # Build a list of matrixes for the related orders:
  #
  # [[1]]
  #          link_id
  # order_id 9H82162
  # R00001      1
  # R00004      1
  #
  # [[2]]
  #          link_id
  # order_id 8BWGYG3 8BWGYH3 8BWGYJ1 9BWGYG5 9BWGYG6 9BWGYH8
  # R00002      1       0       1       1       0       1
  # R00040      0       1       1       0       1       1
  # R00057      1       0       0       1       0       0
  # R00088      0       1       0       0       1       0
  #
  # [[3]]
  #          link_id
  # order_id 8H54976 8H55643 9H55256 9H55257
  # R00003      1       1       1       1
  # R00043      0       0       1       1
  # R00066      0       1       0       0
  #
  # [[4]]
  #          link_id
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

chained_orders <- orders %>% 
  # Join orders with order_chains
  inner_join(order_chain, by = "order_id") %>% 
  # Put chain_id column in the front
  select(chain_id, everything())

