library(dplyr)
library(purrr)
library(stringr)
library(rebus)
library(lubridate)
library(data.table)

#
# Define commonly used patterns
#
pattern <- list(
  # Number in accounting format: "3.00" "123.00" "1,234.00" "1,234,567.00" "-123.00"
  accounting_number = optional("-") %R% zero_or_more( group( digit(1,3) %R% "," ) ) %R% digit(1,3) %R% DOT %R% digit(2),
    
  # Price with 7 digits after the dot: "0.1300000" "10.3500000"
  # They are all less than 1000, so we can use simplified pattern.
  price_number = digit(1,3) %R% DOT %R% digit(7),
  
  # Commission and fee amount: "0.00" "1.00" "0.04" "0.10"
  # They are all less than 10, so we can use simplified pattern.
  commission_number = DGT %R% DOT %R% digit(2),
  
  # Tag number - either a capital letter or a digit followed by 4 digits: "C0998" "B6388" "15056"
  tag_number = or(UPPER,DGT) %R% digit(4),
  
  # Option strike number: "28" "350" "62.50" "1020"
  strike_number = digit(1,4) %R% optional( group( DOT %R% digit(2) ) ),
  
  # Option CUSIP format - seven-character alphanumeric: "9H82162"
  option_cusip_string = repeated(ALNUM, 7),
  
  # Stock CUSIP format - nine-digit string: "369604103"
  stock_cusip_string = repeated(DIGIT, 9)
) # End of patterns deifnitions


transaction_blocks <- list()
transaction_blocks$option = list(
  patterns = c(
    # Examples of line 1 for options:
    # "2  S  08/30/17  08/31/17   1         0.7500000      75.00  1.00  0.06  0.10  E8470      73.84  TUA0831  1 1"
    # "2  B  11/21/17  11/22/17   1         1.0600000     106.00  0.00  0.04  0.10  B4169     106.14  TUA1122  1 1"
    line1 = START %R% DGT %R%                                #   -  Account Type
            SPC %R% or("B", "S", capture = TRUE) %R%         #  (1) BUY or SELL
            SPC %R% capture(MDY) %R%                         #  (2) Trade Date
            SPC %R% MDY %R%                                  #   -  Settle Date
            SPC %R% capture(one_or_more(DGT)) %R%            #  (3) Quantity
            SPC %R% capture(pattern$price_number) %R%        #  (4) Price
            SPC %R% capture(pattern$accounting_number) %R%   #  (5) Principal
            SPC %R% capture(pattern$commission_number) %R%   #  (6) Commission
            SPC %R% capture(pattern$commission_number) %R%   #  (7) Transaction fee
            SPC %R% capture(pattern$commission_number) %R%   #  (8) Additional fee
            SPC %R% capture(pattern$tag_number) %R%          #  (9) Tag number
            SPC %R% capture(pattern$accounting_number),      # (10) Net ammount
    
    # Examples of line 2 for options:
    # "Desc:  PUT  XOP  10/20/17  28  SPDR S&P OIL&GAS EXPL & PRDN  Interest/STTax:  0.00  CUSIP:  9H82162"
    # "Desc:  CALL AAPL 12/15/17 190  APPLE INC                     Interest/STTax:  0.00  CUSIP:  8KLVVP9"
    line2 = START %R% "Desc:" %R%
            SPC %R% capture(or("CALL", "PUT")) %R%           # (11) Option type (CALL or PUT)
            SPC %R% capture(one_or_more(UPPER)) %R%          # (12) Symbol of the underlying stock
            SPC %R% capture(MDY) %R%                         # (13) Expiration date
            SPC %R% capture(pattern$strike_number) %R%       # (14) Strike
            SPC %R% one_or_more(PRINT) %R%                   #   -  UL description
            "Interest/STTax:" %R% SPC %R% "0.00" %R%         #   -  Interest/Tax
            SPC %R% "CUSIP:" %R%
            SPC %R% capture(pattern$option_cusip_string),    # (15) Option CUSIP
    
    # Line 3 is the same for all transaction types.
    # "Currency: USD    ReportedPX:     MarkUp/Down:"
    line3 = START %R% "Currency: USD" %R%
            SPC %R% "ReportedPX:" %R%
            SPC %R% "MarkUp/Down:",
    
    # Examples of line 4 for options:
    # "Trailer:    UNSOLICITED, OPEN CONTRACT"
    # "Trailer:    UNSOLICITED, CLOSING CONTRACT"
    line4 = START %R% "Trailer:" %R%
            SPC %R% capture("UNSOLICITED") %R% "," %R%      # (16) UNSOLICITED
            SPC %R% capture(or("OPEN", "CLOSING")) %R%      # (17) OPEN or CLOSE
            SPC %R% "CONTRACT"
  ),
  token_names = c(
    "buy_sell",         #  (1) BUY or SELL
    "trade_date",       #  (2) Trade Date
    "quantity",         #  (3) Quantity
    "price",            #  (4) Price
    "principal",        #  (5) Principal
    "commission",       #  (6) Commission
    "transaction_fee",  #  (7) Transaction fee
    "additional_fee",   #  (8) Additional fee
    "tag_number",       #  (9) Tag number
    "net_amount",       # (10) Net ammount
    "option_type",      # (11) Option type (CALL or PUT)
    "symbol",           # (12) Symbol of the underlying stock
    "expiration_date",  # (13) Expiration date
    "strike",           # (14) Strike
    "cusip",            # (15) Option CUSIP
    "reason",           # (16) UNSOLICITED
    "open_close"        # (17) OPEN or CLOSE
  ),
  augment = function(df) {
    df %>%
      # Convert string values in the columns to the appropriate data types
      mutate(
        # Convert to factor: "B" -> "BUY", "S" -> "SELL"
        buy_sell        = factor(buy_sell, levels = c("B", "S"), labels = c("BUY", "SELL")),
        trade_date      = mdy(trade_date),
        quantity        = as.integer(quantity),
        price           = as.numeric(price),
        # Drop "," as thuosands separator and convert to numeric 
        principal       = as.numeric(str_replace_all(principal, ",", "")),
        commission      = as.numeric(commission),
        transaction_fee = as.numeric(transaction_fee),
        additional_fee  = as.numeric(additional_fee),
        # Drop "," as thuosands separator and convert to numeric
        net_amount      = as.numeric(str_replace_all(net_amount, ",", "")),
        option_type     = factor(option_type, levels = c("CALL", "PUT")),
        expiration_date = mdy(expiration_date),
        strike          = as.numeric(strike),
        # Convert to factor
        reason          = factor(reason, levels = c("UNSOLICITED", "ASSIGNED", "EXERCISED")),
        # Convert to factor: "OPEN" -> "OPEN", "CLOSING" -> "CLOSE"
        open_close      = factor(open_close, levels = c("OPEN", "CLOSING"), labels = c("OPEN", "CLOSE")),
        # Add "instrument" column and set its value to "OPTION"
        instrument      = factor("OPTION", levels = c("STOCK", "OPTION"))) %>%
      # Change columns' order
      select(
        trade_date,
        reason,
        open_close,
        buy_sell,
        symbol,
        instrument,
        quantity,
        option_type,
        strike,
        expiration_date,
        price:additional_fee,
        net_amount,
        cusip,
        tag_number)
  }
)
  
transaction_blocks$stock = list(
  patterns = c(
    # Exampples of line 1 for stock:
    # "2  B   11/21/17  11/24/17  100 GE  24.0000000  2,400.00  5.00  0.00  0.00  A8103   2,405.00  TUA1124 3 1"
    # "2  S   12/08/17  12/12/17  100 UAL 64.3801000  6,438.01  0.00  0.16  0.08  T2198   6,437.77  TUA1212 6 1"
    line1 = START %R% DGT %R%                                #   -  Account Type
            SPC %R% or("B", "S", capture = TRUE) %R%         #  (1) BUY or SELL
            SPC %R% capture(MDY) %R%                         #  (2) Trade Date
            SPC %R% MDY %R%                                  #   -  Settle Date
            SPC %R% capture(one_or_more(DGT)) %R%            #  (3) Quantity
            SPC %R% capture(one_or_more(UPPER)) %R%          #  (4) Symbol of the stock
            SPC %R% capture(pattern$price_number) %R%        #  (5) Price
            SPC %R% capture(pattern$accounting_number) %R%   #  (6) Principal
            SPC %R% capture(pattern$commission_number) %R%   #  (7) Commission
            SPC %R% capture(pattern$commission_number) %R%   #  (8) Transaction fee
            SPC %R% capture(pattern$commission_number) %R%   #  (9) Additional fee
            SPC %R% capture(pattern$tag_number) %R%          # (10) Tag number
            SPC %R% capture(pattern$accounting_number),      # (11) Net ammount
    
    # Examples of line 2 for stock:
    # "Desc:   GENERAL ELECTRIC COMPANY COM                         Interest/STTax:  0.00  CUSIP:  369604103"
    line2 = START %R% "Desc:" %R%
            SPC %R% one_or_more(PRINT) %R%                   #   -  UL description
            "Interest/STTax:" %R% SPC %R% "0.00" %R%         #   -  Interest/Tax
            SPC %R% "CUSIP:" %R%
            SPC %R% capture(pattern$stock_cusip_string),     # (12) Stock CUSIP
    
    # Line 3 is the same for all transaction types
    transaction_blocks$option$patterns["line3"],
    
    # Example of line 4 for stock:
    # "Trailer: UNSOLICITED"
    line4 = START %R% "Trailer:" %R%
            SPC %R% capture("UNSOLICITED")                   # (13) UNSOLICITED
  ),
  token_names = c(
    "buy_sell",         #  (1) BUY or SELL
    "trade_date",       #  (2) Trade Date
    "quantity",         #  (3) Quantity
    "symbol",           #  (4) Symbol of the stock
    "price",            #  (5) Price
    "principal",        #  (6) Principal
    "commission",       #  (7) Commission
    "transaction_fee",  #  (8) Transaction fee
    "additional_fee",   #  (9) Additional fee
    "tag_number",       # (10) Tag number
    "net_amount",       # (11) Net ammount
    "cusip",            # (12) Stock CUSIP
    "reason"            # (13) UNSOLICITED
  ),
  augment = function(df) {
    df %>%
      # Convert string values in the columns to the appropriate data types
      mutate(
        # Convert to factor: "B" -> "BUY", "S" -> "SELL"
        buy_sell        = factor(buy_sell, levels = c("B", "S"), labels = c("BUY", "SELL")),
        trade_date      = mdy(trade_date),
        quantity        = as.integer(quantity),
        price           = as.numeric(price),
        # Drop "," as thuosands separator and convert to numeric 
        principal       = as.numeric(str_replace_all(principal, ",", "")),
        commission      = as.numeric(commission),
        transaction_fee = as.numeric(transaction_fee),
        additional_fee  = as.numeric(additional_fee),
        # Drop "," as thuosands separator and convert to numeric
        net_amount      = as.numeric(str_replace_all(net_amount, ",", "")),
        # Convert to factor
        reason          = factor(reason, levels = c("UNSOLICITED", "ASSIGNED", "EXERCISED")),
        # If transaction_fee is 0, then it's opening trade, otherwise closing trade
        open_close      = factor(if_else(transaction_fee == 0, "OPEN", "CLOSE"),
                                 levels = c("OPEN", "CLOSE")),
        # Add "instrument" column and set its value to "STOCK"
        instrument      = factor("STOCK", levels = c("STOCK", "OPTION"))) %>% 
      # Change columns' order
      select(
        trade_date,
        reason,
        open_close,
        buy_sell,
        symbol,
        instrument,
        quantity,
        price:additional_fee,
        net_amount,
        cusip,
        tag_number)
  }
)
  
transaction_blocks$assigned = list(
  patterns = c(
    # Line 1 for assigned stock is the same as the line for stocks
    transaction_blocks$stock$patterns["line1"],
    
    # Line 2 for assigned stock is the same as the line for stocks
    transaction_blocks$stock$patterns["line2"],
    
    # Line 3 is the same for all transaction types
    transaction_blocks$stock$patterns["line3"],
    
    # Example of line 4 for assigned stock:
    # Trailer:    A/E 9GMSJJ4 1 ASSIGNED
    line4 = START %R% "Trailer:" %R%
            SPC %R% "A/E" %R%
            SPC %R% pattern$option_cusip_string %R%
            SPC %R% DGT %R%
            SPC %R% capture("ASSIGNED")                      # (13) ASSIGNED
  ),
  # Same token names as for stock transaction
  token_names = transaction_blocks$stock$token_names,
  # Same augment function as for stock transaction
  augment = transaction_blocks$stock$augment
)
  
transaction_blocks$exercised = list(
  patterns = c(
    # Line 1 for exersised option is the same as the line for option
    transaction_blocks$option$patterns["line1"],
    
    # Line 2 for exersised option is the same as the line for option
    transaction_blocks$option$patterns["line2"],
    
    # Line 3 is the same for all transaction types
    transaction_blocks$option$patterns["line3"],
    
    # Example of line 4 for assigned stock:
    # Trailer:    A/E 8BKQXT2 1 EXERCISED, CLOSING CONTRACT
    line4 = START %R% "Trailer:" %R%
            SPC %R% "A/E" %R%
            SPC %R% pattern$option_cusip_string %R%
            SPC %R% DGT %R%
            SPC %R% capture("EXERCISED") %R% "," %R%         # (16) EXERCISED
            SPC %R% capture("CLOSING") %R%                   # (17) CLOSING
            SPC %R% "CONTRACT"
  ),
  # Same token names as for option transaction
  token_names = transaction_blocks$option$token_names,
  # Same augment function as for option transaction
  augment = transaction_blocks$option$augment
)

total_block <- list(
  patterns = c(
      START %R% "TOTAL SHARES" %R%
      SPC %R% or("BOUGHT","SOLD") %R% ":" %R%
      SPC %R% capture(pattern$accounting_number) %R%    # (1) Number of shares bought/sold
      SPC %R% "TOTAL DOLLARS" %R%
      SPC %R% or("BOUGHT","SOLD") %R% ":" %R%   # (2) Dollar amount
      SPC %R% capture(pattern$accounting_number)
  ),
  token_names = c(
    "shares",  # (1) Number of shares bought/sold
    "dollars"  # (2) Dollar amount
  ),
  augment = function(df) {
    df %>%
      # Convert number of shares to integer and dollars to nummeric
      mutate(quantity = as.integer(shares),
             net_amount = as.numeric(str_replace_all(dollars, ",", "")))
  }
)

#
# Parse transaction lines
#
parse_transaction_lines <- function(lines, block) {
  # Apply 4-line patterns to all lines.
  # Returns a list of 4 logical vectors - one per pattern.
  # Example:
  # List of 4
  #   $ 1: logi [1:53] FALSE FALSE FALSE FALSE FALSE FALSE ...
  #   $ 2: logi [1:53] FALSE FALSE FALSE FALSE FALSE FALSE ...
  #   $ 3: logi [1:53] FALSE FALSE FALSE FALSE FALSE FALSE ...
  #   $ 4: logi [1:53] FALSE FALSE FALSE FALSE FALSE FALSE ...
  # Each vector has as may elements as there are lines.
  # An element in a vector indicates if a lines matched a corresponding pattern.
  l <- map(block$patterns, ~ lines %>% str_detect(pattern = .x))
  
  # Convert the list into a matrix with 4 rows and as many columns as there are lines.
  # Example:
  # [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]  [,8]  [,9] [,10] [,11] [,12] [,13] ... [,53]
  # [1,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE ... FALSE
  # [2,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE ... FALSE
  # [3,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE ... FALSE
  # [4,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE ... FALSE
  m <- matrix(unlist(l), byrow = TRUE, nrow = length(l))
  
  # We are intrested in 4 consequetive lines matching the patterns.
  # Such lines will form a "diagonal" with TRUE values in the matrix.
  # In the above example columns 10, 11, 12 and 13 form such "diagonal".
  # In order to find the transactions matching all 4 patterns, we shift the rows
  # from 2 to 4, so that TRUE values form a column.
  # Example:
  # [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]  [,8] [,9] [,10] [,11] [,12] [,13] [,14] ... [,23]
  # [1,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE TRUE FALSE FALSE FALSE  TRUE ... FALSE
  # [2,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE TRUE FALSE FALSE FALSE  TRUE ... FALSE
  # [3,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE TRUE FALSE FALSE FALSE  TRUE ... FALSE
  # [4,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE TRUE FALSE FALSE FALSE  TRUE ... FALSE
  for (i in 2:nrow(m)) {
    # Shift rows by n = i - 1 to the left, where "i" is the row number
    m[i, ] <- shift(m[i, ], n = i - 1, fill = FALSE, type = "lead")
  }
  
  # Find columns that have all TRUE values.
  # Column indexes give us the line numbers of the first lines for the matched transactions.
  first_transaction_lines <- which(colMeans(m) == 1)
  
  # Knowing the indexes of the first lines in the transactions, we can find
  # the indexes of all lines forming the transactions.
  all_transaction_lines <- unlist(map(first_transaction_lines, ~ seq(.x, .x + 3)))
  
  # Apply 4-line patterns again, but now only to the the lines matching the transaction patterns.
  # We want to extract tokens from the transaction lines.
  # "tokens" is a list with 4 dataframes - one for each line in the transaction block.
  tokens <-
    # Apply lines' patterns to all lines in the file
    map(block$patterns,
        # Extracts tokens from each line matching a pattern
        ~ lines[all_transaction_lines] %>% str_match(pattern = .x))
  
  tokens <- tokens %>%
    # The first column in each dataframe contains the entire matched line and we don't need it
    map(~ .[, -1]) %>%
    # Remove rows that contain only NAs
    map(~ na.omit(.x))
  
  # Each row in the dataframes represents part of a transaction.
  # Glue rows in each dataframe together, so that the resulting dataframe
  # has one row per transaction.
  transactions <- map_dfc(tokens, as_tibble)
  
  if ( !is_empty(transactions) ) {
    # Add column names
    colnames(transactions) <- block$token_names
    
    # Augment transactions
    transactions <- block$augment(transactions)
    
    # Add the numbers of the first lines for each transaction.
    # It will be used for sorting the transactions,
    # so that they appear in the same order as in the confrimation.
    transactions <- transactions %>% mutate(sequence = first_transaction_lines)
  }
  
  return(transactions)
}

#
# Parse total lines
#
parse_total_lines <- function(lines, block) {
  # Apply total_line pattern to all lines
  total <- lines %>%
    # Find lines matching the pattern
    str_subset(pattern = block$patterns) %>%
    # Extracts tokens from the lines matching the pattern
    str_match(pattern = block$patterns)
  
  # Drop last column, which contains the entire matched line
  total <- total[,-1]
  
  # Set columns' names
  colnames(total) <- block$token_names
  
  # Convert to tibble
  total <- as_tibble(total)
  
  total <- block$augment(total)
  
  return(total)
}