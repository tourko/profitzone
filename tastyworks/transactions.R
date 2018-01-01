source("confirmation.R")

# Location of the confirmations
path <- file.path("~", "Documents", "Options Trading", "Tastyworks", "Confirmations")

extract_transactions <- function(path) {
  # Confirmation file name example: "2017-08-30-5WT38480-confirmation.pdf"
  confirmation_pattern <- START %R% YMD %R% "-" %R%
    repeated(ALNUM, 8) %R% "-confirmation" %R%
    DOT %R% "pdf" %R% END
  
  # Get a list of confirmation files
  files <- list.files(path = path.expand(path),
                      pattern = confirmation_pattern,
                      full.names = TRUE)
  
  files %>%
    # Read each confirmtaion.
    # The result is a list (1) with as many elements as there are files.
    # Each element in list (1) is yet another a list (2).
    # Each element in list (2) is a tibble that has transactions extracted by a transaction block.
    # List (2) has as many elements as there are transaction blcks.
    #
    # Example:
    # List of 70
    # $ :List of 4
    # ..$ option   :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	5 obs. of  19 variables:
    # .....
    # ..$ stock    :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	0 obs. of  0 variables
    # ..$ assigned :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	0 obs. of  0 variables
    # ..$ exercised:Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	0 obs. of  0 variables
    # $ :List of 4
    # ..$ option   :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	4 obs. of  19 variables:
    # .....
    # ..$ stock    :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	0 obs. of  0 variables
    # ..$ assigned :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	0 obs. of  0 variables
    # ..$ exercised:Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	0 obs. of  0 variables 
    # .....
    map(read_confirmation) %>%
    # Turn the list "inside-out", so that it will look like the following:
    # List of 4:
    # $ option :List of 70 
    #  ..$ :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	0 obs. of  0 variables
    #  ..$ :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	0 obs. of  0 variables
    #  ......
    # $ stock :List of 70
    #  ..$ :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	0 obs. of  0 variables
    #  ..$ :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	0 obs. of  0 variables
    #  ......
    # $ assigned :List of 70
    #  ..$ :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	0 obs. of  0 variables
    #  ..$ :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	0 obs. of  0 variables
    #  ......
    # $ exercised :List of 70
    #  ..$ :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	0 obs. of  0 variables
    #  ..$ :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	0 obs. of  0 variables
    #  ......
    transpose(.names = names(transaction_blocks)) %>% 
    # Bind the rows in each transaction block together:
    # $option
    # # A tibble: 406 x 19
    #   transaction_id trade_date      reason open_close buy_sell symbol instrument ...
    #            <chr>     <date>      <fctr>     <fctr>  <fctr>   <chr>     <fctr> ...
    # 1         T00009 2017-08-30 UNSOLICITED       OPEN     SELL    XOP     OPTION ...
    # 2         T00013 2017-08-30 UNSOLICITED       OPEN      BUY     FB     OPTION ...
    # ...
    #
    # $stock
    # # A tibble: 2 x 16
    #   transaction_id trade_date      reason open_close buy_sell symbol instrument ...
    #            <chr>     <date>      <fctr>     <fctr>   <fctr>  <chr>     <fctr> ...
    # 1         T00013 2017-12-08 UNSOLICITED      CLOSE     SELL    UAL      STOCK ...
    # 2         T00046 2017-12-15 UNSOLICITED       OPEN      BUY    UNG      STOCK ...
    # ...
    # 
    # $assigned
    # # A tibble: 2 x 18
    #   transaction_id trade_date   reason open_close buy_sell symbol instrument ...
    #            <chr>     <date>   <fctr>     <fctr>   <fctr>  <chr>     <fctr> ...
    # 1         T00009 2017-11-09 ASSIGNED       OPEN      BUY    UAL      STOCK ...
    # 2         T00025 2017-11-21 ASSIGNED       OPEN      BUY     GE      STOCK ...
    # ...
    # 
    # $exercised
    # # A tibble: 1 x 19
    #     transaction_id trade_date    reason open_close buy_sell symbol instrument ...
    #              <chr>     <date>    <fctr>     <fctr>   <fctr>  <chr>     <fctr> ...
    #   1         T00017 2017-11-15 EXERCISED      CLOSE     SELL    VIX     OPTION ...
    # ...
    map(~ .x %>% map_dfr(~ .x))
}

process_assigned <- function(transactions) {
  assigned_stocks <- transactions$assigned
  
  # For each assigned stock create a "dummy" option transaction
  assigned_options <- transactions$option %>% 
    filter(cusip %in% transactions$assigned$assigned_cusip) %>% 
    mutate(transaction_id  = assigned_stocks$transaction_id,
           trade_date      = assigned_stocks$trade_date,
           reason          = assigned_stocks$reason,
           open_close      = factor("CLOSE", levels = c("OPEN", "CLOSE")),
           buy_sell        = factor(if_else(buy_sell == "BUY", "SELL", "BUY"), levels = c("BUY", "SELL")),
           quantity        = assigned_stocks$assigned_qty,
           price           = 0,
           commission      = 0,
           transaction_fee = 0,
           additional_fee  = 0,
           net_amount      = 0,
           tag_number      = assigned_stocks$tag_number)
  
  # Append assigned options to the option transactions
  transactions$option <- transactions$option %>% 
    bind_rows(assigned_options)
  
  # Drop "assigned_qty" and "assigned_cusip" from assigned stock transactions
  transactions$assigned <- transactions$assigned %>% 
    select(-assigned_qty, -assigned_cusip)
  
  return(transactions)
}

transactions <- extract_transactions(path) %>% 
  process_assigned() %>%
  # Merge transaction from all transaction blocks into one tibble
  bind_rows() %>%
  # Arrange transactions in the same order as they appear in the confirmations
  arrange(trade_date, transaction_id) %>% 
  select(-transaction_id)
  
# Save transactions to a file
saveRDS(transactions, file = "transactions.rds")
  

