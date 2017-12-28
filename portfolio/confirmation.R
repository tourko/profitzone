library(pdftools)
source("parsers.R")

process_page <- function(page) {
  page %>%
    # Split a page into lines
    str_split(pattern = NEWLINE, simplify = TRUE) %>%
    # Trim white spaces around lines
    str_trim() %>%
    # Replace multiple white spaces with a single space
    str_replace_all(pattern = one_or_more(SPC), replacement = " ") %>%
    # Drop empty lines
    str_subset(pattern = negated_char_class(START %R% zero_or_more(SPC) %R% END))
}

process_document <- function(doc) {
  lines <- doc %>%
    # Drop the first page
    `[`(-1) %>%
    # Process each page
    map(~ .x %>% process_page()) %>% 
    # Merge lines from all pages into one vector 
    unlist()
}

read_confirmation <- function(f) {
  message(paste("Proccessing file", f))
  
  # Read the document
  doc <- pdf_text(f)
  
  # Extract lines from the document
  lines <- process_document(doc)
  
  # Extract transactions from the lines
  transactions <- transaction_blocks %>% map_dfr(~ parse_transaction_lines(lines, .x))

  # Sort by sequence, so that transaction appear
  # in the same order as in the confrimation.
  transactions <- transactions %>%
    arrange(sequence) %>% 
    # Drop sequence column
    select(-sequence)
  
  # Calculate transactions total
  transactions_total <- transactions %>% 
    summarise(
      quantity = sum(if_else(buy_sell == "BUY", quantity, -quantity)),
      net_amount = sum(if_else(buy_sell == "BUY", -net_amount, net_amount))) %>%
    # Summing introduces a rounding error, so we have to round the result
    # for the comparisson to work correctly
    mutate(net_amount = round(net_amount, 2))
  
  # Extract confrimation totals from the lines
  total <- parse_total_lines(lines, total_block)
  
  # Calculate confirmation totals
  confirmation_total <- total %>%
    summarise(quantity = sum(quantity), net_amount = sum(net_amount)) %>%
    # Summing introduces a rounding error, so we have to round the result
    # for the comparisson to work correctly
    mutate(net_amount = round(net_amount, 2))
  
  # Compare confirmation totals with the transactions' totals
  if ( !identical(transactions_total, confirmation_total) ) {
    print(lines)
    print(transactions)
    stop("Total of the transactions does not match total of the confirmation.")
  }

  # Return the transactions
  return(transactions)
}
