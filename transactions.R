library(rebus)
library(dplyr)
library(tastyworks)

# Location of the confirmations
path <- file.path("~", "Documents", "Options Trading", "Tastyworks", "Confirmations")

# Confirmation file name example: "2017-08-30-5WT38480-confirmation.pdf"
filename_pattern <- START %R% YMD %R% "-" %R%
  repeated(ALNUM, 8) %R% "-confirmation" %R%
  DOT %R% "pdf" %R% END

# Get a list of confirmation files
files <- list.files(path = path.expand(path),
                    pattern = filename_pattern,
                    full.names = TRUE)

transactions <- files %>% tastyworks::read_confirmations(files)
  
# Save transactions to a file
saveRDS(transactions, file = "transactions.rds")
  

