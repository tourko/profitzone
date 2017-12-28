source("confirmation.R")

# Location of the confirmations
path <- file.path("~", "Documents", "Options Trading", "Tastyworks", "Confirmations")

# Confirmation file name example: "2017-08-30-5WT38480-confirmation.pdf"
confirmation_pattern <- START %R% YMD %R% "-" %R%
  repeated(ALNUM, 8) %R% "-confirmation" %R%
  DOT %R% "pdf" %R% END

# Get a list of confirmation files
files <- list.files(path = path.expand(path),
                    pattern = confirmation_pattern,
                    full.names = TRUE)

# Read each confirmtaion and merge rows in a single dataframe
transactions <- map_dfr(files, read_confirmation)

# Save transactions to a file
saveRDS(transactions, file = "transactions.rds")
  

