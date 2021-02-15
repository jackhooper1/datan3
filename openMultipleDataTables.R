# How to open multiple Understanding Society data sets.
# 15 Feb 2021
# AB

library(tidyverse)
library(vroom)

# create a vector with the file names and paths
files <- dir(
  # Select the folder where the files are stored.
  "UKDA-6614-tab/tab",
  # Tell R which pattern you want present in the files it will display.
  pattern = "indresp",
  # We want this process to repeat through the entire folder.
  recursive = TRUE,
  # And finally want R to show us the entire file path, rather than just
  # the names of the individual files.
  full.names = TRUE)

files

# Select only files from the UKHLS.
files <- files[stringr::str_detect(files, "ukhls")]
files

# Reorder the file paths. 
files <- files[c(1,3:10,2)]
files

# create a vector of variable names
vars <- c("sex_dv", "age_dv", "fimnnet_dv")

for (i in 1:10) {
  # Create a vector of the variables with the correct prefix.
  varsToSelect <- paste(letters[i], vars, sep = "_")
  # Add pidp to this vector (no prefix for pidp)
  varsToSelect <- c("pidp", varsToSelect)
  # Now read the data. 
  data <- vroom(files[i], col_select = varsToSelect)
  if (i == 1) {
    all10 <- data  
  }
  else {
    all10 <- full_join(all10, data, by = "pidp")
  }
  # Now we can remove data to free up the memory.
  rm(data)
} 

all10
