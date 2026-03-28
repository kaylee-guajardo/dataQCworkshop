# tips for developing for loops:
# - first, write code for a single iteration
# - then, think about which parts need to be updated for each iteration
# - then, turn it into a for loop


# install.packages("readr")
library(readr)
library(tidyverse)

list_of_files = list.files(path = "data/2022_summer/1_raw_csv", pattern = "*.csv")
list_of_files

# here, we would need to create an empty dataframe
# - this is not in the for loop because we only want to do it once,
# before we read in any files

for(this_file in list_of_files){
  # this_file = "data/2022_summer/1_raw_csv/CedarLower_water_sum_22.csv" # this is useful for troubleshooting!!!

  # split the filename and extract the sitename
  sitename = "thissite"

  water_data <- read_csv(
    paste0("data/2022_summer/1_raw_csv/", this_file),
    skip = 2,
    col_names = FALSE
  ) |>
    rename(rowid = X1, datetime = X2, watertemp = X3) |>
    mutate(sitename = sitename)

  # here we would add code to combine files' data into one dataframe

}

