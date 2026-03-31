# load necessary libraries
library(readxl) # for reading in Excel files
# all the following six libraries can be loaded either in a single line using
# library(tidyverse)
# or separately as below
library(lubridate) # for handling dates
library(stringr) # for handling strings (text)
library(readr) # for reading in csv files
library(dplyr) # for manipulating data
library(tidyr) # for tidying data
library(ggplot2) # for making plots

# base folder
base_loc <- "data/2022_summer/"

# input folder; directory containing your raw data
rawdata_loc <- paste0(base_loc, "1_raw_csv/")

# output folder; directory where your cropped data will be (or currently is) stored
cropped_loc <- paste0(base_loc, "2_cropped_csv/")

# also an output folder; directory where plots of cropped and raw data will be stored
croppedplots_loc <- paste0(base_loc, "3_cropped_plots/")

# name of your LDRTimes file that has the lookup table with deployment/retrieval dates
# note: the file path below starts from rawdata_loc folder
ldrtimes_fn <- "../../ldrtimes_2022.xlsx"

# -------------------------------------------
# editing mostly takes place above this line
# -------------------------------------------

# rawdata_loc must already exist
if(!dir.exists(rawdata_loc)){
  stop(paste(rawdata_loc, "does not exist"))
}
# cropped_loc and croppedplots_loc can be created if they don't exist
if(!dir.exists(cropped_loc)){
  dir.create(cropped_loc)
}
if(!dir.exists(croppedplots_loc)){
  dir.create(croppedplots_loc)
}

# check that R can find your raw data files
#  - list.files gets all temperature data filenames
#  - note: * is called a glob, short for global
#  - IMPORTANT: filenames must be in the format
#    sitename_medium_deployseason_deployyear.csv
#            e.g. NolanLower_air_sum_23.csv
csv_files = list.files(path = rawdata_loc, pattern = '*csv')
# csv_files is a list of all filenames to be cropped
# we haven't read in the data yet
# make sure this lists all the raw files you want to crop:
csv_files

# read in LDR file and take a look at it
# - note: this assumes the filepath ldrtimes_fn starts from the folder indicated by rawdata_loc
ldrtimes = readxl::read_xlsx(paste0(rawdata_loc, ldrtimes_fn))

# once you're sure that file paths are working and your ldrtimes looks right,
# crop the files!

i = 0
for(this_file in csv_files){
  i = i + 1
  # this_file = csv_files[7] # uncomment to troubleshoot within loop
  cat(
    paste0("Reading file ", i, " of ", length(csv_files), ": ", this_file),
    fill = TRUE
  )

  # extract metadata from the filename to include in the dataset in new columns later
  filename.parts = stringr::str_split_1(this_file, '[_.]')
  csv_site = filename.parts[1]
  csv_medium = filename.parts[2]
  csv_season = filename.parts[3]
  csv_year = filename.parts[4]

  # convert the character-format datetime to an R POSIXct object.
  # ymd_hm is the format the character string is in initially; it tells R
  # how to read and interpret the character string.
  # Sometimes R reads in the datetime format as mdy_hms and sometimes mdy_hm.
  # This tryCatch handles csv files with either hh:mm:ss or hh:mm format.
  this_data =  tryCatch(
    {
      readr::read_csv(
        paste0(rawdata_loc, this_file),
        skip = 2, # skip the first two lines of the file
        col_select = 1:3, # read only the first three columns of data
        col_names = FALSE, # don't try to name columns from a row of the file
        show_col_types = FALSE
      ) %>% # suppresses print message
        dplyr::rename(
          "rowID" = X1,
          "datetime" = X2,
          "temperature" = X3
        ) %>%
        dplyr::mutate(datetime = lubridate::mdy_hms(datetime)) #for datetime in hh:mm:ss
    },
    warning = function(cond) { #if datetime isn't in hh:mm:ss, will now try hh:mm format
      readr::read_csv(
        paste0(rawdata_loc, this_file),
        skip = 2, # skip the first two lines of the file
        col_select = 1:3, # read only the first three columns of data
        col_names = FALSE, # don't try to name columns from a row of the file
        show_col_types = FALSE
      ) %>% # suppresses print message
        dplyr::rename(
          "rowID" = X1,
          "datetime" = X2,
          "temperature" = X3
        ) %>%
        dplyr::mutate(datetime = lubridate::mdy_hm(datetime)) #for datetime in hh:mm
    }
  )

  # crop the data
  deploy_retrieval = ldrtimes %>%
    # select the row(s) of ldrtimes that match this datafile
    # should be exactly one row, but if there are no rows or multiple rows that
    # match, this step will pull that many rows
    dplyr::filter(
      site == csv_site,
      deploy_season == csv_season,
      deploy_year == csv_year,
      media == csv_medium
    ) %>%
    # keep just the deploy_time and retrieval_time variables/columns
    dplyr::select(deploy_time, retrieval_time)

  if(nrow(deploy_retrieval) == 0){
    stop("no rows of ldrtimes matched this csv file.")
  }
  if(nrow(deploy_retrieval) > 1){
    stop("multiple rows of ldrtimes matched this csv file.")
  }

  deploy = deploy_retrieval$deploy_time
  retrieval = deploy_retrieval$retrieval_time

  if(retrieval > deploy) {
    cropped_data = dplyr::filter(
      this_data,
      datetime > deploy,
      datetime < retrieval
    )

  } # if(retrieval > deploy)

  # write cropped csv files to cropped folder
  readr::write_csv(
    cropped_data,
    file=paste0(
      cropped_loc,
      stringr::str_split_i(this_file, "[.]", 1),
      "_cropped.csv"
    )
  )

  #Create a dataframe of the raw and cropped data
  cropped_vs_raw <- dplyr::left_join(this_data, cropped_data, by=c("rowID", "datetime")) %>%
    dplyr::rename(
      Raw = temperature.x,
      Cropped = temperature.y
    ) |>
    dplyr::mutate(Cropped = ifelse(is.na(Cropped), Raw, NA)) %>%
    #create new column of data type (raw or cropped for plotting in ggplot)
    tidyr::pivot_longer(
      cols = Raw:Cropped,
      names_to="type", values_to="temp"
    ) %>%
    dplyr::mutate(type = factor(type, levels = c("Raw", "Cropped")))

  cropped_plot <- ggplot2::ggplot(
    cropped_vs_raw,
    ggplot2::aes(x = datetime, y = temp, color = type)
  ) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::labs(
      title = paste0(" Raw versus Cropped data"),
      x = "Date",
      y = "Temperature (C)"
    )+
    ggplot2::theme(axis.text = ggplot2::element_text(colour = "black", size = (12)))

  ggplot2::ggsave(
    paste0(croppedplots_loc, csv_site, "_", csv_medium, "_rawvscroppeddata.png"),
    cropped_plot,
    width = 11,
    height = 8.5,
    units = "in"
  )

}; cat("Done.", fill = TRUE)


