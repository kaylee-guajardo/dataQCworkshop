# Script objective: crop data to just when detectors were deployed (ie after
# deployment time, prior to retrieval time). Will use these data to create QC
# plots in subsequent script, 2_QCplots.R. Output of this code is to 2_cropped_csv
# folder and 3_cropped_plots folder inside defined base_loc.

# load necessary libraries
library(readxl)
# all the following six libraries can be loaded either in a single line using
# library(tidyverse)
# or separately as below
library(lubridate) # for handling dates
library(stringr)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Set up folder names / structure to pull files from
# base folder
base_loc <- "data/2022_summer/" # 2022 summer data folder to pull files from

# input folder; directory containing your raw data
rawdata_loc <- paste0(base_loc, "1_raw_csv/") # Folder within 2022_summer folder
# inside data folder to pull from which will contain the raw data

# output folder; directory where your cropped data will be (or currently is) stored
cropped_loc <- paste0(base_loc, "2_cropped_csv/")

# also an output folder; directory where plots of cropped and raw data will be stored
croppedplots_loc <- paste0(base_loc, "3_cropped_plots/")

# name of your LDRTimes file that has the lookup table with deployment/retrieval dates
# note: the line below assumes ldrtimes_fn is in rawdata_loc folder
ldrtimes_fn <- "ldrtimes_2022.xlsx"

# ASK JESS....
# Tried to run the code loop at the bottom but ran into Error: Cannot open file
# for writing: * 'data/2022_summer/2_cropped_csv/CedarLower_water_sum_22_cropped.csv'
# I believe the issues is that the folder 2_cropped_csv does not initially exist within
# the 2022_summer folder - need to create it first, which I do next to address this issue

# Create cropped csv folders to store cropped data in
# Create 2_cropped_csv folder in data/2022_summer
dir.create(cropped_loc,
           recursive = TRUE, # Handles missing parent folders (if parent folder is missing,
           # creates parent folders and new desired folder)
           showWarnings = FALSE) # Don't show warning if file already exists
# Create 3_cropped_plots folder in data/2022_summer
dir.create(croppedplots_loc,
           recursive = TRUE,
           showWarnings = FALSE)

# -------------------------------------------
# editing mostly takes place above this line
# ? When? Why? Like to adjust folder names? Placeholder for data scientists that
# will use / adopt the code as needed?
# -------------------------------------------

# check that R can find your raw data files
# Get all temperature data filenames
# note: * is called a glob, short for global
# IMPORTANT: filenames must be in the format sitename_medium_deployseason_deployyear.csv
#            e.g. NolanLower_air_sum_23.csv
csv_files = list.files(path = rawdata_loc, pattern = '*csv') # Create list of all files in raw data folder
# this is now a list of all filenames; we haven't read in the data yet, but
# make sure this lists all the raw files you want to crop
csv_files # Check to make sure contains all files desired to be processed

# RUN THIS BY JESS ->>>>>>>>>>>
# read in LDR file and take a look at it
# note: this assumes the LDR file is in the folder indicated by rawdata_loc
ldrtimes = readxl::read_xlsx(paste0(rawdata_loc, ldrtimes_fn)) # where ldrtimes_fn
# is the object name for the LDRTimes file, as defined above edit code above placholder chunk
# NOTE: This did not work, fixing issue (ldrtime_2022.xlsx file located just in
# data folder, not rawdata location file path)
ldrtimes_loc <- "data/"
ldrtimes = readxl::read_xlsx(paste0(ldrtimes_loc, ldrtimes_fn))

# once you're sure that file paths are working and your ldrtimes looks right,
# crop the files!

# Initiate iteration
i = 0
# Initiate for loop to crop raw data files...
for(this.file in csv_files){ # For a file in the csv_files list...
  i = i + 1 # Used to keep track of number of files being processed, can help
  # identify which file has issue if necessary
  #this.file = csv_files[1] # uncomment to troubleshoot within loop
  # Add commentary as cropping takes place so one knows where they are at
  cat(paste0("Reading file ", i, " of ", length(csv_files), ": ", this.file), fill = TRUE)
  # Message will look something like, "Reading file 3 of 5: file_name.csv"

  # extract metadata from the filename
  filename.parts = stringr::str_split_1(this.file, '[_.]') # Follows req'd
  # structure defined previously: site_media_season_year
  csv.site = filename.parts[1]
  csv.media = filename.parts[2]
  csv.season = filename.parts[3]
  csv.year = filename.parts[4]

  # convert the character-format datetime to an R POSIXct object
  # ymd_hm is the format the character string is in initially; it tells R
  # how to read and interpret the character string
  # Guessing ymd_hm stands for year month date hours minutes and is the format
  # the data need to be in
  # sometimes R reads in the datetime format as mdy_hms and sometimes mdy_hm.
  # This tryCatch handles either hh:mm:ss or hh:mm format in csv files
  # Recall: a tryCatch tried one option, which if that fails, tries another option, etc
  this.data =  tryCatch( # First attempt to convert the character-format datetime
    # to an R POSIXct object assuming hh:mm:ss format
    {
      readr::read_csv(paste0(rawdata_loc, this.file),
                      skip = 2, # skip the first two lines of the file
                      col_select = 1:3, # read only the first three columns of data
                      col_names = FALSE, # don't try to name columns from a row of the file
                      show_col_types = FALSE) %>% # suppresses print message
        dplyr::rename("row.num" = X1,
                      "datetime" = X2,
                      "temperature" = X3) %>%
        dplyr::mutate(datetime = lubridate::mdy_hms(datetime)) #for datetime in hh:mm:ss
    },
    # Second attempt to convert the character-format datetime to an R POSIXct object, now using hh:mm format
    warning = function(cond) { #if datetime isn't in hh:mm:ss, will now try hh:mm format
      readr::read_csv(paste0(rawdata_loc, this.file),
                      skip = 2, # skip the first two lines of the file
                      col_select = 1:3, # read only the first three columns of data
                      col_names = FALSE, # don't try to name columns from a row of the file
                      show_col_types = FALSE) %>% # suppresses print message
        dplyr::rename("row.num" = X1, # Rename columns..
                      "datetime" = X2,
                      "temperature" = X3) %>%
        dplyr::mutate(datetime = lubridate::mdy_hm(datetime)) #for datetime in hh:mm
    }
  )

  # crop the data (still in same for loop, so automated after date-time is processed)
  deploy.retrieval = ldrtimes %>%
    # select the row(s) of ldrtimes that match this datafile
    # should be exactly one row, but if there are no rows or multiple rows that
    # match, this step will pull that many rows
    dplyr::filter(site == csv.site, deploy_season == csv.season,
                  deploy_year == csv.year, media == csv.media) %>%
    # keep just the deploy_time and retrieval_time variables/columns
    dplyr::select(deploy_time, retrieval_time)

  # Stop and give warning message if: 1) no rows of ldrtimes matched this csv file, or,
  if(nrow(deploy.retrieval) == 0){
    stop("no rows of ldrtimes matched this csv file.")
  } # 2) more than one row of ldrtimes matched this csv file
  if(nrow(deploy.retrieval) > 1){
    stop("multiple rows of ldrtimes matched this csv file.")
  }

  deploy = deploy.retrieval$deploy_time # Creating new objects: deploy as the deploy_time column from
  # filtered / cropped data
  retrieval = deploy.retrieval$retrieval_time # and retrieval from filtered / cropped data

  # If the is more retrieval time is greater than deployed time (which it should be)...
  if(retrieval > deploy) {
    cropped.data = dplyr::filter(this.data, # Pull the data for times that are
                                 # within the deployment window
                                 datetime > deploy, # That is, time past when it was deployed, and
                                 datetime < retrieval) # time prior to when it was retrieved

  } # if(retrieval > deploy), (which it should be),

  # write cropped csv files to cropped folder using cropped data
  readr::write_csv(cropped.data,
                   file=paste0(cropped_loc,
                               stringr::str_split_i(this.file, "[.]", 1), "_cropped.csv")) # Same name as original file,
  # but with a "_cropped.csv" suffix

  # Create a dataframe of the raw and cropped data
  cropvraw <- dplyr::left_join(this.data, cropped.data, by=c("row.num", "datetime")) %>% # Cropped data is joined to original raw df
    dplyr::rename(raw.temp = temperature.x, # Rename columns, such that raw.temp holds the raw temperature
                  cropped.temp = temperature.y) %>%# Cropped temp. has cropped temperature
    #create new column of data type (raw or cropped for plotting in ggplot)
    tidyr::pivot_longer(cols = raw.temp:cropped.temp, # Pivot longer style
                        names_to="type", values_to="temp")
  # Plot cropped data
  cropvraw.plot <- ggplot2::ggplot(cropvraw,
                                   ggplot2::aes(x = datetime,
                                                y = temp,
                                                color = type)) +
    ggplot2::geom_line(na.rm=TRUE) + # Line through points
    ggplot2::geom_point(na.rm=TRUE) + # Scatterplot
    ggplot2::labs(title = paste0(" Raw versus Cropped data"),
                  x = "Date", y = "Temperature (C)")+
    ggplot2::theme(axis.text = ggplot2::element_text(colour = "black", size = (12)))

  ggplot2::ggsave(paste0(croppedplots_loc, csv.site, "_", # Save created plot as image file
                         csv.media, "_rawvscroppeddata.png"),
                  cropvraw.plot,
                  width = 11, height = 8.5, units = "in")

}; cat("Done.", fill = TRUE) # Print message "Done." when complete

# Great, now it is working! - Kaylee

# ASK JESS...
# The cropped data plots seems a bit hard to read, as the cropped temp is fully
# opaque so it is hard to see the raw temp data underneathe. Does that matter?
# Do we want to introduce some transparency to the cropped temp plotted data?
