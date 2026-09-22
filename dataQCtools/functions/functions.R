library(tidyverse)

# FUNCTION 1: Prep data (single file)
prep_raw_data <- function(csv_file, # A single raw data csv file - defined as object above
                          ldrtimes # LDR dataset
){

  just_the_name <- basename(csv_file)
  # extract metadata from the filename
  filename.parts = stringr::str_split_1(just_the_name, '[_.]') # Follows req'd
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
      readr::read_csv(csv_file,
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
      readr::read_csv(csv_file,
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

  return(this.data)

}

# FUNCTION 2: Crop raw data ----
crop_raw_data <- function(this.data, # Output of previous function (prepared data for cropping)
                          csv_file, # A single raw data csv file - defined as object above
                          ldrtimes # LDR dataset
){

  just_the_name <- basename(csv_file)
  # extract metadata from the filename
  filename.parts = stringr::str_split_1(just_the_name, '[_.]') # Follows req'd
  # structure defined previously: site_media_season_year
  csv.site = filename.parts[1]
  csv.media = filename.parts[2]
  csv.season = filename.parts[3]
  csv.year = filename.parts[4]

  # crop the data (still in same for loop, so automated after date-time is processed)
  deploy.retrieval = ldrtimes %>%
    # select the row(s) of ldrtimes that match this datafile
    # should be exactly one row, but if there are no rows or multiple rows that
    # match, this step will pull that many rows
    dplyr::filter(site == csv.site, deploy_season == csv.season,
                  deploy_year == csv.year, media == csv.media) %>%
    # keep just the deploy_time and retrieval_time variables/columns
    dplyr::select(deploy_time, retrieval_time)

  ##############################
  # Again, maybe should comment out for now since will likely need to integrate messages
  # in when setting up RShiny app?
  # Stop and give warning message if: 1) no rows of ldrtimes matched this csv file, or,
  if(nrow(deploy.retrieval) == 0){
    stop("no rows of ldrtimes matched this csv file.")
  } # 2) more than one row of ldrtimes matched this csv file
  if(nrow(deploy.retrieval) > 1){
    stop("multiple rows of ldrtimes matched this csv file.")
  }
  #############################

  deploy = deploy.retrieval$deploy_time # Creating new objects: deploy as the deploy_time column from
  # filtered / cropped data
  retrieval = deploy.retrieval$retrieval_time # and retrieval from filtered / cropped data

  # If the is more retrieval time is greater than deployed time (which it should be)...
  if(retrieval > deploy) {
    cropped.data = dplyr::filter(this.data, # Pull the data for times that are
                                 # within the deployment window
                                 datetime > deploy, # That is, time past when it was deployed, and
                                 datetime < retrieval) # time prior to when it was retrieved

  }

  return(cropped.data)

}

# FUNCTION 3: Plot cropped data -----
plot_cropped_data <- function(cropped.data, # Single cropped data object
                              this.data, # Output of previous
                              # croppedplots_loc, # Cropped plots location filepath - commenting out for now
                              ldrtimes # LDR dataset
){

  # Create a dataframe of the raw and cropped data
  cropvraw <- dplyr::left_join(this.data, cropped.data, by=c("row.num", "datetime")) %>% # Cropped data is joined to original raw df
    dplyr::rename(raw.temp = temperature.x, # Rename columns, such that raw.temp holds the raw temperature
                  cropped.temp = temperature.y) %>% # Cropped temp. has cropped temperature
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

  #######
  # Commenting out for now - ask Jess if we want to give option to download cropped plots images in RShiny
  #ggplot2::ggsave(paste0(croppedplots_loc, csv.site, "_", # Save created plot as image file
  #csv.media, "_rawvscroppeddata.png"),
  #cropvraw.plot,
  #width = 11, height = 8.5, units = "in")
  ######

  return(cropvraw.plot)

}

# FUNCTION 4: Wrapper / putting it all together ----
script1_function <- function(csv_files, # Needed for forloop - a list of csv files
                             rawdata_loc, # Needed for function 1: prep_raw_data
                             ldrtimes, # Needed for all called functions
                             cropped_loc, # Where to save cropped data files
                             croppedplots_loc # Where to save cropped data plot images
){
  # Initiate iteration
  i <- 0

  # Initiate for loop to crop raw data files...
  for(csv_file in csv_files){ # For a file in the csv_files list...

    i <- i + 1
    csv_file <- csv_files[i]

    # Grab naming conventions of csv file for use in custom function 3
    # extract metadata from the filename
    filename.parts = stringr::str_split_1(csv_file, '[_.]') # Follows req'd
    # structure defined previously: site_media_season_year
    csv.site = filename.parts[1]
    csv.media = filename.parts[2]

    # Run custom functions...
    # Function 1
    this.data <- prep_raw_data(rawdata_loc, csv_file, ldrtimes)

    # Function 2
    cropped.data <- crop_raw_data(this.data, csv_file, ldrtimes)
    # Write cropped csv files to cropped folder using cropped data
    readr::write_csv(cropped.data,
                     file=paste0(cropped_loc,
                                 stringr::str_split_i(csv_file, "[.]", 1), "_cropped.csv"))
    # Same name as original file,but with a "_cropped.csv" suffix

    # Function 3
    cropped.plot <- plot_cropped_data(cropped.data, this.data, ldrtimes)
    # Save cropped plot as image file
    ggplot2::ggsave(paste0(croppedplots_loc, csv.site, "_", # Save created plot as image file
                           csv.media, "_rawvscroppeddata.png"),
                    cropped.plot,
                    width = 11, height = 8.5, units = "in")

  }

  return(cat("Done!"))

}
