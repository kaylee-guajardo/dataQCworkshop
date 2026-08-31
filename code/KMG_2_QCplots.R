# Script objective: produce plots of weather, air ranges, as well as max differences,
# save plots to images; create html widgets of interactive plots. Outputs located
# in created folder 4_qc_plots in defined base_loc.

# load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(lubridate)


# base folder
base_loc <- "data/2022_summer/" # Base is folder inside folder, data/2022_summer

# directory containing cropped data to be plotted
cropped_loc <- paste0(base_loc, "2_cropped_csv/")
# directory where QC plots will be stored
qc_plots_loc <- paste0(base_loc, "4_qc_plots/")

# -------------------------------------------
# editing mostly takes place above this line
# -------------------------------------------

# get a list of all the cropped files
filenames <- list.files(path = cropped_loc, pattern=".csv")
# this is now a list of all filenames; we haven't read in the data yet, but
# make sure this lists all the cropped files you want to plot
filenames

# Loop to read all the data files and combine them into one datafile for easier plotting
Combined <-NULL # Create new data frame to hold combined data

cat("Reading in cropped data files...", fill = TRUE) # Message of "reading in cropped data files.." while running
i = 0 # Initiate iteration
for (selection in filenames) { # For a file in filenames list...
  i = i+1 # Establish iteration number
  #selection = filenames[1] # for troubleshooting the for-loop
  # Message to keep researcher updated in progress of creating the QC plots
  cat(paste0("Reading file ", i, " of ", length(filenames), ": ", selection), fill = TRUE)
  # ex: Reading file 3 of 6: file_name.csv

  # split the filename string everywhere there is an underscore or period
  # so that we can get the following metadata from it:
  #  site name, deployment season and year, and media (air/water)
  info_from_filename = unlist(strsplit(selection, split="[_.]"))[1:4]
  sitename = info_from_filename[1]
  media = info_from_filename[2] # air or water; assumes this info is the third chunk after splitting the filename
  deploy_season = info_from_filename[3]
  if(deploy_season != "sum" & deploy_season != "fall"){ # Stop running if media
    # in third chunk does not say sum or fall - req'd to run as intended
    stop("filename does not say sum or fall.")
  }
  # Recall: deployment year at end of file name is a number like "23" so add 2000 to get full deployment year of 2023 for example
  deploy_year = 2000 + as.integer(info_from_filename[4]) # or we can format as character

  oneread <- read.csv( # Read selected csv file from cropped data folder
    file = paste0(cropped_loc, selection), as.is=T, skip=1, fill=T, header=F
  ) %>% ## Reads the selected datafile.
    dplyr::select(1:3) %>% #select the first 3 columns (remove air temp from Hoh River sites)
    # add the metadata as variables/columns
    dplyr::mutate(sitename = sitename, # Since these are objects defined from before, adds them as new variables in df
                  media = media,
                  deploy_season = deploy_season,
                  deploy_year = deploy_year)
  Combined <- dplyr::bind_rows(Combined, oneread)  ## Adds the datafile's data to the existing combined datafile.

} # filenames loop
cat("Done reading in cropped data.", fill = TRUE) # Message once complete: "Done reading in cropped data"

# Initiate next objective after reading in cropped data: compute temp differences
cat("Computing temperature differences...", fill = TRUE) # Message "Computing temp differences"
# only compute AWMaxDiff and AirRange if there is air data for at least one site
if("air" %in% unique(Combined$media)){
  Combined <- Combined %>%
    dplyr::rename(row = V1, datetime = V2, temp = V3) %>%  # Rename the first three variables that came from the csv file
    dplyr::mutate(datetime = lubridate::ymd_hms(datetime)) %>%  # Change format of datetime column
    dplyr::mutate(date = lubridate::date(datetime)) %>% # New variable that is just date from datetime
    dplyr::group_by(sitename, date, media)  %>%  # Compute daily max/min by site, date, and media (air/water)
    dplyr::summarize(dailymax = max(temp, na.rm = TRUE),
                     dailymin = min(temp, na.rm = TRUE)) %>%
    # Pivot dataframe to add air or water to column name; this is necessary to calculate stats
    # this puts air and water on the same rows so we can just subtract columns in the next line
    # this requires that at least one site has air temperature!
    tidyr::pivot_wider(names_from = media,
                       values_from = c(dailymin, dailymax)) %>%
    dplyr::mutate(AWMaxDiff = dailymax_air - dailymax_water, # Calculating temp diff
                  AirRange = dailymax_air - dailymin_air, # Calculating air diff
                  WaterRange = dailymax_water - dailymin_water) %>% # Calculating water diff
    #pivot longer for plotting purposes later; to plot on same graph, need "calc" column ("grouping variable")
    tidyr::pivot_longer(cols = dailymin_air:WaterRange, names_to = "calc", values_to = "value")
}else{
  Combined <- Combined %>%
    dplyr::rename(row = V1, datetime = V2, temp = V3) %>%  # Rename the first three variables that came from the csv file
    dplyr::mutate(datetime = lubridate::ymd_hms(datetime)) %>%  # change format of datetime column
    dplyr::mutate(date = lubridate::date(datetime)) %>% # extract just the date
    dplyr::group_by(sitename, date, media)  %>%  # compute daily max/min by site, date, and media (air/water)
    dplyr::summarize(dailymax = max(temp, na.rm = TRUE), # Daily max
                     dailymin = min(temp, na.rm = TRUE)) %>% # Daily min
    # pivot dataframe to add air or water to column name; this is necessary to calculate stats
    # this puts air and water on the same rows so we can just subtract columns in the next line
    # this requires that at least one site has air temperature!
    tidyr::pivot_wider(names_from = media,
                       values_from = c(dailymin, dailymax)) %>%
    dplyr::mutate(WaterRange = dailymax_water - dailymin_water) %>%
    #pivot longer for plotting purposes later; to plot on same graph, need "calc" column ("grouping variable")
    tidyr::pivot_longer(cols = dailymin_water:WaterRange, names_to = "calc", values_to = "value")
}

# set some plotting parameters
sites <- unique(Combined$sitename)
range.colors <- c(AirRange = "blue", WaterRange = "black") # Set color schema
maxdiff.colors <- c(dailymax_air = "blue", dailymax_water = "black", AWMaxDiff = "purple")

# loop through sites to plot all graphs for all sites
i = 0 # Initiate iteration num.
for(s in sites){
  i = i+1
  # Message of, for example, "Making QC plots for site 3 of 10: site_name"
  cat(paste0("Making QC plots for site ", i, " of ", length(sites), ": ", s), fill = TRUE)
  # s = sites[1] # uncomment if you want to troubleshoot this loop
  # filter the data to just this site to make range plot
  this.site.combined = dplyr::filter(Combined, sitename == s) # Filter data for only specific site in the loop
  rangeplot <- ggplot2::ggplot(this.site.combined %>% # Just look at calc values in Air and Water range
                                 dplyr::filter(calc %in% c("AirRange","WaterRange")),
                               ggplot2::aes(x = date, y = value, color = calc)) + # Date on x axis,
    # y is value, color is calc
    ggplot2::geom_line(na.rm=TRUE) + # Add line
    ggplot2::geom_point(na.rm=TRUE) + # Add scatterplot
    ggplot2::labs(title = paste0(s," Air and Water Temperature ranges"), # Axis, title labels
                  x = "Date", y = "Temperature (C)",
                  color = "Media") +
    ggplot2::scale_color_manual(values = range.colors) +
    ggplot2::geom_hline(yintercept = 3, linewidth = 0.3, color = "red")

  # Create image of range plot, save
  ggplot2::ggsave(paste0(qc_plots_loc, s, "_AirWaterTempRange.png"), rangeplot,
                  width = 11, height = 8.5, units = "in")

  # Creating plot for max difference in water, air ranges based on date, colored by media
  maxdiffplot <- ggplot2::ggplot(this.site.combined %>%
                                   dplyr::filter(calc %in% c("dailymax_air", "dailymax_water", "AWMaxDiff")),
                                 ggplot2::aes(x = date, y = value, color = calc)) +
    ggplot2::geom_line(na.rm=TRUE) +
    ggplot2::labs(title = paste0(s," Max Air and Water Temperature and Difference"),
                  x = "Date", y = "Temperature (C)",
                  color = "Media") +
    ggplot2::scale_color_manual(values = maxdiff.colors) +
    ggplot2::geom_hline(yintercept = 20, linewidth = 0.3, color = "red") # Horizontal line at temp = 20
  # Save image of plot for max difference
  ggplot2::ggsave(paste0(qc_plots_loc, s, "_MaxDiffAirWaterTemp.png"),
                  maxdiffplot, width = 11, height = 8.5, units = "in")

  # Make interactive plots (I think?)
  maxdifplotly <- plotly::ggplotly(maxdiffplot)#to create plotly of maxdifplot to trace plot
  rangeplotly <- plotly::ggplotly(rangeplot)#to create plotly of rangeplot to trace plot

  # Make html widgets of interactive plots (I think x2)
  htmlwidgets::saveWidget(maxdifplotly, paste0(qc_plots_loc, s, "_MaxDiffAirWaterTemp.html"))
  htmlwidgets::saveWidget(rangeplotly, paste0(qc_plots_loc, s, "_AirWaterTempRange.html"))
}; cat("Done.", fill = TRUE) # Print message "Done" once complete
# Note: in final code chunk, message appeared: "Cannot find directory
# data/2022_summer/4_qc_plots. Would you like to create a new directory?
# Pressed 1 for Yes then entered, which created new directory for us. Note that
# we could have the same process inside the for loop with the prompt for creating
# the new folders if we want that more automated

