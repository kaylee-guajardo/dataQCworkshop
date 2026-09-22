#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)

# Define UI for application that draws a histogram
page_fluid(

  # App title
  titlePanel("dataQCworkflow"),

  layout_column_wrap(
    width = 1, # Card takes up full screen - can do as decimals to decrease width
    card(
      card_header("Step 1: Crop raw data"), # Card title
      uiOutput("step_container")
    )
  )
)
