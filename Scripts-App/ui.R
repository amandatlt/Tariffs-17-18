###############################################
#
# project: Compare tariff prices by Monitor
#          from  16/17 to 17/18
#    date: 09/12/16
# details: Creates UI for app  
#
###############################################

###---LIBRARIES AND PATHS ---###

.libPaths("P:/User/Amanda.Teo/R/Packages")
library(shiny)
library(ggvis)
library(dplyr)

###--- UI ---###

shinyUI(fluidPage(
  
  #app title
  titlePanel("Tariff Price Comparison"),
  
  #sidebar
  sidebarLayout(
    
    sidebarPanel(
      
      #select HRG
      selectInput("hrgchoice", "Select HRG of interest",
                  choices = unique(full.df$hrgid.1112)
      ),
      
      #select price info
      radioButtons("pricetype", "Select price of interest",
                   choices = list("Day-case and Elective", "Day-case only", "Elective only", "Non-elective")
      ),
      
      #update button
      actionButton("btn", "Update")
    
    ),
   
    mainPanel(
      ggvisOutput("plot1")
    )
     
  )
    
))