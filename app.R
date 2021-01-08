#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
library(tidyverse)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("MLE Shelter Soil Moistures"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        radioButtons("site", h3("Site"),
                     choices = list("Rhinelander" = 'rhinelander', "Escanaba" = 'escanaba', "Hancock" = 'hancock', "Lake City" = 'lake-city', "Lux Arbor"="lux-arbor", "BCSE" = 'bcse'),selected = 'rhinelander'),
        sliderInput("days", "Days:",
                    min = 1, max =120, value = 30
        )
        ),
      
     
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  plot <- function(mysite, days, data) {
    data$date <- ymd_hms(data$sample_date)
    add_block <- function(value) {
      return(paste("Block", value))
    }
    add_cm <- function(value) {
      return(paste(value, "cm"))
    }
    plot_labeller <- labeller(
      replicate = add_block,
      depth = add_cm,
      .default = label_both
    )
    
    subset(data, site ==mysite & as.POSIXlt(date)$yday > as.POSIXlt(now())$yday - days) %>%  ggplot(aes(date)) + geom_point(aes(y=vwc, color=as.factor(experiment))) + facet_grid('replicate~depth', labeller = plot_labeller) + xlab('Date') + labs(color="Station")
  }
   
  data = read.csv("https://lter.kbs.msu.edu/weather/tables/30")
  output$distPlot <- renderPlot({
   plot(input$site,input$days, data)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)