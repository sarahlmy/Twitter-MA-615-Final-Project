#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)

# Input Data

home <- c('Home Smart', 'Echo Dot', 'Google Home')

# Define UI 
ui <- navbarPage("Navbar!",
   tabPanel("Inrtodction",
            shiny::h1("Home Smart"),
            shiny::hr("Smart Home is building automation for a home. It involves the control 
                      and automation of lighting, heating (such as smart thermostats), 
                      ventilation, air conditioning (HVAC), and security, as well as home 
                      appliances such as washer/dryers, ovens or refrigerators/freezers.
                      Wi-Fi is often used for remote monitoring and control. Home devices, 
                      when remotely monitored and controlled via the Internet, are an 
                      important constituent of the Internet of Things. "),
            shiny::h5(""),
            shiny::hr("This analysis compares two smart home product I like most, Google Home
                      Mini and Echo Dot. Through Google Home I can get hands-free help from 
                      the Google Assistant. Get answers, play songs, tackle your day, enjoy 
                      your entertainment and control your smart home with just my voice.")
            ),
           
                
    tabPanel("Echo Dot",

             
   # Application title
   titlePanel("Comment of Echo dot on twitter"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
),
tabPanel("Word Cloud",
         sidebarPanel(
           selectInput("Selection", "Choose a word:",
                       choices = home),
           actionButton("Update", "Change"),
           hr(),
           sliderInput("Freq",
                       "Minimum Frequency:",
                       min = 1,  max = 50, value = 15),
           sliderInput("Max",
                       "Maximum Number of Words:",
                       min = 1,  max = 300,  value = 100)
         ),
         mainPanel()
         )

)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

