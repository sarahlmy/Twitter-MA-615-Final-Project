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
library(tm)
library(wordcloud)
library(memoise)

# Input Data

smarthome <- readRDS('SmartHome.rds')
echodot <- readRDS('echodot.rds')
googlehome <- readRDS('googlehome.rds')

result_google <- readRDS('result_google.rds')
result_echo <- readRDS('result_echo.rds')

mapdata <- readRDS('smarthome_geo.rds')

home <- c('Home Smart', 'Echo Dot', 'Google Home')

smarthome$text <- iconv(smarthome$text,from = "latin1", to = "ASCII", sub = "")
wordCorpus <- Corpus(VectorSource(str_replace_all(smarthome$text, "@","")))
wordCorpus <- tm_map(wordCorpus, PlainTextDocument)
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, c('the', 'this', stopwords('english')))
wordCorpus <- tm_map(wordCorpus, removeWords, c('smarthome'))
wordCorpus <- tm_map(wordCorpus, stemDocument)

echodot$text <- iconv(echodot$text,from = "latin1", to = "ASCII", sub = "")
wordCorpus1 <- Corpus(VectorSource(str_replace_all(echodot$text, "@","")))
wordCorpus1 <- tm_map(wordCorpus1, PlainTextDocument)
wordCorpus1 <- tm_map(wordCorpus1, removePunctuation)
wordCorpus1 <- tm_map(wordCorpus1, content_transformer(tolower))
wordCorpus1 <- tm_map(wordCorpus1, removeWords, c('the', 'this', stopwords('english')))
wordCorpus1 <- tm_map(wordCorpus1, removeWords, c('echodot'))
wordCorpus1 <- tm_map(wordCorpus1, stemDocument)

googlehome$text <- iconv(googlehome$text,from = "latin1", to = "ASCII", sub = "")
wordCorpus2 <- Corpus(VectorSource(str_replace_all(googlehome$text, "@","")))
wordCorpus2 <- tm_map(wordCorpus2, PlainTextDocument)
wordCorpus2 <- tm_map(wordCorpus2, removePunctuation)
wordCorpus2 <- tm_map(wordCorpus2, content_transformer(tolower))
wordCorpus2 <- tm_map(wordCorpus2, removeWords, c('the', 'this', stopwords('english')))
wordCorpus2 <- tm_map(wordCorpus2, removeWords, c('googlehome'))
wordCorpus2 <- tm_map(wordCorpus2, stemDocument)

# Define UI 
ui <- fluidPage(theme="bootstrap.css",
                titlePanel(fluidRow(
                  column(3, img(src="title.png", height=108, width=150)), 
                  column(8, 
                         h3("MA615 Twitter Data Mining Project - Smart Home"),
                         h6("Presented by Mengyun Li (Sarah), Boston University, MSSP Program. 
                            All the codes can be approached from",
                        a(href="https://github.com/sarahlmy/Twitter-MA-615-Final-Project",
                          "Mengyun Li Github"))
                        ))),
                navbarPage("Navbar!",
   tabPanel("Inrtodction",
            shiny::h1("Smart Home"),
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
                      your entertainment and control your smart home with just my voice.
                      Echo Pot is a similar product made by Amazon")
            ),

   tabPanel("Pictures",
            splitLayout(img(src="googlehome.jpeg", height=512, width=640),
                        img(src="echodot.png", height=512, width=640)),
            hr(),
            p("All pictures are from", span("Google Home/Echo Dot website", style="color:Blue"), 
              "Click the link for more information!"), 
            a(href="https://store.google.com/product/google_home","Google Home official website"),
            br(),
            a(href="https://www.amazon.com/Amazon-Echo-Dot-Previous-Generation/b?ie=UTF8&node=14047587011",
              "Echo Dot official website"),
            br()
   ),
   
tabPanel("Word Cloud",
         sidebarPanel(
           selectInput("word", "Choose a word:",
                       choices = home),
           hr(),
           sliderInput("freq","Minimum Frequency:",
                       min = 1,  max = 50, value = 15),
           sliderInput("max","Maximum Number of Words:",
                       min = 1,  max = 300,  value = 100)
         ),
         mainPanel(plotOutput(outputId = "wordcloud"))
         ),

tabPanel("Sentiment Analyse", 
         sidebarPanel(
           selectInput("type", "Choose a product type:",
                       choices = c('Goolge Home', 'Echo Dot'),selected = 1)
           ),
         mainPanel(plotOutput(outputId = "sentiment"))
         ),

tabPanel("Timeline", 
         sidebarPanel(
           selectInput("category", "Choose a category:",
                       choices = c('Number of favourate', 'Number of retweet'),selected = 1)
         ),
         mainPanel(plotOutput(outputId = "timeline"))
),

tabPanel("Map",
         titlePanel(""),
         mainPanel(
           leafletOutput("map")
         ))
))


# Define server logic required to draw a histogram
server <- function(input, output) {
#Map
    output$map <- renderLeaflet({
    leaflet(data = mapdata)%>%
      addTiles()%>%
      addCircleMarkers(~x,~y, col="red")
  })

#Wordcloud
  output$wordcloud <- renderPlot({
    if (input$word=='Home Smart') {
      wordcloud(words = wordCorpus, scale=c(5,0.5), min.freq = input$freq, max.words=input$max,
                random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(4, "Dark2"))
    }
    if (input$word=='Echo Dot') {
      wordcloud(words = wordCorpus1, scale=c(5,0.5), min.freq = input$freq, max.words=input$max,
                random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(4, "Dark2"))
    }
    if (input$word=='Google Home') {
      wordcloud(words = wordCorpus2, scale=c(5,0.5), min.freq = input$freq, max.words=input$max,
                random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(4, "Dark2"))
    }
  })
  
#Sentiment Plot
  output$sentiment <- renderPlot({
    if (input$type=='Echo Dot') {
      ggplot(result_echo,aes(result_echo$score))+geom_bar(width = 0.3)
    }
    else {
      ggplot(result_google,aes(result_google$score))+geom_bar(width = 0.3)
    }
 })
  
#Timeline
  output$timeline <- renderPlot({
    if (input$category=='Number of favourate') {
      ggplot(smarthome, aes(x=created, y=favoriteCount)) +
        geom_line(col='orange')+labs(x="Time", y="Number of Favourates")
    }
    else {
      ggplot(smarthome, aes(x=created, y=retweetCount)) +
        geom_line(col='orange')+labs(x="Time", y="Number of retweets")
    }
  })
}  
     


# Run the application 
shinyApp(ui = ui, server = server)

