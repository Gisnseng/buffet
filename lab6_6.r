Sys.setenv(LANGUAGE="en")
install.packages("jsonlite")
library(jsonlite)
install.packages("randomForest")
library(randomForest)

restaurants=read.csv("CS438_buffet_team_project_demo/data/restaurants.csv")
restaurants$buffet=factor(restaurants$buffet,levels=c("No flag","Flagged"))
restaurants$cuisine=factor(restaurants$cuisine)
restaurants$price_tier=factor(ifelse(is.na(restaurants$price),"Unknown",as.character(restaurants$price)))
restaurants$log_reviews=log1p(restaurants$review_count)

# Split the training and test data
set.seed(438)
train=sample(1:nrow(restaurants),floor(0.8*nrow(restaurants)))
train_data=restaurants[train,]
test_data=restaurants[-train,]

# Group the cities
cities=names(sort(table(train_data$city),decreasing=TRUE))[1:10]
train_data$city_group=factor(ifelse(train_data$city %in% cities,train_data$city,"Other"),levels=c(cities,"Other"))
test_data$city_group=factor(ifelse(test_data$city %in% cities,test_data$city,"Other"),levels=c(cities,"Other"))

#restaurant <- stream_in(file("yelp_academic_dataset_business.json"))
#restaurant$isBuffet <- grepl("buffet", restaurant$categories)

#restaurant$cuisine <- "NA"
#restaurant$cuisine[grepl("Mexican|mexican|mexico|spanish|Spanish", restaurant$categories)] <- "Mexican"
#restaurant$cuisine[grepl("Italian|italian|italy", restaurant$categories)] <- "Italian"



library(shiny)
library(bslib)

# Define UI for application that draws a histogram
ui <- fluidPage(
    id = "main_page",
    # Application title
    titlePanel("To Buffet or Not to Buffet"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            #textInput(
              #inputId = "text",
              #label = "Enter your cuisine to see how many restaurants with that cuisine:",
              #value = "",
              #placeholder = "Type something here..."
            #),
            navset_card_underline(
              id = "active_tab",
              nav_panel("Summary", value = "summary"),
              nav_panel("Stars", value = "plot"),
            ),
            conditionalPanel(
            condition = "input.active_tab == 'plot2'"
            
            ),
        ),
        # Show a plot of the generated distribution
        mainPanel(
        conditionalPanel(
          condition = "input.active_tab == 'summary'",
          #textInput(inputId = "bins",
          #        label = "Enter bin number",
          #        value = "",
          #        placeholder = "text goes here"),
          #plotOutput("distPlot")
          h1("To Buffet or Not to Buffet"),
          p("that is the question. Whether tis nobler of the mind to eat at a buffet or a la carte"),
          br(),
          p("This project compares normal restaurants and buffet restaurants to see which one gives you a better bang for your buck")
          ),
        conditionalPanel(
          condition = "input.active_tab == 'plot'",
          
        ),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = as.numeric(input$bins) + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
    output$distPlot2 <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 1]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white',
           xlab = 'Eruption time (in mins)',
           main = 'Histogram of eruption times')
    })
    output$clusterPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 1]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      faithful.cluster = kmeans(faithful[, 1:2], iter.max = 500, centers = input$bins, nstart = 2)
      # draw the histogram with the specified number of bins
      plot(faithful, col = faithful.cluster$cluster)
    })
    output$regressPlot <- renderText({
      # generate bins based on input$bins from ui.R
      mytext = input$text
      mynum = as.numeric(mytext)
      faithful.regress = lm(waiting ~ eruptions, data = faithful)
      faithful.predict = predict(faithful.regress, data.frame(eruptions=mynum), type = "response")
      # draw the histogram with the specified number of bins
      paste("wait time: ", faithful.predict)
    })
    output$cuisinePlot <- renderText({
	mycuisine <- input$text
     paste("Number of restuarants with this cuisine: ", sum(grepl(mycuisine, restaurant$categories)))})
}

# Run the application 
shinyApp(ui = ui, server = server)





