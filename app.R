#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyr)
library(mice)
library(rsample)
library(caret)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(randomForest)
library(rsconnect)


Spaceship <- read.csv('Spaceship.csv')

Spaceship$HomePlanet = factor(Spaceship$HomePlanet)
           #CryoSleep = as.factor(input$CryoSleep),
Spaceship$CryoSleep = factor(case_when(Spaceship$CryoSleep == 'False' ~ 0, Spaceship$CryoSleep == 'True' ~ 1))
Spaceship$deck = factor(Spaceship$deck)
Spaceship$side = factor(Spaceship$side)
Spaceship$Destination = factor(Spaceship$Destination)


header <- dashboardHeader(title = "SPACESHIP TITANIC TRANSPORTED EXPLORER", titleWidth = 500)

sidebar <- dashboardSidebar(width = 150,
                            sidebarMenu(menuItem("EDA", tabName = "plots", icon = icon("dashboard")),
                                        menuItem("Prediction", tabName = "pred", icon = icon('tachometer-alt'))))

body <- dashboardBody(tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))),
                      tabItems(
                        #Plots tab content
                        tabItem('plots', 
                                box(status = "primary",width=15, title = h3("Spaceship Titanic"), background = "light-blue",
                                    h4("Predict which passengers are transported to an alternate dimension"), br(),
                                    h4("Welcome to the year 2912, where your data science skills are needed to solve a cosmic mystery. 
                                    We've received a transmission from four lightyears away and things aren't looking good.",br(),br(),"The 
                                    Spaceship Titanic was an interstellar passenger liner launched a month ago. With almost 13,000 
                                    passengers on board, the vessel set out on its maiden voyage transporting emigrants from our 
                                    solar system to three newly habitable exoplanets orbiting nearby stars.", br(),br(), "While rounding Alpha 
                                    Centauri en route to its first destination—the torrid 55 Cancri E—the unwary Spaceship Titanic 
                                    collided with a spacetime anomaly hidden within a dust cloud. Sadly, it met a similar fate as 
                                    its namesake from 1000 years before. Though the ship stayed intact, almost half of the passengers 
                                    were transported to an alternate dimension!")),
                                #Plots filters
                                box(title = 'Explore The Relationship: 
                                    Variables VS Transported Status', width=10,
                                    selectInput('var', "Variables:", c('Home Planet', 'CryoSleep', 
                                                                       'Cabin deck', 'Cabin num', 'Cabin side', 'Destination', 
                                                                       'Age','RoomService', 'FoodCourt Expenses', 
                                                                       'ShoppingMall', 'Spa Expenses','VRDeck Expenses')),
                                    footer = 'Distribution for variables'),
                                #Boxes to display the plots
                                box(title=h3("Variables Descriptions"), h5("HomePlanet - The planet the passenger departed from, 
                                typically their planet of permanent residence.", br(),
                                "CryoSleep - Indicates whether the passenger elected to be put into suspended animation 
                                for the duration of the voyage. Passengers in cryosleep are confined to their cabins.", br(),
                                "deck, num, side - The number of the cabin (takes the form deck/num/side) where the passenger 
                                is staying. The side can be either P for Port or S for Starboard.", br(),
                                "Destination - The planet the passenger will be debarking to.", br(), "Age - The age of the passenger."
                                , br(), "VIP - Whether the passenger has paid for special VIP service during the voyage.", br(), 
                                "RoomService, FoodCourt, ShoppingMall, Spa, VRDeck - Amount the passenger has billed at each of the 
                                Spaceship Titanic's many luxury amenities.", br(), "Transported - Whether the passenger was transported
                                to another dimension. This is the target, the column you are trying to predict.")),
                                box(plotOutput('distPlot'))),
                          # Prediction tab content
                          tabItem("pred", 
                                  #Prediction filters
                                  box(title = "Select Variable", status = "primary",
                                      sliderInput("num",
                                                  "Select the Cabin Number:",
                                                  min = 0,
                                                  max = 1894,
                                                  value = 100),
                                      sliderInput("Age",
                                                  "Select the Age:",
                                                  min = 0,
                                                  max = 79,
                                                  value = 16),
                                      sliderInput("RoomService",
                                                  "Select the Expenses of Room Service:",
                                                  min = 0,
                                                  max = 14350,
                                                  value = 0),
                                      sliderInput("FoodCourt",
                                                  "Select the Expenses of Food Court:",
                                                  min = 0,
                                                  max = 26830,
                                                  value = 0),
                                      sliderInput("ShoppingMall",
                                                  "Select the Expenses of Shopping Mall:",
                                                  min = 0,
                                                  max = 23490,
                                                  value = 0),
                                      sliderInput("Spa",
                                                  "Select the Expenses of Spa:",
                                                  min = 0,
                                                  max = 22408,
                                                  value = 0),
                                      sliderInput("VRDeck",
                                                  "Select the Expenses of VR Deck:",
                                                  min = 0,
                                                  max = 24140,
                                                  value = 0),
                                      selectInput("HomePlanet",
                                                  "Select the Home Planet:",
                                                  choices = list("Earth", "Europa", "Mars")),
                                      selectInput("CryoSleep",
                                                  "Cryo Sleep or not?",
                                                  choices = list("False", "True")),
                                      selectInput("deck",
                                                  "Select the Cabin Deck:",
                                                  choices = list("A", "B", "C", "D", "E", "F","G", "T")),
                                      selectInput("side",
                                                  "Select the Cabin Side:",
                                                  choices = list("P", "S")),
                                      selectInput("Destination",
                                                  "Select the Destination:",
                                                  choices = list("55 Cancri e", "PSO J318.5-22", "TRAPPIST-1e"))),
                                  # box to display the prediction result
                                  box(title=h3("Prediction Result: Transported or Not"),
                                      status="success", solidHeader = TRUE,
                                      textOutput("pred")),
                                  # box to display information about the model
                                  box(title = h3("Model Explanation"), status = "success",
                                      h4("This model will predict the transported status of the passenger
                                         from the missing Titanic Spaceship, the result `True` means the passenger 
                                         was successfully transported to the destination, and `False` means the
                                         passenger was missing in the space.",br(),"The prediction is based on random 
                                         forest superived machine learing model. RF is a reliable ensemble of 
                                         decision trees, which can be used for regression or classification problems.
                                         Here, the individual trees are built via k-fold cross validation (which are 
                                         nothing but multiple train datasets created via randomly split original 
                                         training set in k sets and one set as the test set, the remaing to be the 
                                         training set, and split k times), fit a model on training set and evaluate 
                                         it on the test set, retain the evaluation score and idscard the model.",br(),
                                         "This random forest model had 500 trees, the confusion matrix shows that it having 
                                         an accuracy of 80.51%. The model did also well on the testing data having a 
                                         80.506% accuracy rate.")
                                  )
                        )
                      )
)

ui <- dashboardPage(header, sidebar, body)

server <- shinyServer(function(input, output) {
  #EDA analysis
  output$distPlot <- renderPlot({
    # Column name variable
    Var = ifelse(input$var == 'Home Planet','HomePlanet', 
                     ifelse(
                       input$var == 'CryoSleep', 'CryoSleep',
                            ifelse(
                              input$var == 'Cabin deck','deck', 
                                   ifelse(
                                     input$var == 'Cabin num','num',
                                          ifelse(
                                            input$var == 'Cabin side','side', 
                                                 ifelse(
                                                   input$var == 'Destination', 'Destination',
                                                        ifelse(
                                                          input$var == 'Age', 'Age',
                                                               ifelse(
                                                                 input$var == 'RoomService','RoomService', 
                                                                      ifelse(
                                                                        input$var =='FoodCourt Expenses','FoodCourt',
                                                                             ifelse(
                                                                               input$var =='ShoppingMall','ShoppingMall', 
                                                                                    ifelse(
                                                                                      input$var == 'Spa Expenses','Spa',
                                                                                             ifelse(
                                                                                               input$var == 'VRDeck Expenses', 'VRDeck')
                                                                                      )))))))))))
    #plot
   # ggplot(na.omit(Spaceship), aes(x= Spaceship[[Var]],fill=Transported)) +
    Spaceship <- read.csv('Spaceship.csv')
    catg <- c("HomePlanet","CryoSleep","deck","num","side","Destination")
    if((Var %in% catg )){
    ggplot(na.omit(Spaceship),aes(x=.data[[Var]],fill=as.factor(Transported))) + 
      geom_bar( position='dodge')+
        guides(fill=guide_legend(title="Transported ?"))}
    else{
    ggplot(na.omit(Spaceship),aes(x=.data[[Var]],fill=as.factor(Transported))) + 
        geom_histogram( position='dodge',bins = 20)+
        guides(fill=guide_legend(title="Transported ?"))
    }
      # theme(axis.text = element_text(size = 12),
      #       axis.title = element_text(size = 14),
      #       plot.title = element_text(size = 16, face = 'bold'))+
      # labs(title = sprintf('Distribution of the variable %s', Var),
      #      x = sprintf('%s', input$var),y = 'Count')
  })
  
  #predict
  model <- readRDS("RandomForest.RDS")
  input_df <- reactive({
    data.frame(num = input$num,
               Age = input$Age,
               RoomService = input$RoomService,
               FoodCourt = input$FoodCourt,
               ShoppingMall = input$ShoppingMall,
               Spa = input$Spa,
               VRDeck = input$VRDeck,
               HomePlanet = factor(input$HomePlanet),
               #CryoSleep = as.factor(input$CryoSleep),
               CryoSleep = factor(case_when(input$CryoSleep == 'False' ~ 0, input$CryoSleep == 'True' ~ 1)),
               deck = factor(input$deck),
               side = factor(input$side),
               Destination = factor(input$Destination))
  })
  pred <- reactive({
    a<-predict(model, input_df(), type = "raw")
    ifelse( a == 0,"True","False")
    
  })
  output$pred <- renderText({pred()})
})

# Run the application 
shinyApp(ui = ui, server = server)
