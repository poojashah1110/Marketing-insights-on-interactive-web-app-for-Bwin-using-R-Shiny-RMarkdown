# Load packages
if(!require("shiny")) install.packages("shiny"); library("shiny")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("haven")) install.packages("haven"); library("haven")
if(!require("tidyr")) install.packages("tidyr"); library("tidyr")
if(!require("stringr")) install.packages("stringr"); library("stringr")
if(!require("maps")) install.packages("maps"); library("maps")
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("DBI")) install.packages("DBI"); library("DBI")
if(!require("shinydashboard")) install.packages("shinydashboard"); library("shinydashboard")
if(!require("leaflet")) install.packages("leaflet"); library("leaflet")
if(!require("flexdashboard")) install.packages("flexdashboard"); library("flexdashboard")
if(!require("rsconnect")) install.packages("rsconnect"); library("rsconnect")
if(!require("DT")) install.packages("DT"); library("DT")

#Sys.setlocale(locale="en_US.UTF-8")

#Load our Datamart:
Datamart <- read.csv("created_data/final_datamart.csv")
product_sale <- read.csv("created_data/product_sale.csv")
TopUsers_Overall <- read.csv("created_data/topusers_overall.csv")
Prod_Age <- read.csv("created_data/prod_age.csv")
Prod_Country <- read.csv("created_data/prod_country.csv")

################################################################################################
###################################### THE UI ###################################################
#################################################################################################

header <- dashboardHeader(title = "Internet Gambling Analysis")

sidebar <- dashboardSidebar( 
  
  # Menubar on the left: 
  sidebarMenu(
    menuItem("Project Overview", tabName = "projectoverview", icon = icon("r-project")),
    menuItem("Datamart", tabName = "datamart", icon = icon("table")),
    menuItem("Demographics", icon = icon("id-card"), tabName = "demographics"),
    menuItem("Source", icon = icon("route"), tabName = "source"), 
    menuItem("Subscriptions", tabName = "regsubs", icon = icon("funnel-dollar")),
    menuItem("Product", tabName = "product", icon = icon("gamepad")),
    menuItem("Gamble", icon = icon("dice"), tabName = "Gamble"),
    menuItem("Best Customers", icon = icon("users"), tabName = "bestcustomers")
  )
)

body <- dashboardBody(
  
  ############ Tab 1: Project Overview #################
  tabItems( 
    tabItem (tabName = "projectoverview",
             h1("Internet Gambling Data Analysis"),
             h4("~ Noemie, Pooja, Kiran ~"),
             h4("This app uses the data collected from bwin's online gambling platform to gather key inisghts.
                Below you can find the content of the app and some explanations."),
             h5("In the second tab you can find our datamart. "),
             h5("In the third tab you will find all key insights regarding the demographics of users. 
                This includes: Age of internet sport gamblers overall and by country, gender, and a map
                of where the users are from."),
             h5("Then you can find the Source tab. Here you will find key information about how clients access Bwin's website."),
             h5 ("In Subscriptions you will find all the insights on how many customers are registered
                 and how many spend money on Bwin's website."),
             h5 ("In the Products tab are graphs illustrating which products sell best overall, by country and by age group."),
             h5 ("In the Gamble tab, you will find informaiton regarding the gambling metrics: bets vs winnings."),
             h5 ("Finally, the last tab illustartes the best customers based on different criterias.")
    ),
    
    
    ############ Tab 2: Datamart #################    
    tabItem (tabName = "datamart",
             div(style = 'overflow-x: scroll', DT::dataTableOutput('Datamart'))),
    
    
    ############ Tab 3: Demographics #################     
    tabItem (tabName = "demographics",  fluidRow(
      # Demographics page header
      titlePanel("Demographical Analysis"),
      tabsetPanel(
        tabPanel("Age",  br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h3("Histogram of Age of Users"),
                                   plotOutput("Age", width = "100%")
                          )))),
        
        
        tabPanel("Age by Country", br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h3("Scatter Plot of Age by Country"),
                                   plotOutput("AgeCountry", width = "100%")
                          )))),
        
        
        tabPanel("Top Countries", br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h3("Bar Plot of Top 10 countries"), 
                                   plotOutput("Country", width = "100%")
                          )))),
        
        tabPanel("Map of Users", br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h3("User Location Map"),
                                   plotOutput("Map", width = "100%")
                          )))),
        
        tabPanel("Gender", br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h3("Bar Graph of Gender"),
                                   plotOutput("Gender", width = "100%")
                          )))),
        
        tabPanel("Gender by Country", br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h3("Multi-Bar Plot of Gender by Country"), 
                                   plotOutput("GenderCountry", width = "100%")
                          ))))
        
        
      ))),# end of tab item 3
    
    #############Tab 4: Source  #################
    tabItem (tabName = "source",  fluidRow(
      # Source Page Header
      titlePanel("Most preferred routes to access bwin"),
      tabsetPanel(
        tabPanel("Overall",  br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h3("Bar Plot of Popular Applications"),plotOutput("SourcesOverall", width = "100%")
                          )))),
        
        
        tabPanel("Source By Interactive Selection", br(),
                 sidebarPanel(
                   selectInput("Country", "Select a country",
                               c("Germany","Turkey","Poland","Spain","Greece","France","Denmark","Austria","Italy","Switzerland"),
                               selected ="Germany"),
                   sliderInput("Agegroup", "Select age",
                               min=10, max=100,value=25, step=5)  
                 ),
                 
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h4("Popular Applications by Age & Country"),plotOutput("SourceCountry")
                                   
                          ))))                  
      ))), # end of tab 4

    
    ############ Tab 5: Registrations/Subscriptions #################
    tabItem (tabName = "regsubs",  fluidRow(
      # Subscriptions page header
      titlePanel("Paid Subscription Analysis"),
      tabsetPanel(
        tabPanel("Overall",  br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h3("Bar Plot of monthly conversion to paid subscriptions"),
                                   plotOutput("Conversion", width = "100%")
                          )))),
        
        tabPanel("First Pay by Country", br(),
                 sidebarPanel(
                   selectInput("Countryfp", "Select a country",
                               c("Germany","Turkey","Poland","Spain","Greece","France","Denmark","Austria","Italy","Switzerland"),
                               selected ="Germany")
                 ),
                 
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h4("Monthly Paid Conversion Breakdown"),plotOutput("countryfirstpay")
                                   
                          )))),
        
        tabPanel("First Pay by Agegroup", br(),
                 sidebarPanel(
                   sliderInput("Agegroupfp", "Select an age group",
                               min=1, max=10,value=1, step=1)
                 ),
                 
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h4("Monthly Paid Conversion Breakdown"),plotOutput("agefirstpay")
                                   
                          )))),
        
        tabPanel("First Pay by Gender", br(),
                 sidebarPanel(
                   selectInput("genderfp", "Select Gender",
                               c("Male","Female"),
                               selected ="Male")
                 ),
                 
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h4("Monthly Paid Conversion Breakdown"),plotOutput("genderfirstpay")
                                   
                          ))))            
        
      ))),# end of tab 5
    
    
    
    
    ############ Tab 6: Product ################# 
    tabItem (tabName = "product",  fluidRow(
      # Demographics page header
      titlePanel("Demographical Analysis"),
      tabsetPanel(
        tabPanel("Product Sales",  br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h3("Bar Plot of Product Sales"), 
                                   plotOutput("Product", width = "100%")  
                          )))),
        
        
        tabPanel("Product Sales By Age-Group", br(),
                   mainPanel(mainPanel(
                     column(width=11,
                            fluidRow(height = 200,
                                     h3("Bar Plot of Product Sales by Age Group"), 
                                     plotOutput("ProductAge", width = "100%") 
                            )
                     )
                   ))),
        
        tabPanel("Product Sales By Country", br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h3("Scatter Plot of Product Sales by Country"), 
                                   plotOutput("ProductCountry", width = "100%") 
                          ))))
        
      ))), # end of tab item 6
    
    
    ############ Tab 7: Gamble ################# 
    tabItem (tabName = "Gamble",  fluidRow(
      # Gamble page header
      titlePanel("Gambling Analysis"),
      tabsetPanel(
        tabPanel("Bets Vs Winnings",  br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h3("Scatter Plot of Bets vs Winnings"), 
                                   plotOutput("BW", width = "100%")  
                          ))))
      ))), # end of tab item 7
        
        
    
    
    
    ############ Tab 8: Top Customers #################  
    tabItem (tabName = "bestcustomers",  fluidRow(
      # Best Customers page header
      titlePanel("Top Customers as Per..."),
      tabsetPanel(
        tabPanel("Max Bets",  br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h3("Bar Plot of top 10 customers with Maximum Bets"),
                                   plotOutput("MaxBets", width = "100%")
                          )))),
        
        tabPanel("Max Stakes", br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h3("Bar Plot of top 10 customers with Maximum Stakes"),
                                   plotOutput("MaxStakes", width = "100%")
                          )))),
        
        
        tabPanel("Max Winnings", br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h3("Bar Plot of top 10 customers with Maximum Winnings"),
                                   plotOutput("MaxWins", width = "100%")
                          )))),
        
        tabPanel("Max Poker Chips Buy", br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h3("Bar Plot of top 10 customers with Maximum Poker Chip Buys"),
                                   plotOutput("Max_PC_Buys", width = "100%")
                          )))),
        
        tabPanel("Max Poker Chips Sell", br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h3("Bar Plot of top 10 customers with Maximum Poker Chip Sell"),
                                   plotOutput("Max_PC_Sales", width = "100%")
                          )))),
        
        tabPanel("Max Transactions", br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h3("Bar Plot of top 10 customers with Maximum Transaction"),
                                   plotOutput("MaxTrans", width = "100%")
                          )))),
      
        tabPanel("Table of Top Customers",
                  div(style = 'overflow-x: scroll', DT::dataTableOutput('TopUsers_Overall')))
                       
        
        
        
      )))# end of tab item 8
    
    
  ))# end of tabsitem and body


shinyUI(dashboardPage(
  header, sidebar, body ,skin ="red")
)
