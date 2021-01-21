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
if(!require("urltools")) install.packages("urltools"); library("urltools")

#Sys.setlocale(locale="en_US.UTF-8")

#Load our Datamart:
Datamart <- read.csv("created_data/final_datamart.csv")
product_sale <- read.csv("created_data/product_sale.csv")
TopUsers_Overall <- read.csv("created_data/topusers_overall.csv")
Prod_Age <- read.csv("created_data/prod_age.csv")
Prod_Country <- read.csv("created_data/prod_country.csv")


#####################################################################################################
###################################### THE SERVER ###################################################
####################################################################################################

# Define the server logic
shinyServer(function(input, output) {
  
  ################ TAB 2: the Datamart ###############  
  #Display of The datamart:
  output$Datamart <- DT::renderDataTable({
    Datamart <- read.csv("created_data/final_datamart.csv")
    DT::datatable(Datamart, options = list(dom = 't', lengthMenu = c(150, 300, 450), pageLength = 150))
  })
  
  ################ TAB 3: Demographics ###############   
  #Display Age of Gamblers:
  output$Age <- renderPlot({
    
    #Plot
    ggplot(Datamart, aes(x = AGE)) + 
      theme_bw() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line()) + 
      scale_x_continuous(breaks=seq(0,90,5)) +
      geom_histogram(fill = "#CC5500",bins=30, binwidth=2,color="white")
  }, height = 400, width = 600)
  
  #Display Age of Gamblers by Country:
  output$AgeCountry <- renderPlot({
    
    # Groupby of age per country:
    AgeCountry <- Datamart %>%
      group_by(Country) %>% 
      summarize(AGE = mean(AGE), Count=n())%>%
      filter(!is.na(AGE)) #Took out the clients who have no Age value as they were not in the Internet Gambling Table
    
    # Plot
    ggplot(AgeCountry, aes(x = AGE, y = Country, size=Count)) +
      scale_x_continuous(breaks=seq(10,80,5)) +
      labs(title = "Average Age of Customers per Country", x = "Country", y = "Age") +
      geom_point(fill = "#CC5500") }, height = 400, width = 600 )
  
  #Display Age of Gamblers by Country:
  output$Country <- renderPlot({
    
    # Finding top 10 countries with maximum users: 
    MaxUsers <- Datamart %>%
      group_by(Country) %>% 
      summarize(NbrUsers = n()) %>%
      mutate(Percent=NbrUsers*100/sum(NbrUsers)) %>%
      top_n(10, wt=NbrUsers) %>%
      arrange(desc(NbrUsers))  
    
    # Plot
    ggplot(MaxUsers, aes(x = reorder(Country, NbrUsers), y = NbrUsers )) +  
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line()) + 
      geom_bar(stat = "identity", fill = "#CC5500", position = "dodge") +  coord_flip() +
      labs(title = "Which Country are Clients from", x = "Country", y = "Number of Clients")
  }, height = 400, width = 600 )
  
  # Displaying Map indicating user location with heat-map of lowest to highest users
  output$Map<- renderPlot({
    
    # Count number of users per country and group them in categories:
    UserRegion2 <- Datamart %>% count(Country) %>% rename(region = Country)
    UserRegion2$Category <- ifelse(UserRegion2$n <= 100, 1, ifelse(UserRegion2$n > 1000 & UserRegion2$n<20000, 2, ifelse(UserRegion2$n > 20000, 4, 3)))
    
    # Create World map:
    world_map2 <- map_data("world")
    
    # Merge with data
    world_map_users2 <- left_join(UserRegion2, world_map2, by = "region")
    
    # Plot map
    ggplot(world_map_users2 , aes(x =long, y= lat, group = group)) +
      #coord_fixed(1.3)+
      geom_polygon(aes(fill = Category), color = "white")+
      labs(caption = "Category 1 = lowest number of users, Catgeory 4 = highest number of users")+
      scale_fill_viridis_c(option = "B") 
  }, height = 400, width = 600)
  
  
  #Display the Gender of Customers:
  output$Gender <- renderPlot({
    
    #Group by Gender
    Gender <- Datamart %>% group_by(Gender) %>% summarize(NbrUsers = n()) %>% transmute(Gender,NbrUsers, Percentage=NbrUsers*100/sum(NbrUsers))
    
    #Plot
    ggplot(Gender, aes(x = Gender, y = Percentage)) +  
      theme_bw() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(), 
            axis.line = element_line()) + 
      geom_bar(stat = "identity", fill = "#CC5500", position = "dodge")
  }, height = 400, width = 600 )
  
  
  #Display the Gender of Customers by countries (top 10):
  output$GenderCountry <- renderPlot({ 
    
    #Create groupby of gender per country
    GenderCountry <- Datamart %>%
      group_by(Country,Gender) %>%
      summarize(NbrUsers = n())
    
    # Filtering the users per gender per country for top 10 countries with max users
    GenderCountry_top10 <- dplyr::filter(GenderCountry, grepl('Germany|Turkey|Poland|Spain|Greece|France|Denmark|Austria|Italy|Switzerland', Country))
    
    # Plotting users per gender for top 10 countries
    ggplot(GenderCountry_top10, aes(x = Country, y = NbrUsers)) +  
      theme_bw() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line()) + 
      scale_y_continuous(breaks=seq(0,60,5)) +
      geom_bar(stat = "identity", aes( alpha = Gender), position = "dodge", fill = "#CC5500") +  coord_flip() + 
      scale_alpha_manual(values = c(0.6, 1)) + 
      labs(title = "Users per gender per country", x = "Country & Gender", y = "Number of Users")
  }, height = 400, width = 600 )
  
  
  ############ TAB 4: Source ################
  # Graph to identify popular routes of access to bwin 
  output$SourcesOverall <- renderPlot({ 
    
    # Counting users for each website/application
    PreferredRoute <- Datamart %>% group_by(Website) %>% summarize(Number_of_Users = n())
    
    # Selecting websites through which more than 20 users were acquired
    PreferredRoute1 <- Datamart %>% group_by(Website) %>% summarize(Number_of_Users = n()) %>% filter(Number_of_Users > 20)
    
    # Grouping together websites through which less than 20 users were acquired
    PreferredRoute2 <- PreferredRoute %>% filter(Number_of_Users <= 20) %>% 
      summarize (Website = "OTHER WEBSITES", Number_of_Users = sum(Number_of_Users))
    
    # stacking both tables togther and sorting them in descending order
    AcquisitionSources <- rbind(PreferredRoute1, PreferredRoute2)
    AcquisitionSources <- AcquisitionSources[order(-AcquisitionSources$Number_of_Users),]
    
    # Plotting
    ggplot(AcquisitionSources, aes(x = Website, y = Number_of_Users)) +  
      theme_bw() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line()) + 
      geom_bar(stat = "identity", fill = "#CC5500") + coord_flip() +
      labs(title = "Which route do customers prefer the most to access bwin", x = "Application", y = "Number of User Acquisitions")
  }, height = 400, width = 600 )
  
  
  # Displaying bar graph of most used application in selected country and age to access bwin 
  output$SourceCountry <- renderPlot({ 
    
    countrysource <- input$Country
    agesource <- input$Agegroup
    
    # Filtering and grouping as per given parameters
    Flexisource <- Datamart %>% filter(Country == countrysource, AGE == agesource) %>% group_by(Website) %>% summarize(Number_of_Users = n())
    
    
    # Plot
    ggplot(Flexisource, aes(x = Website, y = Number_of_Users)) +  
      theme_bw() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line()) + 
      geom_bar(stat = "identity", fill = "#CC5500") + coord_flip() 
  })

  
  
  ############ TAB 5:Subscriptions ################  

# Displaying overall monthly first pay variable
output$Conversion <- renderPlot({ 
    
  # Computing monthly FirstPay
count_of_pay <- Datamart %>% mutate(month = month(FirstPay)) %>% group_by(month) %>% summarise(TotalSubscribers = n())
                                        
  # Static overall graph of monthly paid subscriptions/FirstPay
ggplot(count_of_pay, aes(x = month, y = TotalSubscribers)) +  
theme_bw() + 
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.border = element_blank(),
axis.line = element_line()) + 
geom_bar(stat = "identity", fill = "#CC5500", position = "dodge")  +
scale_x_continuous(breaks=seq(2,10,1))+
labs(title = "Number of Conversions to Paid Users", x = "Month", y = "Number of Clients")
  } )
    
    
# Displaying conversion as per country
count_of_pay <- Datamart %>% mutate(month = month(FirstPay)) %>% group_by(month) %>% summarise(TotalSubscribers = n())
    
output$countryfirstpay <- renderPlot({ 
Countryfp <- input$Countryfp
country_pay <- Datamart %>% mutate(month = month(FirstPay)) %>% group_by(month, Country) %>% summarise(Subscribers = n()) %>% filter(Country == input$Countryfp)
country_pay1 <- left_join(x=country_pay, y=count_of_pay, by='month')
      
#Create long format
graph_country <- gather(country_pay1, Sub_Type, Subs, Subscribers:TotalSubscribers) 
      
# Plotting the multi-bar graph: 
ggplot(graph_country, aes(month, Subs, fill=Sub_Type)) +
theme_bw() +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.border = element_blank(), axis.line = element_line()) +
geom_bar(stat = "identity", position = 'dodge') +
scale_x_continuous(breaks=seq(2,10,1))+
labs(title = "Subscribers by Month", x = "Month", y = "Subscribers", fill = "Subscriber")
    }, height = 400, width = 600 )
    

# Displaying age first pay
output$agefirstpay <- renderPlot({ 
Agegroupfp <- input$Agegroupfp
Age_pay <- Datamart %>% mutate(month = month(FirstPay)) %>% group_by(month, Age_Group) %>% summarise(Subscribers = n()) %>% filter(Age_Group== input$Agegroupfp)
Age_pay1 <- left_join(x=Age_pay, y=count_of_pay, by='month')

#Create long format
graph_age <- gather(Age_pay1, Sub_Type, Subs, Subscribers:TotalSubscribers) 
      
# Plotting the multi-bar graph: 
ggplot(graph_age, aes(month, Subs, fill=Sub_Type)) +
theme_bw() +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.border = element_blank(), axis.line = element_line()) +
geom_bar(stat = "identity", position = 'dodge', fill="#CC5500") +
scale_x_continuous(breaks=seq(2,10,1))+
labs(title = "Subscribers by Month", x = "Month", y = "Subscribers", fill = "Subscriber")
  }, height = 400, width = 600 ) 


# Displaying gender first pay
output$genderfirstpay <- renderPlot({  
genderfp <- input$genderfp   
gender_pay <- Datamart %>%  mutate(month = month(FirstPay)) %>% group_by(month, Gender) %>% summarise(Subscribers = n()) %>% filter(Gender == input$genderfp)
gender_pay1 <- left_join(x=gender_pay, y=count_of_pay, by='month')
      
#Create long format
graph_gender <- gather(gender_pay1, Sub_Type, Subs, Subscribers:TotalSubscribers) 
      
# Plotting the multi-bar graph: 
ggplot(graph_gender, aes(month, Subs, fill=Sub_Type)) +
theme_bw() +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.border = element_blank(), axis.line = element_line()) +
geom_bar(stat = "identity", position = 'dodge',  fill = "#CC5500") +
scale_x_continuous(breaks=seq(2,10,1))+
labs(title = "Subscribers by Month", x = "Month", y = "Subscribers", fill = "Subscriber")
      
}, height = 400, width = 600 ) 
  
  
  ############ TAB 6: Products ################  
  #Display the Sales of Products:
  output$Product <- renderPlot({
    #Plot
    ggplot(product_sale, aes(x = Product, y = Percent)) +  
      theme_bw() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line()) + 
      scale_y_continuous(breaks=seq(0,80,5)) +
      geom_bar(stat = "identity", fill = "#CC5500", position = "dodge") + coord_flip() 
  }, height = 400, width = 600 )
  
#Display the Sales of Products by age group:
output$ProductAge <- renderPlot({
 
  #Plot
  ggplot(data=Prod_Age, aes(x=Age_Group, y=Prod_Count)) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.border = element_blank(), axis.line = element_line()) + 
    scale_x_continuous(breaks=seq(0,10)) +
    scale_y_continuous(breaks=seq(0, 550000, 50000)) +
    geom_line(linetype = "dashed")+
    geom_point(color="grey30") +
    geom_text(aes(label=Popular_Prod),size=2.5, vjust=-0.7, color = "#CC5500") +
    labs(title = "Most Popular Product by Age Group", x = "Age Group", y = "Product Sales", 
         caption = "Age Group Legend: 1 = 10s, 2 = 20s, 3 = 30s, 4 = 40s, 5 = 50s, 6 = 60s, 7 = 70s, 8=80s, 9=90s, 10=100s")
  
}, height = 400, width = 600 ) 

#Display the Sales of Products by country:
output$ProductCountry <- renderPlot({
  
  #Plot
  ggplot(data=Prod_Country, aes(x=Country, y=Prod_Count)) +
    theme_bw() + 
    # remove gridlines. panel.grid.major is for verical lines, panel.grid.minor is for horizontal lines
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.border = element_blank(), axis.line = element_line()) + 
    geom_point(color="grey30") +
    labs(title = "Most Popular Product by Country", x = "Country", y = "Product Sales") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}, height = 400, width = 600 ) 
  
  
  
  ############ TAB 7: Gamble ################ 
#Display Bets vs Winnings:
output$BW <- renderPlot({
  
  #Plot
  ggplot(Datamart, aes(Avg_Bets, Avg_Winning)) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line()) + 
    geom_point() +
    scale_x_log10()
}, height = 400, width = 600)


  ############ TAB 8: Best Customers ################ 
  #Display the Best customers per Bets:
  output$MaxBets <- renderPlot({
    #Create Max Bets
    MaxBets <- Datamart %>% group_by(UserID) %>% summarize(Total_Bets) %>% top_n(10, wt=Total_Bets) %>% arrange(desc(Total_Bets))
    #Plot
    ggplot(MaxBets, aes(x = reorder(UserID, Total_Bets), y = Total_Bets )) +  
      theme_bw() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line()) + 
      geom_bar(stat = "identity", fill = "#CC5500", position = "dodge")+
    labs( x = "UsersID", y = "Total Bets") 
  }, height = 400, width = 600 )
  
  
  #Display the Best customers per Stakes:
  output$MaxStakes <- renderPlot({
    #Create Max Bets
    MaxStakes <- Datamart %>% group_by(UserID) %>%  summarize(Total_Stakes) %>% top_n(10, wt=Total_Stakes) %>% arrange(desc(Total_Stakes))
    #Plot
    ggplot(MaxStakes, aes(x=reorder(UserID, Total_Stakes), y = Total_Stakes )) +  
      theme_bw() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line()) + 
      geom_bar(stat = "identity", fill = "#CC5500", position = "dodge") +
      labs( x = "UsersID", y = "Total Stakes") 
  }, height = 400, width = 600 )
  
  
  #Display the Best customers per Winnings:
  output$MaxWins <- renderPlot({
    #Create Max Bets
    MaxWinning <- Datamart %>% group_by(UserID) %>%  summarize(Total_Winning) %>% top_n(10, wt=Total_Winning) %>% arrange(desc(Total_Winning))
    #Plot
    ggplot(MaxWinning, aes(x = reorder(UserID, Total_Winning), y = Total_Winning )) +  
      theme_bw() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line()) + 
      geom_bar(stat = "identity", fill = "#CC5500", position = "dodge")+
      labs( x = "UsersID", y = "Total Winnings") 
  }, height = 400, width = 600 )
  
  
  #Display the Best customers per Poker Chip Buys:
  output$Max_PC_Buys <- renderPlot({
    #Create Max Bets
    Max_PC_Buy <- Datamart %>% group_by(UserID) %>%  summarize(Nbr_PokerChip_Buy_Trans) %>% top_n(10, wt=Nbr_PokerChip_Buy_Trans) %>% arrange(desc(Nbr_PokerChip_Buy_Trans))
    #Plot
    ggplot(Max_PC_Buy, aes(x = reorder(UserID, Nbr_PokerChip_Buy_Trans), y = Nbr_PokerChip_Buy_Trans )) +  
      theme_bw() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line()) + 
      geom_bar(stat = "identity", fill = "#CC5500", position = "dodge")+
      labs( x = "UsersID", y = "Number of Poker Chips Bought") 
  }, height = 400, width = 600 )
  
  
  #Display the Best customers per Poker Chip Sell:
  output$Max_PC_Sales <- renderPlot({
    #Create Max Bets
    Max_PC_Sell <- Datamart %>%group_by(UserID) %>% summarize(Nbr_PokerChip_Sell_Trans) %>% top_n(10, wt=Nbr_PokerChip_Sell_Trans) %>% arrange(desc(Nbr_PokerChip_Sell_Trans))
    #Plot
    ggplot(Max_PC_Sell, aes(x = reorder(UserID, Nbr_PokerChip_Sell_Trans ), y = Nbr_PokerChip_Sell_Trans )) +  
      theme_bw() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line()) + 
      geom_bar(stat = "identity",fill = "#CC5500", position = "dodge")+
      labs( x = "UsersID", y = "Number of Poker Chips Sold") 
  }, height = 400, width = 600 )
  
  #Display the Best customers per number of Transactions:
  output$MaxTrans <- renderPlot({
    #Create Max Bets
    Max_Transactions <- Datamart %>% group_by(UserID) %>% summarize(Nbr_of_Trans) %>%top_n(10, wt=Nbr_of_Trans) %>% arrange(desc(Nbr_of_Trans))
    #Plot
    ggplot(Max_Transactions, aes(x = reorder(UserID, Nbr_of_Trans), y = Nbr_of_Trans )) +  
      theme_bw() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line()) + 
      geom_bar(stat = "identity", fill = "#CC5500", position = "dodge") +
      labs( x = "UsersID", y = "Number of Transactions") 
  }, height = 400, width = 600 )
  
  
  #Display of Table of top customers:
  output$TopUsers_Overall <- DT::renderDataTable({
    TopUsers_Overall <- read.csv("created_data/topusers_overall.csv")
    DT::datatable(TopUsers_Overall, options = list(dom = 't', lengthMenu = c(150, 300, 450), pageLength = 150))
  })
  
  
  
}

)  # end of server 



