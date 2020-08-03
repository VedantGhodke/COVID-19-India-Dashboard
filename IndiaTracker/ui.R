#Loading the required packages
suppressMessages(library(rvest))
suppressMessages(library(readr))
suppressMessages(library(jsonlite))
suppressMessages(library(shiny))
suppressMessages(library(janitor))
suppressMessages(library(shinydashboard))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(highcharter))
suppressMessages(library(readxl))
suppressMessages(library(purrr))
suppressMessages(library(RCurl))
suppressMessages(library("rjson"))
suppressMessages(library(data.table))
suppressMessages(library(DT))




#reading the data


#open connection to the file which is a efficient way.
myfile1 <- getURL('https://api.covid19india.org/csv/latest/state_wise.csv', 
                  ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

#dataset with summariesed case counts of statese
StateCOVID_19 <- read.csv(textConnection(myfile1), header=T)
head(StateCOVID_19)


#dataset with time series data of Indian states
myfile2 <- getURL('https://api.covid19india.org/csv/latest/state_wise_daily.csv', 
                  ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

State_time_series <- read.csv(textConnection(myfile2),header = T)
head(State_time_series)


#total Indian cases time series and daily changes
myfile3 <- getURL('https://api.covid19india.org/csv/latest/case_time_series.csv', 
                  ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

Case_time_series <- read.csv(textConnection(myfile3),header = T)
head(Case_time_series)


myfile5 <- getURL('https://api.covid19india.org/csv/latest/statewise_tested_numbers_data.csv',
                  ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

State_tested<- read.csv(textConnection(myfile5), header = T)




myfile6 <- getURL('https://api.covid19india.org/csv/latest/tested_numbers_icmr_data.csv',
                  ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

Tested_ICMR <- read.csv(textConnection(myfile6),header = T)


#DISTRICT WISE DATA(IN JSON FORMAT)

json_file <- "https://api.covid19india.org/state_district_wise.json"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))



dashboardPage(
  skin="black",
  dashboardHeader(title="Vedant Ghodke - COVID-19 India Tracker Dashboard"),
  
  
  #dashboard sidebar
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("Overview Dashboard ", tabName = "tab1" ,icon=icon("dashboard")),
      menuItem("State-Wise Cases ", tabName = "tab2",icon= icon("globe")),
      menuItem("Samples Tested ", tabName = "tab3",icon= icon("hospital")),
      menuItem("States-Wise Timeline Analysis ", tabName = "tab4",icon= icon("cog")),
      menuItem("National Timeline Analysis ", tabName = "tab5",icon=icon("calendar")),
      menuItem("District-Wise Analysis ", tabName = "tab6",icon= icon("map")),
      menuItem("About Me", tabName = "tab7",icon= icon("award"))
    ) #end sidebarmenu
    
  ), # end dashboardsidebar
  
  
  #dashboardBody
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Monteserrat|Playfair+Display|Raleway")
    ),
    
    #adding all the tabs
    tabItems(
      
      tabItem(tabName ="tab1",
              
              h1("COVID-19 DASHBOARD FOR INDIA",align="center",style="margin-top:0px;"),
              h2("DEVELOPED AND DESIGNED BY : VEDANT GHODKE",align="center",style="margin-top:0px;"),
              p("(For best user experience, please prefer opening this dashboard on a desktop or larger screens | Data and statistics on this website is refreshed every 5 minutes | Few changes might take some 			time to reflect on the website)", align="center"),
              br(),
	      p("(This website contains refined, more readable and comprehensive statistics of COVID-19 from all the states of India fetched from the official website of Central Government Of India.)", 	align="center"),
              
              fluidRow(
                
                               
                box(
                  
                  h4("Confirmed Cases", align="left") , 
                  width=3,
                  textOutput("Confirmed"), #end text Output
                  #adding custom CSS for the text
                  tags$head(tags$style("#Confirmed{
                                 font-size: 28px;
                                 color:black;
                                 font-family:'Raleway', Monteserrat;
                                 }"
                  )
                  ) # end head
                  
                ), #end box

		box(
                  
                  h4("Active Cases", align="left") , 
                  textOutput("Active"), #end text Output
                  width=3,
                  tags$head(tags$style("#Active{
                                 color: blue;
                                 font-size: 28px;
                                 font-family:'Raleway', Monteserrat;
                                 }"
                  )) #end head
                ) ,
               
                
                box(
                  
                  h4("Total Deaths", align="left") , 
                  textOutput("Deaths"), #end text Output
                  width=3,
                  #adding custom CSS for the text
                  tags$head(tags$style("#Deaths{
                                 color: red;
                                 font-size: 28px;
                                 font-family:'Raleway', Monteserrat;
                                 }"
                  )) #end head
                ) , #end box
                
                box(
                  
                  h4("Total Recoveries", align="left") , 
                  textOutput("Recoveries"), #end text Output
                  width=3,
                  tags$head(tags$style("#Recoveries{
                                 color: green;
                                 font-size: 28px;
                                 font-family:'Raleway', Monteserrat;
                                 }"
                  )) #end head
                ), #end box
                
                box(
                  
                  h4("Total Samples Tested", align="left") , 
                  textOutput("Tested"), #end text Output
                  width=4,
                  tags$head(tags$style("#Tested{
                                 color: black;
                                 font-size: 28px;
                                 font-family:'Raleway', Monteserrat;
                                 }"
                  )) #end head
                ),
                
                box(
                  
                  h4("Recovery Rate Percentage (%)", align="left") , 
                  textOutput("RecRate"), #end text Output
                  width=4,
                  tags$head(tags$style("#RecRate{
                                 color: green;
                                 font-size: 28px;
                                 font-family:'Raleway', Monteserrat;
                                 }"
                  )) #end head
                ), 
                
                box(
                  h4("Death Rate Percentage (%)", align="left") , 
                  textOutput("DeadRate"), #end text Output
                  width=4,
                  tags$head(tags$style("#DeadRate{
                                 color: red;
                                 font-size: 28px;
                                 font-family:'Raleway', Monteserrat;
                                 }"
                  )) #end head
                ), 
                
             
                box(
                  
                  width = 12,
                  
                  highchartOutput("stackedCovidIndia")
                  
                  
                ),
             
                box(
                  
                  width=12,
                  h3("State-Wise Data"),
                  p("The following data and statistics are refreshed everyday. Kindly check for daily updates."),
                  dataTableOutput("StateData")
                  
                ) #end box
       
              ) #end fliudRow
      ), #end tab1
      
      
      
      tabItem(tabName = "tab2",
              
              fluidRow(
                
                box(width = 6,
                    
                    h3("Confirmed Cases (All Indian States)"),
                    br(),
                    highchartOutput("StateConf")
                ) ,#end box
                
                box(width = 6,
                    
                    h3("Active Cases (All Indian States)"),
                    br(),
                    highchartOutput("StateActive")
                ),
                
                box(width = 6,
                    
                    h3("Confirmed Deaths (All Indian States)"),
                    br(),
                    highchartOutput("StateDeaths")
                ) ,
                
                
                box(width = 6,
                    
                    h3("Confirmed Recoveries (All Indian States)"),
                    br(),
                    highchartOutput("StateRecoveries")
                ) ,
                
                hr(),
                br(),
                br(),
                br(),
                h2("Recovery And Death Rates (All Indian States)",align="center"),
                br(),
                br(),
                
                #recovery and death rates statewise
                box(
                  
                  width = 6,
                  h3("Recovery Rates For Each State (Out Of Total Confirmed)"),
                  br(),
                  highchartOutput("ratesPlotRecovery")
                  
                  
                ),
                
                
                box(
                  
                  width = 6,
                  h3("Death Rates For Each State (Out Of Total Confirmed)"),
                  br(),
                  highchartOutput("ratesPlotDeath")
                  
                  
                ) #end box
                
              ) #end fluidRow
      ), #end tabItem
      
      
      #testing data tab
      tabItem(tabName = "tab3",
              
              fluidRow(
                
                
                box(
                  
                  width = 12,
                  h2("Total Samples Tested"),
                  p("As per The Indian Council of Medical Research (ICMR), New Delhi"),
                  br(),
                  textOutput("totalTested"),
                  tags$head(tags$style("#totalTested{
                                 color: black;
                                 font-size: 28px;
                                 font-family:'Raleway', Monteserrat;
                                 }"
                  )) #end head
                ), 
                
                box(
                  width = 12,
                  h3("Samples are tested daily by the ICMR"),
                  p("Please Note: Few days have missing data as it has not been updated on the official website of Central Government Of India."),
                  br(),
                  highchartOutput("TestingChart")
                ), 
                
                
                
                box(
                  width = 12,
                  h3("State-Wise Daily COVID-19 Mean Test Positivity Rate"),
                  p("The mean test positive rate is calculated by taking the mean of the number of samples who tested positive, each day, in every state."),
                  p("This helps us understand which state has the highest ratio of tested COVID-19 positive cases out of samples tested daily."),
                  p("Please Note: Few days have missing data as it has not been updated on the official website of Central Government Of India."),
                  br(),
                  highchartOutput("MeanTestRate")
                ),
                
                
                
                br(),
                br(),
                
                box(
                  width = 12,
                  h3("COVID-19 Daily Test Positivity Rate"),
                  p("Please Note: Few days have missing data as it has not been updated on the official website of Central Government Of India."),
                  br(),
                  highchartOutput("RateChartIndia")
                ),
                
               
                
                #Statewise Tests done
                h3("State-Wise Testing Process Analysis",align="center"),
                br(),
                
                #SelectBox                      
                box(
                  
                  width = 12,
                  selectInput("state_test", label = "Please Select Desired State",choices = StateCOVID_19$State[-1])
                  
                ),
                
                box(
                  width = 12,
                  br(),
                  h3("Total State-Wise Testing Done (Cumulative Count)"),
                  p("Please Note: Few days have missing data as it has not been updated on the official website of Central Government Of India."),
                  br(),
                  highchartOutput("StateTestChart")
                ), 
                
                #DAily statewise 
                box(
                  width = 12,
                  br(),
                  h3("Daily State-Wise Testing Done"),
                  p("Please Note: Few days have missing data as it has not been updated on the official website of Central Government Of India."),
                  br(),
                  highchartOutput("StateDailyTestChart")
                ), 
                
                box(
                  width = 12,
                  br(),
                  h3("Test Positivity Rates For Each State"),
                  p("Please Note: Few days have missing data as it has not been updated on the official website of Central Government Of India."),
                  br(),
                  highchartOutput("StatePositiveTestRate")
                )
                
              ) #end fluidRow
              
      ) , #end tabitem
      
      
      tabItem(tabName = "tab4",
              
              
              
              
              fluidRow(
                
                h2("Timeline Analysis of New Cases Reported (Each Indian State)",align="center"),
                br(),
                br(),
                
                box(
                  
                  width = 12,
                  
                  selectInput("state", label = "Please Select Desired State",choices = StateCOVID_19$State_code[-1])
                  
                  
                ), #end box
                
                box(
                  width = 12,
                  h4("New Confirmed Cases In Particular State"),
                  br(),
                  highchartOutput("StateCasesTimeSeries")
                ),
                
                box(
                  width = 12,
                  h4("New Confirmed Recoveries In Particular State"),
                  br(),
                  highchartOutput("StateCasesTimeSeries_recover")
                ),
                
                box(
                  width = 12,
                  h4("New Confirmed Deaths In Particular State"),
                  br(),
                  highchartOutput("StateCasesTimeSeries_death")
                )
                
                
                
                
              ) # end fluidRow 
              
      ), #end tab4
      
      tabItem(tabName ="tab5",
              
              fluidRow(
                
                h2("Timeline Analysis of New Cases Reported In India",align="center"),
                br(),
                br(),
                
                box(
                  
                  width = 12,
                  h3("Daily Confirmed Reported Cases"),
                  highchartOutput("ConfDaily")
                ), #end box
                
                box(
                  
                  width = 12,
                  h3("Daily Confirmed Death Cases"),
                  highchartOutput("DeathsDaily")
                ),
                
                
                box(
                  
                  width = 12,
                  h3("Daily Confirmed Recovered Cases"),
                  highchartOutput("RecoveredDaily")
                  
                )
                
                
              ) #end fluidRow
              
              
      ), #end tabitem
      
      
      #district data tab
      tabItem(tabName ="tab6",
              
              fluidRow( 
                
                
                h2("District-Wise Analysis of All Indian States",align="center"),
		br(),
                p("(Please Note: Few states like Nagaland, Sikkim, Daman and Diu, Dadra and Nagar Haveli have no reported cases so they do not have any district data collected.)", align="center"),
                br(),
                br(),
                
                box(
                  
                  width = 12,
                  selectInput("district_state", label = "Please Select Desired State",choices = StateCOVID_19$State[-1])
                ), #end box
                
                # box(
                
                
                #  width = 12,
                # selectInput("district", label = "Select District", choices = NULL)
                
                
                
                #), #end box
                
                
                
                box(
                  
                  h3("Active Cases In Each District",align="center"),
                  br(),
                  width = 6,
                  highchartOutput("district_active")
                  
                ),
                
                
                box(
                  
                  h3("Confirmed Cases In Each District",align="center"),
                  br(),
                  width = 6,
                  highchartOutput("district_confirmed")
                  
                ),
                
                
                box(
                  
                  h3("Death Cases In Each District",align="center"),
                  br(),
                  width = 6,
                  highchartOutput("district_dead")
                  
                ),
                
                box(
                  
                  h3("Recovered Cases In Each District",align="center"),
                  br(),
                  width = 6,
                  highchartOutput("district_recovered")
                  
                ) ,
                
                
                br(),
                br(),
                br(),
                
                h3("Deaths Rates And Recovery Rates Of Each District",align="center"),
                
                box(
                  
                  width = 12,
                  highchartOutput("district_recovery_rate")
                  
                  
                ) ,
                
                br(),
                br(),
                
                box(
                  
                  width = 12,
                  highchartOutput("district_death_rate")
                  
                  
                ) 
                
                
                
              ) #end fluid row
            
      ), #end tabitem6
      
      tabItem(
        tabName="tab7",
        
        box(
          
          width = 12,
          h2("Data Sources:"),
          a("Ministry Of Health And Family Welfare, Government Of India",href="https://www.mohfw.gov.in/",target="_blank"),br(),
          a("COVID-19 India API",href="https://api.covid19india.org/",target="_blank"),br(),
          a("Check the source code here",href="https://github.com/VEDANTGHODKE/COVID-19-India-Dashboard",target="_blank"),
          br(),
          h5("Developed And Designed By Vedant Ghodke"),
          tags$ol(
            tags$li(a("Vedant Ghodke : (Personal Website)",href="https://vedantghodke.github.io",target="_blank")),
	    tags$li(a("Vedant Ghodke : (Github)",href="https://github.com/VEDANTGHODKE",target="_blank")),
            tags$li(a("Vedant Ghodke : (LinkedIn)",href="https://www.linkedin.com/in/vedantghodke/",target="_blank"))
          
              ) #end tags
          
                   
        ) #end box
        
      ) #end tab7
      
    ) #end tabitems
  ) #end dashboardBody
  
  
  
) #end dashboardPage
