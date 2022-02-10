rm(list=ls())
# 
# Load the shinydashboard package
library(tidyverse)
library(dplyr)
library(plotly)
library(shinydashboard)
library(shiny)

#test maps
library(leaflet)
library(ggplot2)
library(grid)
library(rworldmap)


### Data: 

df <- read.csv2( "C:/Users/abdra/OneDrive/Documents/Projet/Hypermarché_Achats.csv",
                 encoding = "UTF-8", 
                 header = TRUE, 
                 stringsAsFactors = FALSE
)

df <- df %>% 
  mutate(Date_Commande = as.Date(df$Date.de.commande, format="%d/%m/%Y", tz="UTC"),
         Date_Expedition = as.Date(df$Date.d.expédition, format="%d/%m/%Y", tz="UTC"))


####### Computing : 

##### Ventes N-1

df %>%
  mutate( year = format(Date_Commande, "%Y")) %>%
  group_by(year) %>%
  summarise(Ventes_year = sum( Ventes))

##### Profit N-1
df %>%
  mutate( year = format(Date_Commande, "%Y")) %>%
  group_by(year) %>%
  summarise(Profit_year = sum(Profit))



#####  Chiffre d'Affaires
df %>%
  
  summarise(CA = sum(Quantité * Ventes))



##### CA par Region 

df %>%
  group_by(Région) %>%
  summarise(CA = sum(Quantité * Ventes))

df %>%
  group_by(Pays.Région) %>%
  summarise(CA = sum(Quantité * Ventes))





##### Profits par clients Categories: 


df %>%
  group_by(Segment) %>%
  summarise(CA = sum(Profit))






#####################################
head <- dashboardHeader(title = "My Dashboard"
                        )


####################################
# Sidebar items configuration

sidebar <- dashboardSidebar( 
  sidebarMenu( id = "tabs",
   menuItem(strong("Year"), icon = icon("calendar"), tabName = "Year"),
   menuItem(strong("Month"), icon = icon("calendar"), tabName = "Months"),
   menuItem(strong("Day"), icon = icon("calendar"), tabName = "Days"),
   menuItem(strong("About"), icon = icon("cog"), tabName = "About")
)
)


#####################################
# Dashboard Body configuration 

body <- dashboardBody( tags$head(tags$style(HTML('
                              .content-wrapper, .main-body {
                              font-weight: normal;
                              font-size: 18px;
                              } '))),
                       
   tabItems( 
    
    #first tab content == tab year 
    tabItem(tabName = "Year",
            
            fluidRow(
              # First line
            #  column 1
              # Line 1, column is divided in 2 column also :
              #1
              column(6,
                     column(6,
                    infoBox(
                  width = NULL,
                  title = "Profit",
                  subtitle = "Profit N-1"
                )),
                #2
                column(6,
                infoBox(
                  width = NULL,
                  title = "Sales",
                  subtitle = "Sales N-1"
                  ))
              ),
             #column 2 
             column(6,
             infoBox(
                 width = NULL,
                 title = "Total Turnover"
              )               
             ),
             
             
             fluidRow(
          #Second line 
          column(6,
                 infoBox(
                   width = NULL,
                   title = "Table Turnover by Region"
                 )
                 ),
          
          column(6,
                 infoBox(
                   width = NULL,
                   title = "Turnover chart by year "
                 )
          )
            ),
          fluidRow(
            #Third line
            column(6,
                   infoBox(
                     width = NULL,
                     title = "Profit by consumer categories"
                   )),
            column(6,
                   infoBox(
                     width = NULL,
                     title = "Sales Map"
                   ))
            )
            )
    ),
    
    ##Second tab content == tab Month
    tabItem(tabName = "Months",
            
            fluidRow(
              # First line
              #  column 1:
              #1
              column(6,
                     column(6,
                            infoBox(
                              width = NULL,
                              title = "Profit",
                              subtitle = "Profit M-1"
                            )),
                     #2
                     column(6,
                            infoBox(
                              width = NULL,
                              title = "Sales",
                              subtitle = "Sales M-1"
                            ))
              ),
              #column 2 
              column(6,
                     infoBox(
                       width = NULL,
                       title = "Total Turnover"
                     )               
              ),
              
              
              fluidRow(
                #Second line 
                column(6,
                       infoBox(
                         width = NULL,
                         title = "Table Turnover by Region"
                       )
                ),
                
                column(6,
                       infoBox(
                         width = NULL,
                         title = "Turnover chart by Month "
                       )
                )
              ),
              fluidRow(
                #Third line
                column(6,
                       infoBox(
                         width = NULL,
                         title = "Profit by consumer categories"
                       )),
                column(6,
                       infoBox(
                         width = NULL,
                         title = "Sales Map"
                       ))
              )
            )
    ),
    
    
   ###Third tab content == tab Day
   tabItem(tabName = "Days",
           
           fluidRow(
             # First line
             #  column 1:
             #1
             column(6,
                    column(6,
                           infoBox(
                             width = NULL,
                             title = "Profit",
                             subtitle = "Profit J-1"
                           )),
                    #2
                    column(6,
                           infoBox(
                             width = NULL,
                             title = "Sales",
                             subtitle = "Sales J-1"
                           ))
             ),
             #column 2 
             column(6,
                    infoBox(
                      width = NULL,
                      title = "Total Turnover"
                    )               
             ),
             
             
             fluidRow(
               #Second line 
               column(6,
                      infoBox(
                        width = NULL,
                        title = "Table Turnover by Region"
                      )
               ),
               
               column(6,
                      infoBox(
                        width = NULL,
                        title = "Turnover chart by Day "
                      )
               )
             ),
             fluidRow(
               #Third line
               column(6,
                      infoBox(
                        width = NULL,
                        title = "Profit by consumer categories"
                      )),
               column(6,
                      infoBox(
                        width = NULL,
                        title = "Sales Map"
                      ))
             )
           )
   ),
   
   tabItem(tabName = "About"
   
           #This part is about team members presentations and other information 
           #We can delete it, if the team think so
           
           )
   
   )
)

###############################
# UI configuration 

ui <- dashboardPage(header = head,
                    sidebar = sidebar,
                    actionButton("switchtab", "Switch tab"),
                    body = body
)



#This part allow us to change the display of main page by choosing an item in the sidebar 
server <- function(input, output,session){
  
  observeEvent(input$switchtab, {
    
    newtab <- switch(input$tabs, 
                    "Year" = "Month",
                    "Month" = "Day",
                    "Day" = "About",
                    "About" = "Year")
    updateTabItems(session,"tabs",newtab)
    
    
    
  
})
}

######################################
# Launch of the sidebar 

shinyApp(ui, server)


