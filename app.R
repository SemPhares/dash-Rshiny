#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

data <- read_delim("~/Pr_Balti/Hypermarché_Achats.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

str(data)

data <- data %>% 
  mutate(Date_Commande = as.Date(df$Date.de.commande, format="%d/%m/%Y", tz="UTC"),
         Date_Expedition = as.Date(df$Date.d.expédition, format="%d/%m/%Y", tz="UTC"))





evol_CA = data %>%
  group_by(year = format(Date_Commande, "%Y")) %>%
  summarise(CA = sum(Quantité*Ventes  , na.rm = T))



shinyApp(
  ui = dashboardPage(
    dashboardHeader(
      title = "Hypermarchés Dashboard",
      titleWidth = 300
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Vue globale", tabName = "vue", icon = icon("dashboard")),
        menuItem("TOPs", tabName = "top", icon = icon("list-ol")),
        menuItem("Données", icon = icon("database"), href = "https://github.com/SemPhares/dash-Rshiny")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          "vue",
          box(
            title = "Evolution du CA",
            footer = "en euro €",
            status = "info",
            solidHeader = TRUE,
            width = 8,
            plotOutput("evolution")
          ),
          infoBox(
            title = "Progression",
            value = tableOutput("ventes"),
            subtitle = "Entre 2018 et 2021",
            icon = icon("line-chart"),
            fill = TRUE,
            color = "light-blue",
            width = 4
          ),
          infoBox(
            title = "CA par Région",
            value = tableOutput("Reg_CA"),
            subtitle = "CA par Région",
            icon = icon("usd"),
            fill = TRUE,
            color = "light-blue",
            width = 4
          ),
          infoBox(
            title = "Profit par Catégorie des clients: ",
            value = plotOutput("Camen1"), 
            width = 4,
            color = "light-blue"
            
            
          )
        ),
        tabItem(
          "top",
          box(title = "Ville", width = 4, "TOP des meilleures villes"),
          box(title = "Année", width = 4, "TOP des meilleurs années"),
          box(title = "Mois", width = 4, "TOP des meilleurs mois")
        )
      )
    ),
    title = "Hypermarchés Dashboard",
    skin = "red"
  ),
  server = function(input, output) {
    output$evolution <- renderPlot({ 
     
       plot( x= evol_CA$year,y = evol_CA$Profits, type = "l", main = "Volume du CA",
            xlab = "Années", ylab = "Chiffres d'Affaires")
      
      
      
      
      #evol_CA %>%
      #ggplot() + aes(x= year,y= Profits) +
      #  geom_histogram() +
        #geom_point()+
        #theme_minimal()+
       # labs(x = "", y = "Volume du CA")
      
    })
      
      
      output$ventes <- renderTable({ 
        data %>%
          mutate( year = format(Date_Commande, "%Y")) %>%
          group_by(year) %>%
          summarise(Ventes = sum( Ventes))
        
      })
        
        output$Reg_CA <- renderTable({ 
          data %>% 
            group_by(`Pays/Région`) %>% 
            summarise(CA = sum(Ventes*Quantité))
      

    })
        
        Camen <-  data %>%
          group_by(Segment) %>% 
          summarise(Profit2 = sum(Profit))
        
        mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")
        
         Camen %>%
            ggplot() + aes(Segment, Profit2) +
            geom_bar(width = 1,stat = "identity", color = c("yellow","blue","green"))+
            coord_polar("y",start = 0) +
            geom_text(aes(y = `Profit2`, label = `Segment`), color = "White") +
            scale_fill_manual(values = mycols) 
          
          
         output$Camen1 <- renderPlot({  pie(Camen$Profit2, Camen$Segment )
        
        
          })
        
  })   
      
  