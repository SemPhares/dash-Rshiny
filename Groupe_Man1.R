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


data <- data %>% 
  mutate(Date_Commande = as.Date(df$Date.de.commande, format="%d/%m/%Y", tz="UTC"),
         Date_Expedition = as.Date(df$Date.d.expédition, format="%d/%m/%Y", tz="UTC"),
         year = format(Date_Commande, "%Y"))





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
        menuItem("Données", icon = icon("database"), href = "https://github.com/SemPhares/dash-Rshiny"),
        menuItem("Team", tabName = "Team", icon = icon("table"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(  "vue",
                  
   #Prémière ligne             
    fluidRow(
      infoBox(
        title = " " ,# "Profit N-1" ,
        width = 6,
        fill = TRUE,
        color = "black",
        icon = icon("usd"),
        value = tableOutput("profitN1")
        
        
      ),
      
      infoBox(
        title = " ", # "Vente N-1" ,
        width =6 ,
        fill = TRUE,
        color = "black",
        icon = icon("usd"),
        value = tableOutput("venteN1")
        
        
      ),
      selectInput('Annee', 'Selectionner une année: ', choices = c( 2018, 2019,2020,2021) )
      
      
      
    ),            
                  
                  
  #seconde ligne             
     fluidRow(      
          box(
            title = "Evolution du CA",
            footer = "en euro €",
            status = "info",
            solidHeader = FALSE,
            width = 8,
            plotOutput("evolution")
          ),
          infoBox(
            title = "Progression",
            value = tableOutput("ventes"),
            subtitle = "Entre 2018 et 2021",
            icon = icon("line-chart"),
            fill = TRUE,
            color = "black",
            width = 4
          )
          ),
        ##Troisième ligne 
        
        fluidRow( 
          infoBox(
            title = "CA par Région",
            value = tableOutput("Reg_CA"),
            subtitle = "CA par Région",
            icon = icon("usd"),
            fill = TRUE,
            color = "black",
            width = 6
          ),
          infoBox(
            title = "Profit par Catégorie des clients: ",
            value = plotOutput("Camen1"), 
            width = 6,
            color = "black"
            
          )
            
         )
        ),
        
        
        #### les Items dans la page ( le filtre ) Top 
        tabItem(
          "top",
          
          infoBox( title =  "Top Zone Géographique : ", 
                   width = 4, 
                   value =  tableOutput("topzone"),
                   color = "light-blue",
                    ),
          
          
          infoBox( title =  "Top 10 Sous Catégories de Produits : ", 
                   width = 4, 
                   value =  tableOutput("souscat"),
                   color = "orange",
          ),
          
          infoBox( title =  "Top 10 Villes : ", 
                   width = 4, 
                   value =  tableOutput("ville"),
                   color = "purple",
          ),
          
          
      ),
        
        tabItem(
          "Team",
          infoBox(title = "DIALLO Abdramane Aly", width = 3, color = "black",
                  href = "https://www.linkedin.com/in/abdramane-aly-diallo-748a4aab/"),
          infoBox(title = "EGLO LOKOH Sem", width =3 ,color = "black",
                  href = "https://www.linkedin.com/in/sem-egloh-lokoh/"),
          
          infoBox(title = "ONGANIE André", width = 3, color = "black",
                  href = "https://www.linkedin.com/in/andr%C3%A9-ongani-81304614b/"),
          
          infoBox(title = "OUTSKI Manal", width = 3,color = "black",
                  href = "https://www.linkedin.com/in/manal-outski-7b35991b2/")
      )
      )
    ),
    title = "Hypermarchés Dashboard",
    skin = "red"
  ),
  
  
  
  server = function(input, output) {
    
    
 output$profitN1 <- renderTable(
   data %>% 
     filter(year ==  (input$Annee )   ) %>% 
     summarise( Profit = sum(Profit))
 )    
    
 

 
 output$venteN1 <- renderTable(
   data %>% 
     filter(year ==  (input$Annee )   ) %>% 
     summarise( Vente = sum(Ventes))
 ) 
    
    
    
    output$evolution <- renderPlot({ 
      
      plot( x= evol_CA$year,y = evol_CA$CA, type = "l", main = "Volume du CA",
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
    
   # mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")
    
    #Camen %>%
     # ggplot() + aes(Segment, Profit2) +
    #  geom_bar(width = 1,stat = "identity", color = c("yellow","blue","green"))+
    #  coord_polar("y",start = 0) +
    #  geom_text(aes(y = `Profit2`, label = `Segment`), color = "White") +
    #  scale_fill_manual(values = mycols) 
    
    
    
    ### Code pour le camembert
    
    output$Camen1 <- renderPlot({  pie(Camen$Profit2, Camen$Segment )
      
      
      
      
  #### Codes pour les tops 
      
  output$topzone <-   renderTable({ data %>% 
      group_by( Zone_Géographique = data$`Zone géographique`  ) %>%
      summarise(Profit = sum(Profit) ) %>%
    arrange(desc(Profit))
  })
    
  output$souscat <-   renderTable({ data %>% 
      group_by( Sous_Catégorie = data$`Sous-catégorie`  ) %>%
      summarise(Profit = sum(Profit) ) %>%
      arrange(desc(Profit)) %>%
      top_n(10)
  })  
  
  
  output$ville <-   renderTable({ data %>% 
        group_by( Top_Ville = data$Ville  ) %>%
      summarise(Profit = sum(Profit) ) %>%
      arrange(desc(Profit) )%>%
      top_n(10)
    
  })
  
    
    })
    
  })
