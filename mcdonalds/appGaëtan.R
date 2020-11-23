
library(shiny)
library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(kableExtra)
library(here)
library(shinydashboard)
library(reshape2)
library(plotly)



# Burgers <- rep("Burgers", 29) %>% as.data.frame()
# 
# Fries <- rep("Fries and sauces", 13) %>% as.data.frame()
# 
# Salads <- rep("Salads", 8) %>% as.data.frame()
# 
# Drinks <- rep("Drinks",39 ) %>% as.data.frame()
# 
# df <-rbind(Burgers, Fries, Salads, Drinks)
# names(df) <- "class"
# 
# MacD <- cbind(MacD, df) 


#------------

ui <- dashboardPage(
    dashboardHeader(title = "Nutrition Calculator"),
    dashboardSidebar(
        selectizeInput(inputId = "burger",
                       label = "Burgers:",
                       choices = c("-","Big Mac", "Big Mac Bacon","Double Big Mac", "Double Big Mac Bacon","Cheesburger Royal","Big Tasty Single",
                                   "Big Tasty Single Bacon","Big Tasty Double","Big Tasty Double Bacon", "Hamburger avec pain sans gluten","Cheeseburger avec pain sans gluten",
                                   "Double Cheeseburger avec pain sans gluten", "Double Cheeseburger","Cheeseburger","Hamburger", "Homestyle Crispy chicken Honey Mustard",
                                   "Homestyle Crispy Chicken Tomato","McChicken", "Homestyle Crispy Chicken Tenders","Chicken McNuggets 4p","Chicken McNuggets 6p",
                                   "Chicken McNuggets 9p","Chicken McNuggets 20p","Homestyle Honey Mustard Veggie","Homestyle Tomato Veggie","McVeggie","McMuffin","Bacon & Egg McMuffin",
                                   "Filet-O-Fish")),
        
        selectizeInput(inputId = "snack",
                       label = "Snacks:",
                       choices = c("-","Frites Min","Frites Sma","Frites Med","Chicken Wings")),
        
        selectizeInput(inputId = "sauce",
                       label = "Sauces:",
                       choices = c("-","Ketchup","Sauce Pommes Frites","Sauce Barbecue","Sauce Aigre-douce","Sauce Cocktail","Sauce Curry",
                                   "Sauce Deluxe Potatoes","Sauce Moutarde","Hot Devil Sauce")),
        
        selectizeInput(inputId = "salad",
                       label = "Salad:",
                       choices = c("-","Caesar Salad Veggie","The Crispy Chicken Caesar Salad","The Grilled Chicken Caesar Salad","Caesar Salad Nature","Petite salade verte","Carrottes",
                                   "Caesar Dressing","French Dressing")),
        
        selectizeInput(inputId = "drink",
                       label = "Drinks:",
                       choices = c("-","Coca-Cola mini","Coca-Cola Small","Coca-Cola Medium","Coca-Cola Zero mini","Coca-Cola Zero Small","Coca-Cola Zero Medium","Sprite Zero mini","Sprite Zero Small","Sprite Zero Medium","Fanta Zero mini",
                                   "Fanta Zero Small","Fanta Zero Medium","Lipton Ice Tea mini","Lipton Ice Tea Small","Lipton Ice Tea Medium","Henniez Naturelle","Henniez Légère","Jus d'orange Tropicana","Schorle Pomme",
                                   "Smoothie Bottled Tropical Chia","Smoothie Bottled Berries","Red Bull","Frappé Vanille Small","Frappé Vanille Regular","Frappé Fraise Small",
                                   "Frappé Fraise Regular","Frappé Mocca Small","Frappé Mocca Regular","Ristretto","Espresso","Café Crème","Cappuccino Small","Cappuccino Regular","Latte Macchiato Small","Latte Macchiato Regular","Café renversé",
                                   "Chocolat Chaud Small","Chocolat Chaud Regular","Thé" ))
        
        ),
            
    
    dashboardBody(fluidRow(valueBoxOutput("calories")), 
    fluidRow(
        box(title = "Nutrients", solidHeader=T,
            width = 12, collapsible = T,
            plotlyOutput("nutrients")))
    )
) 
    

server <- function(input, output) {
    MacD <- read_excel(here("data/MacD.xlsx")) 
  
    # value boxes
    output$calories <- renderValueBox({

        kcal <- MacD %>%
            select(Kcal, name) %>%
            filter(name %in% input$drink | name %in% input$salad | name %in% input$snack | name %in% input$burger | name %in% input$sauce) %>%
            summarise(Kcal = sum(Kcal)) %>%
            pull(Kcal)
        
        valueBox("Kcal", paste0(kcal, " kcal"), icon = icon("fire"),
        color = "yellow")
    })
    
    output$nutrients <- renderPlotly({
        d <- MacD %>%
            select(proteine, glucides, sucre, lipides, acides_gras_Sat, fibres, sel, name) %>%
            filter(name %in% input$drink | name %in% input$salad | name %in% input$snack | name %in% input$burger | name %in% input$sauce) %>%
            summarise(Proteine = sum(proteine), Glucides = sum(glucides), Sucres = sum(sucre), Lipides = sum(lipides), Acide_Gras = sum(acides_gras_Sat), Fibres = sum(fibres), Sel = sum(sel)) 
            # pull(Proteine, Glucides, Sucres, Lipides, Acide_Gras, Fibres, Sel)
        
        d <- d %>% melt()
        
            # req(input$name)
        plottest <- d %>% ggplot(aes(variable, value)) +
                geom_bar(stat = "identity", fill = "coral") +
                theme_bw()
        ggplotly(plottest)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


#
