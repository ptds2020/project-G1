library(shiny)
library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(kableExtra)
library(here)

ui <- fluidPage(

    # Application title
    titlePanel("Name of app"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "burger",
                        label = "Burgers:",
                        choices = c("Big Mac", "Big Mac Bacon","Double Big Mac", "Double Big Mac Bacon","Cheesburger Royal","Big Tasty Single",
                                    "Big Tasty Single Bacon","Big Tasty Double","Big Tasty Double Bacon", "Hamburger avec pain sans gluten","Cheeseburger avec pain sans gluten",
                                    "Double Cheeseburger avec pain sans gluten", "Double Cheeseburger","Cheeseburger","Hamburger", "Homestyle Crispy chicken Honey Mustard",
                                    "Homestyle Crispy Chicken Tomato","McChicken", "Homestyle Crispy Chicken Tenders","Chicken McNuggets 4p","Chicken McNuggets 6p",
                                    "Chicken McNuggets 9p","Chicken McNuggets 20p","Homestyle Honey Mustard Veggie","Homestyle Tomato Veggie","McVeggie","McMuffin","Bacon & Egg McMuffin",
                                    "Filet-O-Fish")), 
            
            selectInput(inputId = "friteSauce",
                        label = "Fries and sauces:",
                        choices = c("Frites Min","Frites Sma","Frites Med","Chicken Wings","Ketchup","Sauce Pommes Frites","Sauce Barbecue","Sauce Aigre-douce","Sauce Cocktail","Sauce Curry",
                                    "Sauce Deluxe Potatoes","Sauce Moutarde","Hot Devil Sauce")),
            
            selectInput(inputId = "salad",
                        label = "Salad:",
                        choices = c("Caesar Salad Veggie","The Crispy Chicken Caesar Salad","The Grilled Chicken Caesar Salad","Caesar Salad Nature","Petite salade verte","Carrottes",
                                    "Caesar Dressing","French Dressing")),
            
            selectInput(inputId = "drink",
                        label = "Drinks:",
                        choices = c("Coca-Cola mini","Coca-Cola Small","Coca-Cola Medium","Coca-Cola Zero mini","Coca-Cola Zero Small","Coca-Cola Zero Medium","Sprite Zero mini","Sprite Zero Small","Sprite Zero Medium","Fanta Zero mini",
                                    "Fanta Zero Small","Fanta Zero Medium","Lipton Ice Tea mini","Lipton Ice Tea Small","Lipton Ice Tea Medium","Henniez Naturelle","Henniez Légère","Jus d'orange Tropicana","Schorle Pomme",
                                    "Smoothie Bottled Tropical Chia","Smoothie Bottled Berries","Red Bull","Frappé Vanille Small","Frappé Vanille Regular","Frappé Fraise Small",
                                    "Frappé Fraise Regular","Frappé Mocca Small","Frappé Mocca Regular","Ristretto","Espresso","Café Crème","Cappuccino Small","Cappuccino Regular","Latte Macchiato Small","Latte Macchiato Regular","Café renversé",
                                    "Chocolat Chaud Small","Chocolat Chaud Regular","Thé" ))
            
        ),
        

        # The plot on the main panel
        mainPanel(
           plotOutput("macPlot")
        )
    )
)

# Was is going to happen
server <- function(input, output) {

    output$macPlot <- renderPlot({
        
        MacD <- read_excel(here("data/MacD.xlsx")) 
        
        df <- MacD[,-c(2,3,13)]
        
        df <- df %>%
            gather(key = macronut, value = value, -name)
        
        selection <- input$burger
        
        df[df[,1] == selection,] %>% 
            ggplot(aes(macronut, value)) +
            geom_bar(stat = "identity", fill = "blue") + 
            theme_bw()
        
        
    })
    
 #   output$macTable <- renderTable({       #à faire encore avec la table. On ne peut pas utiliser kable
 # })
}

# Run the application 
shinyApp(ui = ui, server = server)
