
# remotes::install_github("ptds2020/project-G1") #Necessary to deploy the shiny app on shinyapps.io

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
library(ECharts2Shiny)
library(shinyWidgets)
library(McDonald)


sidebar <- dashboardSidebar(width = 150,
                            sidebarMenu(
                              menuItem("McDonald", tabName = "McDonald"),
                              menuItem("User data", tabName = "User"),
                              menuItem("Summary", tabName = "Summary"),
                              menuItem("Information", tabName = "Information")

                            ))

body <- dashboardBody(tabItems(
  tabItem(
    tabName = "McDonald",
    fluidRow(
      column(style = "overflow-y: auto; position:fixed; width:300px; top:0; bottom:0;; right:0",
             3,
             box(
               title = "Select your Menu",
               solidHeader = T,
               width = 14,
               collapsible = T,
               selectizeInput(
                 inputId = "burger",
                 label = "Burgers",
                 choices = c(
                   "-",
                   "Big Mac",
                   "Big Mac Bacon",
                   "Double Big Mac",
                   "Double Big Mac Bacon",
                   "Cheesburger Royal",
                   "Big Tasty Single",
                   "Big Tasty Single Bacon",
                   "Big Tasty Double",
                   "Big Tasty Double Bacon",
                   "Hamburger avec pain sans gluten",
                   "Cheeseburger avec pain sans gluten",
                   "Double Cheeseburger avec pain sans gluten",
                   "Double Cheeseburger",
                   "Cheeseburger",
                   "Hamburger",
                   "Homestyle Crispy chicken Honey Mustard",
                   "Homestyle Crispy Chicken Tomato",
                   "McChicken",
                   "Homestyle Crispy Chicken Tenders",
                   "Chicken McNuggets 4p",
                   "Chicken McNuggets 6p",
                   "Chicken McNuggets 9p",
                   "Chicken McNuggets 20p",
                   "Homestyle Honey Mustard Veggie",
                   "Homestyle Tomato Veggie",
                   "McVeggie",
                   "McMuffin",
                   "Bacon & Egg McMuffin",
                   "Filet-O-Fish"
                 )
               ),

               tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), chooseSliderSkin("Modern", color = "orange"), sliderInput("servingburger", "", min = 0, max = 5, value = 1),

               selectizeInput(
                 inputId = "burgerbis",
                 label = "Second burger choice",
                 choices = c(
                   "-",
                   "Big Mac",
                   "Big Mac Bacon",
                   "Double Big Mac",
                   "Double Big Mac Bacon",
                   "Cheesburger Royal",
                   "Big Tasty Single",
                   "Big Tasty Single Bacon",
                   "Big Tasty Double",
                   "Big Tasty Double Bacon",
                   "Hamburger avec pain sans gluten",
                   "Cheeseburger avec pain sans gluten",
                   "Double Cheeseburger avec pain sans gluten",
                   "Double Cheeseburger",
                   "Cheeseburger",
                   "Hamburger",
                   "Homestyle Crispy chicken Honey Mustard",
                   "Homestyle Crispy Chicken Tomato",
                   "McChicken",
                   "Homestyle Crispy Chicken Tenders",
                   "Chicken McNuggets 4p",
                   "Chicken McNuggets 6p",
                   "Chicken McNuggets 9p",
                   "Chicken McNuggets 20p",
                   "Homestyle Honey Mustard Veggie",
                   "Homestyle Tomato Veggie",
                   "McVeggie",
                   "McMuffin",
                   "Bacon & Egg McMuffin",
                   "Filet-O-Fish"
                 ),
               ), sliderInput("servingburgerbis", "", min = 0, max = 5, value = 1, step = 1),
               selectizeInput(
                 inputId = "snack",
                 label = "Snacks",
                 choices = c("-", "Frites Min", "Frites Sma", "Frites Med", "Chicken Wings")
               ),sliderInput("servingsnack", "", min = 0, max = 5, value = 1, step = 1),
               selectizeInput(
                 inputId = "snackbis",
                 label = "Second snack choice",
                 choices = c("-", "Frites Min", "Frites Sma", "Frites Med", "Chicken Wings")
               ),sliderInput("servingsnackbis", "", min = 0, max = 5, value = 1, step = 1),
               selectizeInput(
                 inputId = "sauce",
                 label = "Sauces",
                 choices = c(
                   "-",
                   "Ketchup",
                   "Sauce Pommes Frites",
                   "Sauce Barbecue",
                   "Sauce Aigre-douce",
                   "Sauce Cocktail",
                   "Sauce Curry",
                   "Sauce Deluxe Potatoes",
                   "Sauce Moutarde",
                   "Hot Devil Sauce"
                 )
               ), sliderInput("servingsauce", "", min = 0, max = 5, value = 1, step = 1), selectizeInput(
                 inputId = "saucebis",
                 label = "Second sauce choice",
                 choices = c(
                   "-",
                   "Ketchup",
                   "Sauce Pommes Frites",
                   "Sauce Barbecue",
                   "Sauce Aigre-douce",
                   "Sauce Cocktail",
                   "Sauce Curry",
                   "Sauce Deluxe Potatoes",
                   "Sauce Moutarde",
                   "Hot Devil Sauce"
                 )
               ), sliderInput("servingsaucebis", "", min = 0, max = 5, value = 1, step = 1),

               selectizeInput(
                 inputId = "salad",
                 label = "Salad",
                 choices = c(
                   "-",
                   "Caesar Salad Veggie",
                   "The Crispy Chicken Caesar Salad",
                   "The Grilled Chicken Caesar Salad",
                   "Caesar Salad Nature",
                   "Petite salade verte",
                   "Carrottes"
                 )
               ), sliderInput("servingsalad", "", min = 0, max = 5, value = 1, step = 1 ),
               selectizeInput(
                 inputId = "saladsauce",
                 label = "Salad Sauce",
                 choices = c(
                   "-",
                   "Caesar Dressing",
                   "French Dressing"
                 )
               ), sliderInput("servingsaladsauce", "", min = 0, max = 5, value = 1, step = 1),

               selectizeInput(
                 inputId = "drink",
                 label = "Drinks",
                 choices = c(
                   "-",
                   "Coca-Cola mini",
                   "Coca-Cola Small",
                   "Coca-Cola Medium",
                   "Coca-Cola Zero mini",
                   "Coca-Cola Zero Small",
                   "Coca-Cola Zero Medium",
                   "Sprite Zero mini",
                   "Sprite Zero Small",
                   "Sprite Zero Medium",
                   "Fanta Zero mini",
                   "Fanta Zero Small",
                   "Fanta Zero Medium",
                   "Lipton Ice Tea mini",
                   "Lipton Ice Tea Small",
                   "Lipton Ice Tea Medium",
                   "Henniez Naturelle",
                   "Henniez Légère",
                   "Jus d'orange Tropicana",
                   "Schorle Pomme",
                   "Smoothie Bottled Tropical Chia",
                   "Smoothie Bottled Berries",
                   "Red Bull",
                   "Frappé Vanille Small",
                   "Frappé Vanille Regular",
                   "Frappé Fraise Small",
                   "Frappé Fraise Regular",
                   "Frappé Mocca Small",
                   "Frappé Mocca Regular",
                   "Ristretto",
                   "Espresso",
                   "Café Crème",
                   "Cappuccino Small",
                   "Cappuccino Regular",
                   "Latte Macchiato Small",
                   "Latte Macchiato Regular",
                   "Café renversé",
                   "Chocolat Chaud Small",
                   "Chocolat Chaud Regular",
                   "Thé"
                 )
               ), sliderInput("servingdrink", "", min = 0, max = 5, value = 1, step = 1), selectizeInput(
                 inputId = "dessert",
                 label = "Dessert",
                 choices = c(
                   "-",
                   "Frappé Vanille Small",
                   "Frappé Vanille Regular",
                   "Frappé Fraise Small",
                   "Frappé Fraise Regular",
                   "Frappé Mocca Small",
                   "Frappé Mocca Regular",
                   "McFlurry M&M's Regular",
                   "McFlurry M&M's Maxi",
                   "McFlurry Celebrations Regular",
                   "McFlurry Celebrations Maxi",
                   "McFlurry Oreo Regular",
                   "McFlurry Oreo Maxi",
                   "Sundae Cailler Mini",
                   "Sundae Cailler Regular",
                   "Sundae Caramel Mini ",
                   "Sundae Caramel Regular",
                   "Sundae Fraise Mini",
                   "Sundae Fraise  Regular",
                   "Sundae Nature Mini",
                   "Sundae Nature Regular",
                   "Donut fourré au Nutella",
                   "Donut sucre",
                   "Chausson aux pommes",
                   "Compote de Fruits")
               ), sliderInput("servingdessert", "", min = 0, max = 5, value = 1, step = 1)
             )
      ),
      column(9,
             fluidRow(
               box(
                 title = "Nutrients",
                 solidHeader = T,
                 width = 12,
                 collapsible = T,
                 plotlyOutput("nutrients")
               ))
             ,(fluidRow(valueBoxOutput("calories", width = 12), width = 12))
      )
    )

  ),

  tabItem(
    tabName = "User",
    fluidRow(column(
      4,
      box(
        title = "Your data",
        solidHeader = T,
        width = 16,
        collapsible = T,

        selectizeInput(
          inputId = "gender",
          label = "Gender:",
          choices = c("Male", "Female"),
          selected="Male"
        ),

        selectizeInput(
          inputId = "size",
          label = "Size:",
          choices = 100:250,
          selected=180
        ),

        selectizeInput(
          inputId = "weight",
          label = "Weight:",
          choices = 20:300,
          selected=75
        ),

        selectizeInput(
          inputId = "activity",
          label = "Activity:",
          choices = c("Very light", "Light", "Moderate", "Heavy", "Very heavy"),
          selected="Moderate"
        ),
        h2("BMI classification", align = "center"),
        img(src = "bmi_image_copy.png",height="100%", width="100%", style="display: block; margin-left: auto; margin-right: auto;")


      )),
      column(8, fluidRow(
        box(
          title = "BMI",
          solidHeader = T,
          width = 12,
          collapsible = T,
          plotlyOutput("nutri")

        )),
        valueBoxOutput("bmi", width = 4),
        valueBoxOutput("bmr", width = 4),
        valueBoxOutput("needs", width = 4))
    )),
  tabItem(
    tabName = "Summary",
    fluidRow(column(
      12,
      box(
        title = "Your daily calorie situation",
        solidHeader = T,
        width = 14,
        collapsible = T,
        plotlyOutput("recap")
      )
    )
    ),
    fluidRow(column(
      12,
      box(
        title = "Walking time",
        img(src = "walking_man.png", style="display: block; margin-left: auto; margin-right: auto;", width = 70, height = 70, align = "left"),
        solidHeader = T,
        width = 14,
        collapsible = T,
        textOutput("walk"),align = "center",
        tags$head(tags$style("#walk{font-size: 17px
                                 }", align = "left"
        )
        )
      )))
  ),
  tabItem(tabName = "Information",
          fluidRow(column(
            4,
            box(
              title = "BMI information", solidHeader = T,
              width = 14,
              height = 200,
              collapsible = T,
              textOutput("bmi_information"),align = "left",
              tags$head(tags$style("#walk{font-size: 17px
                                 }", align = "left"
              )
              )
            )),
            column(
              4,
              box(
                title = "BMR information",solidHeader = T,
                width = 14,
                height = 200,
                collapsible = T,
                textOutput("bmr_information"), align = "left",
                tags$head(tags$style("#walk{font-size: 17px
                                 }", align = "left"
                )
                )
              )),

            column(
              4,
              box(
                title = "Calorie need", solidHeader = T,
                width = 14,
                height = 200,
                collapsible = T,
                textOutput("cal_need"),align = "left",
                tags$head(tags$style("#walk{font-size: 17px
                                 }", align = "left"
                )
                )
              ))
          ))
))


ui <- dashboardPage(dashboardHeader(title = "Nutrition Calculator"),
                    sidebar, body)

server <- function(input, output) {
  MacD <- read_excel(here("extdata/MacD.xlsx"))

  output$calories <- renderValueBox({
    kcal <- MacD %>%
      select(Kcal, name) %>%
      filter(
        name %in% input$drink |
          name %in% input$salad |
          name %in% input$snack |
          name %in% input$snackbis |
          name %in% input$burger |
          name %in% input$burgerbis |
          name %in% input$saladsauce |
          name %in% input$saucebis |
          name %in% input$sauce |
          name %in% input$dessert

      ) %>% mutate(
        Servingburger = ifelse(name %in% input$burger, as.numeric(input$servingburger), 0),
        Servingburgerbis = ifelse(name %in% input$burgerbis, as.numeric(input$servingburgerbis), 0),
        Servingsnack = ifelse(name %in% input$snack, as.numeric(input$servingsnack), 0),
        Servingsnackbis = ifelse(name %in% input$snackbis, as.numeric(input$servingsnackbis), 0),
        Servingsauce = ifelse(name %in% input$sauce, as.numeric(input$servingsauce), 0),
        Servingsaucebis = ifelse(name %in% input$saucebis, as.numeric(input$servingsaucebis), 0),
        Servingsalad = ifelse(name %in% input$salad, as.numeric(input$servingsalad), 0),
        Servingsaladsauce = ifelse(name %in% input$saladsauce, as.numeric(input$servingsaladsauce), 0),
        Servingdrink = ifelse(name %in% input$drink, as.numeric(input$servingdrink), 0),
        Servingdessert = ifelse(name %in% input$dessert, as.numeric(input$servingdessert), 0),

        Totalburger = ((as.numeric(Kcal) * Servingburger)),
        Totalburgerbis = ((as.numeric(Kcal) * Servingburgerbis)),
        Totalsnack = ((as.numeric(Kcal) * Servingsnack)),
        Totalsnackbis = ((as.numeric(Kcal) * Servingsnackbis)),
        Totalsauce = ((as.numeric(Kcal) * Servingsauce)),
        Totalsaucebis = ((as.numeric(Kcal) * Servingsaucebis)),
        Totalsalad = ((as.numeric(Kcal) * Servingsalad)),
        Totalsaladsauce = ((as.numeric(Kcal) * Servingsaladsauce)),
        Totaldrink = ((as.numeric(Kcal) * Servingdrink)),
        Totaldessert = ((as.numeric(Kcal) * Servingdessert))

      ) %>%
      summarise(Kcal = sum(Totalburger, Totalburgerbis, Totalsnack, Totalsnackbis, Totalsauce, Totalsaucebis, Totalsalad, Totalsaladsauce,Totaldrink, Totaldessert)) %>%
      pull(Kcal)

    valueBox("Kcal",
             paste0(kcal, " kcal"),
             icon = icon("fas fa-fire-alt"),
             color = "yellow")
  })


  output$nutrients <- renderPlotly({
    d <- MacD %>%
      select(proteine,
             glucides,
             sucre,
             lipides,
             acides_gras_Sat,
             fibres,
             sel,
             name) %>%
      filter(
        name %in% input$drink |
          name %in% input$salad |
          name %in% input$snack |
          name %in% input$snackbis |
          name %in% input$burger |
          name %in% input$burgerbis |
          name %in% input$saladsauce |
          name %in% input$saucebis |
          name %in% input$sauce |
          name %in% input$dessert
      ) %>%
      mutate(
        Servingburger = ifelse(name %in% input$burger, as.numeric(input$servingburger), 0),
        Servingburgerbis = ifelse(name %in% input$burgerbis, as.numeric(input$servingburgerbis), 0),
        Servingsnack = ifelse(name %in% input$snack, as.numeric(input$servingsnack), 0),
        Servingsnackbis = ifelse(name %in% input$snackbis, as.numeric(input$servingsnackbis), 0),
        Servingsauce = ifelse(name %in% input$sauce, as.numeric(input$servingsauce), 0),
        Servingsaucebis = ifelse(name %in% input$saucebis, as.numeric(input$servingsaucebis), 0),
        Servingsalad = ifelse(name %in% input$salad, as.numeric(input$servingsalad), 0),
        Servingsaladsauce= ifelse(name %in% input$saladsauce, as.numeric(input$servingsaladsauce), 0),
        Servingdrink = ifelse(name %in% input$drink, as.numeric(input$servingdrink), 0),
        Servingdessert = ifelse(name %in% input$dessert, as.numeric(input$servingdessert), 0))


    d <-  d %>% mutate(Totalburgerprot = ((as.numeric(proteine) * Servingburger)),
                       Totalburgergluc = ((as.numeric(glucides) * Servingburger)),
                       Totalburgersug = ((as.numeric(sucre) * Servingburger)),
                       Totalburgerlip = ((as.numeric(lipides) * Servingburger)),
                       Totalburgeracid = ((as.numeric(acides_gras_Sat) * Servingburger)),
                       Totalburgerfiber = ((as.numeric(fibres) * Servingburger)),
                       Totalburgersalt = ((as.numeric(sel) * Servingburger)),

                       Totalburgerbisprot = ((as.numeric(proteine) * Servingburgerbis)),
                       Totalburgerbisgluc = ((as.numeric(glucides) * Servingburgerbis)),
                       Totalburgerbissug = ((as.numeric(sucre) * Servingburgerbis)),
                       Totalburgerbislip = ((as.numeric(lipides) * Servingburgerbis)),
                       Totalburgerbisacid = ((as.numeric(acides_gras_Sat) * Servingburgerbis)),
                       Totalburgerbisfiber = ((as.numeric(fibres) * Servingburgerbis)),
                       Totalburgerbissalt = ((as.numeric(sel) * Servingburgerbis)),

                       Totalsnackprot = ((as.numeric(proteine) * Servingsnack)),
                       Totalsnackgluc = ((as.numeric(glucides) * Servingsnack)),
                       Totalsnacksug = ((as.numeric(sucre) * Servingsnack)),
                       Totalsnacklip = ((as.numeric(lipides) * Servingsnack)),
                       Totalsnackacid = ((as.numeric(acides_gras_Sat) * Servingsnack)),
                       Totalsnackfiber = ((as.numeric(fibres) * Servingsnack)),
                       Totalsnacksalt = ((as.numeric(sel) * Servingsnack)),

                       Totalsnackbisprot = ((as.numeric(proteine) * Servingsnackbis)),
                       Totalsnackbisgluc = ((as.numeric(glucides) * Servingsnackbis)),
                       Totalsnackbissug = ((as.numeric(sucre) * Servingsnackbis)),
                       Totalsnackbislip = ((as.numeric(lipides) * Servingsnackbis)),
                       Totalsnackbisacid = ((as.numeric(acides_gras_Sat) * Servingsnackbis)),
                       Totalsnackbisfiber = ((as.numeric(fibres) * Servingsnackbis)),
                       Totalsnackbissalt = ((as.numeric(sel) * Servingsnackbis)),

                       Totalsauceprot = ((as.numeric(proteine) * Servingsauce)),
                       Totalsaucegluc = ((as.numeric(glucides) * Servingsauce)),
                       Totalsaucesug = ((as.numeric(sucre) * Servingsauce)),
                       Totalsaucelip = ((as.numeric(lipides) * Servingsauce)),
                       Totalsauceacid = ((as.numeric(acides_gras_Sat) * Servingsauce)),
                       Totalsaucefiber = ((as.numeric(fibres) * Servingsauce)),
                       Totalsaucesalt = ((as.numeric(sel) * Servingsauce)),

                       Totalsaucebisprot = ((as.numeric(proteine) * Servingsaucebis)),
                       Totalsaucebisgluc = ((as.numeric(glucides) * Servingsaucebis)),
                       Totalsaucebissug = ((as.numeric(sucre) * Servingsaucebis)),
                       Totalsaucebislip = ((as.numeric(lipides) * Servingsaucebis)),
                       Totalsaucebisacid = ((as.numeric(acides_gras_Sat) * Servingsaucebis)),
                       Totalsaucebisfiber = ((as.numeric(fibres) * Servingsaucebis)),
                       Totalsaucebissalt = ((as.numeric(sel) * Servingsaucebis)),

                       Totalsaladprot = ((as.numeric(proteine) * Servingsalad)),
                       Totalsaladgluc = ((as.numeric(glucides) * Servingsalad)),
                       Totalsaladsug = ((as.numeric(sucre) * Servingsalad)),
                       Totalsaladlip = ((as.numeric(lipides) * Servingsalad)),
                       Totalsaladacid = ((as.numeric(acides_gras_Sat) * Servingsalad)),
                       Totalsaladfiber = ((as.numeric(fibres) * Servingsalad)),
                       Totalsaladsalt = ((as.numeric(sel) * Servingsalad)),

                       Totalsaladsauceprot = ((as.numeric(proteine) * Servingsaladsauce)),
                       Totalsaladsaucegluc = ((as.numeric(glucides) * Servingsaladsauce)),
                       Totalsaladsaucesug = ((as.numeric(sucre) * Servingsaladsauce)),
                       Totalsaladsaucelip = ((as.numeric(lipides) * Servingsaladsauce)),
                       Totalsaladsauceacid = ((as.numeric(acides_gras_Sat) * Servingsaladsauce)),
                       Totalsaladsaucefiber = ((as.numeric(fibres) * Servingsaladsauce)),
                       Totalsaladsaucesalt = ((as.numeric(sel) * Servingsaladsauce)),

                       Totaldrinkprot = ((as.numeric(proteine) * Servingdrink)),
                       Totaldrinkgluc = ((as.numeric(glucides) * Servingdrink)),
                       Totaldrinksug = ((as.numeric(sucre) * Servingdrink)),
                       Totaldrinklip = ((as.numeric(lipides) * Servingdrink)),
                       Totaldrinkacid = ((as.numeric(acides_gras_Sat) * Servingdrink)),
                       Totaldrinkfiber = ((as.numeric(fibres) * Servingdrink)),
                       Totaldrinksalt = ((as.numeric(sel) * Servingdrink)),

                       Totaldessertprot = ((as.numeric(proteine) * Servingdessert)),
                       Totaldessertgluc = ((as.numeric(glucides) * Servingdessert)),
                       Totaldessertsug = ((as.numeric(sucre) * Servingdessert)),
                       Totaldessertlip = ((as.numeric(lipides) * Servingdessert)),
                       Totaldessertacid = ((as.numeric(acides_gras_Sat) * Servingdessert)),
                       Totaldessertfiber = ((as.numeric(fibres) * Servingdessert)),
                       Totaldessertsalt = ((as.numeric(sel) * Servingdessert)))



    e <- d %>%
      summarise(
        Proteine = sum(sum(Totaldessertprot), sum(Totalburgerprot), sum(Totalburgerbisprot), sum(Totalsnackprot), sum(Totalsnackbisprot), sum(Totalsauceprot), sum(Totalsaucebisprot), sum(Totalsaladprot), sum(Totalsaladsauceprot),sum(Totaldrinkprot)),
        Glucides = sum(sum(Totaldessertgluc), sum(Totalburgergluc), sum(Totalburgerbisgluc), sum(Totalsnackgluc), sum(Totalsnackbisgluc), sum(Totalsaucegluc), sum(Totalsaucebisgluc), sum(Totalsaladgluc), sum(Totalsaladsaucegluc) ,sum(Totaldrinkgluc)),
        Sucres = sum(sum(Totaldessertsug), sum(Totalburgersug), sum(Totalburgerbissug), sum(Totalsnacksug), sum(Totalsnackbissug), sum(Totalsaucesug), sum(Totalsaucebissug), sum(Totalsaladsug), sum(Totalsaladsaucesug), sum(Totaldrinksug)),
        Lipides = sum(sum(Totaldessertlip), sum(Totalburgerlip), sum(Totalburgerbislip), sum(Totalsnacklip), sum(Totalsnackbislip), sum(Totalsaucelip), sum(Totalsaucebislip), sum(Totalsaladlip), sum(Totalsaladsaucelip), sum(Totaldrinklip)),
        Acide_Gras = sum(sum(Totaldessertacid), sum(Totalburgeracid), sum(Totalburgerbisacid), sum(Totalsnackacid), sum(Totalsnackbisacid), sum(Totalsauceacid), sum(Totalsaucebisacid), sum(Totalsaladacid), sum(Totalsaladsauceacid), sum(Totaldrinkacid)),
        Fibres = sum(sum(Totaldessertfiber), sum(Totalburgerfiber), sum(Totalburgerbisfiber), sum(Totalsnackfiber), sum(Totalsnackbisfiber), sum(Totalsaucefiber), sum(Totalsaucebisfiber), sum(Totalsaladfiber), sum(Totalsaladsaucefiber), sum(Totaldrinkfiber)),
        Sel = sum(sum(Totaldessertsalt), sum(Totalburgersalt), sum(Totalburgerbissalt), sum(Totalsnacksalt), sum(Totalsnackbissalt), sum(Totalsaucesalt), sum(Totalsaucebissalt), sum(Totalsaladsalt), sum(Totalsaladsaucesalt), sum(Totaldrinksalt))
      )

    e <- e %>% melt()

    plottest <- e %>% ggplot(aes(variable, value)) +
      geom_bar(stat = "identity", fill = "#E69F00") +
      theme_bw() + labs(y= "Value in grams", x = "Nutrient")
    ggplotly(plottest)
  })

  output$nutri <- renderPlotly({
    size <- 1:300 %>% as.data.frame()
    names(size) <- "size"
    weight <- 1:300 %>% as.data.frame()
    names(weight) <- "weight"

    x <- size %>%
      filter(size %in% input$size) %>%
      summarise(size = size)

    y <- weight %>%
      filter(weight %in% input$weight) %>%
      summarise(weight = weight)


    bmi_val <- bmi(x, y) %>% round(2) %>% as.numeric()


    fig <- plot_ly(
      type = "indicator",
      mode = "gauge+number+delta",
      value = bmi_val,
      delta = list(reference = 25, increasing = list(color = "red")),
      gauge = list(
        axis = list(range = list(NULL, 40), tickwidth = 1, tickcolor = "black"),
        bar = list(color = "black"),
        bgcolor = "white",
        borderwidth = 2,
        bordercolor = "gray",
        steps = list(
          list(range = c(0, 18.49), color = "#00FFCC"),
          list(range = c(18.5, 24.99), color = "#33CC33"),
          list(range = c(25, 29.99), color = "#FF9900"),
          list(range = c(30, 34.99), color = "#FF6600"),
          list(range = c(35, 40), color = "red"))))
    fig <- fig %>%
      layout(
        margin = list(l=20,r=30),
        paper_bgcolor = "white",
        font = list(color = "darkblue", family = "Arial"))

    ggplotly(fig)
  })


  output$bmi <- renderValueBox({
    size <- 1:300 %>% as.data.frame()
    names(size) <- "size"
    weight <- 1:300 %>% as.data.frame()
    names(weight) <- "weight"

    x <- size %>%
      filter(size %in% input$size) %>%
      summarise(size = size)

    y <- weight %>%
      filter(weight %in% input$weight) %>%
      summarise(weight = weight)

    bmi <- bmi(x, y) %>% round(2) %>% as.numeric()


    valueBox("BMI",
             paste(bmi),
             icon = icon(""),
             color = "blue")
  })

  output$bmr <- renderValueBox({
    size <- 1:300 %>% as.data.frame()
    names(size) <- "size"
    weight <- 1:300 %>% as.data.frame()
    names(weight) <- "weight"
    bodyfat <- (1:100)
    leanfactoremale <-
      c(rep(1, 14), rep(0.95, 6), rep(0.90, 8), rep(0.85, 72)) %>% as.data.frame()
    names(leanfactoremale) <- "leanfactor"
    leanfactorfemale <-
      c(rep(1, 18), rep(0.95, 10), rep(0.90, 10), rep(0.85, 62)) %>% as.data.frame()
    names(leanfactorfemale) <- "leanfactor"
    male <- rep("Male", 100) %>% as_data_frame()
    valuemalerep <- rep(1, 100) %>% as_data_frame()
    names(male) <- "gender"
    names(valuemalerep) <- "value"

    female <- rep("Female", 100) %>% as_data_frame()
    valuefemalerep <- rep(0.9, 100) %>% as_data_frame()
    names(female) <- "gender"
    names(valuefemalerep) <- "value"


    measuresmale <-
      cbind(bodyfat, leanfactoremale, male, valuemalerep)
    measuresfemale <-
      cbind(bodyfat, leanfactorfemale, female, valuefemalerep)
    measures <- rbind(measuresmale, measuresfemale)


    x <- size %>%
      filter(size %in% input$size) %>%
      summarise(size = size)

    y <- weight %>%
      filter(weight %in% input$weight) %>%
      summarise(weight = weight)


    bmi <- bmi(x, y) %>% round(0) %>% as.numeric()
    d <- 24
    leanfactor <- measures %>%
      filter(bodyfat == bmi, gender %in% input$gender) %>%
      summarise(leanfactor)

    coefficient <- measures %>%
      filter(bodyfat == bmi, gender %in% input$gender) %>%
      summarise(value)


    bmr <- bmr(y, coefficient, d, leanfactor)


    valueBox("BMR",
             paste(bmr),
             icon = icon("fitness"),
             color = "red")
  })

  output$needs <- renderValueBox({
    size <- 1:300 %>% as.data.frame()
    names(size) <- "size"
    weight <- 1:300 %>% as.data.frame()
    names(weight) <- "weight"
    bodyfat <- (1:100)
    leanfactoremale <-
      c(rep(1, 14), rep(0.95, 6), rep(0.90, 8), rep(0.85, 72)) %>% as.data.frame()
    names(leanfactoremale) <- "leanfactor"
    leanfactorfemale <-
      c(rep(1, 18), rep(0.95, 10), rep(0.90, 10), rep(0.85, 62)) %>% as.data.frame()
    names(leanfactorfemale) <- "leanfactor"
    male <- rep("Male", 100) %>% as_data_frame()
    valuemalerep <- rep(1, 100) %>% as_data_frame()
    names(male) <- "gender"
    names(valuemalerep) <- "value"

    female <- rep("Female", 100) %>% as_data_frame()
    valuefemalerep <- rep(0.9, 100) %>% as_data_frame()
    names(female) <- "gender"
    names(valuefemalerep) <- "value"


    measuresmale <-
      cbind(bodyfat, leanfactoremale, male, valuemalerep)
    measuresfemale <-
      cbind(bodyfat, leanfactorfemale, female, valuefemalerep)
    measures <- rbind(measuresmale, measuresfemale)


    x <- size %>%
      filter(size %in% input$size) %>%
      summarise(size = size)

    y <- weight %>%
      filter(weight %in% input$weight) %>%
      summarise(weight = weight)

    #bmi function
    bmi <- bmi(x, y) %>% round(0) %>% as.numeric()
    sexe <- "male"
    d <- 24
    leanfactor <- measures %>%
      filter(bodyfat == bmi, gender %in% input$gender) %>%
      summarise(leanfactor)

    coefficient <- measures %>%
      filter(bodyfat == bmi, gender %in% input$gender) %>%
      summarise(value)

    # bmr function
    bmr <- bmr(y, coefficient, d, leanfactor)


    activity <- c("Very light", "Light", "Moderate", "Heavy", "Very heavy") %>% as.data.frame()
    names(activity) <- "activity"
    values <- c(1.3, 1.55, 1.65, 1.80, 2.0) %>% as.data.frame()
    names(values) <- "values"
    sport <- cbind(activity, values)

    multiplier <- sport %>%
      filter(activity %in% input$activity) %>% summarise(values)

    #need function
    needs <- needs(bmr, multiplier)

    valueBox(
      "Calorie need",
      paste(needs, "kcal"),
      icon = icon("fitness"),
      color = "olive"
    )
  })

  output$recap <- renderPlotly({
    MacD <- read_excel(here("extdata/MacD.xlsx"))

    kcal <- MacD %>%
      select(Kcal, name) %>%
      filter(
        name %in% input$drink |
          name %in% input$salad |
          name %in% input$snack |
          name %in% input$snackbis |
          name %in% input$burger |
          name %in% input$burgerbis |
          name %in% input$saladsauce |
          name %in% input$saucebis |
          name %in% input$sauce |
          name %in% input$dessert

      ) %>% mutate(
        Servingburger = ifelse(name %in% input$burger, as.numeric(input$servingburger), 0),
        Servingburgerbis = ifelse(name %in% input$burgerbis, as.numeric(input$servingburgerbis), 0),
        Servingsnack = ifelse(name %in% input$snack, as.numeric(input$servingsnack), 0),
        Servingsnackbis = ifelse(name %in% input$snackbis, as.numeric(input$servingsnackbis), 0),
        Servingsauce = ifelse(name %in% input$sauce, as.numeric(input$servingsauce), 0),
        Servingsaucebis = ifelse(name %in% input$saucebis, as.numeric(input$servingsaucebis), 0),
        Servingsalad = ifelse(name %in% input$salad, as.numeric(input$servingsalad), 0),
        Servingsaladsauce = ifelse(name %in% input$saladsauce, as.numeric(input$servingsaladsauce), 0),
        Servingdrink = ifelse(name %in% input$drink, as.numeric(input$servingdrink), 0),
        Servingdessert = ifelse(name %in% input$dessert, as.numeric(input$servingdessert), 0),

        Totalburger = ((as.numeric(Kcal) * Servingburger)),
        Totalburgerbis = ((as.numeric(Kcal) * Servingburgerbis)),
        Totalsnack = ((as.numeric(Kcal) * Servingsnack)),
        Totalsnackbis = ((as.numeric(Kcal) * Servingsnackbis)),
        Totalsauce = ((as.numeric(Kcal) * Servingsauce)),
        Totalsaucebis = ((as.numeric(Kcal) * Servingsaucebis)),
        Totalsalad = ((as.numeric(Kcal) * Servingsalad)),
        Totalsaladsauce = ((as.numeric(Kcal) * Servingsaladsauce)),
        Totaldrink = ((as.numeric(Kcal) * Servingdrink)),
        Totaldessert = ((as.numeric(Kcal) * Servingdessert))

      ) %>%
      summarise(Kcal = sum(Totalburger, Totalburgerbis, Totalsnack, Totalsnackbis, Totalsauce, Totalsaucebis, Totalsalad, Totalsaladsauce,Totaldrink, Totaldessert)) %>%
      pull(Kcal)

    size <- 1:300 %>% as.data.frame()
    names(size) <- "size"
    weight <- 1:300 %>% as.data.frame()
    names(weight) <- "weight"
    bodyfat <- (1:100)
    leanfactoremale <-
      c(rep(1, 14), rep(0.95, 6), rep(0.90, 8), rep(0.85, 72)) %>% as.data.frame()
    names(leanfactoremale) <- "leanfactor"
    leanfactorfemale <-
      c(rep(1, 18), rep(0.95, 10), rep(0.90, 10), rep(0.85, 62)) %>% as.data.frame()
    names(leanfactorfemale) <- "leanfactor"
    male <- rep("Male", 100) %>% as_data_frame()
    valuemalerep <- rep(1, 100) %>% as_data_frame()
    names(male) <- "gender"
    names(valuemalerep) <- "value"

    female <- rep("Female", 100) %>% as_data_frame()
    valuefemalerep <- rep(0.9, 100) %>% as_data_frame()
    names(female) <- "gender"
    names(valuefemalerep) <- "value"


    measuresmale <-
      cbind(bodyfat, leanfactoremale, male, valuemalerep)
    measuresfemale <-
      cbind(bodyfat, leanfactorfemale, female, valuefemalerep)
    measures <- rbind(measuresmale, measuresfemale)

    x <- size %>%
      filter(size %in% input$size) %>%
      summarise(size = size)

    y <- weight %>%
      filter(weight %in% input$weight) %>%
      summarise(weight = weight)

    bmi <- bmi(x, y) %>% round(0) %>% as.numeric()
    d <- 24
    leanfactor <- measures %>%
      filter(bodyfat == bmi, gender %in% input$gender) %>%
      summarise(leanfactor)

    coefficient <- measures %>%
      filter(bodyfat == bmi, gender %in% input$gender) %>%
      summarise(value)

    bmr <- bmr(y, coefficient, d, leanfactor)

    activity <- c("Very light", "Light", "Moderate", "Heavy", "Very heavy") %>% as.data.frame()
    names(activity) <- "activity"
    values <- c(1.3, 1.55, 1.65, 1.80, 2.0) %>% as.data.frame()
    names(values) <- "values"
    sport <- cbind(activity, values)

    multiplier <- sport %>%
      filter(activity %in% input$activity) %>% summarise(values)

    needs <- needs(bmr, multiplier)
    names(needs) <- "vec"
    kcal <- data.frame(kcal)
    names(kcal) <- "vec"
    tofeed <- needs-kcal
    names(tofeed) <- "vec"
    df1<- rbind(kcal, tofeed)
    df <- data.frame(df1,label=c("Number of consummed calories", "Number of remaining calories"))

    fig <- plot_ly(df, labels = ~label, values = ~vec, type = 'pie', marker = list(colors = c('#E69F00', '#182844')))
    fig <- fig %>% layout(
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

    ggplotly(fig)

  })

  output$walk <- renderText({

    kcal <- MacD %>%
      select(Kcal, name) %>%
      filter(
        name %in% input$drink |
          name %in% input$salad |
          name %in% input$snack |
          name %in% input$snackbis |
          name %in% input$burger |
          name %in% input$burgerbis |
          name %in% input$saladsauce |
          name %in% input$saucebis |
          name %in% input$sauce |
          name %in% input$dessert

      ) %>% mutate(
        Servingburger = ifelse(name %in% input$burger, as.numeric(input$servingburger), 0),
        Servingburgerbis = ifelse(name %in% input$burgerbis, as.numeric(input$servingburgerbis), 0),
        Servingsnack = ifelse(name %in% input$snack, as.numeric(input$servingsnack), 0),
        Servingsnackbis = ifelse(name %in% input$snackbis, as.numeric(input$servingsnackbis), 0),
        Servingsauce = ifelse(name %in% input$sauce, as.numeric(input$servingsauce), 0),
        Servingsaucebis = ifelse(name %in% input$saucebis, as.numeric(input$servingsaucebis), 0),
        Servingsalad = ifelse(name %in% input$salad, as.numeric(input$servingsalad), 0),
        Servingsaladsauce = ifelse(name %in% input$saladsauce, as.numeric(input$servingsaladsauce), 0),
        Servingdrink = ifelse(name %in% input$drink, as.numeric(input$servingdrink), 0),
        Servingdessert = ifelse(name %in% input$dessert, as.numeric(input$servingdessert), 0),

        Totalburger = ((as.numeric(Kcal) * Servingburger)),
        Totalburgerbis = ((as.numeric(Kcal) * Servingburgerbis)),
        Totalsnack = ((as.numeric(Kcal) * Servingsnack)),
        Totalsnackbis = ((as.numeric(Kcal) * Servingsnackbis)),
        Totalsauce = ((as.numeric(Kcal) * Servingsauce)),
        Totalsaucebis = ((as.numeric(Kcal) * Servingsaucebis)),
        Totalsalad = ((as.numeric(Kcal) * Servingsalad)),
        Totalsaladsauce = ((as.numeric(Kcal) * Servingsaladsauce)),
        Totaldrink = ((as.numeric(Kcal) * Servingdrink)),
        Totaldessert = ((as.numeric(Kcal) * Servingdessert))

      ) %>%
      summarise(Kcal = sum(Totalburger, Totalburgerbis, Totalsnack, Totalsnackbis, Totalsauce, Totalsaucebis, Totalsalad, Totalsaladsauce,Totaldrink, Totaldessert)) %>%
      pull(Kcal)

    weight <- 1:300 %>% as.data.frame()
    names(weight) <- "weight"

    y <- weight %>%
      filter(weight %in% input$weight) %>%
      summarise(weight = weight)

    u <- round(kcal/((3*3.5*y)/200))
    v <- round(kcal/((4.5*3.5*y)/200))

    paste("To burn the calories absorbed, you need to walk:", u, "minute(s) at a normal pace or", v, "minute(s) at a fast pace.")
  })

  output$bmi_information <- renderText({
    "Body Mass Index (BMI) is a person’s weight in
    kilograms divided by the square of height in meters.
    A high BMI can be an indicator of high body fatness.
    BMI can be used to screen for weight categories that
    may lead to health problems but it is not diagnostic
    of the body fatness or health of an individual."
  })

  output$bmr_information <- renderText({
    "Basal Metabolic Rate, also known as BMR, is the amount
    of energy your body burns at rest on a daily basis.
    In other words, it is the number of calories required to
    keep your body function while you are not doing any physical activities."
  })


  output$cal_need <- renderText({
    "A calorie is the amount of heat needed to raise the temperature of
    one gram of water by one degree Celsius. Calories in food provide
    energy in the form of heat so that our bodies can function.
    Our bodies store and burn calories as fuel. Many people who are
    trying to reach or maintain a healthy weight will count calories and
    try to decrease caloric intake."
  })

}

# Run the application
shinyApp(ui = ui, server = server)

