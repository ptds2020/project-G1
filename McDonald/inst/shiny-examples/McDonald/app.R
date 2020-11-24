
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
library(McDonald)

sidebar <- dashboardSidebar(width = 150,
                            sidebarMenu(
                              menuItem("McDonald", tabName = "McDonald"),
                              menuItem("User", tabName = "User")
                            ))

body <- dashboardBody(tabItems(
  tabItem(
    tabName = "McDonald",
    h2("McDonald"),
    fluidRow(column(
      3,
      box(
        title = "Menus",
        solidHeader = T,
        width = 14,
        collapsible = T,
        selectizeInput(
          inputId = "burger",
          label = "Burgers:",
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
        selectizeInput(
          inputId = "snack",
          label = "Snacks:",
          choices = c("-", "Frites Min", "Frites Sma", "Frites Med", "Chicken Wings")
        ),

        selectizeInput(
          inputId = "sauce",
          label = "Sauces:",
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
        ),

        selectizeInput(
          inputId = "salad",
          label = "Salad:",
          choices = c(
            "-",
            "Caesar Salad Veggie",
            "The Crispy Chicken Caesar Salad",
            "The Grilled Chicken Caesar Salad",
            "Caesar Salad Nature",
            "Petite salade verte",
            "Carrottes",
            "Caesar Dressing",
            "French Dressing"
          )
        ),

        selectizeInput(
          inputId = "drink",
          label = "Drinks:",
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
        )
      )
    ),
    column(9, fluidRow(
      box(
        title = "Nutrients",
        solidHeader = T,
        width = 12,
        collapsible = T,
        plotlyOutput("nutrients")
      )
    ))),
    column(3, fluidRow(valueBoxOutput("calories", {
      "margin: 5px"
    })))
  ),

  tabItem(
    tabName = "User",
    h2("User"),
    fluidRow(column(
      3,
      box(
        title = "User",
        solidHeader = T,
        width = 14,
        collapsible = T,

        # selectizeInput(inputId = "age",
        #                label = "Age:",
        #                choices = 0:99),

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
        )
      )
    )),
    fluidRow(column(9, fluidRow(
      valueBoxOutput("bmi")
    ))),
    fluidRow(column(9, fluidRow(
      valueBoxOutput("bmr")
    ))),
    fluidRow(column(9, fluidRow(
      valueBoxOutput("needs")
    )))
  )
))

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

ui <- dashboardPage(dashboardHeader(title = "Nutrition Calculator"),
                    sidebar, body)


server <- function(input, output) {
  MacD <- read_excel(here("data/MacD.xlsx"))

  # value boxes
  output$calories <- renderValueBox({
    kcal <- MacD %>%
      select(Kcal, name) %>%
      filter(
        name %in% input$drink |
          name %in% input$salad |
          name %in% input$snack |
          name %in% input$burger | name %in% input$sauce
      ) %>%
      summarise(Kcal = sum(Kcal)) %>%
      pull(Kcal)

    valueBox("Kcal",
             paste0(kcal, " kcal"),
             icon = icon("fire"),
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
          name %in% input$burger | name %in% input$sauce
      ) %>%
      summarise(
        Proteine = sum(proteine),
        Glucides = sum(glucides),
        Sucres = sum(sucre),
        Lipides = sum(lipides),
        Acide_Gras = sum(acides_gras_Sat),
        Fibres = sum(fibres),
        Sel = sum(sel)
      )
    # pull(Proteine, Glucides, Sucres, Lipides, Acide_Gras, Fibres, Sel)

    d <- d %>% melt()

    # req(input$name)
    plottest <- d %>% ggplot(aes(variable, value)) +
      geom_bar(stat = "identity", fill = "#E69F00") +
      theme_bw()
    ggplotly(plottest)
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

    # x <- bmi_size
    # y <- bmi_weight

    # bmi <- function(x, y) {
    #   bmi_final <- (y / (x * x)) * 10000
    #   bmi_final
    # }

    bmi <- bmi(x, y) %>% round(2) %>% as.numeric()


    valueBox("BMI",
             paste0(bmi, " BMI"),
             icon = icon("fitness"),
             color = "blue")
  })

  output$bmr <- renderValueBox({
    size <- 1:300 %>% as.data.frame()
    names(size) <- "size"
    weight <- 1:300 %>% as.data.frame()
    names(weight) <- "weight"
    bodyfat <- (1:50)
    leanfactoremale <-
      c(rep(1, 14), rep(0.95, 6), rep(0.90, 8), rep(0.85, 22)) %>% as.data.frame()
    names(leanfactoremale) <- "leanfactor"
    leanfactorfemale <-
      c(rep(1, 18), rep(0.95, 10), rep(0.90, 10), rep(0.85, 12)) %>% as.data.frame()
    names(leanfactorfemale) <- "leanfactor"
    male <- rep("Male", 50) %>% as_data_frame()
    valuemalerep <- rep(1, 50) %>% as_data_frame()
    names(male) <- "gender"
    names(valuemalerep) <- "value"

    female <- rep("Female", 50) %>% as_data_frame()
    valuefemalerep <- rep(0.9, 50) %>% as_data_frame()
    names(female) <- "gender"
    names(valuefemalerep) <- "value"


    measuresmale <-
      cbind(bodyfat, leanfactoremale, male, valuemalerep)
    measuresfemale <-
      cbind(bodyfat, leanfactorfemale, female, valuefemalerep)
    measures <- rbind(measuresmale, measuresfemale)

    # taille <- 183
    # poids <- 75

    x <- size %>%
      filter(size %in% input$size) %>%
      summarise(size = size)

    y <- weight %>%
      filter(weight %in% input$weight) %>%
      summarise(weight = weight)
    #
    # x <- bmi_size
    # y <- bmi_weight
    #
    # bmi <- function(x, y) {
    #   bmi_final <- (y / (x * x)) * 10000
    #   bmi_final
    # }

    bmi <- bmi(x, y) %>% round(0) %>% as.numeric()
    # sexe <- "male"
    d <- 24
    leanfactor <- measures %>%
      filter(bodyfat == bmi, gender %in% input$gender) %>%
      summarise(leanfactor)

    coefficient <- measures %>%
      filter(bodyfat == bmi, gender %in% input$gender) %>%
      summarise(value)


    # bmr <- function(y, coefficient, d, leanfactor) {
    #   bmr_final <- (y * coefficient * d * leanfactor)
    #   bmr_final
    # }
    #
    bmr <- bmr(y, coefficient, d, leanfactor)



    valueBox("BMR",
             paste0(bmr, " BMR"),
             icon = icon("fitness"),
             color = "red")
  })

  output$needs <- renderValueBox({
    size <- 1:300 %>% as.data.frame()
    names(size) <- "size"
    weight <- 1:300 %>% as.data.frame()
    names(weight) <- "weight"
    bodyfat <- (1:50)
    leanfactoremale <-
      c(rep(1, 14), rep(0.95, 6), rep(0.90, 8), rep(0.85, 22)) %>% as.data.frame()
    names(leanfactoremale) <- "leanfactor"
    leanfactorfemale <-
      c(rep(1, 18), rep(0.95, 10), rep(0.90, 10), rep(0.85, 12)) %>% as.data.frame()
    names(leanfactorfemale) <- "leanfactor"
    male <- rep("Male", 50) %>% as_data_frame()
    valuemalerep <- rep(1, 50) %>% as_data_frame()
    names(male) <- "gender"
    names(valuemalerep) <- "value"

    female <- rep("Female", 50) %>% as_data_frame()
    valuefemalerep <- rep(0.9, 50) %>% as_data_frame()
    names(female) <- "gender"
    names(valuefemalerep) <- "value"


    measuresmale <-
      cbind(bodyfat, leanfactoremale, male, valuemalerep)
    measuresfemale <-
      cbind(bodyfat, leanfactorfemale, female, valuefemalerep)
    measures <- rbind(measuresmale, measuresfemale)

    # taille <- 183
    # poids <- 75

    x <- size %>%
      filter(size %in% input$size) %>%
      summarise(size = size)

    y <- weight %>%
      filter(weight %in% input$weight) %>%
      summarise(weight = weight)
    #
    # x <- bmi_size
    # y <- bmi_weight
    #
    # bmi <- function(x, y) {
    #   bmi_final <- (y / (x * x)) * 10000
    #   bmi_final
    # }

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


    # bmr <- function(y, coefficient, d, leanfactor) {
    #   bmr_final <- (y * coefficient * d * leanfactor)
    #   bmr_final
    # }

    # bmr function
    bmr <- bmr(y, coefficient, d, leanfactor)


    activity <- c("Very light", "Light", "Moderate", "Heavy", "Very heavy") %>% as.data.frame()
    names(activity) <- "activity"
    values <- c(1.3, 1.55, 1.65, 1.80, 2.0) %>% as.data.frame()
    names(values) <- "values"
    sport <- cbind(activity, values)

    # what <- "moderate"
    multiplier <- sport %>%
      filter(activity %in% input$activity) %>% summarise(values)


    # needs <- function(bmr, multiplier) {
    #   needs_final <- (bmr * multiplier)
    #   needs_final
    # }

    needs <- needs(bmr, multiplier)



    valueBox(
      "Calorie need",
      paste0(needs, " Calorie need"),
      icon = icon("fitness"),
      color = "olive"
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
