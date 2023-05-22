
# Load Packages -----------------------------------------------------------

library(ggplot2)
library(gapminder)
library(ggforce)
library(gh)
library(globals)
library(openintro)
library(profvis) 
library(RSQLite)
library(shiny)
library(shinycssloaders)
library(shinyFeedback)
library(shinythemes)
library(testthat)
library(thematic)
library(tidyverse)
library(vroom)
library(waiter)
library(xml2)
library(zeallot)


# Chapter 1 - Your first shiny app -------------------------------------------------------

ui <- fluidPage(
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
  verbatimTextOutput("summary"),
  tableOutput("table")
)
server <- function(input, output, session) {
  # Create a reactive expression
  dataset <- reactive({
    get(input$dataset, "package:datasets")
  })
  
  output$summary <- renderPrint({
    # Use a reactive expression by calling it like a function
    summary(dataset())
  })
  
  output$table <- renderTable({
    dataset()
  })
}
shinyApp(ui, server)


# Exercises #1 ---------------------------------------------------------------

##Exercise 1

ui <- fluidPage(
  textInput("name", "What's your name?"),
  textOutput("greeting")
)
server <- function(input, output, session) {
  output$greeting <- renderText({
    paste0("Hello ", input$name)
  })
}
shinyApp(ui, server)


##Exerciese 2

ui <- fluidPage(
  sliderInput('x', label = "If x is", min = 1, max = 50, value = 30),
  "then x times 5 is",
  textOutput("product")
)

server <- function(input, output, session) {
  output$product <- renderText({ 
    input$x * 5
  })
}

shinyApp(ui, server)


##Exercise 3

ui <- fluidPage(
  sliderInput('x', label = "If x is", min = 1, max = 50, value = 30),
  sliderInput("y", label = "and y is", min = 1, max = 50, value = 5),
  "then x times y is",
  textOutput("product")
)

server <- function(input, output, session) {
  output$product <- renderText({ 
    input$x * input$y
  })
}

shinyApp(ui, server)


##Exercise 4


ui <- fluidPage(
  sliderInput("x", "If x is", min = 1, max = 50, value = 30),
  sliderInput("y", "and y is", min = 1, max = 50, value = 5),
  "then, (x * y) is", textOutput("product"),
  "and, (x * y) + 5 is", textOutput("product_plus5"),
  "and (x * y) + 10 is", textOutput("product_plus10")
)

server <- function(input, output, session) {
  
  product <- reactive(input$x * input$y)
  
  output$product <- renderText({
    product()
  })
  output$product_plus5 <- renderText({ 
    product() + 5
  })
  output$product_plus10 <- renderText({ 
    product() + 10
  })
}

shinyApp(ui, server)


##Exercise 5

datasets <- c("economics", "faithfuld", "seals")
ui <- fluidPage(
  selectInput("dataset", "Dataset", choices = datasets),
  verbatimTextOutput("summary"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  dataset <- reactive({
    get(input$dataset, "package:ggplot2")
  })
  output$summary <- renderPrint({
    summary(dataset())
  })
  output$plot <- renderPlot({
    plot(dataset())
  }, res = 96)
}

shinyApp(ui, server)


# Chapter 2 - Basic UI ----------------------------------------------------

# Exercises #2 ------------------------------------------------------------


# Exercise 2.1

ui <- fluidPage(
  textInput("name", NULL, placeholder = "Your name"),
  textOutput("greeting")
)
server <- function(input, output, session) {
  output$greeting <- renderText({
    paste0("Hello ", input$name)
  })
}
shinyApp(ui, server)


#Exercise 2.2

ui <- fluidPage(
  sliderInput("date", "When should we deliver?", 
              value = as.Date("2020-09-17"), 
              min = as.Date("2020-09-16"), 
              max = as.Date("2020-09-23"))
)
server <- function(input, output, session) {
  output$greeting <- renderText({
    paste0("Hello ", input$name)
  })
}
shinyApp(ui, server)


#Exercise 2.3

ui <- fluidPage(
  sliderInput("number", NULL, 
              value = 0, 
              min = 0, 
              max = 100,
              step = 5,
              animate = TRUE)
)
server <- function(input, output, session)
{
  output$number <- renderText({
    paste0("Hello ", input$name)
  })
}
shinyApp(ui, server)


# Exercise 2.4

ui <- fluidPage(
  plotOutput("plot", height = "300px", width = "700px")
)
server <- function(input, output, session) {
  output$plot <- renderPlot(plot(1:5), res = 96)
}
shinyApp(ui, server)


#Exercise 2.5

ui <- fluidPage(
  dataTableOutput("table")
)
server <- function(input, output, session) {
  output$table <- renderDataTable(mtcars, options = list(ordering = FALSE, searching = FALSE))
}
shinyApp(ui, server)


library(reactable)

ui <- fluidPage(
  reactableOutput("table")
)

server <- function(input, output) {
  output$table <- renderReactable({
    reactable(iris)
  })
}

shinyApp(ui, server)


# Chapter 3 ---------------------------------------------------------------

# Exercises Chapter 3 ---------------------------------------------------------------

## Exercise 3.3.6.1

ui <- fluidPage(
  textInput("name", "What's your name?"),
  textOutput("greeting")
)
server1 <- function(input, output, server) {
  greeting <- renderText(paste0("Hello ", input$name))
}

server2 <- function(input, output, server) {
  greeting <- renderText(paste0("Hello ", input$name))
  output$greeting <- renderText("greeting")
}

server3 <- function(input, output, server) {
  output$greeting <- renderText(paste0("Hello ", input$name))
}

shinyApp(ui, server3)

##Exercise 3.3.6.2

var <- reactive(df[[input$var]])
range <- reactive(range(var(), na.rm = TRUE))


# Chapter 3 - Code 3.4.2 --------------------------------------------------


freqpoly <- function(x1, x2, binwidth = 0.1, xlim = c(-3, 3)) {
  df <- data.frame(
    x = c(x1, x2),
    g = c(rep("x1", length(x1)), rep("x2", length(x2)))
  )
  
  ggplot(df, aes(x, colour = g)) +
    geom_freqpoly(binwidth = binwidth, size = 1) +
    coord_cartesian(xlim = xlim)
}

t_test <- function(x1, x2) {
  test <- t.test(x1, x2)
  
  # use sprintf() to format t.test() results compactly
  sprintf(
    "p value: %0.3f\n[%0.2f, %0.2f]",
    test$p.value, test$conf.int[1], test$conf.int[2]
  )
}

ui <- fluidPage(
  fluidRow(
    column(4, 
           "Distribution 1",
           numericInput("n1", label = "n", value = 1000, min = 1),
           numericInput("mean1", label = "µ", value = 0, step = 0.1),
           numericInput("sd1", label = "σ", value = 0.5, min = 0.1, step = 0.1)
    ),
    column(4, 
           "Distribution 2",
           numericInput("n2", label = "n", value = 1000, min = 1),
           numericInput("mean2", label = "µ", value = 0, step = 0.1),
           numericInput("sd2", label = "σ", value = 0.5, min = 0.1, step = 0.1)
    ),
    column(4,
           "Frequency polygon",
           numericInput("binwidth", label = "Bin width", value = 0.1, step = 0.1),
           sliderInput("range", label = "range", value = c(-3, 3), min = -5, max = 5)
    )
  ),
  fluidRow(
    column(9, plotOutput("hist")),
    column(3, verbatimTextOutput("ttest"))
  )
)

server <- function(input, output, session) {
  output$hist <- renderPlot({
    x1 <- rnorm(input$n1, input$mean1, input$sd1)
    x2 <- rnorm(input$n2, input$mean2, input$sd2)
    
    freqpoly(x1, x2, binwidth = input$binwidth, xlim = input$range)
  }, res = 96)
  
  output$ttest <- renderText({
    x1 <- rnorm(input$n1, input$mean1, input$sd1)
    x2 <- rnorm(input$n2, input$mean2, input$sd2)
    
    t_test(x1, x2)
  })
}

shinyApp(ui, server)


ui <- fluidPage(
  fluidRow(
    column(3, 
           numericInput("lambda1", label = "lambda1", value = 3),
           numericInput("lambda2", label = "lambda2", value = 5),
           numericInput("n", label = "n", value = 1e4, min = 0),
           actionButton("simulate", "Simulate!")
    ),
    column(9, plotOutput("hist"))
  )
)

server <- function(input, output, session) {
  x1 <- eventReactive(input$simulate, {
    rpois(input$n, input$lambda1)
  })
  x2 <- eventReactive(input$simulate, {
    rpois(input$n, input$lambda2)
  })
  
  output$hist <- renderPlot({
    freqpoly(x1(), x2(), binwidth = 1, xlim = c(0, 40))
  }, res = 96)
}

shinyApp(ui, server)


# Chapter 4: Case study ---------------------------------------------------

library(shiny)
library(vroom)
library(tidyverse)

# import dataset
dir.create("neiss")
#> Warning in dir.create("neiss"): 'neiss' already exists
#download <- function(name) { url <- "https://github.com/hadley/mastering-shiny/tree/main/neiss" download.file(paste0(url, name), paste0("neiss/", name), quiet = TRUE)}
#download("https://github.com/hadley/mastering-shiny/blob/main/neiss/injuries.tsv.gz", "/neiss")
#download.file("https://github.com/hadley/mastering-shiny/blob/main/neiss/population.tsv", "/neiss")
#download.file("https://github.com/hadley/mastering-shiny/blob/main/neiss/products.tsv", "/neiss")

injuries <- vroom::vroom("neiss/injuries.tsv.gz")
injuries

products <- vroom::vroom("neiss/products.tsv")
products

population <- vroom::vroom("neiss/population.tsv")
population

## Exploration

selected <- injuries %>% filter(prod_code == 649)
nrow(selected)

selected %>% count(location, wt = weight, sort = TRUE)
selected %>% count(body_part, wt = weight, sort = TRUE)
selected %>% count(diag, wt = weight, sort = TRUE)

summary <- selected %>% 
  count(age, sex, wt = weight)
summary
summary %>% 
  ggplot(aes(age, n, colour = sex)) + 
  geom_line() + 
  labs(y = "Estimated number of injuries")

summary <- selected %>% 
  count(age, sex, wt = weight) %>% 
  left_join(population, by = c("age", "sex")) %>% 
  mutate(rate = n / population * 1e4)

summary %>% 
  ggplot(aes(age, rate, colour = sex)) + 
  geom_line(na.rm = TRUE) + 
  labs(y = "Injuries per 10,000 people")
