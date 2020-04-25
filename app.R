library(shiny)
bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)
print("aha")

ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", min = 0, max = 100,
                  value = c(25, 40), pre = "$"),
      radioButtons("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      selectInput("countryInput", "Country",
                  choices = c("CANADA", "FRANCE", "ITALY"))),
    mainPanel(plotOutput("coolplot"),
              br(),
              br(),
              tableOutput("results"))
  )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)
