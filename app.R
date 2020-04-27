### Load Packages
require(shiny)
require(shinydashboard)
require(shinythemes)
require(leaflet)
require(data.table)
require(htmltools)
#require(jsonlite)
require(rgdal)

### Data sources:
# https://github.com/ppatrzyk/polska-geojson


### Load Data
#z <- rgdal::readOGR("https://raw.githubusercontent.com/ppatrzyk/polska-geojson/master/wojewodztwa/wojewodztwa-min.geojson")
#mazowieckie <- c(172, 479, 2302, 52.15, 21.01)
#slaskie <- c(93, 170, 1813, 50.253118, 19.029845)
#dolnoslaskie <- c(40, 132, 1278, 51.110359, 17.032347)
#wielkopolskie <- c(75, 167, 1266, 52.406363, 16.925176)
#zachodniopomorskie <- c(,,, 53.439023, 14.570372)
#woj.data4 <- rbind(mazowieckie, slaskie, dolnoslaskie, wielkopolskie)
#colnames(woj.data4) <- c("zgony", "wyleczeni", "zakazeni", "lat", "lng")
#woj.data4
#woj.dt4 <- as.data.table(woj.data)

#ww <- c("mazowieckie", "slaskie", "dolnoslaskie", "wielkopolskie", "lodzkie",
#        "malopolskie", "kujawsko-pomorskie", "opolskie", "zachodniopomorskie",
#        "podlaskie", "pomorskie", "lubelskie", "podkarpackie", "swietokrzyskie",
#        "warminsko-mazurskie", "lubuskie")

#populacja <- c(5349.1, 4570.8, 2904.2, 3475.3, 2493.6, 3372.6, 2086.2, 996, 1710.5,
#               1188.8, 2307.7, 2139.7, 2127.6, 1257.2, 1439.7, 1018)

#woj.dt.all <- cbind("wojewodztwo"=ww, rbind(woj.dt4), populacja)
#woj.dt.all[,"zakazeni na 100k"] <- woj.dt.all[, zakazeni/populacja] * 100
#write.csv(woj.dt.all, "woj_dt_all.csv")
w <- fread("woj_dt_all.csv")[,-1]
w[,"zakazeni na 100k"] <- w[, zakazeni/populacja] * 100

#ww[c(2,8,4,9, 14,7, 10, 3,13, 6, 11, 15, 5, 1,12,16)]
ord <- c(2,8,4,9, 14,7, 10, 3,13, 6, 11, 15, 5, 1,12,16)


#################################################################################
ui <- dashboardPage(
  skin="black",
  dashboardHeader(
    title="Poland Voivodeship Covid-19 Data",
    titleWidth="38%"),
  dashboardSidebar(
    selectInput(inputId = "zmienna",
                label="Rodzaj danych:",
                choices = c("Zgony" = "zgony",
                  "Zakazeni" = "zakazeni",
                  "Wyleczeni" = "wyleczeni",
                  "Zakazeni na 100k" = "zakazeni na 100k")
    ),
    radioButtons(inputId = "calcType",
                 label = "Transformacja danych:",
                choices = c("Logarytmiczna" = "lg",
                            "Brak" = "zw"),
                
    )
  ),
  dashboardBody(
    leafletOutput("leaflet_map"),
    br(),
    #br(),
    tableOutput("results")
  )
)

server <- function(input, output) {
  
  output$leaflet_map <- renderLeaflet({
  pal <- colorNumeric("Reds", NULL)
  st <- w[[input$zmienna]][ord]
  
  ### Transformation - radioButton input
  if(input$calcType == "zw") {
    stpal <- st
  } else if (input$calcType == "lg") {
    ### add a pseudocount where 0 (just for colors generation,
    ### it will not affect the results on hover not the results in the table)
    tmp <- w[[input$zmienna]][ord]
    tmp[tmp==0] <- 1
    stpal <- log10(tmp)
  }
  ### Plot the map:
  leaflet(z) %>%
     addTiles() %>% setView(lng = 19.1, lat = 52.14, zoom = 6) %>%
     addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.3,
                 fillColor = ~pal(stpal),
                 dashArray = "",
                 highlightOptions = highlightOptions(
                   weight = 5, dashArray = "",
                   color="#667",
                   fillOpacity = 0.7,
                   bringToFront = TRUE
                   ), 
                 label = paste0(w[["wojewodztwo"]][ord], ": ", formatC(st, big.mark = ","))) %>%
    addLegend(pal = pal, values = st, opacity = 1.0, title=input$zmienna,
                              labFormat = labelFormat(transform = function(x) round(x)))
   })
   
   output$results <- renderTable({
     #input$calcType},
     #as.data.frame(st)},
     as.data.frame(w)[,c("wojewodztwo","zgony","wyleczeni","zakazeni", "populacja", "zakazeni na 100k")]},
     hover = TRUE,
     colnames = TRUE,
     digits = 0
   )

#   output$leaflet_map <- renderPlot({
#     hist(rnorm(30))
#   })
  
    
  #leaflet(data = woj.dt) %>% setView(lng = 19.1, lat = 52.14, zoom = 6) %>% addTiles() %>%
  #  addMarkers(~lng, ~lat, popup = ~as.character(mag), label = ~as.character(mag))
  #leaflet(data = woj.dt) %>% setView(lng = 19.1, lat = 52.14, zoom = 6) %>% addTiles() %>%
  #  +   addMarkers(~lng, ~lat)
  
  
}

shinyApp(ui = ui, server = server)
