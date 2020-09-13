library(shiny)
library(png)
library(tidyverse)
source("helpers.R")

# Import data
annual_visits <- read_rds("data/annual_visits.rds")
monthly_visits <- read_rds("data/monthly_visits.rds")

parks <- annual_visits$park_name %>% unique()
ui <- navbarPage(
  title = img(src="infraestructura.svg", height = "40px"), id = "navBar",
  theme = "paper.css",
  collapsible = TRUE,
  inverse = FALSE,
  windowTitle = "DataSet",
  position = "fixed-top",
  footer = includeHTML("./www/include_footer.html"),
  header = tags$style(
    ".navbar-right {
                       float: right !important;
                       }",
    "body {padding-top: 75px;}"),
  
  tabPanel("Info",value = "info",
     shinyjs::useShinyjs(),
     tags$head(tags$script(HTML('var fakeClick = function(tabName) {
                                 var dropdownList = document.getElementsByTagName("a");
                                 for (var i = 0; i < dropdownList.length; i++) {
                                 var link = dropdownList[i];
                                 if(link.getAttribute("data-value") == tabName) {
                                  link.click();};
                                 }
                                };'))),
    fluidRow(
       HTML("<section class='banner'>
           <h2 class='parallax'>PARQUES NACIONALES DE ESTADOS UNIDOS</h2>
           <p class='parallax_description'>Shiny es un paquete de software R que le permite crear web apps a partir de scripts en R.</p>
           </section>
        ")
     ),
     
     fluidRow(
       column(3),
       column(6,
              shiny::HTML("<br><br><center> <h1>DataSet</h1> </center><br>"),
              shiny::HTML("<h5>Vamos a tomar como ejemplo visitas de parques nacionales de Estados Unidos de America.</h5>")
       ),
       column(3)
     ),
     
     fluidRow(
       
       style = "height:50px;"),
     
     # PAGE BREAK
     tags$hr(),
     
     fluidRow(
       column(3),
       
       column(2,
          div(class="panel panel-default", 
              div(class="panel-body",  width = "600px",
                  align = "center",
                  div(
                    tags$img(src = "one.svg", 
                             width = "50px", height = "50px")
                  ),
                  div(
                    h5(
                      "Grafica de visitantes de cada parque, del anio 1979 a 2019.
                       Total de visitantes."
                    )
                  )
              )
          )
       ),
       column(2,
          div(class="panel panel-default",
              div(class="panel-body",  width = "600px", 
                  align = "center",
                  div(
                    tags$img(src = "two.svg", 
                             width = "50px", height = "50px")
                  ),
                  div(
                    h5(
                      "Comparativa de todos los parques por mes, destacando el parque seleccionado, se pinta una tabla con cada visita al mes"
                    )
                  )
              )
          )
       ),
       column(2,
          div(class="panel panel-default",
              div(class="panel-body",  width = "600px", 
                  align = "center",
                  div(
                    tags$img(src = "three.svg", 
                             width = "50px", height = "50px")),
                  div(
                    h5(
                      "Predicción de visitantes 2021"
                    )
                  )
              )
          )
       ),
       column(3)
       
     ),
  ),
  
  tabPanel("Data",
     fluidRow(
       HTML("<section class='banner2'> </section>")
     ),
     fluidRow(
       column(12, 
              tableOutput("annual_visits")
       )
     )
  ),
    
  tabPanel("Graph",
    fluidRow(
      HTML("<section class='banner2'> </section>")
    ),
    fluidRow(
      column(4, 
             selectInput("selected_park", "Parque", choices = parks,
                         selected = "Crater Lake NP")
      ),
      column(4,
             sliderInput("selected_year", "Anio", 
                         value = 2019,
                         min = min(annual_visits$year), 
                         max = max(annual_visits$year),
                         step = 1,
                         sep = "")
      )
    ),
    textOutput("park_name"),
    fluidRow(
      column(8,
             plotOutput("annual_plot")
      ),
      column(4,
             textOutput("park_summary")
      )
    ),
    fluidRow(
      column(8, 
             plotOutput("monthly_plot"),
             checkboxInput("display_average", "¿Mostrar promedio mensual?")
      ),
      column(4, 
             tableOutput("monthly_table")
      )
    )
  ),
  tabPanel('Lineal')
)

server <- function(input, output, session) {
  output$park_name <- renderText(input$selected_park)
  
  # display 10 rows initially
  output$ex1 <- DT::renderDataTable(
    DT::datatable(annual_data, options = list(pageLength = 25))
  )
  
  annual_data <- reactive({
    annual_visits %>% 
      filter(park_name == input$selected_park)
  })
  
  monthly_data <- reactive({
    monthly_visits %>% 
      filter(park_name == input$selected_park) 
  })
  
  output$park_summary <- renderText({
    annual_data() %>% 
      filter(year == input$selected_year) %>% 
      summarize_park()
  })
  
  output$monthly_table <- renderTable(digits = 0, {
    monthly_data() %>% 
      filter(year == input$selected_year) %>% 
      select(month_name, recreation_visits)
  })
  
  output$annual_plot <- renderPlot({
    annual_data() %>% 
      plot_annual(highlight_year = input$selected_year)
  })
  
  output$monthly_plot <- renderPlot({
    monthly_data() %>% 
      plot_monthly(display_average = input$display_average,
                   highlight_year = input$selected_year) 
  })
  
}
shinyApp(ui,server)

