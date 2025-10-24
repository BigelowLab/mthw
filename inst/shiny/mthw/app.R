suppressPackageStartupMessages({
  library(andreas)
  library(mthw)
  library(stars)
  library(shiny)
  library(bslib)
  library(bigelowshinytheme)
  library(dplyr)
  library(leaflet)
  library(ggplot2)
  library(patchwork)
})

region = 'chfc'
depths = "sur"
vars = c("temp", "sal")
andpath = andreas::copernicus_path(region)
mwpath = andreas::copernicus_path("mthw")

db = mthw::read_database(file.path(mwpath, region)) |>
  dplyr::filter(.data$depth %in% depths, 
                .data$name %in% vars)
cDB = andreas::read_database(andpath, multi = TRUE) |>
  dplyr::filter(.data$depth %in% depths, 
                .data$name %in% c("temp", "sal"), 
                .data$period == "day")
temp = mthw::generate_wave(
  db = db |> dplyr::filter(name == "temp"),
  region = region,
  cDB = cDB)
sal = mthw::generate_wave(
  db = db |> dplyr::filter(name == "sal"),
  region = region,
  cDB = cDB)
tempd = mthw::encode_wave(temp) 
sald = mthw::encode_wave(sal) 

DATES <- tempd |> stars::st_get_dimension_values("time")
DATE = dates[6]

get_slices = function(date = DATE, dates = DATES){
  ix = which(dates %in% date)
  list(
    temp = dplyr::slice(tempd, "time", ix),
    sal = dplyr::slice(sald, "time", ix),
    date = date
  )
}



##### UI ######

ui <- shiny::fluidPage(
  theme = bigelowshinytheme::bigelow_theme(),
  includeCSS("www/additionalStyles.css"),
  
  # Header
  bigelowshinytheme::bigelow_header(
    h2("Marine Thermohaline Waves"), 
    h6("Cape Hatteras to Flemish Cap")),
  
  # Main content
  bigelowshinytheme::bigelow_main_body(
    # Introduction
    p("Marine Thermohaline Waves are define by 5 or more consecutive days to metrics above the 90th or below the 10th quantiles of a 30-year baseline."),
    p("Marine Waves definition follows Hobday et al (2016) https://www.sciencedirect.com/science/article/abs/pii/S0079661116000057" ),
    br(),

    bigelow_card(headerContent = "Temperature and Salinity", 
                 footerContent = NULL, 
                 sliderInput("dateSlider",
                             label = "Date",
                             min = min(DATES), max = max(DATES),
                             value = DATE),
                 plotOutput("plotOutput")) ,
    bigelow_footer("Footer")
  )
)

server <- function(input, output, session) {
  
  
  
  output$plotOutput <- renderPlot({
    x = get_slices(input$dateSlider)
    plot_mwd_paired(x$temp, x$sal)
  })
  
}

# Applying ggplot styling and render app
# bigelowshinytheme::bigelow_style_plots()
shinyApp(ui, server)


