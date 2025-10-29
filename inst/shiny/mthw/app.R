suppressPackageStartupMessages({
  library(stars)
  library(mthw)
  library(shiny)
  library(bslib)
  library(bigelowshinytheme)
  library(dplyr)
})

region = 'chfc'
depths = "sur"
vars = c("temp", "sal")
path = system.file(package = "mthw")
tempd = mthw::read_raster(filename = mthw_filename(region = region,
                                                   depth = depths[1],
                                                   variable = "temp",
                                                   path = path))
sald = mthw::read_raster(filename = mthw_filename(region = region,
                                                   depth = depths[1],
                                                   variable = "sal",
                                                   path = path))
DATES <- tempd |> stars::st_get_dimension_values("time")
DATE = DATES[6]

get_slices = function(date = DATE, dates = DATES){
  ix = which(dates %in% date)
  list(
    temp = mthw::slice_date(tempd, ix),
    sal = mthw::slice_date(sald,  ix),
    date = date
  )
}

uri = a("Hobday et al (2016)", 
        href="https://www.sciencedirect.com/science/article/abs/pii/S0079661116000057")


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
    p("Marine Thermohaline Waves are define by 5 or more consecutive days to metrics above the 90th or below the 10th percentiles of a 30-year baseline. Marine Waves definition follows", uri, "."),
    br(),

    bigelowshinytheme::bigelow_card(
      headerContent = "Sea Surface Temperature and Salinity", 
      footerContent = NULL, 
      sliderInput("dateSlider",
                  label = "Date",
                  min = min(DATES), max = max(DATES),
                  value = DATE),
      plotOutput("plotOutput")) ,
    bigelowshinytheme::bigelow_footer("Data courtesy of Copernicus Marine Data Store")
  )
)

server <- function(input, output, session) {
  
  output$plotOutput <- renderPlot({
    x = get_slices(input$dateSlider)
    mthw::plot_mwd_paired(x$temp, x$sal,
                          title = sprintf("Marine Thermohaline Waves %s",
                                          format(x$date, "%Y-%m-%d")))
  })
  
}

# Applying ggplot styling and render app
# bigelowshinytheme::bigelow_style_plots()
shinyApp(ui, server)


