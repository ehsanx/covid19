## app.R ##
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
require(googleVis)

dta <- load(file="C:\\Users\\wildscop\\Google Drive\\service\\ShinyApps\\covid19\\jhwbxx.RData")


ui <- dashboardPage(
  dashboardHeader(title = "COVID-19 Map", dropdownMenu(
    type = "notifications",
    notificationItem(
      text="Ehsan's page",
      href="https://ehsanx.netlify.com/"
    )
  )),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Confirmed map",
               tabName = "data1"),
      menuItem("Death map",
               tabName = "data2"),
      menuItem("BCG vaccine map",
               tabName = "data3"),
      menuItem("About",
               tabName = "data0")
    )
  ),
  dashboardBody(
    #headerPanel("COVID-19 World Map"),
    tabItems(
      tabItem(tabName = "data1",
              htmlOutput("map1"),
              tabsetPanel(
                tabPanel(h4('Data sources'),
                         h6("1: World Health Organization (WHO: https://www.who.int/)", align = "left"),
                         h6("2: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE: https://github.com/CSSEGISandData/COVID-19)", align = "left")
                ))
      ),
      tabItem(tabName = "data2",
              htmlOutput("map2"),
              tabsetPanel(
                tabPanel(h4('Data sources'),
                         h6("1: World Health Organization (WHO: https://www.who.int/)", align = "left"),
                         h6("2: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE: https://github.com/CSSEGISandData/COVID-19)", align = "left")
                ))
      ),
      tabItem(tabName = "data3",
              htmlOutput("map3"),
              tabsetPanel(
                tabPanel(h4('Data sources'),
                         h6("1: BCG world atlas (http://bcgatlas.org/ to create BCG.status variable).", align = "left"),
                         h6("BCG.status legend: -1 = Past national BCG vaccination policy for all, 0 = BCG recommendation only for specific groups or none at all, 1 = Current national BCG vaccination policy for all", align = "left"),
                         h6("2: World Health Organization (WHO: https://www.who.int/ to create BCG variable).", align = "left"),
                         h6("BCG legend: 0[cross] = country-specific vaccination data not available in the WHO data (2009-2018); 1[check] = country-specific vaccination data available (2009-2018).", align = "left"),
                         h6("Ref: Miller, A., Reandelar, M. J., Fasciglione, K., Roumenova, V., Li, Y., & Otazu, G. H. (2020). Correlation between universal BCG vaccination policy and reduced morbidity and mortality for COVID-19: an epidemiological study. medRxiv: https://tinyurl.com/covidBCG", align = "left"),
                         h6("**Note**: This app is not claiming any association between BCG vaccination and covid-19 morbidity / mortality. This app is simply a pictorial depiction of what the data shows.", align = "left")
                ))
      ),
      tabItem(tabName = "data0",
              widgetUserBox(
                title = "Ehsan Karim",
                subtitle = "Assistant Professor, Population and Public Health, University of British Columbia, Canada. URL: https://ehsanx.netlify.com/",
                type = 2,
                width = 12,
                src = "https://raw.githubusercontent.com/ehsanx/ehsankarim/master/public/authors/admin/avatar_hued94dba2ab1c4ee0abf0e343e738c01c_361060_270x270_fill_q90_lanczos_center.jpg",
                color = "blue",
                "Data source: World Health Organization (WHO: https://www.who.int/), BCG world atlas (http://bcgatlas.org/) and Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE: https://github.com/CSSEGISandData/COVID-19). Data download functions from Joachim Gassen's github was used: https://github.com/joachim-gassen/tidy_covid19. Thanks to James Johnston [BCCDC: http://www.bccdc.ca/our-research/people/james-johnston] for informing me about the BCG world atlas webpage.",
                footer = "Feel free to send me comments <ehsan.karim at ubc.ca> regarding how to update this app. Last updated: 5th April, 2020."
              )
      )
    )
  )
)



server <- function(input, output,session) {
  output$map1 <- renderGvis({
    G <- gvisGeoChart(dta, locationvar = "iso2c", colorvar = "confirmed", 
                      options=list(width=700, height=450,
                                   colorAxis = "{colors:['white','blue']}", 
                                   #backgroundColor = "lightblue",
                                   projection="kavrayskiy-vii"))
    T <- gvisTable(dta[c("country","iso2c","confirmed","deaths")], options=list(width=350, height=450))
    map <- gvisMerge(G,T, horizontal=TRUE) 
    #plot(map)
    return(map)
  })
  output$map2 <- renderGvis({
    G <- gvisGeoChart(dta, locationvar = "iso2c", colorvar = "deaths", 
                      options=list(width=700, height=450,
                                   colorAxis = "{colors:['white','red']}", 
                                   #backgroundColor = "lightblue",
                                   projection="kavrayskiy-vii"))
    T <- gvisTable(dta[c("country","iso2c","confirmed","deaths")], options=list(width=350, height=450))
    map <- gvisMerge(G,T, horizontal=TRUE) 
    #plot(map)
    return(map)
  })
  output$map3 <- renderGvis({
    G <- gvisGeoChart(dta, locationvar = "iso2c", colorvar = "BCG.status", 
                      options=list(width=700, height=450,
                                   colorAxis = "{colors:['orangered','yellow']}", 
                                   #backgroundColor = "lightblue",
                                   projection="kavrayskiy-vii"))
    T <- gvisTable(dta[c("country","iso2c","BCG", "BCG.status")], options=list(width=350, height=450))
    map <- gvisMerge(G,T, horizontal=TRUE) 
    #plot(map)
    return(map)
  })
  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)
