library(shiny)
library(plotly)
library(shinythemes)
library(tidyverse)
library(ggrepel)
library(dplyr)
library(ggmap)
library(ggthemes)

#importing created data from my R. files 
#Wine data
data <- readRDS("Nz_wine_map_data")
#World Map Data
dots <- readRDS("dots_new")
locations <- readRDS("locations_new")

#UI
#add bootstrap theme to make it look cleaner 
#experimented with a lot of themes and sandstone looked best 
#going for a earthy look and going to try add some deep reds or white for "wine"
#navbarPage create a page that contains a top level navigation
#then has tabs along the top bar
#this will work well to show the progression of the years 
ui <- navbarPage(theme = shinytheme("sandstone"), 
  "New Zealand Wine",
  
#Making a title page/home page 
#Clean and welcomes people to my site 
  tabPanel("Home", 
           tags$img(src = "Nz_wine.png", height = "650px")),
  
  
  
  # Adding a information tab so people know that my data is about 
  tabPanel("Information",
           fluidPage(
             div(class = "jumbotron", 
                 h2("Exploration of New Zealand Wine")),
   
               h3("New Zealand’s wine industry is one of the youngest in the world, but the 'can-do' attitude of our pioneering winemakers and growers put in place a solid base for our innovative, world-class industry to flourish.")
                 ,
                hr(),
             div(class="panel panel-default",
                 div(class="panel-heading", h4("Goal")), 
                 div(class="panel-body",
                  h4("Explore this app to find out more about how the amount of wineries in each region of New Zealand has grown over the past 8 years. 
                     Click on 2010, 2012, 2014, 2016 or 2018 tab at the top of this page 
                      to see how many wineries there were in each region at this certian point in time."),
                  h4("New Zealand wine is becoming more and more popular around the world and in the WORLD tab you can see where in the world New Zealand wine is being exported to.")
                  )),
             div(class="panel panel-danger",
                 div(class="panel-heading",
                     div(class="panel-title", h5("Data Source"))),
                     div(class="panel-body",
             div(class = "text-muted",
                 p("New Zealand Wine Data: https://www.nzwine.com/en/news-media/statistics-reports/")), 
             div(class = "text-muted",
                 p("Regional Data of New Zealand: https://koordinates.com/layer/1244-nz-regional-councils-2006-census/"))
           ))
  )),

#chose to only do even years
#having 2009 to 2018 was a bit much and a waste if space 
#having only the even numbers shows the growth clearly and effectively

  #creating a 2010 tab
  tabPanel("2010",
           #creates a fluid page layout with columns and rows 
           #makes sure the elements appear on the same line
           #scales to differernt browsers automatically
           fluidPage(
             #Heading
             h2("2010"),
             #Guidence
             h5("Hover over map to see how many wineries are in each region"),
             #show 2010 map
             plotlyOutput("plot2010")
           )
  ),
  
  #creating a 2012 tab
  tabPanel("2012",
           fluidPage(
             h2("2012"),
             h5("Hover over map to see how many wineries are in each region"),
             plotlyOutput("plot2012")
           )
  ), 

  #creating a 2014 tab
  tabPanel("2014",
           fluidPage(
             h2("2014"),
             h5("Hover over map to see how many wineries are in each region"),
             plotlyOutput("plot2014")
           )
  ), 
  
  #creating a 2016 tab
  tabPanel("2016",
           fluidPage(
             h2("2016"),
             h5("Hover over map to see how many wineries are in each region"),
             plotlyOutput("plot2016")
           )
  ), 
  

  #creating a 2018 tab
  tabPanel("2018",
           fluidPage(
             h2("2018"),
             h5("Hover over map to see how many wineries are in each region"),
             plotlyOutput("plot2018")
           )
  ), 
  
  #creating a World tab
  #shows where New Zealand wine is being exported to 
  tabPanel("World", 
           fluidPage(
             div(class = "jumbotron", 
             h3("New Zealand Wine Distribution Around the World"),
             h5("Top 10 Countries"),
             h5("(Hover for country names)")),
             plotlyOutput("World")
           
           )
)

)


#SERVER
#where the map(plot) code is storded 
server <- function(input, output, session) {
  
  
  ######################### 2010
  #2010 map created here
  
  output$plot2010 <- renderPlotly({
    # specify some map projection/options
    p2010 <- ggplot(data = data) +
      geom_sf(aes(fill = Region, text = x2010)
              ) +
      #to remove map plot grid 
      theme_void()
    
    #plot map
    ggplotly(p2010) %>%
      highlight(
        "plotly_hover",
        selected = attrs_selected(line = list(color = "black"))
      )
  })
  
 #repeat for each year (even)
  ######################### 2012
  
  output$plot2012 <- renderPlotly({
    p2012 <- ggplot(data = data) +
      geom_sf(aes(fill = Region, text = x2012)
      ) +
      theme_void()
    
    ggplotly(p2012) %>%
      highlight(
        "plotly_hover",
        selected = attrs_selected(line = list(color = "black"))
      )
  })
  
  

  ######################### 2014
  
  output$plot2014 <- renderPlotly({
    p2014 <- ggplot(data = data) +
      geom_sf(aes(fill = Region, text = x2014)
      ) +
      theme_void()
    
    ggplotly(p2014) %>%
      highlight(
        "plotly_hover",
        selected = attrs_selected(line = list(color = "black"))
      )
  })
  

  
  ######################### 2016
  
  output$plot2016 <- renderPlotly({
    p2016 <- ggplot(data = data) +
      geom_sf(aes(fill = Region, text = x2016)
      ) +
      theme_void()
    
    ggplotly(p2016) %>%
      highlight(
        "plotly_hover",
        selected = attrs_selected(line = list(color = "black"))
      ) 
  })
  

  ######################### 2018
  
  output$plot2018 <- renderPlotly({
    p2018 <- ggplot(data = data) +
      geom_sf(aes(fill = Region, text = x2018)
      ) +
      theme_void()
    
    ggplotly(p2018) %>%
      highlight(
        "plotly_hover",
        selected = attrs_selected(line = list(color = "black"))
      )
  })
  

  ######################### World
  #create a world map 
  
  output$World <- renderPlotly({
    
    
    #create map geography 
    geo <- list(
      showland = TRUE,
      landcolor = toRGB("gray95"),
      countrycolor = toRGB("gray80")
    )
  
    #specify map projections
    pWorld <- plot_geo(color = I("red")) %>%
      add_markers(
        data = dots, x = ~lon, y = ~lat, text = ~dot,
        size = ~order, hoverinfo = "text", alpha = 0.5) %>%
      
      add_segments(
        data = group_by(locations, Order),
        x = ~lon.y, xend = ~lon.x,
        y = ~lat.y, yend = ~lat.x,
        alpha = 0.3, size = I(1), hoverinfo = "none") %>%
      
      layout(
        geo = geo, 
        showlegend = FALSE, 
        height=800, 
        dragmode = "select")
      
    
    #plot map
    ggplotly(pWorld)

  })
  
}

shinyApp(ui, server)


