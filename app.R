library(shiny)
library(shinydashboard)
library(biscale)
library(cowplot)
library(ggpattern)
library(gridGraphics)
library(ggmagnify)
library(ggpubr)
library(rgl)
library(tidyverse)
library(maps)

##### DATA PREPARATION ####
#setwd(D:/OneDrive/SPU/Courses/GR/Spring 2023/ISM 6361 - Data Visualization/Projects/ScDp_Shiny)

dat <- readxl::read_xlsx("data.xlsx")

#Line Chart
lineDat <- dat %>% group_by(year) %>%
  summarise(
    Rate_Depression = sum(rateDprss),
    `Rate_Self-Harm` = sum(rateSuicide),
    
    Raw_Depression = sum(numDprss),
    `Raw_Self-Harm` = sum(numSuicide)
  )
lineDat <- lineDat %>% pivot_longer(
  cols = c(2:5),
  names_to = c(".value", "item"),
  names_sep = "_"
)

#Bivariate Map
mapDat <- dat %>% group_by(year, location) %>%
  summarise(
    dpRate = sum(rateDprss),
    scRate = sum(rateSuicide),
    
    dpRaw = sum(numDprss),
    scRaw = sum(numSuicide)
  )

map.data <- map_data("world")

#Sex Difference
sexDat <- dat %>% group_by(year, Region, sex) %>%
  summarise(
    dpRate = sum(rateDprss),
    scRate = sum(rateSuicide),
    
    dpRaw = sum(numDprss),
    scRaw = sum(numSuicide)
  )
sexDat <- sexDat %>% 
  pivot_wider(
    names_from = sex, 
    values_from = c(dpRate, scRate, dpRaw, scRaw)
) %>% 
  mutate(
    dpRawDelta = dpRaw_Male - dpRaw_Female, dpRaw_Male = NULL, dpRaw_Female = NULL,
    dpRateDelta = dpRate_Male - dpRate_Female, dpRate_Male = NULL, dpRate_Female = NULL,
    
    scRawDelta = scRaw_Male - scRaw_Female, scRaw_Male = NULL, scRaw_Female = NULL,
    scRateDelta = scRate_Male - scRate_Female, scRate_Male = NULL, scRate_Female = NULL
  )

sexDat$dpRaw_lab <- ifelse(sexDat$dpRawDelta >= 0, "Male", "Female")
sexDat$dpRate_lab <- ifelse(sexDat$dpRateDelta >= 0, "Male", "Female")
sexDat$scRaw_lab <- ifelse(sexDat$scRawDelta >= 0, "Male", "Female")
sexDat$scRate_lab <- ifelse(sexDat$scRateDelta >= 0, "Male", "Female")

#3-D Chart
cube <- dat %>% group_by(year, GDP, Population, Income.group) %>%
  summarise(
    dpRate = sum(rateDprss),
    scRate = sum(rateSuicide),
    GDPperCapita = sum(GDP, na.rm = T) / sum(Population, na.rm = T),
    
    dpRaw = sum(numDprss),
    scRaw = sum(numSuicide)
  )
cube$Income.group <- as.factor(cube$Income.group)
mycolors <- c('#0E3B43', '#729EA1', '#BA3F1D', "#F3C677")
cube$color <- mycolors[as.numeric(cube$Income.group)]

rgl_init <- function(new.device = FALSE, bg = "white", width = 640) { 
  if( new.device | rgl.cur() == 0 ) {
    open3d()
    par3d(windowRect = 50 + c( 0, 0, width, width ) )
    bg3d(color = bg )
  }
  clear3d(type = c("shapes", "bboxdeco"))
  view3d(theta = 15, phi = 20, zoom = 1.1)
}

#### SHINY APP ####
##### UI #####
ui <- dashboardPage(
  dashboardHeader(
    title = ("ISM 6369 - Huy Nguyen Final Project"),
    titleWidth = 450
  ),
  
  dashboardSidebar(
    sidebarMenu(id = "tabs",style = "position:fixed;width:220px;",
               menuItem(style = "position:fixed;width: inherit;",
                        fluidRow(
                          column(12, offset = .5, 
                                 title = strong("Controls"), 
                                 radioButtons(inputId = "metric", label = "Select Metric:",
                                              choices = c("Number", "Rate"),
                                              selected = "Number"), inline = T), #Select Metric
                          column(12, offset = .5,
                                 sliderInput(inputId = "year", label = "Year", 
                                             min = min(mapDat$year), max = max(mapDat$year), value = min(mapDat$year),
                                             step = 1, animate = T)))
                        )
               )
    ),
  
  dashboardBody(
    fluidRow(
      box(width = 6, plotOutput("line", height = '550px')), #Line plot
      
      tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), #Remove minor tick marks
      
      box(width = 6, plotOutput("sexDiff", height = '450px'),
          title = strong("Sex Difference"),
          footer = "Red = Higher Depression Prevalence, Blue = Higher Self-Harm Prevalence"), #Sex Difference
      
      box(width = 7, plotOutput("biMap", height = '550px'),
          footer = "Red = Higher Depression Prevalence, Blue = Higher Self-Harm Prevalence"), #Bivariate Map
      
      box(width = 5, 
          title = strong("Relationships between Depression, Self-Harm, and GDP", p("Group by Country Income Group")), 
          footer = em("Blue = High income, Light blue = Upper middle income, red = Lower middle income, yellow = Low income"),
          rglwidgetOutput("myWebGL", height = '500px')), #3-D chart 
    )
  ),
)

###### SERVER #####
server <- function(input, output) {
  #Radio Button Condition
  metricCond <- reactive({input$metric %in% "Number"})
  
    
  ### Line Chart
  #Switch for x/y-axis
  metricVal <- reactive({
    switch(input$metric,
           "Number" = lineDat$Raw,
           "Rate" = lineDat$Rate)
  })
  
  output$line <- renderPlot({
    
    #If-Else for geom_magnify
    if(metricCond()){
      scFrom <-  c(xmin = 2000, xmax = 2019, ymin = 13800000, ymax = 17000000)
      scTo <- c(xmin = 1999, xmax = 2009, ymin = 25000000 , ymax = 105000000)
      
      dpFrom <- c()
      dpTo <- c()
    } 
    else {
      scFrom <-  c(xmin = 2000, xmax = 2019, ymin = 1250000,  ymax = 1700000)
      scTo <- c(xmin = 2010, xmax = 2020, ymin = 2500000, ymax = 10500000)
      
      dpFrom <- c(xmin = 2000, xmax = 2019, ymin = 27000000,  ymax = 28000000)
      dpTo <- c(xmin = 1999, xmax = 2009, ymin = 17000000, ymax = 25000000)
    }
    
    #The plot
    ggplot(lineDat, aes(x = year, color = item))+
      geom_line(aes_string(y = metricVal()), size = 1.15)+
      theme_bw()+
      scale_x_continuous(name = "Year", breaks = scales::pretty_breaks(n = 5))+
      scale_y_continuous(name = "Value")+
      theme(text = element_text(size = 15),
            plot.title = element_text(face = "bold"))+
      ggtitle(paste("Global", input$metric, "\nof Depression and Self-Harm Prevalence"))+
      geom_magnify( #Self- Harm
        from = scFrom,
        to = scTo,
        shadow = F
      )+
      geom_magnify( #Depression
        from = dpFrom,
        to = dpTo,
        shadow = F
      )
  })
  
  ### Bivariate Map
  output$biMap <- renderPlot({
    
    #If-Else to choose Metrics
    if (metricCond()) {
      biClass <- bi_class(mapDat, x = dpRaw, y = scRaw, style = "quantile", dim = 4)
    } 
    else {
      biClass <- bi_class(mapDat, x = dpRate, y = scRate, style = "quantile", dim = 4)
    }
    
    #Selecting year
    mapYear <- biClass[biClass$year == input$year,]
    
    #The plot 
    mapPlot <- ggplot()+
      geom_map(data = map.data, map = map.data, 
               aes(x = long, y =lat, group = group, map_id = region), alpha = .1)+
      geom_map(data = mapYear, map = map.data, 
               aes(fill = bi_class, map_id = location))+
      bi_scale_fill(pal = "GrPink2", dim = 4)+
      bi_theme()+
      theme(legend.position = "none",
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank())+
      labs(title = paste("YEAR =", input$year))
    
    legend <- bi_legend(pal = "GrPink2",
                        dim = 4, 
                        xlab = "Depression", 
                        ylab = "Self-Harm",
                        size = 12)
    
    ggdraw()+
      draw_plot(mapPlot, 0,0, 1,1)+
      draw_plot(legend, .0, .15, .25, .25)
  })
  
  ### Sex Difference Col Chart
  #Switch for y-axis
  data1 <- reactive({
    switch(input$metric,
           "Number" = sexDat$dpRawDelta,
           "Rate" = sexDat$dpRateDelta)
  })
  
  legend1 <- reactive({
    switch(input$metric,
           "Number" = sexDat$dpRaw_lab,
           "Rate" = sexDat$dpRate_lab)
  })
  
  data2 <- reactive({
    switch(input$metric, 
           "Number" = sexDat$scRawDelta,
           "Rate" = sexDat$scRateDelta)
  })
  
  legend2 <- reactive({
    switch(input$metric,
           "Number" = sexDat$scRaw_lab,
           "Rate" = sexDat$scRate_lab)
  })
  
  output$sexDiff <- renderPlot({
    if(metricCond()){
      dpLab <- "Depression Number"
      scLab <- "Self-Harm Number"
    }
    else{
      dpLab <- "Depression Rate"
      scLab <- "Self-Harm Rate"
    }
    
    #The plot
    dp <- ggplot(data = sexDat, aes(x = year, y = data1(), fill = legend1()))+
      geom_col()+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90))+
      scale_x_reverse(name = "Year", breaks = scales::pretty_breaks(n = 4))+
      facet_wrap(.~Region, labeller = label_wrap_gen())+
      scale_fill_manual(name = "Predominant",
                        values = c("#EC4E20", "#129FF8"))+
      coord_flip()+
      scale_y_continuous(name = dpLab)+
      theme(text = element_text(size = 15))
    
    sc <- ggplot(data = sexDat, aes(x = year, y = data2(), fill = legend2()))+
      geom_col()+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90))+
      scale_x_reverse(name = "Year", breaks = scales::pretty_breaks(n = 4))+
      facet_wrap(.~Region, labeller = label_wrap_gen())+
      scale_fill_manual(name = "Predominant",
                        values = c("#EC4E20", "#129FF8"))+
      coord_flip()+
      scale_y_continuous(name = scLab)+
      theme(text = element_text(size = 15),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
    
    ggarrange(dp, sc, common.legend = T, legend = "right", legend.grob = get_legend(sc))
  })
  
  ###3-D Chart
  output[["myWebGL"]] <- renderRglwidget({
    
    cubeYear <- cube[cube$year == input$year,]
    
    if(metricCond()) {
      x <- cubeYear[["dpRaw"]]
      y <- cubeYear[["scRaw"]]
      z <- cubeYear[["GDP"]]
      
      xlab <- "Depression Number"
      ylab <- "Self-Harm Number"
      zlab <- "GDP"
    }
    else{
      x <- cubeYear[["dpRate"]]
      y <- cubeYear[["scRate"]]
      z <- cubeYear[["GDPperCapita"]]
      
      xlab <- "Depression Rate"
      ylab <- "Self-Harm Rate"
      zlab <- "GDP per Capita"
    }
    
    save <- options(rgl.inShiny = T)
    on.exit(options(save))
    
    try(rgl_init())
    plot3d(
      x = x, xlab = xlab,
      y = y, ylab = ylab,
      z = z, zlab = zlab,
      col = cubeYear[["color"]],
      type = "s",
      box = T,
      size = 1.5
    )
    rglwidget()
  })
}

shinyApp(ui, server)

