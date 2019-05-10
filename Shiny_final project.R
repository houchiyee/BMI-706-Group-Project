library(ggthemes)
library(shinyjs)
library(plotly)
library(tidyverse)
library(maps)
library(ggplot2)
library(shiny)
library(scales)
library(reshape2)



# data pre-processing

airdata <- read.csv("airdata.csv",header = T)
parameter_classes <- read.csv("parameter_classes.csv", header=T)

data <- merge(airdata, parameter_classes, by="Parameter.Code")
data <- data %>% 
  select(Parameter.Name, Year, Units.of.Measure, State.Name, County.Name, 
         Arithmetic.Mean, Latitude, Longitude, Class.Name)

paramcounts <- data %>% group_by(Parameter.Name, State.Name, Year) %>% summarize(n()) %>%
  group_by(Parameter.Name) %>% summarize(counts = n())

# filter out parameters with few observations
filtered_data <- data %>% 
  filter(Parameter.Name %in% paramcounts$Parameter.Name[which(paramcounts$counts>150)]) %>%
  filter(Class.Name %in% c("Urban Air Toxic Pollutants", "Hazardous Air Pollutants",
                           "Volatile organic compounds", 
                           "Urban Air Toxics Monitoring Program VOCs", "School Air Toxics Program",
                           "Compounds Measured for the BP Oil Spill in the Gulf of Mexico",
                           "The core list of toxics of interest to the NATTS program.",
                           "Key Pollutants identified for the School Air Toxics Program",
                           "Urban Air Toxics Monitoring Program Carbonyls",
                           "Chemical Speciation Network Trace Elements",
                           "PM2.5 Speciation Metals Pollutants", "PM2.5 Speciation Cation/Anion Pollutants"))

# normalize function
normalize <- function(x){
  return((x-min(x)) / (max(x)-min(x)))
}


# actual visualzation
ui <- fluidPage(
  titlePanel("Explore the Major Pollutants with the Air Data EPA"),
  
  sidebarPanel(
    h2("Welcome"),
    p("Find out about major pollutants for different regions! To enable comparison across pollutants/years, the original values for each pollutant at a given year were normalized to values between 0 and 1. The major pollutant is defined as the pollutant with the highest scaled value for a given region."),
    # https://aqs.epa.gov/aqsweb/airdata/FileFormats.html#_annual_summary_files
    # p("Data preprocess step can be found ", a("here.", href="xxx.R")),
    
    h2("How to use"),
    p("After selecting from the dropdown menus the class and year of interest, the US map is color-coded by the scaled value of the major pollutant for each state. Hovering over a state shows the major pollutant (among the selected class) for the region at the selected year. Clicking on a state displays a barchart showing the top 10 pollutants (scaled values) for the state. Clicking on a state also displays a state level view showing the major pollutant for each county within the state."),
    
    selectInput("selectclass", 
                label = "Select Class:", 
                choices = unique(filtered_data$Class.Name),
                selected = "Urban Air Toxic Pollutants"),
    
    selectInput("selectyear", 
                label = "Select Year for Map:", 
                choices = as.character(sort(unique(filtered_data$Year))),
                selected = "1999")
  ),
  
  mainPanel(
    tabsetPanel(
      
      tabPanel("Map", 
               h3("Major pollutant for US states"),
               textOutput("title"),
               tags$head(tags$style("#title{color: black; font-size: 15px;}")),
               plotlyOutput("map"),
               fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("countymap"), plotlyOutput("barchart")))
      ),
      tabPanel("Trend", 
               #h3("Top 5 Pollutants of Each Year of the selected state"),
               textOutput("trend_title"),
               tags$head(tags$style("#trend_title{color: black; font-size: 20px;}")),
               plotlyOutput("lineplot"))
      
    )
  ));

server <- function(input, output, session) {
  
  
  
  output$map <- renderPlotly({
    
    classdata <- filtered_data %>% 
      filter(Class.Name==input$selectclass) %>% 
      filter(Year==input$selectyear) %>%
      group_by(Parameter.Name) %>% 
      mutate(Scaled.Value = normalize(Arithmetic.Mean))
    
    statedata <- classdata %>% 
      group_by(Parameter.Name, State.Name) %>% 
      summarize(Mean.Value = mean(Scaled.Value))
    
    geo <- list(
      scope = 'usa',
      showland = TRUE,
      landcolor = toRGB("gray95"),
      countrycolor = toRGB("gray80")
    )
    
    dat <- statedata %>% group_by(State.Name) %>% arrange(desc(Mean.Value)) %>% slice(1)
    dat$code <- c(state.abb, 'DC')[match(dat$State.Name, c(state.name, 'District Of Columbia'))]
    dat <- dat %>% filter(!is.na(code))
    
    p <- plot_geo(dat, locationmode = 'USA-states', source="state") %>%
      add_trace(
        z = ~Mean.Value, text = ~Parameter.Name, locations = ~code,
        color = ~Mean.Value, colors = 'Blues'
      ) %>%
      colorbar(title = "Normalized value") %>%
      layout(
        # title = 'Major pollutant for US states',
        geo = geo
      )
    
    p
    
  })
  
  
  output$barchart <- renderPlotly({
    d <- event_data("plotly_click", source="state")
    if (is.null(d)) NULL else {
      
      classdata <- filtered_data %>% 
        filter(Class.Name==input$selectclass) %>% 
        filter(Year==input$selectyear) %>%
        group_by(Parameter.Name) %>% 
        mutate(Scaled.Value = normalize(Arithmetic.Mean))
      
      statedata <- classdata %>% 
        group_by(Parameter.Name, State.Name) %>% 
        summarize(Mean.Value = mean(Scaled.Value))
      
      dat <- statedata %>% group_by(State.Name) %>% arrange(desc(Mean.Value)) %>% slice(1)
      dat$code <- c(state.abb, 'DC')[match(dat$State.Name, c(state.name, 'District Of Columbia'))]
      dat <- dat %>% filter(!is.na(code))
      
      selected_state <- dat$State.Name[d$pointNumber+1]
      
      dat <- statedata %>% filter(State.Name==selected_state) %>% arrange(desc(Mean.Value)) %>% head(10)
      
      p <- ggplot(dat, aes(x=Parameter.Name, y=Mean.Value)) + ylim(0,1) +
        geom_bar(stat="identity") + scale_x_discrete(limits=dat$Parameter.Name) +
        theme_minimal() + theme(axis.text.x = element_text(angle = 60))
      
      ggplotly(p)
    }
    
  })
  
  
  output$lineplot <- renderPlotly({
    d <- event_data("plotly_click", source="state")
    if (is.null(d)) NULL else {
      
      classdata <- filtered_data %>% 
        filter(Class.Name==input$selectclass) %>% 
        group_by(Parameter.Name, Year) %>% 
        mutate(Scaled.Value = normalize(Arithmetic.Mean))
      
      statedata <- classdata %>% 
        group_by(Parameter.Name, State.Name, Year) %>% 
        summarize(Mean.Value = mean(Scaled.Value))
      
      dat <- statedata %>% filter(Year==input$selectyear) %>%
        group_by(State.Name) %>% arrange(desc(Mean.Value)) %>% slice(1)
      dat$code <- c(state.abb, 'DC')[match(dat$State.Name, c(state.name, 'District Of Columbia'))]
      dat <- dat %>% filter(!is.na(code))
      
      selected_state <- dat$State.Name[d$pointNumber+1]
      
      dat <- statedata %>% 
        filter(State.Name==selected_state) %>%
        group_by(Year) %>%
        top_n(n = 5, wt = Mean.Value)
      
      p <- ggplot(dat, aes(x=Year, y=Mean.Value)) + ylim(0,1) +
        geom_line(aes(color=Parameter.Name))+geom_point(aes(color=Parameter.Name)) + theme_minimal() 
      
      ggplotly(p)
    }
    
  })
  
  # all title
  output$title <- renderText({
    d <- event_data("plotly_click", source="state")
    if (is.null(d)) NULL else{
      classdata <- filtered_data %>% 
        filter(Class.Name==input$selectclass) %>% 
        filter(Year==input$selectyear) %>%
        group_by(Parameter.Name) %>% 
        mutate(Scaled.Value = normalize(Arithmetic.Mean))
      
      statedata <- classdata %>% 
        group_by(Parameter.Name, State.Name) %>% 
        summarize(Mean.Value = mean(Scaled.Value))
      
      dat <- statedata %>% group_by(State.Name) %>% arrange(desc(Mean.Value)) %>% slice(1)
      dat$code <- c(state.abb, 'DC')[match(dat$State.Name, c(state.name, 'District Of Columbia'))]
      dat <- dat %>% filter(!is.na(code))
      
      selected_state <- dat$State.Name[d$pointNumber+1]
      paste0("The Selected State is ",selected_state)
    }
  })
  
  # trend title
  output$trend_title <- renderText({
    d <- event_data("plotly_click", source="state")
    if (is.null(d)) NULL else{
      classdata <- filtered_data %>% 
        filter(Class.Name==input$selectclass) %>% 
        filter(Year==input$selectyear) %>%
        group_by(Parameter.Name) %>% 
        mutate(Scaled.Value = normalize(Arithmetic.Mean))
      
      statedata <- classdata %>% 
        group_by(Parameter.Name, State.Name) %>% 
        summarize(Mean.Value = mean(Scaled.Value))
      
      dat <- statedata %>% group_by(State.Name) %>% arrange(desc(Mean.Value)) %>% slice(1)
      dat$code <- c(state.abb, 'DC')[match(dat$State.Name, c(state.name, 'District Of Columbia'))]
      dat <- dat %>% filter(!is.na(code))
      
      selected_state <- dat$State.Name[d$pointNumber+1]
      paste0("Top 5 Pollutants of Each Year of State ",selected_state)
    }
  })
  
  output$countymap <- renderPlotly({
    d <- event_data("plotly_click", source="state")
    if (is.null(d)) NULL else{
      classdata <- filtered_data %>% 
        filter(Class.Name==input$selectclass) %>% 
        filter(Year==input$selectyear) %>%
        group_by(Parameter.Name) %>% 
        mutate(Scaled.Value = normalize(Arithmetic.Mean))
      
      statedata <- classdata %>% 
        group_by(Parameter.Name, State.Name) %>% 
        summarize(Mean.Value = mean(Scaled.Value))
      
      dat <- statedata %>% group_by(State.Name) %>% arrange(desc(Mean.Value)) %>% slice(1)
      dat$code <- c(state.abb, 'DC')[match(dat$State.Name, c(state.name, 'District Of Columbia'))]
      dat <- dat %>% filter(!is.na(code))
      
      selected_state <- dat$State.Name[d$pointNumber+1]
      
      countydata <- classdata %>% filter(State.Name==selected_state) %>%
        group_by(Parameter.Name, County.Name) %>%
        mutate(Mean.Value = mean(Scaled.Value))
      
      dat <- countydata %>% 
        group_by(County.Name) %>% 
        arrange(desc(Mean.Value)) %>% 
        slice(1) %>% 
        mutate(region=tolower(County.Name))
      
      county_data <- map_data("county") %>%
        filter(region == tolower(selected_state))
      
      county_para <- merge(county_data, dat, by.x = "subregion", by.y = "region", sort=FALSE)
      
      county_para$color <- cut(county_para$Mean.Value,
                               breaks = seq(min(county_para$Mean.Value), 
                                            max(county_para$Mean.Value), 
                                            by = (max(county_para$Mean.Value)-
                                                    min(county_para$Mean.Value)) / 10))
      
      county_para <- county_para[order(county_para$order), ]
      
      # create state boundaries
      p <- county_para %>%
        group_by(group) %>%
        plot_ly(
          x = ~ long,
          y = ~ lat,
          color = ~ color,
          colors = "Blues",
          name = ~Parameter.Name) %>% 
        add_polygons(line = list(width = 0.4)) %>%
        add_polygons(
          line = list(color = 'black', width = 0.5),
          showlegend = FALSE
        ) %>%
        layout(
          title = "Major pollutant by county",
          showlegend = FALSE,
          xaxis = list(title = "", showgrid = FALSE,
                       zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(title = "", showgrid = FALSE,
                       zeroline = FALSE, showticklabels = FALSE)
        )
      p
      
    }
  })
  
};

shinyApp(ui, server, options = list(height=600))