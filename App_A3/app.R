library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(forcats)
library(plotly)
library(leaflet)
library(wordcloud)
#install.packages("leafpop")
library(leafpop)
library(sf)
library(withr)


# DATA RENDERING

# Read data
chocolate <- read.csv("data/chocolate.csv")
taste <- read.csv("data/chocolate_taste_dataset.csv")
world <- read.csv("data/world.csv") %>% select(country, latitude,longitude)

# Clean data 
#Change country names
chocolate$beans <- rep(1,length(chocolate$beans))
chocolate$cocoa_butter <- ifelse(chocolate$cocoa_butter == "have_cocoa_butter",1,0)
chocolate$vanilla <- ifelse(chocolate$vanilla == "have_vanila",1,0)
chocolate$lecithin <- ifelse(chocolate$lecithin == "have_lecithin",1,0)
chocolate$salt <- ifelse(chocolate$salt == "have_salt",1,0)
chocolate$sugar <- ifelse(chocolate$sugar == "have_sugar",1,0)
chocolate$sweetener_without_sugar <- ifelse(chocolate$sweetener_without_sugar == "have_sweetener_without_sugar",1,0)
chocolate$company_location[chocolate$company_location == "Costa rica"] = "Costa Rica"
chocolate$company_location[chocolate$company_location == "New zealand"] = "New Zealand"
chocolate$company_location[chocolate$company_location == "Scotland"] = "United Kingdom"
chocolate$company_location[chocolate$company_location == "Wales"] = "United Kingdom"
chocolate$company_location[chocolate$company_location == "Czech republic"] = "Czech Republic"
chocolate$company_location[chocolate$company_location == "Puerto rico"] = "Puerto Rico"
chocolate$company_location[chocolate$company_location == "Dominican republic"] = "Dominican Republic"
chocolate$company_location[chocolate$company_location == "Sao tome"] = "São Tomé and Príncipe"
chocolate$company_location[chocolate$company_location == "South korea"] = "South Korea"
chocolate$company_location[chocolate$company_location == "El salvador"] = "El Salvador"
chocolate$company_location[chocolate$company_location == "U.k."] = "United Kingdom"
chocolate$company_location[chocolate$company_location == "U.S.A"] = "United States"
chocolate$company_location[chocolate$company_location == "U.a.e."] = "United Arab Emirates"
chocolate$company_location[chocolate$company_location == "South africa"] = "South Africa"
chocolate$company_location[chocolate$company_location == "St. lucia"] = "Saint Lucia"
chocolate$company_location[chocolate$company_location == "St.vincent-grenadines"] = "Saint Vincent and the Grenadines"
chocolate$company_location[chocolate$company_location == "Sao tome & principe"] = "São Tomé and Príncipe"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "Costa rica"] = "Costa Rica"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "New zealand"] = "New Zealand"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "Scotland"] = "United Kingdom"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "Wales"] = "United Kingdom"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "Czech republic"] = "Czech Republic"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "Puerto rico"] = "Puerto Rico"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "Dominican republic"] = "Dominican Republic"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "Sao tome"] = "São Tomé and Príncipe"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "South korea"] = "South Korea"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "El salvador"] = "El Salvador"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "U.k."] = "United Kingdom"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "U.S.A"] = "United States"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "U.a.e."] = "United Arab Emirates"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "South africa"] = "South Africa"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "St. lucia"] = "Saint Lucia"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "St.vincent-grenadines"] = "Saint Vincent and the Grenadines"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "Sao tome & principe"] = "São Tomé and Príncipe"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "Blend"] = "Madagascar"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "Dr congo"] = "Congo [DRC]"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "Sierra leone"] = "Sierra Leone"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "Sumatra"] = "Indonesia"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "Bolvia"] = "Bolivia"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "Solomon islands"] = "Solomon Islands"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "Tobago"] = "Trinidad and Tobago"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "Burma"] = "Myanmar [Burma]"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "Papua new guinea"] = "Papua New Guinea"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "Congo"] = "Congo [DRC]"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "Sri lanka"] = "Sri Lanka"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "Trinidad"] = "Trinidad and Tobago"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "Principe"] = "São Tomé and Príncipe"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "Sulawesi"] = "Indonesia"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "U.s.a."] = "United States"
chocolate$country_of_bean_origin[chocolate$country_of_bean_origin == "Ivory coast"] = "Côte d'Ivoire"
# Change some company names
choc_part <- chocolate %>% filter(company == "Spencer")
for(i in 1:nrow(choc_part)){
  choc_part$company[i] <- ifelse(choc_part$company_location[i] == "United States", "Spencer - USA", "Spencer - AUS")
}
chocolate[chocolate$company == "Spencer",] <- choc_part
choc_part <- chocolate %>% filter(company == "Kah Kow - USA")
for(i in 1:nrow(choc_part)){
  choc_part$company[i] <- ifelse(choc_part$company_location[i] == "United States", "Kah Kow - USA", "Kah Kow - DOM")
}
chocolate[chocolate$company == "Kah Kow - USA",] <- choc_part
choc_part <- chocolate %>% filter(company == "organicfair")
for(i in 1:nrow(choc_part)){
  choc_part$company[i] <- ifelse(choc_part$company_location[i] == "Canada", "organicfair - CAN", "organicfair - SA")
}
chocolate[chocolate$company == "organicfair",] <- choc_part
choc_part <- chocolate %>% filter(company == "Metiisto")
for(i in 1:nrow(choc_part)){
  choc_part$company[i] <- ifelse(choc_part$company_location[i] == "Sweden", "Metiisto - SW", "Metiisto - AUS")
}
chocolate[chocolate$company == "Metiisto",] <- choc_part

# Add taste popularity variable
tot_pop_taste = c()
for(i in 1:length(chocolate$X)){
  pop_taste1 = taste$count_of_taste[taste$taste == chocolate$first_taste[i]]
  pop_taste2 = ifelse(chocolate$second_taste[i] == "", 0, taste$count_of_taste[taste$taste == chocolate$second_taste[i]])
  pop_taste3 = ifelse(chocolate$third_taste[i] == "", 0, taste$count_of_taste[taste$taste == chocolate$third_taste[i]])
  pop_taste4 = ifelse(chocolate$fourth_taste[i] == "", 0, taste$count_of_taste[taste$taste == chocolate$fourth_taste[i]])
  cnt = sum(c(pop_taste1>0, pop_taste2>0, pop_taste3>0, pop_taste4>0))
  tot_pop_taste[i] = ((pop_taste1 + pop_taste2 + pop_taste3 + pop_taste4)/cnt)/max(taste$count_of_taste)
}
chocolate$pop_taste <- tot_pop_taste

# Merge chocolate and world data
chocolate <- merge(chocolate,world,by.x="company_location",by.y="country",all.x=TRUE)
names(chocolate)[names(chocolate) == "longitude"] <- "company_lon"
names(chocolate)[names(chocolate) == "latitude"] <- "company_lat"
chocolate <- merge(chocolate,world,by.x="country_of_bean_origin",by.y="country",all.x=TRUE)
names(chocolate)[names(chocolate) == "longitude"] <- "bean_lon"
names(chocolate)[names(chocolate) == "latitude"] <- "bean_lat"

# App specific data
ordered_companies <- chocolate %>% group_by(company)%>% summarise(avg_rating = mean(rating))%>% arrange(desc(avg_rating))
company_data <- chocolate %>% group_by(company) %>% count()
avg <- chocolate %>% group_by(company)%>% summarise(country = unique(company_location), avg_rating = mean(rating))
company_data <- merge(company_data, avg, by.x = "company", by.y = "company", all.x = TRUE) %>% arrange(desc(avg_rating))
company_data$rank <- 1:nrow(company_data)

world_map <- world
world_map$count_origin <- rep(0, nrow(world_map))
for(i in 1:nrow(world)){
  world_map$count_origin[i] <- sum(chocolate$country_of_bean_origin == world_map$country[i])
}
world_map <- world_map %>% filter(count_origin > 0)

#pal <- colorRampPalette(c("yellow", "brown"))
pal <- colorNumeric(
  palette = "YlOrBr",
  domain =world_map$count_origin
)

fig <- leaflet(world_map) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude, color = ~pal(count_origin), radius = 10, stroke = FALSE, 
                   fillOpacity = 0.5) %>%
  addLegend("bottomleft", pal = pal, values = ~count_origin,
            title = "Frequency as bean origin",
            opacity = 1
  )

icons <- iconList(
  factory = makeIcon("www/factory.png", "www/factory@2x.png", 18, 18),
  bean = makeIcon("www/bean.png", "www/bean@2x.png", 18, 18)
)

# DEFINE UI
ui <- fluidPage(
  
  titlePanel(
    strong(em("More than a Chocolate Bar Chart"))),

  # ROW 1
  fluidRow(
    
    class = "Row1",
    
    column(4,
           tags$a(href="https://www.kaggle.com/datasets/soroushghaderi/chocolate-bar-2020?select=chocolate_taste_dataset.csv", "Chocolate data"),
           tags$a(href="https://www.kaggle.com/datasets/paultimothymooney/latitude-and-longitude-for-every-country-and-state", "Country data"),
           numericInput("rank",
                        label=h5("Select a company based on their rank according to the average rating the company received:"),
                        value = NULL,
                        min = 1,
                        max = 506,
                        width = "400px"),
           strong(textOutput("text")),
           h4("Bean and company origin"),
           leafletOutput("world", width = "750px")

    )
    ,
    
    column(3,
           div("The map below shows where the companies are located and the country of origin for the cocoa beans for each bar, 
           using a factory icon for the former and a bean icon for the latter. If a specific company is selected,
               more information will show by clicking the icons.")
    ),
  column(5,
         plotlyOutput("companydata", height="250px", width="480px"),
         plotlyOutput("points", height="340px", width="480px")
  ),
    
  
  tags$head(tags$style("
      .Row1{height:650px;backgroundcolor:wheat}"
  ))
  
))


# Define server logic required to draw a histogram ----
server <- function(input, output) {

  output$text <- renderText({
    if(is.na(input$rank)){
      "Please select a company based on the rank."
    }else{paste0("Selected company: ", ordered_companies$company[input$rank])}
  })

  output$points <- renderPlotly(({
    
    colfunc <- colorRampPalette(c("yellow", "brown"))
    
    if(is.na(input$rank)){
    p <- ggplot(chocolate, aes(x=factor(rating),y = pop_taste, col = cocoa_percent, text = paste0("Name: ",specific_bean_origin_or_bar_name)))+
      scale_color_gradientn(colours = colfunc(as.integer(max(chocolate$cocoa_percent) - min(chocolate$cocoa_percent)))) + 
      geom_jitter() +
      geom_boxplot(col="grey")+
      labs(x = "Rating", y="Popularity of the tastes", col = "Cocoa\npercent", title = "Rating versus Taste Popularity")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1),legend.position=c(.1,.9))
    ggp <- ggplotly(p, tooltip = c("text")) 
    #ggp$x$data[[1]]$hoverinfo <- "none"
    ggp
    }else{
    specific <- chocolate %>% filter(company == ordered_companies$company[input$rank])
    specific$total_tastes <- paste0("\n\t",specific$first_taste, "\n\t", specific$second_taste,"\n\t", specific$third_taste, "\n\t", specific$fourth_taste)
    
    p <- ggplot(chocolate, aes(x=factor(rating),y = pop_taste, col = cocoa_percent))+
      scale_color_gradientn(colours = colfunc(as.integer(max(chocolate$cocoa_percent) - min(chocolate$cocoa_percent)))) + 
      geom_jitter(alpha = 0) +
      geom_boxplot(col="grey") + 
      geom_point(data=specific, aes(x=factor(rating), y = pop_taste, col = cocoa_percent,
                                    text= paste0("Bar: ",specific_bean_origin_or_bar_name,
                                                 "\nReview date: ",review_date,
                                                 "\nRating: ", rating,
                                                 "\nTastes: ", total_tastes))) +
      labs(x = "Rating", y="Popularity of the tastes", col = "Cocoa\npercent", title = "Rating versus Taste Popularity")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1),legend.position=c(.1,.9))
    ggp <- ggplotly(p, tooltip = c("text")) 
    ggp$x$data[[1]]$hoverinfo <- "none"
    ggp
    }
  }))
  
  output$companydata <- renderPlotly(({
    
    if(is.na(input$rank)){
      c <- ggplot(data=company_data, aes(x = n, y = avg_rating,
                                         text= paste0("Name: ",company,
                                                      "\nRank: ",rank, 
                                                      "\nAverage rating: ",round(avg_rating, digits=3),
                                                      "\nNumber of bars: ", n))) +
        geom_jitter(col="chocolate") +
        theme_bw() +
        labs(title="Number of Bars versus Average Rating", x="Number of bars", y="Average rating")
      
      ggp2 <- ggplotly(c, tooltip = c("text")) 
      ggp2
    }
    else{
    this_company <- company_data %>% filter(company == ordered_companies$company[input$rank])

    c <- ggplot(data=company_data, aes(x = n, y = avg_rating,
                                       text= paste0("Name: ",company,
                                                    "\nRank: ",rank, 
                                                    "\nAverage rating: ",round(avg_rating, digits=3),
                                                    "\nNumber of bars: ", n))) +
      geom_jitter(col="chocolate") +
      geom_point(data=this_company, aes(x=n, y=avg_rating), col="black", shape="star", size=2) +
      theme_bw() +
      labs(title="Number of bars versus average rating", x="Number of bars", y="Average rating")
    
    ggp2 <- ggplotly(c, tooltip = c("text")) 
    ggp2
    }
  }))
  
  output$world <- renderLeaflet({

    if(is.na(input$rank)){
      
      fig <- fig %>% 
        addMarkers(data = chocolate, lng = ~company_lon, lat = ~company_lat,label = ~company, icon=~icons["factory"]) %>%
        addMarkers(data = chocolate, lng = ~bean_lon, lat = ~bean_lat,label = ~specific_bean_origin_or_bar_name, icon=~icons["bean"])
      
    }else{
      
    specific <- chocolate %>% filter(company == ordered_companies$company[input$rank])
    specific$bean_lat <- jitter(specific$bean_lat, factor = 1)
    specific$bean_lon <- jitter(specific$bean_lon, factor = 1)
    names(specific)[names(specific) == "beans"] <- "Cocoa beans"
    names(specific)[names(specific) == "cocoa_butter"] <- "Cocoa butter"
    names(specific)[names(specific) == "vanilla"] <- "Vanilla"
    names(specific)[names(specific) == "lecithin"] <- "Lecithin"
    names(specific)[names(specific) == "salt"] <- "Salt"
    names(specific)[names(specific) == "sugar"] <- "Sugar"
    names(specific)[names(specific) == "sweetener_without_sugar"] <- "Sweetener"
    t_specific <- data.frame(t(specific))
    rows <- c("Cocoa beans","Cocoa butter","Vanilla","Lecithin","Salt","Sugar","Sweetener")
    colnames(t_specific) <- t_specific["specific_bean_origin_or_bar_name",]

    # Add info for company
    pnt <- st_as_sf(data.frame(x = as.numeric(specific$company_lon[1]), y = as.numeric(specific$company_lat[1])),
                    coords = c("x", "y"),
                    crs = 4326)
    
    AVG <- company_data$avg_rating[company_data$company == specific$company[1]]
    
    content <- paste0(
                     "<b>",specific$company[1],"</b>",
                     "<br/>Average rating: ",
                     round(AVG, digits=3)
    )

    fig <- fig %>% 
      addMarkers(data = pnt,group ="pnt", label = specific$company[1], icon=~icons["factory"], popup = content)
    
    for(i in 1:nrow(specific)){

      # Add graph for specific bars
      data1 <- data.frame(group = rows, value = as.numeric(t_specific[rows,colnames(t_specific)[i]])) %>% 
        filter(value == 1)%>% 
        arrange(desc(group)) %>%
        mutate(ypos = cumsum(value) - .5 )
      
      p <- ggplot(data1, aes(x="", y=value, fill=group)) +
        geom_bar(stat="identity", width=1) +
        scale_fill_brewer(palette="YlOrBr")+
        geom_text(aes(y = ypos, label = group), color = "Black", size=3) +
        labs(title=" Ingredients:")+
        theme_void() +
        theme(legend.position="none")

      pnt <- st_as_sf(data.frame(x = as.numeric(specific$bean_lon[i]), y = as.numeric(specific$bean_lat[i])),
                     coords = c("x", "y"),
                     crs = 4326)
      fig <- fig %>% 
        addMarkers(data = pnt,group ="pnt", label = specific$specific_bean_origin_or_bar_name[i], icon = ~icons["bean"],popup = popupGraph(list(p), width=75, height=100))
    }
    fig
    }

  })
  
}


shinyApp(ui = ui, server = server)