
# package loading
library(dplyr); library(ggplot2); library(hrbrthemes); library(stringr); library(lubridate); library(scales); library(shiny); library(leaflet)

# data loading
df = read.csv("Crimes_2020.csv")

# data preprocessing
df.1 = df %>% select(c("Date", "Primary.Type", "Arrest", "Latitude", "Longitude"))
df.2 = df.1 %>% filter(df.1$Primary.Type != "NON-CRIMINAL")
df.3 = df.2 %>% na.omit()

# data encoding
df.3$Arrest = factor(df.3$Arrest, levels = c("FALSE", "TRUE"), labels = c(0, 1))
df.3$Date.Time = strptime(df.3$Date, format = "%m/%d/%Y %H:%M")
df.4 = df.3 %>% na.omit() # no 2.31, 6.31
df.4$Year = year(df.4$Date.Time)
df.4$Month = month(df.4$Date.Time)
df.4$Day = day(df.4$Date.Time)
df.4$Hour = hour(df.4$Date.Time)
df.5 = df.4 %>% na.omit() %>% select(c("Date", "Year", "Month", "Day", "Hour", "Primary.Type", "Arrest", "Latitude", "Longitude"))
df.5$Time = paste(df.5$Year, df.5$Month, df.5$Day, sep = "-")
df.5$Time = date(df.5$Time)

# data preparing
tab.1 = data.frame(table(df.5[, c("Month", "Primary.Type")]))
tab.2 = df.5[, c("Time", "Primary.Type", "Latitude", "Longitude")]
tab.3 = data.frame(table(df.5[, c("Primary.Type", "Hour")]))
tab.3.sf.1 = tab.3 %>% group_by(Primary.Type, Hour) %>% summarise(Ratio.Freq = Freq/sum(Freq)) %>% select("Primary.Type", "Hour")
tab.3.sf.2 = tab.3 %>% group_by(Primary.Type) %>% summarise(Ratio.Freq = Freq/sum(Freq)*100) %>% select("Ratio.Freq")
tab.3.sf.3 = cbind(tab.3.sf.1, tab.3.sf.2) %>% select(c(1,2,4))
colnames(tab.3.sf.3) = c("Primary.Type", "Hour", "Ratio.Freq")
tab.4 = data.frame(table(df.5[, c("Primary.Type", "Arrest")]))

# ui.r
ui = fluidPage(
    
    titlePanel(title = "Incidents of Crime in the City of Chicago in 2020"),
    h5("This is an informative webpage to know the incidents of crime from the Chicago area in 2020."),
    h5("Tab.1 - Frequency of crime by month and crime type via a barchart"),
    h5("Tab.2 - Location of crime by date via a map"),
    h5("Tab.3 - Relationship of crime for crime type and hour via a heatmap"),
    h5("Tab.4 - Proportion of crime arrested by crime type via a piechart"),
    
    tabsetPanel(
        
        # tab 1 - bar
        tabPanel(title = "Frequency of Crime by Month and Type",
                 sidebarLayout(
                     sidebarPanel(selectInput(inputId = "crime.type.tab.1",
                                              label = "Select Crime Type",
                                              choices = sort(unique(tab.1$Primary.Type), decreading = F),
                                              selected = sort(unique(tab.1$Primary.Type), decreading = F)[1],
                                              multiple = T),
                                  selectInput(inputId = "month",
                                              label = "Select Month",
                                              choices = sort(unique(tab.1$Month), decreading = F),
                                              selected = sort(unique(tab.1$Month), decreading = F)[1],
                                              multiple = T)),
                     mainPanel(plotOutput(outputId = "bar")))),
        
        # tab 2 - map
        tabPanel(title = "Location of Crime by Date",
                 sidebarLayout(
                     sidebarPanel(selectInput(inputId = "date",
                                              label = "Select Date",
                                              choices = sort(unique(tab.2$Time), decreading = F),
                                              selected = sort(unique(tab.2$Time), decreading = F)[1])),
                     mainPanel(leafletOutput(outputId = "map", height = 600)))),
        
        # tab 3 - heatmap
        tabPanel(title = "Relationship of Crime for Type and Hour",
                 plotOutput(outputId = "heatmap")),
        
        # tab 4 - pie
        tabPanel(title = "Proportion of Crime Arrested by Type",
                 sidebarLayout(
                     sidebarPanel(selectInput(inputId = "crime.type.tab.2",
                                              label = "Select Crime Type",
                                              choices = sort(unique(tab.4$Primary.Type), decreading = F),
                                              selected = sort(unique(tab.4$Primary.Type), decreading = F)[1])),
                     mainPanel(plotOutput(outputId = "pie"))))
    )
)

# server.r
server = function(input, output) {
    
    # tab 1 - bar
    output$bar = renderPlot({
        df.tab.1 = subset(tab.1, ((tab.1$Month %in% input$month) & (tab.1$Primary.Type %in% input$crime.type.tab.1)))
        ggplot(data = df.tab.1, aes(x = df.tab.1$Month, y = df.tab.1$Freq, fill = df.tab.1$Primary.Type)) +
            geom_bar(stat = "identity") +
            scale_x_discrete(labels = c("1" = "Jan",
                                        "2" = "Feb",
                                        "3" = "Mar",
                                        "4" = "Apr",
                                        "5" = "May",
                                        "6" = "Jun",
                                        "7" = "Jul",
                                        "8" = "Aug",
                                        "9" = "Sep")) +
            labs(title = "Frequency of Incident by Month and Crime Type", x = "Month", y = "Number of Incident", fill = "Crime Type") +
            theme_minimal() +
            theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5)) +
            theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14))
    }) 
    
    # tab 2 - map
    output$map = renderLeaflet({
        df.tab.2 = tab.2 %>% filter(tab.2$Time == input$date)
        factpal = colorFactor(rgb(t(col2rgb(palette()))/255), df.tab.2$Primary.Type)
        df.tab.2 %>% 
            leaflet() %>% 
            addTiles() %>%
            addCircleMarkers(lng = df.tab.2$Longitude, lat = df.tab.2$Latitude, color = factpal(df.tab.2$Primary.Type), radius = 3, weight = 5) %>% 
            addLegend("topright", pal = factpal, values = df.tab.2$Primary.Type, title = "Crime Type", opacity = 1)
    })    
    
    # tab 3 - heatmap
    output$heatmap = renderPlot({
        ggplot(data = tab.3.sf.3, aes(x = tab.3.sf.3$Hour, y = tab.3.sf.3$Primary.Type)) +
            geom_tile(aes(fill = tab.3.sf.3$Ratio.Freq)) +
            scale_fill_gradient(name = "Percent", low = "white", high = "red") +
            labs(title = "Heatmap for Type of Crime and Hour of Day", x = "Hour of Day", y = "Crime Type") +
            theme_ipsum() +
            theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5))
    })
    
    # tab 4 - pie
    output$pie = renderPlot({
        df.tab.4 = tab.4 %>% filter(tab.4$Primary.Type == input$crime.type.tab.2)
        df.tab.4.prop = df.tab.4 %>% 
            mutate(prop = df.tab.4$Freq / sum(df.tab.4$Freq)) %>% 
            mutate(perc = label_percent()(prop))    
        ggplot(data = df.tab.4.prop, aes(x = "", y = df.tab.4.prop$prop, fill = df.tab.4.prop$Arrest)) +
            geom_bar(width = 1, stat = "identity", color = "white") +
            coord_polar("y", start = 0) +
            theme_void() +
            geom_text(aes(label = df.tab.4.prop$perc), position = position_stack(vjust = 0.5), color = "white", size = 5) +
            labs(title = "Arrest Result from Crime Commited", subtitle = "Arrest Result (0 = No; 1 = Yes)", fill = "Crime Type") +
            theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.6),
                  plot.subtitle = element_text(size = 18, hjust = 0.5))
    })
}

# shiny.r
shinyApp(ui = ui, server = server)