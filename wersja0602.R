library(readr)
base <- read_csv("C:/Users/Dawid/Downloads/base.csv")
pop <- read_csv("C:/Users/Dawid/Downloads/pop.csv")
continents<-read_csv("C:/Users/Dawid/Downloads/continents.csv")

library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggspatial)
library(cowplot)
library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(shinythemes)
library(data.table)  # do funkcji setorder()
library(ggrepel)

names.base <- c('Country','Stability', 'Safety','Score', 'Ranking')
colnames(base) <- names.base
base$Country[46] <- 'Hong Kong'
base$Country[71] <- 'Macau'
base$Country[26] <- 'Democratic Republic of the Congo'
base$Country[30] <- 'Czech Republic'
base$Country[128] <- 'USA'
base$Country[127] <- 'UK'

names.pop <- c('Country','Continent', 'Population', 'Area (km2)')
colnames(pop) <- names.pop
pop$Country[222] <- 'USA'
pop$Country[56] <- 'Democratic Republic of the Congo'
pop$Country[221] <- 'UK'
pop$Continent[172] <- 'Asia'

cont <- continents
colnames(cont) <- c('Country', 'Continent')
cont$Country[203] <- 'USA'
cont$Country[121] <- 'Czech Republic'
cont$Country[235] <- 'Bolivia'
cont$Country[163] <- 'UK'
cont$Country[78] <- 'Iran'
cont$Country[86] <- 'Laos'
cont$Country[152] <- 'Russia'
cont$Continent[152] <- 'Asia'
cont$Country[74] <- 'North Korea'
cont$Country[102] <- 'Syria'
cont$Country[249] <- 'Venezuela'
cont$Country[110] <- 'Vietnam'
cont$Country[57] <- 'Tanzania'
cont$Country[101] <- 'Palestine'
cont$Country[14] <- 'Ivory Coast'
cont$Country[97] <- 'South Korea'
cont$Country[13] <- 'Republic of Congo'
cont$Country[150] <- 'Moldova'
cont$Country[20] <- 'Swaziland'
kosovo <- c('Kosovo', 'Europe')
taiwan <- c('Taiwan', 'Asia')
rbind(cont, kosovo, taiwan) -> cont

df <- merge(base, pop)
df2<-df
df %>%
  mutate(Country = reorder(Country,-Ranking),
         Pop_M=paste(round(Population / 1e6, 2), "M")) -> df

world2 <- map_data("world") %>%
  filter(region != "Antarctica")

dane <- merge(df, world2, by.x = 'Country', by.y = 'region', all.y = TRUE)
setorder(dane, order)
dane_cont <- merge(cont, dane, by.x = 'Country', by.y = 'Country', all.y = TRUE)
setorder(dane_cont, order)

short <- c('Africa', 'Asia', 'Europe', 'North Am.', 'Oceania', 'South Am.')

windowsFonts(napisy=windowsFont('Corbel'),
             liczby=windowsFont(('Source Sans Pro')))

#UI####
ui <- fluidPage(
  theme=shinytheme('sandstone'),
  navbarPage(
    'Safety and Political and Economic Stability around the World',
    position=c('fixed-top'),
    br(),
    br(),
    br(),
    br(),
    tabPanel('Plots',
             sidebarLayout(
               sidebarPanel (
                 selectInput('continent','Choose a continent:',
                             c('All Continents','Africa', 'Asia','Europe', 
                               'North America', 'South America', 'Oceania')),
                 tableOutput(outputId = 'summary'),
                 plotOutput(outputId = 'mean', width = '100%', height = '220px')),
               mainPanel(
                 tabsetPanel(
                   type='tabs',
                   tabPanel(
                            'Map',br(), plotlyOutput('inter'), width = '130%', height = '470px'),
                   tabPanel(
                            'Plotly',br(), plotlyOutput(outputId = 'plotly')),
                   tabPanel(
                            'Relationship',br(), 
                            plotOutput(outputId = 'bubble', width = '100%', height = '440px'),
                            checkboxInput('names','Show Names',TRUE))),
                 
               ))),
    tabPanel('Dataset',
             wellPanel(tags$a('SEE ORIGINAL SOURCE',
                    href='https://www.worlddata.info/quality-of-life.php?expats=0&stability=50&rights=1&health=1&safety=50&climate=1&costs=1&popularity=1#ranges')),
             dataTableOutput(outputId = 'df'))
  ))



#SERVER####
server <- function(input, output) {
  observe({
    
    output$df <- renderDataTable(df2)

    if(input$continent == 'All Continents'){
      bar_colors <- c(rep('lightblue', 6))
    }else if(input$continent == 'Africa'){
      bar_colors <- c('Africa' = 'red', 'Asia' = 'lightblue','Europe' = 'lightblue', 
                      'North Am.' = 'lightblue', 'South Am.' = 'lightblue', 'Oceania' = 'lightblue' )
    }else if(input$continent == 'Asia'){
      bar_colors <- c('Asia' = 'red', 'Africa' = 'lightblue','Europe' = 'lightblue', 
                      'North Am.' = 'lightblue', 'South Am.' = 'lightblue', 'Oceania' = 'lightblue' )
    }else if(input$continent == 'Oceania'){
      bar_colors <- c('Oceania' = 'red', 'Africa' = 'lightblue','Europe' = 'lightblue', 
                      'North Am.' = 'lightblue', 'South Am.' = 'lightblue', 'Asia' = 'lightblue' )
    }else if(input$continent == 'Europe'){
      bar_colors <- c('Europe' = 'red', 'Africa' = 'lightblue','Asia' = 'lightblue', 
                      'North Am.' = 'lightblue', 'South Am.' = 'lightblue', 'Oceania' = 'lightblue' )
    }else if(input$continent == 'North America'){
      bar_colors <- c('North Am.' = 'red', 'Africa' = 'lightblue','Europe' = 'lightblue', 
                      'Asia' = 'lightblue', 'South Am.' = 'lightblue', 'Oceania' = 'lightblue' )
    }else if(input$continent == 'South America'){
      bar_colors <- c('South Am.' = 'red', 'Africa' = 'lightblue','Europe' = 'lightblue', 
                      'North Am.' = 'lightblue', 'Asia' = 'lightblue', 'Oceania' = 'lightblue' )}
    
    
    output$mean <- renderPlot(
      
      df %>%
        group_by(Continent) %>%
        summarise(Mean = mean(Score)) %>%
        cbind(short) %>%
        mutate(order = reorder(short, Mean)) %>%
        ggplot(aes(x = order, y = Mean)) +
        geom_col(aes(fill = short)) +
        guides(fill = FALSE) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 55, hjust= 1, vjust=1)) +
        xlab(NULL) +
        ylab(NULL) +
        ggtitle('Mean score for each continent') +
        scale_fill_manual(values = bar_colors)
      
    )
    
    output$summary <- renderTable(
      
      if(input$continent == 'All Continents'){
        df %>%
          summarise('Max Score' = max(Score), 'Min Score' = min(Score), 'Mean Score' = round(mean(Score), 2)) -> summ
        summ
      }else{
        df %>%
          filter(Continent == input$continent) %>%
          summarise('Maximum Score' = max(Score), 'Minimum Score' = min(Score), 'Mean Score' = round(mean(Score), 2)) %>%
          as_tibble() -> summ
        summ
      }
    )
    
    output$bubble <- renderPlot({
      if(input$continent == 'All Continents'){
        df -> bazaCont}
      else if(input$continent != 'All Continents'){
        df %>%
          filter(Continent == input$continent) -> bazaCont}
      ggplot(bazaCont, aes(x = Stability, y =Safety)) +
        geom_point(aes(col=Continent, size=Population),alpha=0.7)+
        scale_size(guide='none', range=c(.1,24))+
        #ggtitle(as.character(paste('Safety and stability level in', input$continent))) +
        geom_smooth(method='lm',se=FALSE,color='slateblue2') +
        xlim(c(6, 99)) +
        annotate("text", x = 75, y = 25, 
                 label = 'Size of the dots is determined \nby the size of the population\n of each country.', 
                 hjust = -0.05, size = 3,
                 family='napisy') +
        coord_cartesian(xlim = c(9, 100), clip = 'off') +
        theme_minimal()+
        theme(
          text = element_text(family='napisy', size=12),
          axis.title = element_text(face='bold',size=12),
          axis.text = element_text(family='liczby',size=7),
          plot.title = element_text(family = 'napisy', face = 'bold', size = 16))+
        scale_color_manual(values=c('firebrick2','gold2','dodgerblue','darkolivegreen4','deeppink','chartreuse2'))+
        if(input$names){geom_text_repel(aes(label = Country))}
    })
    
    output$plotly <- renderPlotly({
      if(input$continent == 'All Continents'){
        df -> bazaCont
      }
      else if(input$continent != 'All Continents'){
        df %>%
          filter(Continent == input$continent) -> bazaCont}
      bazaCont %>% 
        plot_ly(x = ~Stability, 
                y = ~Safety,
                hoverinfo = 'text',
                text = ~paste('Country:', Country, 
                              '<br>Ranking:', Ranking, 
                              '<br>Stability:', Stability, 
                              '<br>Safety:', Safety, 
                              '<br>Population:', Pop_M)) %>% 
        add_markers(color = ~Continent)%>%
        layout(font = 'Source Sans Pro', 
               #title = list(text=paste('Safety and stability level in', input$continent)), 
               legend = list(x=0.77, y=0.1, bgcolor = 'aliceblue', bordercolor = 'white')) 
    })
    
    output$inter <- renderPlotly({
      if(input$continent == 'All Continents'){
        dane -> bazaCont}
      else if(input$continent != 'All Continents'){
        dane_cont %>%
          filter(Continent.x == input$continent) -> bazaCont}
      
      mapka <- ggplot(data = bazaCont, aes( x = long, y = lat, group = group)) +
        geom_polygon(aes(text1 = Country,
                         text2 = Safety, 
                         text3 = Stability,
                         fill = Score, 
                         text4 = Ranking), 
                     color = 'black', size = 0.02) +
        theme_map() +
        scale_fill_continuous(na.value = 'gray', low = 'red', high = 'green') +
        ggtitle(input$continent) +
        guides(fill=guide_colorbar(title="Score")) +
        theme(plot.title = element_text(size = 12, hjust = 0.5))
      
      if(input$continent == 'Oceania'){
        mapka <- mapka + coord_fixed(ratio = 1, xlim = c(100, 180), ylim = c(-70, 0))
        mapka <- ggplotly(mapka, tooltip = c('text1', 'text2', 'text3', 'fill', 'text4'))
        mapka
        
      }else if(input$continent == 'North America'){
        mapka <- mapka + coord_fixed(ratio = 1, xlim = c(-180, 10), ylim = c(-10, 90))
        mapka <- ggplotly(mapka, tooltip = c('text1', 'text2', 'text3', 'fill', 'text4'))
        mapka
        
      }else{
        mapka <- mapka + coord_fixed(ratio = 1)
        
        ggplotly(mapka, tooltip = c('text1', 'text2', 'text3', 'fill', 'text4')) %>%
          layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE)) -> mapka
        mapka
      }
    })
  })}

#WYWO?ANIE####
shinyApp(ui = ui, server = server)

