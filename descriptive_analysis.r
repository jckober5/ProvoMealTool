### Read in the Necessary Packages
library(RMySQL)
library(ggplot2)
library(plotly)
library(devtools)
# devtools::install_github('jckober5/R_Package_Remove_Outliers/jkoutliers')
library(jkoutliers)
options(scipen=999)

### Bring in the Data Successfully
mysqlconnection <- dbConnect(RMySQL::MySQL(),
                             dbname='forms',
                             host='23.239.4.168',
                             port=3306,
                             user='jkober',
                             password='4N4CrFHevPzpdt!')

df <- dbReadTable(mysqlconnection, 'provo_food')
provoRest <- read.csv(file.choose(),header = TRUE)

### Merge the 2 data sets together
df2 <- merge(df, provoRest, by.x = 'restaurant', by.y = 'Name', all.x = TRUE)



model.data <- data.frame(Restaurant = df2$restaurant
                         , Cuisine = df2$type
                         , Surveys = rep(1,nrow(df2))
)
model.data <- aggregate(Surveys ~ Cuisine, data = model.data, sum)

ggplotly(ggplot(data = model.data, aes(x = Cuisine, y = Surveys), fill = 'darkgreen') +
           geom_bar(stat = "identity", color = "#000000", fill = 'darkgreen', alpha = .4, width = .75, show.legend = FALSE) +
           coord_flip() + 
           theme(panel.background = element_rect(fill = "#202123"),
                 plot.background = element_rect(fill = "#202123"),
                 title = element_text(colour = '#ffffff')) +
           ggtitle("Survey Recommendations Per Cuisine")
)

### aggregate Provo Restaurants by the Cuisine
provo.model.data <- aggregate(rep(1,nrow(provoRest)) ~ type, data = provoRest, sum)
provo.model.data <- data.frame(Cuisine = provo.model.data$type
                               , Restaurants = provo.model.data$`rep(1, nrow(provoRest))`)

### build a pie chart for distribution of Cuisines in Provo UT
colors <- c('rgb(90,117,98)', 'rgb(123,145,129)', 'rgb(75,92,107)', 'rgb(224,164,156)', 'rgb(235,194,189)', 'rgb(251,244,232)',
            'rgb(216,215,215)',
            'rgb(131,181,108)',
            'rgb(156,196,137)',
            'rgb(181,211,167)',
            'rgb(205,225,196)',
            'rgb(230,240,226)')

provo.pie <- plot_ly(provo.model.data, labels = ~Cuisine, values = ~Restaurants, type = 'pie',
                     textposition = 'inside',
                     textinfo = 'label+percent',
                     #insidetextfont = list(color = '#FFFFFF'),
                     hoverinfo = 'text',
                     text = ~paste(Restaurants, 'Restaurants'),
                     marker = list(colors = colors,
                                   line = list(color = '#FFFFFF', width = 1)),
                     #The 'pull' attribute can also be used to create space between the sectors
                     showlegend = FALSE)
provo.pie <- provo.pie %>% layout(title = 'Distribution of Cusines in Provo, UT',
                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
  layout(plot_bgcolor='transparent') %>% 
  layout(paper_bgcolor='transparent')

provo.pie

### build a pie chart for distribution of Cuisines recommended in Survey
survey.pie <- plot_ly(model.data, labels = ~Cuisine, values = ~Surveys, type = 'pie',
                      textposition = 'inside',
                      textinfo = 'label+percent',
                      #insidetextfont = list(color = '#FFFFFF'),
                      hoverinfo = 'text',
                      text = ~paste(Surveys, 'Surveys'),
                      marker = list(colors = colors,
                                    line = list(color = '#FFFFFF', width = 1)),
                      #The 'pull' attribute can also be used to create space between the sectors
                      showlegend = FALSE)
survey.pie <- survey.pie %>% layout(title = 'Distribution of Cusines Recommended in Survey',
                                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
  layout(plot_bgcolor='transparent') %>% 
  layout(paper_bgcolor='transparent')

survey.pie