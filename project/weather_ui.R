#install.packages("shiny") 
#install.packages("ggplot2") 
#install.packages("markdown") 
#install.packages("DT") 
#install.packages("corrplot") 
#install.packages(c("rpart", "rpart.plot")) 

#getwd()
#setwd("C:/git_practice/KNUBI_Visualization_Reporting/project")

library(shiny) 
library(ggplot2) 
library(DT) 
library(markdown) 
library(rpart) 
library(rpart.plot) 
library(corrplot)

weather_data <- read.csv("Korea_weather.csv",header = T) 
names(weather_data) <- c("지점","지점명","일시","평균기온","최저기온","최고기온","일강수량","최대풍속","평균풍속","합계일사","평균전운량") 
head(weather_data) 
str(weather_data)
weather_data$일시 <- as.Date(weather_data$일시)


ui <- fluidPage( 
  titlePanel("통계 대시보드"), 
  navbarPage("목록", 
             tabPanel("Table", 
                      DT::dataTableOutput("table") ), 
             tabPanel("Summary", 
                      verbatimTextOutput("summary"), 
                      verbatimTextOutput("str") ), 
             tabPanel("Statistics", 
                      sidebarLayout( 
                        sidebarPanel( 
                            selectInput('region', 'Where', choices = unique(weather_data$지점명)), 
                            selectInput('xcol', 'X Variable', names(weather_data)), 
                            selectInput('ycol', 'Y Variable', names(weather_data), 
                                      selected=names(weather_data)[[2]]) ), 
                        mainPanel( 
                          verbatimTextOutput("cor"), 
                          plotOutput("corrplot"), 
                          verbatimTextOutput("totalcorr") 
                        ) 
                      ) 
             ), 
             tabPanel("Plot", 
                      sidebarLayout( 
                        sidebarPanel( 
                          radioButtons("plotType", "Plot type", 
                                       c("Scatter"="p", "Line"="l", "BoxPlot"="b", "BarPlot"="bp") ) ), 
                        mainPanel( 
                          plotOutput("plot")
                        ) 
                      ) 
             ), 
             tabPanel("Linear Model", 
                      mainPanel( 
                        verbatimTextOutput("lm"), 
                        plotOutput("lmplot") 
                      ) 
             ), 
             tabPanel("Tree Model", 
                      mainPanel( 
                        verbatimTextOutput("tree"), 
                        plotOutput("treeplot")
                      )
             ) 
  ) 
) #server <- function(input, output){ 

selectedData<-reactive({ 
  get(input$dataSelect) 
})

weather_data2 = weather_data[sample(nrow(weather_data), 1000), ] 
output$mytable1 <- DT::renderDataTable({ 
  DT::datatable(weather_data2[, input$show_vars, drop = FALSE])
}) 
