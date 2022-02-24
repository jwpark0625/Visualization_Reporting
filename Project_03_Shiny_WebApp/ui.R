# install.packages("shinydashboard")
library(shinydashboard)
library(shiny)
library(ggplot2)
library(DT)
library(markdown)
library(rpart)
library(rpart.plot)
library(corrplot)
library(dplyr)
library(plotly)

overview_data <- read.csv("weather1.csv", header = T)
head(overview_data)
names(overview_data) <- c("지점","지점명","일시","평균기온","최저기온","최고기온","일강수량","최대풍속","평균풍속","평균상대습도","합계일사","평균전운량")
head(overview_data)
str(overview_data)
overview_data$일시 <- as.Date(overview_data$일시)

ui <- fluidPage(
    titlePanel("통계 대시보드"),
    navbarPage("목록",
               tabPanel("Table",
                        DT::dataTableOutput("table")
               ),
               
               tabPanel("Overview",
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            selectInput('region', 'where?', choices = unique(overview_data$지점명)),
                            dateRangeInput('daterange','Set Range',
                                           start = NULL, end = NULL,max = '2022-02-22', min = '2015-01-01',
                                           language = 'kr'), #input$daterange[1]
                            numericInput('month', 'month(1-12)', 1, min = 1, max = 12) #input$month/ month(date)
                          ),
                          
                          mainPanel(
                            htmlOutput('title', style='font-size:20px; padding:10px'),
                            fluidRow(
                              column(4,
                                     htmlOutput('temp', style='font-size:13px; padding:5px; font-weight:700;'),
                                     htmlOutput('tempValue',style='font-size:18px; font-weight:900;')),
                              column(4,
                                     htmlOutput('htemp', style='font-size:13px; padding:5px;font-weight:700;'),
                                     htmlOutput('htempValue',style='font-size:18px; font-weight:900;')),
                              column(4,
                                     htmlOutput('ltemp', style='font-size:13px; padding:5px;font-weight:700;'),
                                     htmlOutput('ltempValue',style='font-size:18px; font-weight:900;')),
                              div(style = 'height:70px;')
                            ),
                            fluidRow(
                              column(4,
                                     htmlOutput('wind', style='font-size:13px; padding:5px;font-weight:700;'),
                                     htmlOutput('windValue',style='font-size:18px; font-weight:900;')),
                              column(4,
                                     htmlOutput('rain', style='font-size:13px; padding:5px;font-weight:700;'),
                                     htmlOutput('rainValue',style='font-size:18px; font-weight:900;')),
                              column(4,
                                     htmlOutput('humid', style='font-size:13px; padding:5px;font-weight:700;'),
                                     htmlOutput('humidValue',style='font-size:18px; font-weight:900;')),
                              div(style = 'height:60px;')
                            ),
                            
                            plotlyOutput('tempPlot')
                            
                          )
                        )
               ),
                              
               
               tabPanel("Summary",
                        verbatimTextOutput("summary"),
                        verbatimTextOutput("str")
               ),
               tabPanel("Statistics",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput('xcol', 'X Variable', names(overview_data)),
                                selectInput('ycol', 'Y Variable', names(overview_data),
                                            selected=names(overview_data)[[2]])
                            ),
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
                                             c("Scatter"="p", "Line"="l", "BoxPlot"="b", "BarPlot"="bp")
                                )
                            ),
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
     )
   )
 )

