library(shiny)
library(lubridate)


server <- function(input, output,session){
    
  overviewData <- reactive({
    overview_data <- overview_data[overview_data$지점명 == input$region,]
    overview_data <- overview_data[input$daterange[1] <= overview_data$일시 & 
                                     overview_data$일시 <= input$daterange[2],]
  }) 
  
    
  output$title <- renderText({
    paste('<B>',input$region,"의...</B>")
  })
  
  output$temp <- renderText({
    paste(input$month,"월 평균 기온")
  })
  
    
  output$tempValue <- renderText({
    overview_data <- overviewData()
    month_data <- overview_data[month(overview_data$일시)==input$month,]
    v <- mean(month_data$평균기온, na.rm=TRUE)
    paste(round(v,1), "℃")
  })
  
  output$htemp <- renderText({
    paste(input$month,"월 평균 최고기온")
  })
  
  output$htempValue <- renderText({
    overview_data <- overviewData()
    month_data <- overview_data[month(overview_data$일시)==input$month,]
    v <- mean(month_data$최고기온,na.rm=TRUE)
    paste(round(v,1), "℃")
  })
  
  output$humid <- renderText({
    paste(input$month,"월 평균 상대 습도")
  })
  
  output$humidValue <- renderText({
    overview_data <- overviewData()
    month_data <- overview_data[month(overview_data$일시)==input$month,]
    v <- mean(month_data$평균상대습도,na.rm=TRUE)
    paste(round(v,1), "%")
  })
  
  output$wind <- renderText({
    paste(input$month,"월 평균 풍속")
  })
  
  output$windValue <- renderText({
    overview_data <- overviewData()
    month_data <- overview_data[month(overview_data$일시)==input$month,]
    v <- mean(month_data$평균풍속,na.rm=TRUE)
    paste(round(v,1), "m/s")
  })
  
  output$rain <- renderText({
    paste(input$month,"월 평균 일강수량")
  })
  
  output$rainValue <- renderText({
    overview_data <- overviewData()
    month_data <- overview_data[month(overview_data$일시)==input$month,]
    v <- mean(month_data$일강수량,na.rm=TRUE)
    paste(round(v,1), "mm")
  })
  
  output$ltemp <- renderText({
    paste(input$month,"월 평균 최저기온")
  })
  
  output$ltempValue <- renderText({
    overview_data <- overviewData()
    month_data <- overview_data[month(overview_data$일시)==input$month,]
    v <- mean(month_data$최저기온,na.rm=TRUE)
    paste(round(v,1), "℃")
  })
  
  output$tempPlot <- renderPlotly({
    overview_data <- overviewData()
    overview_data %>%
      plot_ly() %>%
      add_trace(x=~일시,y=~최저기온, name='최저기온',type='scatter',opacity=0.6,marker=list(color='steelblue')) %>%
      add_trace(x=~일시,y=~최고기온, name='최고기온',type='scatter',opacity=0.5, marker=list(color='tomato'))%>%
      add_bars(x=~일시, y=~일강수량,type='bar', name='강수량(mm)')%>%
      layout(yaxis=list(title='기온',side='left'),yaxis2 = list(overlaying='y',side='right',title='강수량'))
  })
    
############### End of Part 'Overview' #################
  
    selectedData <- reactive({
      overview_data[, c(input$xcol, input$ycol)] 
    }) 
    
    output$table <- DT::renderDataTable({ 
        DT::datatable(overview_data) 
    }) 
    
    output$summary <- renderPrint({ 
        summary(overview_data) 
    }) 
    
    output$str <- renderPrint({ 
        str(overview_data) 
    })
    
    output$cor <- renderPrint({
        cor(selectedData(), use="complete.obs")
    })
    
    output$corrplot <- renderPlot({
        corrplot(cor(overview_data[,4:12], use="complete.obs"))
    })
    
    output$totalcorr <- renderPrint({
        cor(overview_data[,4:12], use="complete.obs")
    })
    
    output$lm <- renderPrint({ 
      summary(lm(평균기온 ~ 최저기온 + 
                       최고기온 + 
                       일강수량 + 
                       최대풍속 + 
                       평균풍속 + 
                       합계일사 + 
                       평균전운량, 
                     data=overview_data)) 
    }) 
    output$lmplot <- renderPlot({ 
      plot(lm(평균기온 ~ 최저기온 + 
                    최고기온 + 
                    일강수량 + 
                    최대풍속 + 
                    평균풍속 + 
                    합계일사 + 
                    평균전운량, 
                  data=overview_data)) 
})
}
    