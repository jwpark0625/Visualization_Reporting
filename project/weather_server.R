
server <- function(input, output, session) { 
  selectedData <- reactive({ 
      weather_data <- weather_data[weather_data$지점명 == input$region,]
    weather_data[, c(input$xcol, input$ycol)] 
  }) 
  output$table <- DT::renderDataTable({ 
    DT::datatable(weather_data) 
  }) 
  output$summary <- renderPrint({ 
    summary(weather_data) 
  }) 
  output$str <- renderPrint({ 
    str(weather_data) 
  }) 
  output$cor <- renderPrint({ 
    cor(selectedData(), use="complete.obs") 
  }) 
  output$corrplot <- renderPlot({ 
    corrplot(cor(weather_data[,3:10], use="complete.obs")) 
  }) 
  output$totalcorr <- renderPrint({ 
    cor(weather_data[,3:10], use="complete.obs") 
  }) 
  output$plot <- renderPlot({ # plot(weather_data, type=input$plotType) 
    if(input$plotType == "p"){ plot(weather_data) 
    } 
    else if(input$plotType == "l"){ 
      ggplot(weather_data, aes(x=일시, y=평균기온)) + geom_line() 
    } 
    else if(input$plotType == "b"){ 
      ggplot(weather_data, aes(x=지점명, y=평균기온)) + geom_boxplot() 
      ggplot(weather_data, aes(x=지점명, y=평균전운량)) + geom_boxplot() 
    } 
    else if(input$plotType == "bp"){ 
      ggplot(weather_data, aes(x=최저기온)) + geom_bar() 
      # 원본
      #ggplot(weather_data, aes(x=최저기온, y=평균기온)) + geom_bar() 
    } 
  }) 
  output$lm <- renderPrint({ 
    summary(lm(평균기온 ~ 최저기온 + 
                     최고기온 + 
                     일강수량 + 
                     최대풍속 + 
                     평균풍속 + 
                     합계일사 + 
                     평균전운량, 
                   data=weather_data)) 
  }) 
  output$lmplot <- renderPlot({ 
    plot(lm(평균기온 ~ 최저기온 + 
                  최고기온 + 
                  일강수량 + 
                  최대풍속 + 
                  평균풍속 + 
                  합계일사 + 
                  평균전운량, 
                data=weather_data)) 
  }) 
  output$tree <- renderPrint({ 
    rpart(평균기온 ~ 최저기온 + 
                최고기온 + 
                일강수량 + 
                최대풍속 + 
                평균풍속 + 
                합계일사 + 
                평균전운량, 
              data=weather_data) 
  }) 
  output$treeplot <- renderPlot({ 
    rpart.plot(rpart(평균기온 ~ 최저기온 + 
                           최고기온 + 
                           일강수량 + 
                           최대풍속 + 
                           평균풍속 + 
                           합계일사 + 
                           평균전운량, 
                         data=weather_data)) 
  }) 
} 