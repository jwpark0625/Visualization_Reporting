library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)

train <- read.csv('train.csv')


df <- train
df <- mutate_all(df, ~replace(., is.na(.), 0))  # 결측치 0으로 
df <- df[, c('SalePrice','LotFrontage','LotArea','YearBuilt')]
# 판매가, 부동산과 도로와의 거리, 면적, 지어진 년도
str(df)

plot(df, col = 'steelblue',pch = 19)

cor(df)

model <- lm(SalePrice ~ YearBuilt+LotFrontage+
              LotArea+YearBuilt, data = df)
#종속변수 판매가

summary(model)
#p value가 LotFrontage가 좀 더 신뢰성이 낮다

model <- lm(SalePrice ~ YearBuilt, data = df)
# 독립변수를 지어진년도로 종속변수를 판매가로
plot(model)    # model의 네가지 plot

