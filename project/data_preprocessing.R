data1<-read.csv(file="korea_2020.csv")
data2<-read.csv(file="korea_2021.csv")

# 일시를 기준으로 dat1, dat2 데이터 합치기
annual_data<- rbind(data1, data2)

write.csv(annual_data,file="Korea_weather.csv")
