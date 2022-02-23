data1<-read.csv(file="korea_2020.csv")
data2<-read.csv(file="korea_2021.csv")

# 일시를 기준으로 dat1, dat2 데이터 합치기
annual_data<- rbind(data1, data2)

#생성 이후 맨 앞의 컬럼 제거 필요
write.csv(annual_data,file="Korea_weather.csv")
