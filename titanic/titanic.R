#install.packages('plotly')
#install.packages('ggthemes')
library(tidyverse)
library(plotly)
library(data.table)
library(ggplot2)
library(ggthemes)
library(dplyr)

test <- read.csv('C:/Users/User/Desktop/데이터/titanic/test.csv', na.strings = c(''))
train <- read.csv('C:/Users/User/Desktop/데이터/titanic/train.csv', na.strings = c(''))

#결측치/이상치 확인
colSums(is.na(test))  #NA: Cabin(327),Age(86), Fare(1)
colSums(is.na(train)) #NA: Cabin(687),Age(177), Embarked(2)

summary(test) #age 이상치 존재

##feature engineering##
#Family Size 열 추가
test$FamilySize <- test$SibSp + test$Parch + 1 #형제+부모+자신
train$FamilySize <- train$SibSp + train$Parch + 1


#Fare 결측치 평균으로 대체
test$Fare[is.na(test$Fare)] <- mean(test$Fare, na.rm = TRUE)

#Embarked 결측치 다수값으로 대체
table(train$Embarked) #S가 가장 많다
train$Embarked[is.na(train$Embarked)] <- 'S'


#age평균 - 이름에서 surname별로 구하기
#1 surname 추출 및 변환
train$SName <- gsub('(.*, )|(\\..*)','',train$Name)
table(train$Sex, train$SName)

ect <- c('Capt','Col','Don','Dr','Jonkheer','the Countess',
         'Lady','Major','Mlle','Mme','Rev','Sir','Dona')
train$SName[train$SName %in% ect] <- 'Others'
table(train$Sex, train$SName)


#2 surname별 나이 평균
m_M <- mean(train$Age[train$SName=='Master'], na.rm = TRUE)
m_Mr <- mean(train$Age[train$SName=='Mr'], na.rm = TRUE)
m_Mrs <- mean(train$Age[train$SName=='Mrs'], na.rm = TRUE)
m_Mis <- mean(train$Age[train$SName=='Miss'], na.rm = TRUE)
m_Ms <- mean(train$Age[train$SName=='Ms'], na.rm = TRUE)
m_O <- mean(train$Age[train$SName=='Others'], na.rm = TRUE)

#3 위에서 구한 평균으로 NaN 채우기
train$Age <- ifelse(train$SName=='Master', ifelse(is.na(train$Age), m_M, train$Age), train$Age)
train$Age <- ifelse(train$SName=='Mr', ifelse(is.na(train$Age), m_Mr, train$Age), train$Age)
train$Age <- ifelse(train$SName=='Mrs', ifelse(is.na(train$Age), m_Mrs, train$Age), train$Age)
train$Age <- ifelse(train$SName=='Miss', ifelse(is.na(train$Age), m_Mis, train$Age), train$Age)
train$Age <- ifelse(train$SName=='Ms', ifelse(is.na(train$Age), m_Ms, train$Age), train$Age)
train$Age <- ifelse(train$SName=='Others', ifelse(is.na(train$Age), m_O, train$Age), train$Age)

#4 testset도 동일하게
test$SName <- gsub('(.*, )|(\\..*)','',test$Name)
test$SName[test$SName %in% ect] <- 'Others'

M <- mean(test$Age[test$SName=='Master'], na.rm = TRUE)
Mr <- mean(test$Age[test$SName=='Mr'], na.rm = TRUE)
Mrs <- mean(test$Age[test$SName=='Mrs'], na.rm = TRUE)
Mis <- mean(test$Age[test$SName=='Miss'], na.rm = TRUE)
Ms <- mean(test$Age[test$SName=='Ms'], na.rm = TRUE)
O <- mean(test$Age[test$SName=='Others'], na.rm = TRUE)

test$Age <- ifelse(test$SName=='Master', ifelse(is.na(test$Age), M, test$Age), test$Age)
test$Age <- ifelse(test$SName=='Mr', ifelse(is.na(test$Age), Mr, test$Age), test$Age)
test$Age <- ifelse(test$SName=='Mrs', ifelse(is.na(test$Age), Mrs, test$Age), test$Age)
test$Age <- ifelse(test$SName=='Miss', ifelse(is.na(test$Age), Mis, test$Age), test$Age)
test$Age <- ifelse(test$SName=='Ms', ifelse(is.na(test$Age), Ms, test$Age), test$Age)
test$Age <- ifelse(test$SName=='Others', ifelse(is.na(test$Age), O, test$Age), test$Age)

#필요없는 열 제거
train <- subset(train, select=-c(Name, Cabin, Ticket))
test <- subset(test, select=-c(Name, Cabin, Ticket))

#라벨 변환
train$FamilySize = cut(train$FamilySize, c(0, 1, 4, 15), 
                       include.lowest = TRUE)
levels(train$FamilySize) = c("single", "small family(2~4)", "big family(5+)")
levels(train$Survived) = c('Not Survived','Survived')
#----------test dataset
test$FamilySize = cut(test$FamilySize, c(0, 1, 4, 15), 
                      include.lowest = TRUE)
levels(test$FamilySize) = c("single", "small family(2~4)", "big family(5+)")



#타입변환 : 범주형데이터(factor)
train <- train %>%
  dplyr::mutate(Survived = factor(Survived),
                Pclass   = factor(Pclass,ordered = TRUE),
                SName    = factor(SName),
                Sex      = factor(Sex),
                FamilySize   = factor(FamilySize),
                Embarked = factor(Embarked))
#--------test dataset
test <- test %>%
  dplyr::mutate(Pclass   = factor(Pclass,ordered = TRUE),
                SName    = factor(SName),
                Sex      = factor(Sex),
                FamilySize   = factor(FamilySize),
                Embarked = factor(Embarked))

##EDA
#1 Fare
p1 <- plot_ly(type='box',
              data=train, 
              x=~Survived, 
              y=~Fare,
              color=~Survived,
              alpha = 0.3
              )%>%
  layout(title='Survivor by Fare')
p1

group_by(train$Embarked)
#2 Embarked
p2 <- train %>%
  ggplot(aes(Embarked,fill = Survived)) +
  geom_bar(stat='count',position = "fill", alpha=0.85) + 
  scale_fill_brewer(palette = "Paired") + 
  theme_minimal()+
  #scale_y_continuous(labels = percent) +
  labs(x = "Embarked", y = "Survival Rate",
       title = "Survival by Embarked",
       fill = 'Survival')

p2

#3 family size
p3 <- train %>% 
  ggplot(aes(FamilySize,fill = Survived)) +
  geom_bar(stat='count',position = "fill", alpha=0.85) + 
  scale_fill_brewer(palette = "Paired") + 
  theme_minimal()+
  #scale_y_continuous(labels = percent) +
  labs(x = "FamilySize", y = "Survival Rate",
       title = "Survival by FamilySize",
       fill = 'Survival')

p3

#4 Age, Sex
p4 <- train %>%
  filter(Survived=='Survived') %>% #생존자들 분포
  ggplot(aes(x=Age, fill=Sex)) +
  geom_density(alpha=0.4)+
  theme_minimal()+
  labs(title = "Survival Distribution")
p4

p5 <- train %>%
  filter(Survived=='Not Survived') %>% #사망자들 분포
  ggplot(aes(x=Age, fill=Sex)) +
  geom_density(alpha=0.4)+
  theme_minimal()+
  labs(title = "Non-Survival Distribution")
p5

##로지스틱선형 회귀
#모델
levels(train$Survived) = c(0,1) #복구,, 필요한가?
#View(train)
model <- glm(Survived ~., family = binomial(link=logit), data=train)
model
summary(model)
anova(model, test="Chisq")

# 예측
result <- predict(model,newdata=test,type='response')
result <- ifelse(result > 0.5, 1, 0)
result

#성능평가(오류남)
library(caret)
confusionMatrix(result, test$Survived)