forecast=read.csv("FinalForecastData.csv")
#removing redundancy
forecast=forecast[-c(5,9,13,15,19,21,23,25,26,28,27,29,31,33)]
View(forecast)
names(forecast)=make.names(names(forecast))
names(forecast)
nrow(forecast)
ncol(forecast)
str(forecast)

#visualising columns which are continuous
par(mfrow=c(4,3),mar=c(1,1,1,1))
continuous_distribution=forecast[c(4,7,8,10,11,12,14,15,16,17,18,19)]
x=names(continuous_distribution)
j=0
for(i in continuous_distribution){
   j = j + 1
   hist(i,xlab=x[j],main=paste("hist",x[j]))
}

library(ggplot2)
#visualizing categorical variables
barplot(table(forecast$is_day),xlab="is_day",ylab="counts",main="plotting day and night counts",col=c("red","green"))

#visualising weather conditions
ggplot(forecast,aes(y=condition,fill=condition,main="weather_conditions"))+geom_bar()

#visualising wind direction
ggplot(forecast,aes(x=wind_dir,fill=wind_dir,main="wind_Directions"))+geom_bar()

#visualising state distribution
ggplot(forecast,aes(y=state,fill=state,main="states"))+geom_bar()

#state wise temperature
ggplot(forecast,aes(x=temp_c,y=state,fill=state))+geom_col(position=position_dodge())

#scatterplot 
plot(forecast$temp_c,forecast$humidity,main = "scatter plot of temperature and humidity")

#regression model
forecast=read.csv("FinalForecastData.csv")
#removing redundancy
forecast=forecast[-c(5,9,13,15,19,21,23,25,26,28,27,29,31,33)]
View(forecast)
names(forecast)=make.names(names(forecast))
forecast=na.omit(forecast)
dim(forecast)
df=forecast[,c(4,14,15,16,17)]
View(df)
str(df)
split=sample(0.8*nrow(df))
training=df[split,]
head(training)
testing=df[-split,]
head(testing)
reg_model=lm(temp_c~.,data=training)
summary(reg_model)
prediction=predict(reg_model,testing)
accuracy=mean(prediction==testing$temp_c)
accuracy
data.frame(prediction,testing$temp_c)
plot(testing$temp_c,prediction,xlab="actual temperature",ylab="predicted temperature")
#predicting with new data
x=data.frame(feelslike_c=24, windchill_c=25 ,heatindex_c=26 ,dewpoint_c=28)
predict(reg_model,x)

#kmeans clustering
library(corrplot)
corrplot(cor(forecast[,unlist(lapply(forecast,is.numeric))]))
forecast_new=forecast[-c(1,2,3,6,9,20,21)]
View(forecast_new)
maximum=10
scal=scale(forecast_new)
wss=sapply(1:maximum,function(k){kmeans(scal,k,nstart=50,iter.max = 15)$tot.withinss})
plot(1:maximum,wss,type='b',xlab='k values')
abline(v=6)
km=kmeans(forecast_new,6,iter.max = 50)
km
km$withinss
km$tot.withinss
km$iter
km$centers
km$cluster=as.factor(km$cluster)
km$cluster
library(ggfortify)
autoplot(km,forecast_new,frame=TRUE)




