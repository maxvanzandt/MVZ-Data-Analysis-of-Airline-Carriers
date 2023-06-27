##Data Cleaning: 

library(dplyr)
library(ggplot2)

airline.delays<- read.csv("/Users/anneyoung/Desktop/airline_delay.csv")

our.airline.data<- airline.delays %>% subset(select = -c(carrier_ct, weather_ct,nas_ct, security_ct, late_aircraft_ct, arr_del15))

clean.airline.data<- our.airline.data %>% rename(Year = year, Airline = carrier,Airline.Name = carrier_name, Arrival.Delay = arr_delay, Month = month, Airport = airport, Airport.Name = airport_name, Arrival.Flights = arr_flights, Arrivals.Cancelled = arr_cancelled, Arrivals.Diverted = arr_diverted,Carrier.Delays = carrier_delay, Weather.Delay = weather_delay, NAS.Delay = nas_delay, Late.Aircraft.Delay = late_aircraft_delay, Security.Delay = security_delay)

clean.airline.data <- clean.airline.data %>% mutate(Month.Name = as.character(Month))

airline<-clean.airline.data %>% mutate(Month.Name = recode(Month.Name,"5" =  "May", "1" = "January", "2" = "February", "3" = "March", "4" = "April","6" = "June", "7" = "July", "8" = "August", "9" = "September",  "10" = "October", "11" = "November", "12" = "December"))


## Categorical Varibale Summary Measures: 
  
Number of Arrivals Diverted by Airline Name 

tapply(airline$Arrivals.Diverted,airline$Airline.Name,sum,na.rm=TRUE)


Maximum Number of Arrivals Cancelled by Airline  

tapply(airline$Arrivals.Cancelled,airline$Airline,max,na.rm=TRUE)


How many airports do the ten major airlines fly to?  

airline17<-airline%>% filter(Month.Name=="December" & Year=="2021")
table(airline17$Airline.Name,exclude=c("AirTran Airways Corporation", "Aloha Airlines Inc.", "America West Airlines Inc.","American Eagle Airlines Inc.", "Comair Inc.","ATA Airlines d/b/a ATA","Atlantic Coast Airlines","Atlantic Southeast Airlines","Continental Air Lines Inc.","ExpressJet Airlines Inc.","ExpressJet Airlines LLC","Horizon Air","Independence Air","Mesa Airlines Inc.","Northwest Airlines Inc.","Pinnacle Airlines Inc.","PSA Airlines Inc.","Republic Airline","SkyWest Airlines Inc.", "Endeavor Air Inc.","Envoy Air","US Airways Inc.","Virgin America"))



How many airlines fly into each airport in August 2020?
  
airline18<-airline%>% filter(Month.Name=="August" & Year=="2020")
table(airline17$Airport)


## Numerical Variable Summary Measures 
Numerical Summary Measures: 
  
What is the average weather delay by month?

airline %>% group_by(Month.Name) %>% summarise(Average.Weather.Delay= mean(na.omit(Weather.Delay)))


What year on average had the most canceled flights? 

airline %>% group_by(Year) %>% summarise(Average.Flights.Cancelled= mean(na.omit(Arrivals.Cancelled)))


Arrival Delay 

summary(na.omit(airline$Arrival.Delay))


Carrier Delay 

summary(na.omit(airline$Carrier.Delays))


Security Delay 

summary(na.omit(airline$Security.Delay))


Weather Delay 

summary(na.omit(airline$Weather.Delay))


NAS Delay 

summary(na.omit(airline$NAS.Delay))


Late Aircraft Delay 

summary(na.omit(airline$Late.Aircraft.Delay))

## Bar Graph 
library(RColorBrewer)
one_airline<-subset(airline, Airport=="ATL")
bargraph_airline<-one_airline%>%
filter(Year==2017)%>%  
group_by(Airline.Name)%>% 
summarise(Arrival.Delay=mean(Arrival.Delay, na.rm=T))
ggplot(bargraph_airline,aes(x=Airline.Name, y=Arrival.Delay))+geom_bar(stat="identity",fill=brewer.pal(10,"Spectral"))+theme(axis.text.x=element_text(angle = 90))+labs(x="Airline Name", y="Arrival Delay in Minutes", title="Average Delay by Airline Name in Atlanta Airport in 2017")+theme(plot.title = element_text(hjust = 0.5))


## Histogram 
histogram_airline<-subset(airline, Airport=="CHS")%>%
filter(Year==2003)
ggplot(histogram_airline,aes(x=Carrier.Delays))+geom_histogram(bins=6,fill="tomato",color="black")+labs(x="Minutes Added due to Carrier Delay", y="Frequency", title="Carrier Delay added Monthy to Airlines at the Charleston Airport in 2003")+theme(plot.title = element_text(hjust = 0.5))
sqrt(39)


## ScatterPlot 
avl_airport<-subset(airline, Airport=="AVL")%>%filter(Year==2021)
ggplot(avl_airport,aes(Weather.Delay,y=Arrival.Delay,color=Airline.Name))+geom_point(shape=18,alpha=0.8)+labs(x="Weather Dealy in Minutes", y= "Arrival Dealy in Minutes", title="Correlation of Weather Delay and Arrival Dealy in Asheville Regional Airport 2021")+theme(legend.position="bottom")+guides(col=guide_legend("Airline Name"))+scale_color_brewer(palette="Set1")


## Side by Side Box Plot 
library(RColorBrewer)
box_airline<-subset(airline, Airport=="AVL" | Airport=="USA" | Airport=="FAY" | Airport=="GSO"| Airport=="ILM" | Airport=="OAJ" | Airport=="EWN")%>%filter(Year==2018)
ggplot(box_airline, aes(x=Airport,y=Arrival.Delay))+geom_boxplot(fill=brewer.pal(7,"Spectral"))+labs(x="Medium to Small Airports in NC", y="Total Arrival Delay in Minutes", title = "Arrival Delay of Airports in NC in 2018")+ theme(plot.title=element_text(hjust=0.5))


## Heat Map 
library(ggplot2)
library(dplyr)
library(caret)
airline<-read.csv("/Users/anneyoung/Desktop/clean_airline_delay.csv")
airline2<-subset(airline, Airport==c("LGA","ORD","ATL"))
ggplot(airline2,aes(x=as.factor(Month),y=Airline.Name,fill=Weather.Delay))+geom_tile()+scale_fill_distiller(palette="Spectral")+labs(x="Month", y="Airline Name",title="Weather Dealay in Major Airports by Month from 2003 to 2022")+theme(plot.title=element_text(hjust=0.5))+facet_wrap(~Airport)

## Animated Bubble Plot 
library(gganimate)
library(av)
library(gifski)
library(dplyr)

ADC1<-Airline_Delay_Cause%>%
filter(year>="2012" & year<="2022")%>%
filter(carrier=="AA"| carrier=="UA"| carrier=="DL")
ADCPlot<-ggplot(ADC1,aes(x=weather_ct,y=arr_del15,fill=carrier,size=late_aircraft_ct))+geom_point(alpha=0.4,shape=21)+scale_size(range=c(0.1,13))+labs(x="Weather Delays",y="Total Delay",title="Year:{frame_time}")+transition_time(year)

animate(ADCPlot,nframes=10,fps=2)



## Multiple Linear Regression 
library(car)
library(dplyr)
library(ggcorrplot)
library(caret)

new_airline<-airline%>%
select(Year,Month,Arrivals.Cancelled,Carrier.Delays,Weather.Delay,Security.Delay,NAS.Delay)%>%
filter(Month==6 & Year==2007)

airline_cor_mat<-round(cor(new_airline,use="complete.obs"), 2)
ggcorrplot(airline_cor_mat,type="lower",lab=TRUE)

Model<-train(Arrivals.Cancelled~Weather.Delay+Carrier.Delays+Security.Delay+NAS.Delay, new_airline,method="lm",na.action = na.omit)
summary(Model)
vif(Model$finalModel)


ModelAIC<-train(Arrivals.Cancelled~Weather.Delay+Carrier.Delays+Security.Delay+NAS.Delay, new_airline,method="lmStepAIC",trace=FALSE,na.action = na.omit)
summary(ModelAIC)


MLR_Predictor<-data.frame(Weather.Delay=50, Carrier.Delays=1090,Security.Delay=10,NAS.Delay=6)
predict(ModelAIC,MLR_Predictor)

## Logistic Regression 
airline8<-airline%>%
filter(Airline.Name=="Delta Air Lines Inc." | Airline.Name =="United Air Lines Inc.")%>%
filter(Airport == "LAX")%>%
select(Airline.Name,Arrival.Delay,Carrier.Delays,Weather.Delay,NAS.Delay,Security.Delay,Late.Aircraft.Delay)

VAR<-select(airline8,-Airline.Name)

cor_mat_LR<-round(cor(VAR),2)
ggcorrplot(cor_mat_LR,type="lower",lab=TRUE)

logit<-train(Airline.Name~.,airline8,method="glm",family="binomial")
summary(logit)


logit_AIC<-train(Airline.Name~.,airline8,method="glmStepAIC",family="binomial",direction="both",trace=FALSE)
summary(logit_AIC)
exp(coef(logit_AIC$finalModel))


newdat<-data.frame(Arrival.Delay=402,Carrier.Delays=7, Weather.Delay=100, NAS.Delay=60, Security.Delay=35,Late.Aircraft.Delay=200)
predict(logit_AIC,newdat)


## KNN Model 
library(dplyr)

airline_test<-na.omit(airline)
airline_test2<-airline_test%>%
select(Airport, Year, Month, Month.Name,Arrival.Flights, Arrivals.Cancelled, Arrivals.Diverted, Arrival.Delay)%>% 
filter(Airport=="BOS" & Year==2010)


knn_model<-train(Month.Name~Arrival.Delay+Arrivals.Cancelled+Arrival.Flights+Arrivals.Diverted, airline_test2,method="knn") 
ggplot(knn_model)                  
Predictor_Flight<-data.frame(Arrival.Delay=19,Arrivals.Cancelled=8, Arrival.Flights=15, Arrivals.Diverted=10)         

predict(knn_model,Predictor_Flight)
















