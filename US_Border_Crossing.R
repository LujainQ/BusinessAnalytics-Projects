##Final Project
##Lujain Alqassar & Keith Tyser
##ISYE 480 - Data Science

rm(list=ls())

library(dplyr)
library(base)
library(lubridate)
library(ggplot2)
library(stringr)
library(tidyr)

borders = read.csv("EntryDataF19.csv")
ydm(borders$Date)
borders$Date = ymd(borders$Date)
borders$Month = month(borders$Date)
borders$Year = year(borders$Date)

borders = subset(borders, Year<=2017) #removing 2018, 2019

borders$MonthYear = paste(borders$Month, "_", borders$Year, sep = "")
  
USRate = read.csv("USRate.csv")

USRate$MonthYear = paste(USRate$Month, "_" , USRate$Year, sep="")

borders = left_join(borders, USRate, by="MonthYear")

USCanImpExp = read.csv("USCanImpExp.csv")
USCanImpExp$MonthYear = paste(USCanImpExp$Month,"_",USCanImpExp$Year,sep="")

borders = left_join(borders, USCanImpExp, by="MonthYear")

CanMexRate = read.csv("CanMexRate.csv")
CanMexRate$MonthYear = paste(CanMexRate$Month,"_",CanMexRate$Year,sep="")

borders = left_join(borders, CanMexRate, by="MonthYear")

USInflation = read.csv("USInflation.csv")
USInflation$MonthYear = paste(USInflation$Month,"_",USInflation$Year,sep="")

borders = left_join(borders, USInflation, by="MonthYear")

borders$X = NULL
borders$Month.x=NULL
borders$Year.x=NULL
borders$Year.y=NULL
borders$Month.y=NULL
borders$X.1=NULL
borders$X.2=NULL
borders$X.3=NULL
borders$X.4=NULL
borders$X.5=NULL
borders$X.6=NULL
borders$X.7=NULL
borders$X.8=NULL
borders$X.9=NULL
borders$X.10=NULL
borders$X.11=NULL
borders$X.12=NULL
borders$X.13=NULL
borders$X.14=NULL
borders$X.15=NULL
borders$X.16=NULL
borders$X.17=NULL
borders$Year.x.x=NULL
borders$Month.x.x=NULL
borders$Year.y.y=NULL
borders$Month.y.y=NULL
borders$MonthYear=NULL
borders$X.x=NULL
borders$X.y=NULL
borders$Location=NULL

##Task 1 - Data Visualization:
#Making Graphs with respect to year

count_Year =  borders %>% 
  group_by(Year) %>%
  summarize(Sum_Value = sum(as.numeric(Value))) 

count_Year = subset(count_Year, Year<= 2017) #removing 2018 because it skews graphs
  
#Graph 1
ggplot(data=count_Year, aes(x=Year, y=Sum_Value, group=1)) +
  geom_line()+ggtitle("Number of Entries through the Years") +
  xlab("Years") + ylab("Count")+theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Graph 2 for windows PC
################################################################################################################
#borders$Date = as.character.Date(borders$Date)
#borders$MonthGraph = month(as.POSIXlt(borders$Date, format="%Y-%m-%d"))
#mymonths = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

#borders$MonthGraph = mymonths[borders$Month ]

#count_Month =  borders %>% 
  #group_by(MonthGraph) %>%
  #summarize(Sum_Value = sum(as.numeric(Value)))
# borders$Month=as.factor(borders$Month)

count_Month =  borders %>% 
  group_by(Month) %>%
  summarize(Sum_Value = sum(as.numeric(Value)))

#ggplot(data=count_Month, aes(x=Month, y=Sum_Value, group=1)) +
#  geom_bar(stat = 'identity',fill=count_Month$Sum_Value)+ggtitle("Number of Entries by Month") +
#  xlab("Month") + ylab("Count")+theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = count_Month, aes(x=Month, y=Sum_Value, group=1)) + geom_bar(stat="Identity",aes(group=Month,fill=Month))+
  xlab("Month") + ylab("Count")
################################################################################################################

#Graph 2 for macbook
################################################################################################################
#borders$Date = as.character.Date(borders$Date)
#borders$Month = month(as.POSIXlt(borders$Date, format="%Y-%m-%d"))
#mymonths = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

#borders$Month = mymonths[borders$Month ]

#count_Month =  borders %>% 
#  group_by(Month) %>%
#  summarize(Sum_Value = sum(as.numeric(Value)))


#ggplot(data=count_Month, aes(x=Month, y=Sum_Value, group=1)) +
#  geom_bar(stat = 'identity',fill=count_Month$Sum_Value)+ggtitle("Number of entries by month") +
#  xlab("Month") + ylab("Count")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
################################################################################################################

#Graph 3
library(forcats)
borders %>% 
  group_by(State) %>%
  summarize(Sum_Value = sum(as.numeric(Value))) %>%
  ggplot(aes(x=fct_reorder(State, Sum_Value), y=Sum_Value,fill=Sum_Value,label = Sum_Value))+
  geom_col()+
  ylim(0,3500064635)+
  coord_flip()+
  geom_text(hjust = -0.1, size = 3)+
  labs(
    title='Border Crossing Activity Count by State')+
  xlab('State')+
  ylab('Count')

#Graph 4
borders %>%
  group_by(Measure,Year) %>%
  summarize(Sum_Value = sum(Value)) %>% 
  mutate(Year = as.numeric(Year)) %>%
  ggplot() +
  geom_line(aes(x= Year, y = Sum_Value, colour = Measure))+
  scale_x_discrete(limits = c(1996:2017))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(
    title = 'Total Border Crossing Activity Count 1996 - 2017',
    subtitle = 'by Measure')+
  ylab('Count')+
  geom_vline(xintercept = 2017, linetype = 'dashed', color = 'steelblue', size = 1)

#Graph 5 - U.S. Map
library(leaflet)

leaflet(data=borders[1:250,]) %>%  addTiles() %>%  addMarkers(~Lon, ~Lat)



##Task 2 - Predictive Model:
#Separate into Different Borders and Modality: 
borders$USInflation = as.numeric(gsub("[\\%,]", "", borders$USInflation))/100
borders$Port.Code = NULL


MexPed = borders[(borders$Border=="US-Mexico Border" & borders$Measure=="Pedestrians"),]
MexPV = borders[(borders$Border=="US-Mexico Border" & borders$Measure=="Personal Vehicles"),]
MexPVP = borders[(borders$Border=="US-Mexico Border" & borders$Measure=="Personal Vehicle Passengers"),]
MexOther = borders[(borders$Border=="US-Mexico Border" & borders$Measure %in% c("Bus Passengers","Buses","Rail Containers Empty","Rail Containers Full","Train Passengers","Trains","Truck Containers Empty","Truck Containers Full","Trucks")),]
MexOther$Measure = factor(MexOther$Measure)

CanPed = borders[(borders$Border=="US-Canada Border" & borders$Measure=="Pedestrians"),]
CanPV = borders[(borders$Border=="US-Canada Border" & borders$Measure=="Personal Vehicles"),]
CanPVP = borders[(borders$Border=="US-Canada Border" & borders$Measure=="Personal Vehicle Passengers"),]
CanOther = borders[(borders$Border=="US-Canada Border" & borders$Measure %in% c("Bus Passengers","Buses","Rail Containers Empty","Rail Containers Full","Train Passengers","Trains","Truck Containers Empty","Truck Containers Full","Trucks")),]
CanOther$Measure = factor(CanOther$Measure)

library(caTools)

#MexPed:
set.seed(100)
MexPed$Border=NULL
MexPed$Measure = NULL
split=sample.split(MexPed$Value, SplitRatio = 0.7)
TrainMexPed = subset(MexPed, split==TRUE)
TestMexPed = subset(MexPed, split==FALSE)
str(MexPed)

MexPedlm = lm(Value~.-USInflation-Lat-Month,data=TrainMexPed)
summary(MexPedlm)

MexPedpred = predict(MexPedlm, newdata=TrainMexPed)
RMSEMexPed = sqrt(sum((MexPedpred-TrainMexPed$Value)^2)/nrow(TrainMexPed))
RMSEMexPed

MexPedpred = predict(MexPedlm, newdata=TestMexPed)
RMSEMexPed = sqrt(sum((MexPedpred-TestMexPed$Value)^2)/nrow(TestMexPed))
RMSEMexPed

#MexPV:
set.seed(100)
MexPV$Border=NULL
MexPV$Measure = NULL
split=sample.split(MexPV$Value, SplitRatio = 0.7)
TrainMexPV = subset(MexPV, split==TRUE)
TestMexPV = subset(MexPV, split==FALSE)

MexPVlm = lm(Value~.-Month-USInflation-Lat,data=TrainMexPV)
summary(MexPVlm)

MexPVpred = predict(MexPVlm, newdata=TrainMexPV)
RMSEMexPV = sqrt(sum((MexPVpred-TrainMexPV$Value)^2)/nrow(TrainMexPV))
RMSEMexPV

MexPVpred = predict(MexPVlm, newdata=TestMexPV)
RMSEMexPV = sqrt(sum((MexPVpred-TestMexPV$Value)^2)/nrow(TestMexPV))
RMSEMexPV

#MexPVP:
set.seed(100)
MexPVP$Border=NULL
MexPVP$Measure = NULL
split=sample.split(MexPVP$Value, SplitRatio = 0.7)
TrainMexPVP = subset(MexPVP, split==TRUE)
TestMexPVP = subset(MexPVP, split==FALSE)

MexPVPlm = lm(Value~.-Month-USRate-USInflation-Lat-CanRate,data=TrainMexPVP)
summary(MexPVPlm)

MexPVPpred = predict(MexPVPlm, newdata=TrainMexPVP)
RMSEMexPVP = sqrt(sum((MexPVPpred-TrainMexPVP$Value)^2)/nrow(TrainMexPVP))
RMSEMexPVP

MexPVPpred = predict(MexPVPlm, newdata=TestMexPVP)
RMSEMexPVP = sqrt(sum((MexPVPpred-TestMexPVP$Value)^2)/nrow(TestMexPVP))
RMSEMexPVP

#MexOther:
set.seed(100)
MexOther$Border=NULL
MexOther$Measure = NULL
MexOther$Port.Name=factor(MexOther$Port.Name)
split=sample.split(MexOther$Value, SplitRatio = 0.7)
TrainMexOther = subset(MexOther, split==TRUE)
TestMexOther = subset(MexOther, split==FALSE)

MexOtherlm = lm(Value~.-Month-USRate-CanRate-Lon-USInflation-USCanImpExp-Lat,data=TrainMexOther)
summary(MexOtherlm)

MexOtherpred = predict(MexOtherlm, newdata=TrainMexOther)
RMSEMexOther = sqrt(sum((MexOtherpred-TrainMexOther$Value)^2)/nrow(TrainMexOther))
RMSEMexOther

MexOtherpred = predict(MexOtherlm, newdata=TestMexOther)
RMSEMexOther = sqrt(sum((MexOtherpred-TestMexOther$Value)^2)/nrow(TestMexOther))
RMSEMexOther

#CanPed:
set.seed(100)
CanPed$Border=NULL
CanPed$Measure = NULL
CanPed$Location = NULL
split = sample.split(CanPed$Value, SplitRatio = 0.7)
TrainCanPed = subset(CanPed, split==TRUE)
TestCanPed = subset(CanPed, split==FALSE)

CanPedlm = lm(Value~.-Lon-USInflation-Lat-Port.Name-Year-CanRate-USRate,data=TrainCanPed)
summary(CanPedlm)

CanPedpred = predict(CanPedlm, newdata=TrainCanPed)
RMSECanPed = sqrt(sum((CanPedpred-TrainCanPed$Value)^2)/nrow(TrainCanPed))
RMSECanPed

CanPedpred = predict(CanPedlm, newdata=TestCanPed)
RMSECanPed = sqrt(sum((CanPedpred-TestCanPed$Value)^2)/nrow(TestCanPed))
RMSECanPed

#CanPV:
set.seed(100)
CanPV$Border=NULL
CanPV$Measure = NULL
split=sample.split(CanPV$Value, SplitRatio = 0.7)
TrainCanPV = subset(CanPV, split==TRUE)
TestCanPV = subset(CanPV, split==FALSE)

CanPVlm = lm(Value~.-USInflation-Lon,data=TrainCanPV)
summary(CanPVlm)

CanPVpred = predict(CanPVlm, newdata=TrainCanPV)
RMSECanPV = sqrt(sum((CanPVpred-TrainCanPV$Value)^2)/nrow(TrainCanPV))
RMSECanPV

CanPVpred = predict(CanPVlm, newdata=TestCanPV)
RMSECanPV = sqrt(sum((CanPVpred-TestCanPV$Value)^2)/nrow(TestCanPV))
RMSECanPV

#CanPVP:
set.seed(100)
CanPVP$Border=NULL
CanPVP$Measure = NULL
split=sample.split(CanPVP$Value, SplitRatio = 0.7)
TrainCanPVP = subset(CanPVP, split==TRUE)
TestCanPVP = subset(CanPVP, split==FALSE)

CanPVPlm = lm(Value~.-Lon-USInflation,data=TrainCanPVP)
summary(CanPVPlm)

CanPVPpred = predict(CanPVPlm, newdata=TrainCanPVP)
RMSECanPVP = sqrt(sum((CanPVPpred-TrainCanPVP$Value)^2)/nrow(TrainCanPVP))
RMSECanPVP

CanPVPpred = predict(CanPVPlm, newdata=TestCanPVP)
RMSECanPVP = sqrt(sum((CanPVPpred-TestCanPVP$Value)^2)/nrow(TestCanPVP))
RMSECanPVP

#CanOther:
set.seed(100)
CanOther$Border=NULL
CanOther$Measure = NULL
split=sample.split(CanOther$Value, SplitRatio = 0.7)
TrainCanOther = subset(CanOther, split==TRUE)
TestCanOther = subset(CanOther, split==FALSE)

CanOtherlm = lm(Value~.-Lon-Port.Name-Year-USInflation-Month,data=TrainCanOther)
summary(CanOtherlm)

CanOtherpred = predict(CanOtherlm, newdata=TrainCanOther)
RMSECanOther = sqrt(sum((CanOtherpred-TrainCanOther$Value)^2)/nrow(TrainCanOther))
RMSECanOther

CanOtherpred = predict(CanOtherlm, newdata=TestCanOther)
RMSECanOther = sqrt(sum((CanOtherpred-TestCanOther$Value)^2)/nrow(TestCanOther))
RMSECanOther



##Applying to Evaluation Set:
Eval = read.csv("EvalDataF19.csv")

Eval$X=NULL
Eval$X.1=NULL

ydm(Eval$Date)
Eval$Date = ymd(Eval$Date)
Eval$Month = month(Eval$Date)
Eval$Year = year(Eval$Date)

Eval$MonthYear = paste(Eval$Month, "_", Eval$Year, sep = "")

USRate2019 = read.csv("USRate2019.csv")

USRate2019$MonthYear = paste(USRate2019$Month, "_" , USRate2019$Year, sep="")

Eval = left_join(Eval, USRate2019, by="MonthYear")

CanMexRate2019 = read.csv("CanMexRate2019.csv")

CanMexRate2019$MonthYear = paste(CanMexRate2019$Month, "_", CanMexRate2019$Year, sep="")

Eval = left_join(Eval, CanMexRate2019, by="MonthYear")

Eval$Month.x=NULL
Eval$Month.y=NULL
Eval$Year.x=NULL
Eval$Year.y=NULL

USInflation2019 = read.csv("USInflation2019.csv")

USInflation2019$MonthYear = paste(USInflation2019$Month, "_" , USInflation2019$Year, sep="")

Eval = left_join(Eval, USInflation2019, by="MonthYear")

Eval$Month.x=NULL
Eval$Month.y=NULL
Eval$Year.x=NULL
Eval$Year.y=NULL

USCanImpExp2019 = read.csv("USCanImpExp2019.csv")

USCanImpExp2019$MonthYear = paste(USCanImpExp2019$Month, "_" , USCanImpExp2019$Year, sep="")

Eval = left_join(Eval, USCanImpExp2019, by="MonthYear")


Eval$MonthYear=NULL
Eval$X=NULL
Eval$X.1=NULL
Eval$X.2=NULL
Eval$X.3=NULL
Eval$X.4=NULL
Eval$X.5=NULL
Eval$X.6=NULL
Eval$X.7=NULL
Eval$X.8=NULL
Eval$X.9=NULL
Eval$X.10=NULL
Eval$X.11=NULL

pred = 0 
for(i in 1:nrow(Eval)){ 
  
  if(Eval$Border[i]=="US-Mexico Border" & Eval$Measure[i]=="Pedestrians"){
    pred[i] = predict(MexPedlm, newdata=Eval[i,]) }
  
  else if(Eval$Border[i]=="US-Mexico Border" & Eval$Measure[i]=="Personal Vehicles"){
    pred[i] = predict(MexPVlm, newdata=Eval[i,]) }
  
  else if(Eval$Border[i]=="US-Mexico Border" & Eval$Measure[i]=="Personal Vehicle Passengers"){
    pred[i] = predict(MexPVPlm, newdata=Eval[i,]) }
  
  else if(Eval$Border[i]=="US-Mexico Border" & Eval$Measure[i] %in% c("Bus Passengers","Buses","Rail Containers Empty","Rail Containers Full","Train Passengers","Trains","Truck Containers Empty","Truck Containers Full","Trucks")){
    pred[i] = predict(MexOtherlm, newdata=Eval[i,]) }
  
  else if(Eval$Border[i]=="US-Canada Border" & Eval$Measure[i]=="Pedestrian"){
    pred[i] = predict(CanPedlm, newdata=Eval[i,]) }
  
  else if(Eval$Border[i]=="US-Canada Border" & Eval$Measure[i]=="Personal Vehicles"){
    pred[i] = predict(CanPVlm, newdata=Eval[i,]) }
  
  else if(Eval$Border[i]=="US-Canada Border" & Eval$Measure[i]=="Personal Vehicle Passengers"){
    pred[i] = predict(CanPVPlm, newdata=Eval[i,]) }
  else { 
    pred[i] = predict(CanOtherlm, newdata=Eval[i,])
  }
}

pred

Eval$Prediction = pred

write.csv(Eval,"EvalWithPrediction.csv",row.names = FALSE)






