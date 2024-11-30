setwd('D:/Studia/2 semestr/PDU/R/Projekt02')
flights <- read.csv('flights.csv')


library(ggplot2)
library(dplyr)
#Pogoda ewr
pogoda_ewr<-read.csv('pogoda_ewr.csv')
data_time_ewr<-pogoda_ewr[3:nrow(pogoda_ewr),'latitude']
pogoda_ewr <- data.frame(data = substr(data_time_ewr, 1, 10), time=substr(data_time_ewr, 12, 13), precipation=pogoda_ewr[3:nrow(pogoda_ewr), 'longitude'])
pogoda_ewr['date_hour'] <- paste(pogoda_ewr$data, pogoda_ewr$time, sep='-')
pogoda_ewr$precipation <- as.numeric(pogoda_ewr$precipation)
#Pogoda jfk
pogoda_jfk<-read.csv('pogoda_jfk.csv')
data_time_jfk<-pogoda_jfk[3:nrow(pogoda_jfk),'latitude']
pogoda_jfk <- data.frame(data = substr(data_time_jfk, 1, 10), time=substr(data_time_jfk, 12, 13), precipation=pogoda_jfk[3:nrow(pogoda_jfk), 'longitude'])
pogoda_jfk['date_hour'] <- paste(pogoda_jfk$data, pogoda_jfk$time, sep='-')
pogoda_jfk$precipation <- as.numeric(pogoda_jfk$precipation)
#Pogoda lga
pogoda_lga<-read.csv('pogoda_lga.csv')
data_time_lga<-pogoda_lga[3:nrow(pogoda_lga),'latitude']
pogoda_lga <- data.frame(data = substr(data_time_lga, 1, 10), time=substr(data_time_lga, 12, 13), precipation=pogoda_lga[3:nrow(pogoda_lga), 'longitude'])
pogoda_lga['date_hour'] <- paste(pogoda_lga$data, pogoda_lga$time, sep='-')
pogoda_lga$precipation <- as.numeric(pogoda_lga$precipation)
#Pogoda chicago
pogoda_chicago <- read.csv('chicago.csv')
data_time_ord <- pogoda_chicago[3:nrow(pogoda_chicago) , 'latitude']
pogoda_chicago <- data.frame(data = substr(data_time_ord, 1, 10), time=substr(data_time_ord, 12, 13), precipation=pogoda_chicago[3:nrow(pogoda_chicago), 'longitude'])
pogoda_chicago['date_hour'] <- paste(pogoda_chicago$data, pogoda_chicago$time, sep='-')
pogoda_chicago$precipation <- as.numeric(pogoda_chicago$precipation)
#Pogoda atlanta
pogoda_atlanta <- read.csv('atlanta.csv')
data_time_atl <- pogoda_atlanta[3:nrow(pogoda_atlanta) , 'latitude']
pogoda_atlanta <- data.frame(data = substr(data_time_atl, 1, 10), time=substr(data_time_atl, 12, 13), precipation=pogoda_atlanta[3:nrow(pogoda_atlanta), 'longitude'])
pogoda_atlanta['date_hour'] <- paste(pogoda_atlanta$data, pogoda_atlanta$time, sep='-')
pogoda_atlanta$precipation <- as.numeric(pogoda_atlanta$precipation)
#Pogoda los angeles
pogoda_lax <- read.csv('pogoda_lax.csv')
data_time_lax <- pogoda_lax[3:nrow(pogoda_lax) , 'latitude']
pogoda_lax<- data.frame(data = substr(data_time_lax, 1, 10), time=substr(data_time_lax, 12, 13), precipation=pogoda_lax[3:nrow(pogoda_lax), 'longitude'])
pogoda_lax['date_hour'] <- paste(pogoda_lax$data, pogoda_lax$time, sep='-')
pogoda_lax$precipation <- as.numeric(pogoda_lax$precipation)
#Pogoda bos
pogoda_bos<-read.csv('pogoda_bos.csv')
data_time_bos<-pogoda_bos[3:nrow(pogoda_bos),'latitude']
pogoda_bos <- data.frame(data = substr(data_time_bos, 1, 10), time=substr(data_time_bos, 12, 13), precipation=pogoda_bos[3:nrow(pogoda_bos), 'longitude'])
pogoda_bos['date_hour'] <- paste(pogoda_bos$data, pogoda_bos$time, sep='-')
pogoda_bos$precipation <- as.numeric(pogoda_bos$precipation)
#Pogoda mco
pogoda_mco<-read.csv('pogoda_mco.csv')
data_time_mco<-pogoda_mco[3:nrow(pogoda_mco),'latitude']
pogoda_mco <- data.frame(data = substr(data_time_mco, 1, 10), time=substr(data_time_mco, 12, 13), precipation=pogoda_mco[3:nrow(pogoda_mco), 'longitude'])
pogoda_mco['date_hour'] <- paste(pogoda_mco$data, pogoda_mco$time, sep='-')
pogoda_mco$precipation <- as.numeric(pogoda_mco$precipation)
#Pogoda clt
pogoda_clt<-read.csv('pogoda_clt.csv')
data_time_clt<-pogoda_clt[3:nrow(pogoda_clt),'latitude']
pogoda_clt <- data.frame(data = substr(data_time_clt, 1, 10), time=substr(data_time_clt, 12, 13), precipation=pogoda_clt[3:nrow(pogoda_clt), 'longitude'])
pogoda_clt['date_hour'] <- paste(pogoda_clt$data, pogoda_clt$time, sep='-')
pogoda_clt$precipation <- as.numeric(pogoda_clt$precipation)
#Pogoda fll
pogoda_fll<-read.csv('pogoda_fll.csv')
data_time_fll<-pogoda_fll[3:nrow(pogoda_fll),'latitude']
pogoda_fll <- data.frame(data = substr(data_time_fll, 1, 10), time=substr(data_time_fll, 12, 13), precipation=pogoda_fll[3:nrow(pogoda_fll), 'longitude'])
pogoda_fll['date_hour'] <- paste(pogoda_fll$data, pogoda_fll$time, sep='-')
pogoda_fll$precipation <- as.numeric(pogoda_fll$precipation)
#Pogoda mia
pogoda_mia<-read.csv('pogoda_mia.csv')
data_time_mia<-pogoda_mia[3:nrow(pogoda_mia),'latitude']
pogoda_mia <- data.frame(data = substr(data_time_mia, 1, 10), time=substr(data_time_mia, 12, 13), precipation=pogoda_mia[3:nrow(pogoda_mia), 'longitude'])
pogoda_mia['date_hour'] <- paste(pogoda_mia$data, pogoda_mia$time, sep='-')
pogoda_mia$precipation <- as.numeric(pogoda_mia$precipation)


############ Samoloty do chicago
samoloty_do_chicago <- flights[flights$dest=='ORD', ]
samoloty_do_chicago['date'] <- sprintf("%04d-%02d-%02d", samoloty_do_chicago$year, samoloty_do_chicago$month, samoloty_do_chicago$day)
samoloty_do_chicago['godzina_przylotu_pelna'] <- sprintf("%02d:%02d", samoloty_do_chicago$arr_time%/%100, samoloty_do_chicago$arr_time %% 100)
samoloty_do_chicago['przylot'] <- substr(samoloty_do_chicago$godzina_przylotu, 1, 2)
samoloty_do_chicago['date_hour'] <- paste(samoloty_do_chicago$date, samoloty_do_chicago$przylot, sep='-')
samoloty_do_chicago<-merge(samoloty_do_chicago, pogoda_chicago, by.x='date_hour', by.y='date_hour')
samoloty_do_chicago<-na.omit(samoloty_do_chicago)
#Samoloty z ewr do chicago
ord_ewr<-samoloty_do_chicago[samoloty_do_chicago$origin=="EWR",]
ord_ewr<-ord_ewr[,c(1,8,11,28)]
ord_ewr<-ord_ewr%>%
        inner_join(.,pogoda_ewr,by=c("date_hour"="date_hour"))
ord_ewr['percipitation']<-(ord_ewr$precipation.x+ord_ewr$precipation.y)/2
ord_ewr<-ord_ewr[,c(2,3,8)]
ord_ewr['delay']<-ord_ewr$arr_delay-ord_ewr$dep_delay
#Samoloty z lga do chicago
ord_lga<-samoloty_do_chicago[samoloty_do_chicago$origin=="LGA",]
ord_lga<-ord_lga[,c(1,8,11,28)]
ord_lga<-ord_lga%>%
  inner_join(.,pogoda_lga,by=c("date_hour"="date_hour"))
ord_lga['percipitation']<-(ord_lga$precipation.x+ord_lga$precipation.y)/2
ord_lga<-ord_lga[,c(2,3,8)]
ord_lga['delay']<-ord_lga$arr_delay-ord_lga$dep_delay
#Samoloty z jfk do chicago 
ord_jfk<-samoloty_do_chicago[samoloty_do_chicago$origin=="JFK",]
ord_jfk<-ord_jfk[,c(1,8,11,28)]
ord_jfk<-ord_jfk%>%
  inner_join(.,pogoda_jfk,by=c("date_hour"="date_hour"))
ord_jfk['percipitation']<-(ord_jfk$precipation.x+ord_jfk$precipation.y)/2
ord_jfk<-ord_jfk[,c(2,3,8)]
ord_jfk['delay']<-ord_jfk$arr_delay-ord_jfk$dep_delay
### Wszystkie samoloty
samoloty_do_chicago<-rbind(ord_jfk,ord_lga,ord_ewr)
samoloty_do_chicago<-samoloty_do_chicago[ ,c(3,4)]
samoloty_do_chicago<-samoloty_do_chicago[samoloty_do_chicago$delay>=0,]

####### Samoloty do atlanty
samoloty_do_atlanta <- flights[flights$dest=='ATL', ]
samoloty_do_atlanta['date'] <- sprintf("%04d-%02d-%02d", samoloty_do_atlanta$year, samoloty_do_atlanta$month, samoloty_do_atlanta$day)
samoloty_do_atlanta['godzina_przylotu_pelna'] <- sprintf("%02d:%02d", samoloty_do_atlanta$arr_time%/%100, samoloty_do_atlanta$arr_time %% 100)
samoloty_do_atlanta['przylot'] <- substr(samoloty_do_atlanta$godzina_przylotu, 1, 2)
samoloty_do_atlanta['date_hour'] <- paste(samoloty_do_atlanta$date, samoloty_do_atlanta$przylot, sep='-')
samoloty_do_atlanta<-merge(samoloty_do_atlanta, pogoda_atlanta, by.x='date_hour', by.y='date_hour')
samoloty_do_atlanta<-na.omit(samoloty_do_atlanta)
#### samoloty z ewr do altanty
atl_ewr<-samoloty_do_atlanta[samoloty_do_atlanta$origin=="EWR",]
atl_ewr<-atl_ewr[,c(1,8,11,28)]
atl_ewr<-atl_ewr%>%
  inner_join(.,pogoda_ewr,by=c("date_hour"="date_hour"))
atl_ewr['percipitation']<-(atl_ewr$precipation.x+atl_ewr$precipation.y)/2
atl_ewr<-atl_ewr[,c(2,3,8)]
atl_ewr['delay']<-atl_ewr$arr_delay-atl_ewr$dep_delay
#Samoloty z lga do altanty
atl_lga<-samoloty_do_atlanta[samoloty_do_atlanta$origin=="LGA",]
atl_lga<-atl_lga[,c(1,8,11,28)]
atl_lga<-atl_lga%>%
  inner_join(.,pogoda_lga,by=c("date_hour"="date_hour"))
atl_lga['percipitation']<-(atl_lga$precipation.x+atl_lga$precipation.y)/2
atl_lga<-atl_lga[,c(2,3,8)]
atl_lga['delay']<-atl_lga$arr_delay-atl_lga$dep_delay
#Samoloty z jfk do atlanty
atl_jfk<-samoloty_do_atlanta[samoloty_do_atlanta$origin=="JFK",]
atl_jfk<-atl_jfk[,c(1,8,11,28)]
atl_jfk<-atl_jfk%>%
  inner_join(.,pogoda_jfk,by=c("date_hour"="date_hour"))
atl_jfk['percipitation']<-(atl_jfk$precipation.x+atl_jfk$precipation.y)/2
atl_jfk<-atl_jfk[,c(2,3,8)]
atl_jfk['delay']<-atl_jfk$arr_delay-atl_jfk$dep_delay
#Wszystkie samoloty do atlanty
samoloty_do_atlanta<-rbind(atl_jfk,atl_lga,atl_ewr)
samoloty_do_atlanta<-samoloty_do_atlanta[,c(3,4)]
samoloty_do_atlanta<-samoloty_do_atlanta[samoloty_do_atlanta$delay>=0,]

####### Samoloty do los angeles
samoloty_do_losangeles <- flights[flights$dest=='LAX', ]
samoloty_do_losangeles['date'] <- sprintf("%04d-%02d-%02d", samoloty_do_losangeles$year, samoloty_do_losangeles$month, samoloty_do_losangeles$day)
samoloty_do_losangeles['godzina_przylotu_pelna'] <- sprintf("%02d:%02d", samoloty_do_losangeles$arr_time%/%100, samoloty_do_losangeles$arr_time %% 100)
samoloty_do_losangeles['przylot'] <- substr(samoloty_do_losangeles$godzina_przylotu, 1, 2)
samoloty_do_losangeles['date_hour'] <- paste(samoloty_do_losangeles$date, samoloty_do_losangeles$przylot, sep='-')
samoloty_do_losangeles<-merge(samoloty_do_losangeles, pogoda_lax, by.x='date_hour', by.y='date_hour')
samoloty_do_losangeles<-na.omit(samoloty_do_losangeles)
#### samoloty z ewr do los angeles
lax_ewr<-samoloty_do_losangeles[samoloty_do_losangeles$origin=="EWR",]
lax_ewr<-lax_ewr[,c(1,8,11,28)]
lax_ewr<-lax_ewr%>%
  inner_join(.,pogoda_ewr,by=c("date_hour"="date_hour"))
lax_ewr['percipitation']<-(lax_ewr$precipation.x+lax_ewr$precipation.y)/2
lax_ewr<-lax_ewr[,c(2,3,8)]
lax_ewr['delay']<-lax_ewr$arr_delay-lax_ewr$dep_delay
#Samoloty z lga do los angeles
lax_lga<-samoloty_do_losangeles[samoloty_do_losangeles$origin=="LGA",]
lax_lga<-lax_lga[,c(1,8,11,28)]
lax_lga<-lax_lga%>%
  inner_join(.,pogoda_lga,by=c("date_hour"="date_hour"))
lax_lga['percipitation']<-(lax_lga$precipation.x+lax_lga$precipation.y)/2
lax_lga<-lax_lga[,c(2,3,8)]
lax_lga['delay']<-lax_lga$arr_delay-lax_lga$dep_delay
#Samoloty z jfk do los angeles
lax_jfk<-samoloty_do_losangeles[samoloty_do_losangeles$origin=="JFK",]
lax_jfk<-lax_jfk[,c(1,8,11,28)]
lax_jfk<-lax_jfk%>%
  inner_join(.,pogoda_jfk,by=c("date_hour"="date_hour"))
lax_jfk['percipitation']<-(lax_jfk$precipation.x+lax_jfk$precipation.y)/2
lax_jfk<-lax_jfk[,c(2,3,8)]
lax_jfk['delay']<-lax_jfk$arr_delay-lax_jfk$dep_delay
#Wszystkie samoloty do los angeles
samoloty_do_losangeles<-rbind(lax_jfk,lax_lga,lax_ewr)
samoloty_do_losangeles<-samoloty_do_losangeles[,c(3,4)]
samoloty_do_losangeles<-samoloty_do_losangeles[samoloty_do_losangeles$delay>=0,]

############ Samoloty do bostony
samoloty_do_bostonu <- flights[flights$dest=='BOS', ]
samoloty_do_bostonu['date'] <- sprintf("%04d-%02d-%02d", samoloty_do_bostonu$year, samoloty_do_bostonu$month, samoloty_do_bostonu$day)
samoloty_do_bostonu['godzina_przylotu_pelna'] <- sprintf("%02d:%02d", samoloty_do_bostonu$arr_time%/%100, samoloty_do_bostonu$arr_time %% 100)
samoloty_do_bostonu['przylot'] <- substr(samoloty_do_bostonu$godzina_przylotu, 1, 2)
samoloty_do_bostonu['date_hour'] <- paste(samoloty_do_bostonu$date, samoloty_do_bostonu$przylot, sep='-')
samoloty_do_bostonu<-merge(samoloty_do_bostonu, pogoda_bos, by.x='date_hour', by.y='date_hour')
samoloty_do_bostonu<-na.omit(samoloty_do_bostonu)
#Samoloty z ewr do bostonu
bos_ewr<-samoloty_do_bostonu[samoloty_do_bostonu$origin=="EWR",]
bos_ewr<-bos_ewr[,c(1,8,11,28)]
bos_ewr<-bos_ewr%>%
  inner_join(.,pogoda_ewr,by=c("date_hour"="date_hour"))
bos_ewr['percipitation']<-(bos_ewr$precipation.x+bos_ewr$precipation.y)/2
bos_ewr<-bos_ewr[,c(2,3,8)]
bos_ewr['delay']<-bos_ewr$arr_delay-bos_ewr$dep_delay
#Samoloty z lga do bostonu
bos_lga<-samoloty_do_bostonu[samoloty_do_bostonu$origin=="LGA",]
bos_lga<-bos_lga[,c(1,8,11,28)]
bos_lga<-bos_lga%>%
  inner_join(.,pogoda_lga,by=c("date_hour"="date_hour"))
bos_lga['percipitation']<-(bos_lga$precipation.x+bos_lga$precipation.y)/2
bos_lga<-bos_lga[,c(2,3,8)]
bos_lga['delay']<-bos_lga$arr_delay-bos_lga$dep_delay
#Samoloty z jfk do bostonu 
bos_jfk<-samoloty_do_bostonu[samoloty_do_bostonu$origin=="JFK",]
bos_jfk<-bos_jfk[,c(1,8,11,28)]
bos_jfk<-bos_jfk%>%
  inner_join(.,pogoda_jfk,by=c("date_hour"="date_hour"))
bos_jfk['percipitation']<-(bos_jfk$precipation.x+bos_jfk$precipation.y)/2
bos_jfk<-bos_jfk[,c(2,3,8)]
bos_jfk['delay']<-bos_jfk$arr_delay-bos_jfk$dep_delay
### Wszystkie samoloty
samoloty_do_bostonu<-rbind(bos_jfk,bos_lga,bos_ewr)
samoloty_do_bostonu<-samoloty_do_bostonu[ ,c(3,4)]
samoloty_do_bostonu<-samoloty_do_bostonu[samoloty_do_bostonu$delay>=0,]

############ Samoloty do orlando
samoloty_do_orlando <- flights[flights$dest=='MCO', ]
samoloty_do_orlando['date'] <- sprintf("%04d-%02d-%02d", samoloty_do_orlando$year, samoloty_do_orlando$month, samoloty_do_orlando$day)
samoloty_do_orlando['godzina_przylotu_pelna'] <- sprintf("%02d:%02d", samoloty_do_orlando$arr_time%/%100, samoloty_do_orlando$arr_time %% 100)
samoloty_do_orlando['przylot'] <- substr(samoloty_do_orlando$godzina_przylotu, 1, 2)
samoloty_do_orlando['date_hour'] <- paste(samoloty_do_orlando$date, samoloty_do_orlando$przylot, sep='-')
samoloty_do_orlando<-merge(samoloty_do_orlando, pogoda_mco, by.x='date_hour', by.y='date_hour')
samoloty_do_orlando<-na.omit(samoloty_do_orlando)
#Samoloty z ewr do orlando
mco_ewr<-samoloty_do_orlando[samoloty_do_orlando$origin=="EWR",]
mco_ewr<-mco_ewr[,c(1,8,11,28)]
mco_ewr<-mco_ewr%>%
  inner_join(.,pogoda_ewr,by=c("date_hour"="date_hour"))
mco_ewr['percipitation']<-(mco_ewr$precipation.x+mco_ewr$precipation.y)/2
mco_ewr<-mco_ewr[,c(2,3,8)]
mco_ewr['delay']<-mco_ewr$arr_delay-mco_ewr$dep_delay
#Samoloty z lga do orlando
mco_lga<-samoloty_do_orlando[samoloty_do_orlando$origin=="LGA",]
mco_lga<-mco_lga[,c(1,8,11,28)]
mco_lga<-mco_lga%>%
  inner_join(.,pogoda_lga,by=c("date_hour"="date_hour"))
mco_lga['percipitation']<-(mco_lga$precipation.x+mco_lga$precipation.y)/2
mco_lga<-mco_lga[,c(2,3,8)]
mco_lga['delay']<-mco_lga$arr_delay-mco_lga$dep_delay
#Samoloty z jfk do orlando 
mco_jfk<-samoloty_do_orlando[samoloty_do_orlando$origin=="JFK",]
mco_jfk<-mco_jfk[,c(1,8,11,28)]
mco_jfk<-mco_jfk%>%
  inner_join(.,pogoda_jfk,by=c("date_hour"="date_hour"))
mco_jfk['percipitation']<-(mco_jfk$precipation.x+mco_jfk$precipation.y)/2
mco_jfk<-mco_jfk[,c(2,3,8)]
mco_jfk['delay']<-mco_jfk$arr_delay-mco_jfk$dep_delay
### Wszystkie samoloty
samoloty_do_orlando<-rbind(mco_jfk,mco_lga,mco_ewr)
samoloty_do_orlando<-samoloty_do_orlando[ ,c(3,4)]
samoloty_do_orlando<-samoloty_do_orlando[samoloty_do_orlando$delay>=0,]

####### Samoloty do miami
samoloty_do_miami <- flights[flights$dest=='MIA', ]
samoloty_do_miami['date'] <- sprintf("%04d-%02d-%02d", samoloty_do_miami$year, samoloty_do_miami$month, samoloty_do_miami$day)
samoloty_do_miami['godzina_przylotu_pelna'] <- sprintf("%02d:%02d", samoloty_do_miami$arr_time%/%100, samoloty_do_miami$arr_time %% 100)
samoloty_do_miami['przylot'] <- substr(samoloty_do_miami$godzina_przylotu, 1, 2)
samoloty_do_miami['date_hour'] <- paste(samoloty_do_miami$date, samoloty_do_miami$przylot, sep='-')
samoloty_do_miami<-merge(samoloty_do_miami, pogoda_mia, by.x='date_hour', by.y='date_hour')
samoloty_do_miami<-na.omit(samoloty_do_miami)
#### samoloty z ewr do miami
mia_ewr<-samoloty_do_miami[samoloty_do_miami$origin=="EWR",]
mia_ewr<-mia_ewr[,c(1,8,11,28)]
mia_ewr<-mia_ewr%>%
  inner_join(.,pogoda_ewr,by=c("date_hour"="date_hour"))
mia_ewr['percipitation']<-(mia_ewr$precipation.x+mia_ewr$precipation.y)/2
mia_ewr<-mia_ewr[,c(2,3,8)]
mia_ewr['delay']<-mia_ewr$arr_delay-mia_ewr$dep_delay
#Samoloty z lga do miami
mia_lga<-samoloty_do_miami[samoloty_do_miami$origin=="LGA",]
mia_lga<-mia_lga[,c(1,8,11,28)]
mia_lga<-mia_lga%>%
  inner_join(.,pogoda_lga,by=c("date_hour"="date_hour"))
mia_lga['percipitation']<-(mia_lga$precipation.x+mia_lga$precipation.y)/2
mia_lga<-mia_lga[,c(2,3,8)]
mia_lga['delay']<-mia_lga$arr_delay-mia_lga$dep_delay
#Samoloty z jfk do miami
mia_jfk<-samoloty_do_miami[samoloty_do_miami$origin=="JFK",]
mia_jfk<-mia_jfk[,c(1,8,11,28)]
mia_jfk<-mia_jfk%>%
  inner_join(.,pogoda_jfk,by=c("date_hour"="date_hour"))
mia_jfk['percipitation']<-(mia_jfk$precipation.x+mia_jfk$precipation.y)/2
mia_jfk<-mia_jfk[,c(2,3,8)]
mia_jfk['delay']<-mia_jfk$arr_delay-mia_jfk$dep_delay
#Wszystkie samoloty do miami
samoloty_do_miami<-rbind(mia_jfk,mia_lga,mia_ewr)
samoloty_do_miami<-samoloty_do_miami[,c(3,4)]
samoloty_do_miami<-samoloty_do_miami[samoloty_do_miami$delay>=0,]

####### Samoloty do charlotte
samoloty_do_charlotte <- flights[flights$dest=='CLT', ]
samoloty_do_charlotte['date'] <- sprintf("%04d-%02d-%02d", samoloty_do_charlotte$year, samoloty_do_charlotte$month, samoloty_do_charlotte$day)
samoloty_do_charlotte['godzina_przylotu_pelna'] <- sprintf("%02d:%02d", samoloty_do_charlotte$arr_time%/%100, samoloty_do_charlotte$arr_time %% 100)
samoloty_do_charlotte['przylot'] <- substr(samoloty_do_charlotte$godzina_przylotu, 1, 2)
samoloty_do_charlotte['date_hour'] <- paste(samoloty_do_charlotte$date, samoloty_do_charlotte$przylot, sep='-')
samoloty_do_charlotte<-merge(samoloty_do_charlotte, pogoda_clt, by.x='date_hour', by.y='date_hour')
samoloty_do_charlotte<-na.omit(samoloty_do_charlotte)
#### samoloty z ewr do charlotte
clt_ewr<-samoloty_do_charlotte[samoloty_do_charlotte$origin=="EWR",]
clt_ewr<-clt_ewr[,c(1,8,11,28)]
clt_ewr<-clt_ewr%>%
  inner_join(.,pogoda_ewr,by=c("date_hour"="date_hour"))
clt_ewr['percipitation']<-(clt_ewr$precipation.x+clt_ewr$precipation.y)/2
clt_ewr<-clt_ewr[,c(2,3,8)]
clt_ewr['delay']<-clt_ewr$arr_delay-clt_ewr$dep_delay
#Samoloty z lga do charlotte
clt_lga<-samoloty_do_charlotte[samoloty_do_charlotte$origin=="LGA",]
clt_lga<-clt_lga[,c(1,8,11,28)]
clt_lga<-clt_lga%>%
  inner_join(.,pogoda_lga,by=c("date_hour"="date_hour"))
clt_lga['percipitation']<-(clt_lga$precipation.x+clt_lga$precipation.y)/2
clt_lga<-clt_lga[,c(2,3,8)]
clt_lga['delay']<-clt_lga$arr_delay-clt_lga$dep_delay
#Samoloty z jfk do charlotte
clt_jfk<-samoloty_do_charlotte[samoloty_do_charlotte$origin=="JFK",]
clt_jfk<-clt_jfk[,c(1,8,11,28)]
clt_jfk<-clt_jfk%>%
  inner_join(.,pogoda_jfk,by=c("date_hour"="date_hour"))
clt_jfk['percipitation']<-(clt_jfk$precipation.x+clt_jfk$precipation.y)/2
clt_jfk<-clt_jfk[,c(2,3,8)]
clt_jfk['delay']<-clt_jfk$arr_delay-clt_jfk$dep_delay
#Wszystkie samoloty do charlotte
samoloty_do_charlotte<-rbind(clt_jfk,clt_lga,clt_ewr)
samoloty_do_charlotte<-samoloty_do_charlotte[,c(3,4)]
samoloty_do_charlotte<-samoloty_do_charlotte[samoloty_do_charlotte$delay>=0,]

####### Samoloty do fll
samoloty_do_fll <- flights[flights$dest=='FLL', ]
samoloty_do_fll['date'] <- sprintf("%04d-%02d-%02d", samoloty_do_fll$year, samoloty_do_fll$month, samoloty_do_fll$day)
samoloty_do_fll['godzina_przylotu_pelna'] <- sprintf("%02d:%02d", samoloty_do_fll$arr_time%/%100, samoloty_do_fll$arr_time %% 100)
samoloty_do_fll['przylot'] <- substr(samoloty_do_fll$godzina_przylotu, 1, 2)
samoloty_do_fll['date_hour'] <- paste(samoloty_do_fll$date, samoloty_do_fll$przylot, sep='-')
samoloty_do_fll<-merge(samoloty_do_fll, pogoda_fll, by.x='date_hour', by.y='date_hour')
samoloty_do_fll<-na.omit(samoloty_do_fll)
#### samoloty z ewr do fll
fll_ewr<-samoloty_do_fll[samoloty_do_fll$origin=="EWR",]
fll_ewr<-fll_ewr[,c(1,8,11,28)]
fll_ewr<-fll_ewr%>%
  inner_join(.,pogoda_ewr,by=c("date_hour"="date_hour"))
fll_ewr['percipitation']<-(fll_ewr$precipation.x+fll_ewr$precipation.y)/2
fll_ewr<-fll_ewr[,c(2,3,8)]
fll_ewr['delay']<-fll_ewr$arr_delay-fll_ewr$dep_delay
#Samoloty z lga do fll
fll_lga<-samoloty_do_fll[samoloty_do_fll$origin=="LGA",]
fll_lga<-fll_lga[,c(1,8,11,28)]
fll_lga<-fll_lga%>%
  inner_join(.,pogoda_lga,by=c("date_hour"="date_hour"))
fll_lga['percipitation']<-(fll_lga$precipation.x+fll_lga$precipation.y)/2
fll_lga<-fll_lga[,c(2,3,8)]
fll_lga['delay']<-fll_lga$arr_delay-fll_lga$dep_delay
#Samoloty z jfk do fll
fll_jfk<-samoloty_do_fll[samoloty_do_fll$origin=="JFK",]
fll_jfk<-fll_jfk[,c(1,8,11,28)]
fll_jfk<-fll_jfk%>%
  inner_join(.,pogoda_jfk,by=c("date_hour"="date_hour"))
fll_jfk['percipitation']<-(fll_jfk$precipation.x+fll_jfk$precipation.y)/2
fll_jfk<-fll_jfk[,c(2,3,8)]
fll_jfk['delay']<-fll_jfk$arr_delay-fll_jfk$dep_delay
#Wszystkie samoloty do fll
samoloty_do_fll<-rbind(fll_jfk,fll_lga,fll_ewr)
samoloty_do_fll<-samoloty_do_fll[,c(3,4)]
samoloty_do_fll<-samoloty_do_fll[samoloty_do_fll$delay>=0,]

all_planes<-rbind(samoloty_do_atlanta,samoloty_do_chicago,samoloty_do_losangeles,
                  samoloty_do_bostonu,samoloty_do_charlotte,samoloty_do_fll,
                  samoloty_do_miami,samoloty_do_orlando)

x<-as.data.frame(table(all_planes$percipitation))
y<-x[x$Freq>=20,]
z<-all_planes[all_planes$percipitation%in%y$Var1,]
delay_data <- aggregate(delay ~ percipitation, data = z, FUN = mean)

ggplot(delay_data, aes(x = percipitation, y = delay)) +
  geom_line(color = "#729B79",lwd=1) +
  geom_text(data = delay_data[delay_data$delay == max(delay_data$delay), ], 
            aes(label = delay), hjust = -0.2, vjust = 0.5, size = 4, fontface = "bold") +
  labs(x = "Opady, mm", y = "Średnie opóźnienie, min") +
  ggtitle("Średnie opóźnienie samolotów w zależności od opadów")+
  theme(axis.text = element_text(size = 12, face = "bold"),  
       axis.title = element_text(size = 14, face = "bold"),
       plot.title = element_text(size = 16),
       plot.background = element_rect(fill = "#BACDB0", color = NA),
       panel.background = element_rect(fill = "#BACDB0", color = NA)
  )


### Sprawdzenie jaki jest procent opoznionych samolotow 
freg_dalayed_planes<-as.data.frame(table(all_planes$percipitation[all_planes$delay>5,]))
freg_dalayed_planes<-freg_dalayed_planes[freg_dalayed_planes$Freq>2,]
freq_all_planes<-as.data.frame(table(all_planes$percipitation))
percentage_delayed<-merge(freg_dalayed_planes,freq_all_planes,by="Var1")
percentage_delayed$perc<-percentage_delayed$Freq.x/percentage_delayed$Freq.y*100
percentage_delayed$Var1<-as.numeric(as.character(percentage_delayed$Var1))
percentage_delayed<-percentage_delayed[percentage_delayed$Var1<5,]
percentage_delayed_50<-percentage_delayed[percentage_delayed$perc>50,]

ggplot(percentage_delayed_50, aes(x = Var1, y = perc,group=1)) +
  geom_line(color = "#FFA62F", size = 1) + 
  geom_point(color = "#FFC96F", size = 3) + 
  labs(title = "Zależność miedzy ilością opóźnionych samolotów a ilości opadów", x = "Opady, mm", y = "Ilość opóźnionych pociągów, %") +
  theme_minimal()+
  theme(plot.background = element_rect(fill = "#FFE8C8", color = NA))+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()  
  )


#Sprawdzic wystepowanie pytan o opoznienia samolotow w dniach w ktorych bylya najwieksze opady
#Sprawdzic wykres w jakie dni najczesciej oopoznialy sie samoloty 


satysfakcja<-read.csv('satysfakcja.csv')
satysfakcja<-satysfakcja[,c(23,24,25)]
satysfakcja['delay']<-satysfakcja$Arrival.Delay.in.Minutes - satysfakcja$Departure.Delay.in.Minutes
satysfakcja<-satysfakcja[,c(3,4)]
satysfakcja<-satysfakcja[satysfakcja$delay>=10 & satysfakcja$delay<=35,]

ilosc_samolotow<-as.data.frame(table(satysfakcja$delay))
colnames(ilosc_samolotow)<-c("delay","airplane_count")

neutral_or_dissatisfied_data <- subset(satysfakcja, satisfaction == "neutral or dissatisfied")
delay_summary <- table(neutral_or_dissatisfied_data$delay)
delay_summary_df <- as.data.frame(delay_summary)
colnames(delay_summary_df) <- c("delay", "count")

result<-merge(ilosc_samolotow,delay_summary_df, by="delay")
result[is.na(result$count), "count"] <- 0
result['dissatiscaftion_precentage']<-result$count / result$airplane_count
result$delay_group <- cut(as.numeric(as.character(result$delay)), 
                          breaks = seq(0, max(as.numeric(as.character(result$delay))), by = 3), 
                          include.lowest = TRUE, right = FALSE)
grouped_result <- aggregate(cbind(airplane_count, count) ~ delay_group, data = result, sum)
grouped_result$dissatisfaction_percentage <- grouped_result$count / grouped_result$airplane_count

#wykres slupkowy 
ggplot(grouped_result, aes(x = delay_group, y = dissatisfaction_percentage, fill = delay_group)) +
  geom_bar(stat = "identity", color = "black") +  # Dodanie czarnych obramowań
  geom_text(aes(label = scales::percent(dissatisfaction_percentage)), vjust = -0.5) +  # Dodanie etykiet procentowych nad słupkami
  theme_minimal() +
  labs(title = "Procent nie usatysfakcjonowanych klientów ",
       x = "Grupa opóźnienia (minuty)",
       y = "Procent nie usatysfakcjonowanych klientów",
       fill = "Grupa opóźnienia") +
  scale_fill_manual(values = rainbow(length(grouped_result$delay_group)))+
  theme(plot.background = element_rect(fill = "#F8F4E1", color = NA))+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()  
  )

