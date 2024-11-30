setwd('D:/Studia/2 semestr/PDU/R/Projekt02/Pociagi')
library(dplyr)
library(ggplot2)

#Pobranie danych
train_18_12<-read.csv("2018_12.csv")
train_19_01<-read.csv("2019_01.csv")
train_19_02<-read.csv("2019_02.csv")
train_19_03<-read.csv("2019_03.csv")
train_19_12<-read.csv("2019_12.csv")
train_20_01<-read.csv("2020_01.csv")
train_20_02<-read.csv("2020_02.csv")
train_20_03<-read.csv("2020_03.csv")
trains<-rbind(train_18_12,train_19_01,train_19_02,train_19_03,
              train_19_12,train_20_01,train_20_02,train_20_03)

new_york_train<-max_delay_dest[max_delay_dest$from%in%c("Newark Penn Station","New York Penn Station","Newark Broad Street",
                                                        "Secaucus Upper Lvl","Maplewood","Secaucus Lower Lvl","Hoboken"
                                                        ,"Rutherford","South Orange","Newark Airport","Murray Hill","Linden"),]
princeton_train<-max_delay_dest[max_delay_dest$from%in%c("Princeton Junction","Princeton","Teterboro"),]
trenton_train<-max_delay_dest[max_delay_dest$from%in%c("Trenton"),]
metropark_train<-max_delay_dest[max_delay_dest$from%in%c("Metropark","Metuchen","Rahway"),]
summit_train<-max_delay_dest[max_delay_dest$from%in%c("Summit","Millburn","Cranford"),]


#przygotowanie pogody
#Pogoda nowy york
pogoda_ny_1<-read.csv("pogoda_new_york_2018_12-2019_03.csv")
pogda_ny_2<-read.csv("pogoda_new_york_19_12_20_03.csv")
pogoda_ny_1<-pogoda_ny_1[-(1:2),]
pogda_ny_2<-pogda_ny_2[-(1:2),]
pogoda_ny<-rbind(pogoda_ny_1,pogda_ny_2)
rownames(pogoda_ny)<-NULL
pogoda_ny<-pogoda_ny[,1:3]
colnames(pogoda_ny)<-c("date",'rain','snow')
#Pogoda princeton
pogoda_pr_1<-read.csv("pogoda_princeton_19_20.csv")
pogda_pr_2<-read.csv("pogoda_princeton_18_19.csv")
pogoda_pr_1<-pogoda_pr_1[-(1:2),]
pogda_pr_2<-pogda_pr_2[-(1:2),]
pogoda_pr<-rbind(pogoda_pr_1,pogda_pr_2)
rownames(pogoda_pr)<-NULL
pogoda_pr<-pogoda_pr[,1:3]
colnames(pogoda_pr)<-c("date",'rain','snow')
#pogoda trenton
pogoda_tr_1<-read.csv("pogoda_trenton_19_20.csv")
pogda_tr_2<-read.csv("pogoda_trenton_18_19.csv")
pogoda_tr_1<-pogoda_tr_1[-(1:2),]
pogda_tr_2<-pogda_tr_2[-(1:2),]
pogoda_tr<-rbind(pogoda_tr_1,pogda_tr_2)
rownames(pogoda_tr)<-NULL
pogoda_tr<-pogoda_tr[,1:3]
colnames(pogoda_tr)<-c("date",'rain','snow')
#pogoda metropark
pogoda_mt_1<-read.csv("pogoda_metropark_19_20.csv")
pogda_mt_2<-read.csv("pogoda_metropark_18_19.csv")
pogoda_mt_1<-pogoda_mt_1[-(1:2),]
pogda_mt_2<-pogda_mt_2[-(1:2),]
pogoda_mt<-rbind(pogoda_mt_1,pogda_mt_2)
rownames(pogoda_mt)<-NULL
pogoda_mt<-pogoda_mt[,1:3]
colnames(pogoda_mt)<-c("date",'rain','snow')
#pogoda summit
pogoda_su_1<-read.csv("pogoda_summit_19_20.csv")
pogda_su_2<-read.csv("pogoda_summit_18_19.csv")
pogoda_su_1<-pogoda_su_1[-(1:2),]
pogda_su_2<-pogda_su_2[-(1:2),]
pogoda_su<-rbind(pogoda_su_1,pogda_su_2)
rownames(pogoda_su)<-NULL
pogoda_su<-pogoda_su[,1:3]
colnames(pogoda_su)<-c("date",'rain','snow')

#Zaleznosc opoznienia od opadow sniegu
new_york_train_snow<-merge(new_york_train,pogoda_ny[,c(1,3)],by="date")
# new_york_train_snow<-new_york_train_snow[new_york_train_snow$snow>0.01,]

princeton_train_snow<-merge(princeton_train,pogoda_pr[,c(1,3)],by="date")
# princeton_train_snow<-princeton_train_snow[princeton_train_snow$snow>0.01,]

trenton_train_snow<-merge(princeton_train,pogoda_tr[,c(1,3)],by="date")
# trenton_train_snow<-trenton_train_snow[trenton_train_snow$snow>0.01,]

metropark_train_snow<-merge(metropark_train,pogoda_mt[,c(1,3)],by="date")
# metropark_train_snow<-metropark_train_snow[metropark_train_snow>0.01,]

summit_train_snow<-merge(summit_train,pogoda_su[,c(1,3)],by="date")
# summit_train_snow<-summit_train_snow[summit_train_snow$snow>0.01,]

all_trains<-rbind(new_york_train_snow,princeton_train_snow,trenton_train_snow,
                       metropark_train_snow,summit_train_snow)
all_trains_snow<-all_trains[all_trains$average>0.1,]
all_trains_snow<-all_trains_snow[,c(5,6)]

snow_10<-as.data.frame(table(all_trains_snow$snow))
snow_10<-snow_10[snow_10$Freq>10,]
trains_snow<-all_trains_snow[all_trains_snow$snow%in%snow_10$Var1,]

delay_data<-aggregate(average ~ snow,data = trains_snow, FUN = mean)
delay_data<-delay_data[delay_data$average>1,]
delay_data$snow<-as.numeric(delay_data$snow)
delay_data<-delay_data%>%arrange(-desc(snow))

mean_snow<-mean(delay_data$snow)
sd_snow<-sd(delay_data$snow)
#Wykres Średnie opóźnienie pociągów w zależności od opadów śniegu
barplot(delay_data$average,
        names.arg = delay_data$snow,
        xlab = "Opady, cm",
        ylab = "Średnie opóźnienie, min",
        main = "Średnie opóźnienie pociągów w zależności od opadów śniegu",
         col = ifelse(delay_data$average == max(delay_data$average), "#7469B6", "#AD88C6"),
        las = 2)
avg_delay <- mean(delay_data$average)
abline(h = avg_delay, col = "#E1AFD1",lwd=2)
text(x = 2.5, y = avg_delay+0.5, labels = round(avg_delay, 2), pos = 2, col = "#7469B6", cex = 0.8)

############## 
freq_trains_delayed_snow<-as.data.frame(table(all_trains_snow$snow[all_trains_snow$average>0.5]))
freq_trains_delayed_snow<-freq_trains_delayed_snow[freq_trains_delayed_snow$Freq>25,]
freq_trains_snow<-as.data.frame(table(all_trains_snow$snow))
freq_trains_snow<-freq_trains_snow[freq_trains_snow$Freq>1,]
percentage_delayed_snow<-merge(freq_trains_delayed_snow,freq_trains_snow,by="Var1")
percentage_delayed_snow$perc<-percentage_delayed_snow$Freq.x/percentage_delayed_snow$Freq.y*100
percentage_delayed_snow$Var1 <- as.numeric(as.character(percentage_delayed_snow$Var1))
percentage_delayed_snow <- percentage_delayed_snow[percentage_delayed_snow$Var1 < 9, ]
percentage_delayed_snow_60<-percentage_delayed_snow[percentage_delayed_snow$perc>60,]

ggplot(percentage_delayed_snow_60, aes(x = Var1, y = perc,group=1)) +
  geom_line(color = "#254336", size = 1) + 
  geom_point(color = "#6B8A7A", size = 3) + 
  labs(title = "Zależność miedzy ilością opóznionych pociągów a ilości opadów śniegu", x = "Opady, cm", y = "Ilość opóźnionych pociągów, %") +
  theme_minimal()+
  theme(plot.background = element_rect(fill = "#DAD3BE", color = NA))+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()  
  )

#### Sprawdzimy ilosc opoznionych samolotow w zaleznosci od opadow 

freq_trains_delayed<-as.data.frame(table(all_trains$snow[all_trains$average>0.5]))
freq_trains_delayed<-freq_trains_delayed[freq_trains_delayed$Freq>1,]
freq_trains<-as.data.frame(table(all_trains$snow))
freq_trains<-freq_trains[freq_trains$Freq>1,]
percentage_delayed<-merge(freq_trains_delayed,freq_trains,by="Var1")
percentage_delayed$perc<-percentage_delayed$Freq.x/percentage_delayed$Freq.y*100
percentage_delayed$Var1 <- as.numeric(as.character(percentage_delayed$Var1))
percentage_delayed <- percentage_delayed[percentage_delayed$Var1 < 9 ]

percentage_delayed_20<-percentage_delayed[percentage_delayed$perc>20,]
mean_perc <- mean(percentage_delayed_20$perc)
sd_perc <- sd(percentage_delayed_20$perc)

# Rysowanie histogramu 
ggplot(percentage_delayed_20, aes(x = perc)) +
  geom_histogram(aes(y = ..density..), binwidth = 10, fill = "#201658", color = "black", alpha = 0.8) +
  stat_function(fun = dnorm, args = list(mean = mean_perc, sd = sd_perc), color = "#910A67", size = 1.5) +
  labs(title = "Histogram ilości opóźnionych pociągów z powodu opadów śniegu", x = "Procent opóźnionych pociągów", y = "Częstość") +
  theme_minimal()+
  theme(plot.background = element_rect(fill = "#98ABEE", color = NA))

#### zaleznosci od opadow deszczu

new_york_train_rain<-merge(new_york_train,pogoda_ny[,c(1,2)],by="date")

princeton_train_rain<-merge(princeton_train,pogoda_pr[,c(1,2)],by="date")

trenton_train_rain<-merge(princeton_train,pogoda_tr[,c(1,2)],by="date")

metropark_train_rain<-merge(metropark_train,pogoda_mt[,c(1,2)],by="date")

summit_train_rain<-merge(summit_train,pogoda_su[,c(1,2)],by="date")

all_trains_rain<-rbind(new_york_train_rain,princeton_train_rain,trenton_train_rain,metropark_train_rain
                       ,summit_train_rain)

all_trains_rain<-all_trains_rain[,c(5,6)]

filter<-as.data.frame(table(all_trains_rain$rain))
filter<-filter[filter$Freq>20,]

trains_rain<-all_trains_rain[all_trains_rain$rain%in%filter$Var1,]

delay_data_rain<-aggregate(average ~ rain,data = trains_rain, FUN = mean)
delay_data_rain<-delay_data_rain[delay_data_rain$average>1,]
delay_data_rain$rain<-as.numeric(delay_data_rain$rain)
delay_data_rain<-delay_data_rain%>%arrange(-desc(rain))

colors <- ifelse(delay_data_rain$average == max(delay_data_rain$average), 
                 "#5B2A86", 
                 ifelse(delay_data_rain$average > mean(delay_data_rain$average), "#7785AC", "#B2FFA9"))

barplot(delay_data_rain$average,
        names.arg = delay_data_rain$rain,
        xlab = "Opady, mm",
        ylab = "Średnie opóźnienie, min",
        main = "Średnie opóźnienie pociągów w zależności od opadów deszczu",
        col = colors,
        las = 2)

avg_delay <- mean(delay_data_rain$average)
abline(h = avg_delay, col = "#565656",lwd=2)
text(x = 179, y = avg_delay+0.5, labels = round(avg_delay, 2), pos = 2, col = "#565656", cex = 0.8)

############## Procent opoznionych samolotow w zalenzosci od opadow deszczu
freq_trains_delayed_rain<-as.data.frame(table(all_trains_rain$rain[all_trains$average>0.5]))
freq_trains_delayed_rain<-freq_trains_delayed_rain[freq_trains_delayed$Freq>20,]
freq_trains_rain<-as.data.frame(table(all_trains_rain$rain))
freq_trains_rain<-freq_trains_rain[freq_trains_rain$Freq>1,]
percentage_delayed_rain<-merge(freq_trains_delayed_rain,freq_trains_rain,by="Var1")
percentage_delayed_rain$perc<-percentage_delayed_rain$Freq.x/percentage_delayed_rain$Freq.y*100
percentage_delayed_rain$Var1 <- as.numeric(as.character(percentage_delayed_rain$Var1))
percentage_delayed_rain <- percentage_delayed_rain[percentage_delayed_rain$Var1 < 38, ]
percentage_delayed_rain_40<-percentage_delayed_rain[percentage_delayed_rain$perc>50,]

ggplot(percentage_delayed_rain_40, aes(x = Var1, y = perc,group=1)) +
  geom_line(color = "#803D3B", size = 1) + 
  geom_point(color = "#AF8260", size = 3) + 
  labs(title = "Zależność miedzy ilością opóznionych pociągów a ilości opadów deszczu", x = "Opady, mm", y = "Ilość opóźnionych pociągów, %") +
  theme_minimal()+
  theme(plot.background = element_rect(fill = "#E4C59E", color = NA))+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()  
  )

###
percentage_delayed_rain_20<-percentage_delayed_rain[percentage_delayed_rain$perc>20,]
mean_perc <- mean(percentage_delayed_rain_20$perc)
sd_perc <- sd(percentage_delayed_rain_20$perc)

# Rysowanie histogramu 
ggplot(percentage_delayed_rain_20, aes(x = perc)) +
  geom_histogram(aes(y = ..density..), binwidth = 10, fill = "#28666E", color = "black", alpha = 0.8) +
  stat_function(fun = dnorm, args = list(mean = mean_perc, sd = sd_perc), color = "#274690", size = 1.5) +
  labs(title = "Histogram ilości opóźnionych pociągów z powodu opadów deszczu", x = "Procent opóźnionych pociągów", y = "Częstość") +
  theme_minimal()+
  theme(plot.background = element_rect(fill = "#94B0DA", color = NA))



