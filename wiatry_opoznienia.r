setwd('C:/Users/janwi/OneDrive/Pulpit/studia/programowanie/R/projekt')
flights <- read.csv('flights.csv')
pogoda_atlanta_x <- read.csv('wiatr_atlanta.csv')
t <- nrow(pogoda_atlanta_x)
wiatr_atlanta <- data.frame(time=pogoda_atlanta_x[3:t, 'latitude'], wind_spped = as.integer(pogoda_atlanta_x[3:t, 'longitude']), wind_direction = as.integer(pogoda_atlanta_x[3: t, 'elevation']))

library(dplyr)

wiatr_atlanta <- wiatr_atlanta %>%
  mutate(
    direction_text = case_when(
      wind_direction >= 0 & wind_direction < 90 ~ "polnocno-wschodni",
      wind_direction >= 90 & wind_direction < 180 ~ "poludniowo-wschodni",
      wind_direction >= 180 & wind_direction < 270 ~ "poludniowo-zachodni",
      TRUE ~ "polnocno-zachodni"
    )
  )

pogoda_boston_x <- read.csv('wiatr_boston.csv')
t <- nrow(pogoda_boston_x)
wiatr_boston <- data.frame(time=pogoda_boston_x[3:t, 'latitude'], wind_spped = as.integer(pogoda_boston_x[3:t, 'longitude']), wind_direction = as.integer(pogoda_boston_x[3: t, 'elevation']))
library(dplyr)

wiatr_boston <- wiatr_boston %>%
  mutate(
    direction_text = case_when(
      wind_direction >= 0 & wind_direction < 90 ~ "polnocno-wschodni",
      wind_direction >= 90 & wind_direction < 180 ~ "poludniowo-wschodni",
      wind_direction >= 180 & wind_direction < 270 ~ "poludniowo-zachodni",
      TRUE ~ "polnocno-zachodni"
    )
  )

pogoda_los_x <- read.csv('wiatr_losangeles.csv')
t <- nrow(pogoda_los_x)
wiatr_los <- data.frame(time=pogoda_los_x[3:t, 'latitude'], wind_spped = as.integer(pogoda_los_x[3:t, 'longitude']), wind_direction = as.integer(pogoda_los_x[3: t, 'elevation']))
library(dplyr)

wiatr_los <- wiatr_los %>%
  mutate(
    direction_text = case_when(
      wind_direction >= 0 & wind_direction < 90 ~ "polnocno-wschodni",
      wind_direction >= 90 & wind_direction < 180 ~ "poludniowo-wschodni",
      wind_direction >= 180 & wind_direction < 270 ~ "poludniowo-zachodni",
      TRUE ~ "polnocno-zachodni"
    )
  )

flights <- flights %>%
  mutate(
    flight_direction = case_when(
      dest %in% c('ATL', 'LAX') ~ "poludniowo-zachodni",
      dest == 'BOS' ~ "polnocno-wschodni",
      TRUE ~ " "
    )
  )
########## Eniu zrob mape usa i nanies na nią lotniska
flights['delay'] <- flights$dep_delay-flights$arr_delay
asd <- aggregate(flights['delay'], flights['dest'], mean, na.rm=TRUE)
ord <- order(asd$delay, decreasing=TRUE)
rty <- asd[ord, ]

#########
flights['date'] <- sprintf("%04d-%02d-%02d", flights$year, flights$month, flights$day)
flights['godzina_przylotu_pelna'] <- sprintf("%02d:%02d", flights$arr_time%/%100, flights$arr_time %% 100)
godz_przyl <- substr(flights$godzina_przylotu, 1, 2)
flights['przylot'] <- godz_przyl
head(wiatr_los)

head(flights)
loty <- flights[flights$flight_direction != ' ', c(7, 10, 22, 23, 25,24)]
colnames(loty)
loty['x'] <- paste(as.character(loty$date), as.character(loty$godzina_przylotu_pelna), sep='-')
loty['x'] <- substr(loty$x, 1, 13)

wiatry <- rbind(wiatr_atlanta, wiatr_boston, wiatr_los)

date <- substr(wiatry$time, 1, 10)
time <- substr(wiatry$time, 12, 13)
wiatry['date_time'] <- paste(date, time, sep='-')

head(wiatry)
head(loty)
wyniki <- merge(loty, wiatry, by.x='x', by.y='date_time')
head(wyniki)
delay <- wyniki$arr_delay-wyniki$dep_delay
wyniki['delay'] <- delay
wyniki <- wyniki[ ,c(4, 5,9,11)]
head(wyniki)
wyniki['wind_direction'] <- wyniki$direction_text


wyniki <- wyniki %>%
  mutate(
    wiatr = case_when(
      flight_direction == wind_direction ~ 'z wiatrem',
      flight_direction == "polnocno-wschodni" & wind_direction == "poludniowo-zachodni" ~ "pod wiatr",
      flight_direction == "poludniowo-zachodni" & wind_direction == "polnocno-wschodni" ~ "pod wiatr",
      TRUE ~ "kat jest mniejszy od 180 ale wiekszy od 0"
    )
  )
z_wiatrem <- wyniki[wyniki$wiatr=='z wiatrem', c(5,2,3)]
head(z_wiatrem)
u1 <- z_wiatrem[z_wiatrem$delay>0, ]
v1 <- u1[!is.na(u1$delay), ]
z_wiatrem <- z_wiatrem[!is.na(z_wiatrem$delay), ]
q1<- nrow(v1)/nrow(z_wiatrem)


pod_wiatr <- wyniki[wyniki$wiatr=='pod wiatr', c(5,2,3)]


u2 <- pod_wiatr[pod_wiatr$delay>0, ]
v2 <- u2[!is.na(u2$delay), ]
pod_wiatr <- pod_wiatr[!is.na(pod_wiatr$delay), ]
q2<- nrow(v2)/nrow(pod_wiatr)
u22 <- aggregate(u2['delay'], u2['wind_spped'], mean, na.rm=TRUE)
u23 <- aggregate(pod_wiatr['delay'], pod_wiatr['wind_spped'], mean, na.rm=TRUE)
u23 <- u23[1:41, ]
u23 <- u23[u23$delay>0, ]
inny <- wyniki[wyniki$wiatr=='kat jest mniejszy od 180 ale wiekszy od 0', c(5,2,3)]
u3 <- inny[inny$delay>0, ]
v3 <- inny[!is.na(u3$delay), ]
inny <- inny[!is.na(inny$delay), ]
q3<- nrow(v3)/nrow(inny)

abc1 <- aggregate(z_wiatrem['delay'], z_wiatrem['wind_spped'], mean, na.rm=TRUE)
abc11 <- z_wiatrem %>%
  group_by(wind_spped) %>%
  summarise(mean_delay = mean(delay, na.rm = TRUE))

# Stwórz wykres za pomocą ggplot2
ggplot(abc11, aes(x = wind_spped, y = mean_delay)) +
  geom_line(color = "#006989", size = 1) +       
  geom_point(color = "#E88D67", size = 3) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "#005C78")+
  labs(title = "Średnie opóźnienie samolotów lecących zgodnie z kierunkiem wiatru",
       x = "Prędkość wiatru",
       y = "Średnie opóźnienie") +  
  theme_minimal() +                           
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),   
        axis.title = element_text(size = 14),                               
        axis.text = element_text(size = 12))  

ggplot(u23, aes(x = wind_spped, y = delay)) +
  geom_line(color = "#006989", size = 1) +       
  geom_point(color = "#E88D67", size = 3) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "white")+
  labs(title = "Średnie opóźnienie samolotów lecących przeciwnie do kierunku wiatru",
       x = "Prędkość wiatru",
       y = "Średnie opóźnienie") +  
  theme_minimal() +                           
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),   
        axis.title = element_text(size = 14),                               
        axis.text = element_text(size = 12))  

library(ggplot2)
opoznienie_z_wiatrem <- abc1[c(-42, -41, -37, -39), ]
abc2 <- aggregate(pod_wiatr['delay'], pod_wiatr['wind_spped'], mean, na.rm=TRUE)
abc3 <- aggregate(inny['delay'], inny['wind_spped'], mean, na.rm=TRUE)
