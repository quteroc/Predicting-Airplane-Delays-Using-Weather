setwd('C:/Users/janwi/OneDrive/Pulpit/studia/programowanie/R/projekt')
flights <- read.csv('flights.csv')
head(flights)
u <- as.data.frame(table(flights$dest))
idx <- order(u$Freq, decreasing = TRUE)
u[idx, ]
pogoda_chicago_1 <- read.csv('chicago.csv')

data_time <- pogoda_chicago_1[3:nrow(pogoda_chicago_1) , 'latitude']


pogoda_chicago <- data.frame(data = substr(data_time, 1, 10), time=substr(data_time, 12, 13), precipation=pogoda_chicago_1[3:nrow(pogoda_chicago_1), 'longitude'])

pogoda_atlanta <- read.csv('atlanta.csv')

pogoda_chicago['date_hour'] <- paste(pogoda_chicago$data, pogoda_chicago$time, sep='-')
samoloty_do_chicago <- flights[flights$dest=='ORD', ]

samoloty_do_chicago['date'] <- sprintf("%04d-%02d-%02d", samoloty_do_chicago$year, samoloty_do_chicago$month, samoloty_do_chicago$day)
samoloty_do_chicago['godzina_przylotu_pelna'] <- sprintf("%02d:%02d", samoloty_do_chicago$arr_time%/%100, samoloty_do_chicago$arr_time %% 100)
godz_przyl <- substr(samoloty_do_chicago$godzina_przylotu, 1, 2)
samoloty_do_chicago['przylot'] <- godz_przyl

samoloty_do_chicago['date_hour'] <- paste(samoloty_do_chicago$date, samoloty_do_chicago$przylot, sep='-')
x<-merge(samoloty_do_chicago, pogoda_chicago, by.x='date_hour', by.y='date_hour')

a <- x[ ,c(8, 11,28)]

a['delay'] <- a$arr_delay - a$dep_delay


pogoda_atlanta1 <- read.csv('atlanta.csv')
data_time_a <- pogoda_chicago_1[3:nrow(pogoda_atlanta1) , 'latitude']
pogoda_atlanta <- data.frame(data = substr(data_time_a, 1, 10), time=substr(data_time_a, 12, 13), precipation=pogoda_atlanta1[3:nrow(pogoda_atlanta1), 'longitude'])
pogoda_atlanta['date_hour'] <- paste(pogoda_atlanta$data, pogoda_atlanta$time, sep='-')

samoloty_do_atlanty <- flights[flights$dest=='ATL', ]
samoloty_do_atlanty['date'] <- sprintf("%04d-%02d-%02d", samoloty_do_atlanty$year, samoloty_do_atlanty$month, samoloty_do_atlanty$day)
samoloty_do_atlanty['godzina_przylotu_pelna'] <- sprintf("%02d:%02d", samoloty_do_atlanty$arr_time%/%100, samoloty_do_atlanty$arr_time %% 100)
godz_przyl <- substr(samoloty_do_atlanty$godzina_przylotu, 1, 2)
samoloty_do_atlanty['przylot'] <- godz_przyl

samoloty_do_atlanty['date_hour'] <- paste(samoloty_do_atlanty$date, samoloty_do_atlanty$przylot, sep='-')
y<-merge(samoloty_do_atlanty, pogoda_atlanta, by.x='date_hour', by.y='date_hour')

b <- y[ ,c(8, 11,28)]

b['delay'] <- b$arr_delay - b$dep_delay

c <- rbind(a, b)

#analizowalismy ponad 25 000 lotow do chicago i atlanty
c['delay'] <- as.integer(c$delay)
awq <- aggregate(c['delay'], c['precipation'], mean, na.rm=TRUE)
end <- awq[c(-38, -56), ]
end


###############################################################################################################
flights['delay'] <- flights$dep_delay - flights$arr_delay
xd <- aggregate(flights['delay'], flights['dest'], mean, na.rm=TRUE)
ending <- xd[-52, ]
ending
