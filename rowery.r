setwd('C:/Users/janwi/OneDrive/Pulpit/studia/programowanie/R/projekt')
styczen <- read.csv('styczen.csv')
luty <- read.csv('luty.csv')
marzec <- read.csv('marzec.csv')
kwiecien <- read.csv('kwiecien.csv')
maj <- read.csv('maj.csv')
czerwiec <- read.csv('czerwiec.csv')
lipiec <- read.csv('lipiec.csv')
sierpien <- read.csv('sierpień.csv')
wrzesien <- read.csv('wrzesien.csv')
pazdziernik <- read.csv('pazdziernik.csv')
listopad <- read.csv('listopad.csv')
grudzien <- read.csv('grudzien.csv')

dane <- rbind(styczen, luty, marzec, kwiecien, maj, czerwiec, lipiec, sierpien, wrzesien, pazdziernik, listopad, grudzien)
a <- as.integer(substr(dane$ended_at, 12, 13))*3600 + as.integer(substr(dane$ended_at, 15, 16))*60 + as.integer(substr(dane$ended_at, 18, 19))
b <- as.integer(substr(dane$started_at, 12, 13))*3600 + as.integer(substr(dane$started_at, 15, 16))*60 + as.integer(substr(dane$started_at, 18, 19))

pogoda <- read.csv('pogoda.csv')

data_zak_podrozy <- substr(dane$ended_at, 1, 13)
dane['end_date'] <- data_zak_podrozy
dane['dr'] <- (a-b)/60
dane <- dane[dane$dr > 3, ]
pogoda_dobra <- data.frame(data = pogoda[3: nrow(pogoda),'latitude'], temperature=pogoda[3:nrow(pogoda), 'longitude'])
pogoda_dobra['date'] <- substr(pogoda_dobra$data, 1, 10)
pogoda_dobra['hour'] <- substr(pogoda_dobra$data, 12, 13)
pogoda_dobra['date_hour'] <- paste(pogoda_dobra$date, pogoda_dobra$hour, sep = " ")
head(pogoda_dobra)
x <- merge(dane, pogoda_dobra, by.x='end_date', by.y='date_hour')
head(x)

x['duration'] <- x$dr
head(q)
q <- x[, c(18, 4, 5, 17, 20)]
q['cost'] <- q$duration * 0.5
q['temperature'] <- round(as.double(q$temperature))
r <- aggregate(q['duration'], q['temperature'], mean)
r
s <- aggregate(q['cost'], q['temperature'], sum)
s['cost'] <- s$cost/1000
s

library(ggplot2)
install.packages("ggplot2")

wykres1 <- ggplot(data = r, mapping=aes(x=temperature, y=duration)) +
  geom_point() +
  geom_point(
    colour='blue', size = 3) +
  labs(x='temperature[C]', y='duration[min]')
  
wykres1

indeks_max1 <- which.max(r$duration)
indeks_min1 <- which.min(r$duration)


x_najwieksza1 <- r$temperature[indeks_max1]
x_najmniejsza1 <- r$temperature[indeks_min1]


wykres2 <- ggplot(data = r, mapping = aes(x = temperature, y = duration)) +
  geom_point(aes(color = ifelse(temperature == x_najwieksza1 | temperature == x_najmniejsza1, "Extreme Duration", "Other")), size = 3) +
  scale_color_manual(values = c("red", "blue"), guide = FALSE) +
  labs(x = "Temperature [C]", y = "Duration [min]") +
  geom_vline(xintercept = c(x_najwieksza1, x_najmniejsza1), linetype = "dashed", color = "black") +
  geom_text(aes(x = x_najwieksza1, y = 0, label = x_najwieksza1), vjust = -0.5, col = "black", size = 3) +
  geom_text(aes(x = x_najmniejsza1, y = 0, label = x_najmniejsza1), vjust = -0.5, col = "black", size = 3)
print(wykres2)

max_index <- which.max(s$cost)
colors <- ifelse(s$cost > 3000, "green", "gray") 
colors[max_index] <- "blue" 
selected_indices <- which(s$cost > 3000)

font_labels <- ifelse(s$cost > 3000, 2, 1) 
font_labels[max_index] <- 2 
par(mar=c(5, 5, 4, 2) + 0.1)
par(cex.axis=1.2)
par(cex.lab=1.5)
par(cex.main=1.5)


bp <- barplot(s$cost, 
              names.arg=s$temperature,    
              las=1, 
              ylim=c(0, 3400),    
              yaxt="n",           
              col=colors)         


axis(2, at=seq(0, 3400, by=200), las=1)

abline(h=seq(200, 3400, by=400), col="gray", lty=2)



### zaleznosc temperatury od ilosci przejazdow
head(q)
u <- as.data.frame(table(q$temperature))
u['temperature'] <- u$Var1
u['frequency'] <- u$Freq/1000
e <- data.frame(temperature=u$temperature, frequency=u$frequency)


min_wartosc <- min(e$frequency)
max_wartosc <- max(e$frequency)

indeks_min <- which.min(e$frequency)
indeks_max <- which.max(e$frequency)
wartosc_x_min <- e$temperature[indeks_min]
wykres <- barplot(e$frequency, names.arg = e$temperature, ylim=c(0, 400), xlim=c(2, 60), las=2, xlab='temperature [C]', ylab='frequency[*10^3]', col='grey85')
abline(h = seq(0, 800, by = 50), lty = 3, col = 'gray1')
segments(wykres[indeks_min], 0, wykres[indeks_min], min_wartosc, col = "red", lwd = 2)
segments(wykres[indeks_max], 0, wykres[indeks_max], max_wartosc, col = "red", lwd = 2)
wykres
text(wykres[indeks_min], -1, labels = e$temperature[indeks_min], pos = 1, col = "red")
text(wykres[indeks_max], -1, labels = ramka_danych$kategoria[indeks_max], pos = 1, col = "green")

