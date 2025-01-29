library("testthat")
library("ggplot2")
library("dplyr")
library("plotly")
#3.1
#Anzahl Personen die Wichteln
n <- 8
#Anzahl an Iterationen für die Simualation
m <- 100000                
#Vektor der Personen wird erstellt
x <- 1:n                  
#zähler zählt wie oft mind. eine Person sein Geschenk wiederbekommt
zähler <- 0
#Schleife der Simulation
for (i in 1:m) {
  #Vektor x wird zufällig sortiert (jeder bekommt ein zufälliges Geschenk)
  y <- sample(x)           
  #Gucken ob jemand sein Geschenk bekommt 
  #(wenn ein Eintrag = 0 dann hat er seins wiederbekommen, sonst nicht)
  z <- x-y                 
  #Wenn mind. ein Eintrag von z Null ist, dann geht man in die If-Schleife
  if (sum(z == 0)  > 0) {  
    #Hier wird der zähler um 1 erhöht
    zähler <- zähler + 1   
  }
}
#Hier ist das Ergebnis. 
#zähler/m gibt W'keit an, dass jemand sein Geschenk wiederbekommt. 1-den Wert 
#ist Gegenw'keit, also W'keit für keiner bekommt sein eigenes Geschenk wieder
1 - zähler / m             

#3.2
#Funktionsaufruf definiert
wichtel_unglueck <- function(n, k, iterationen = 1e6){   
  #Fehlerausgabe wenn mind. eine Variable kein numerischer Wert ist
  if(!is.numeric(n) || !is.numeric(k) || !is.numeric(iterationen)){    
    stop("Falsche Werte für Funktionsaufruf")                          
  }
  #Vektor der Länge n, also Anzahl der Personen
  x <- 1:n                      
  #zähler wie oben definiert
  zähler <- 0                   
  #for schleife wie oben definiert mit iterationen als Anzahl der Itertionen
  for (i in 1:iterationen) {    
    #Vektor wie Permutiert
    y <- sample(x)              
    #geguckt ob man sein Geschenkt wiederbekommen hat
    z <- x-y                    
    #wird gezählt wie viele ihr Geschenk wiederbekommen haben,
    #wenn der Wert größer als das zugelassene k ist, wird der zähler um 1 erhöht
    if (sum(z == 0) > k){       
      zähler <- zähler + 1      
    }
  }
  #Ausgabe wieder wie oben, W'keit dafür, dass bei n Personen
  #maximal k ihr Geschenk wiederbekommen
  return(1-zähler/iterationen)  
}

#3.4
#Test, ob wenn man 5 Personen hat, dann höchstens 5 ihr Geschenk bekommen
#Kann nicht Auftreten, daher muss 1 rauskommen
#Passt, daher kein Feedback beim Ausführen
expect_equal(                   
  wichtel_unglueck(5,5),         
  1                             
)
#Test, ob wenn einer der Variablen kein numerischer Wert ist,
#dass dann der richtige Fehlernachricht kommt
expect_error(                          
  wichtel_unglueck(3,1,"das"),         
  "Falsche Werte für Funktionsaufruf"
)
#Test, ob die W'keit immer ein Wert ist und nicht mehrdimensionales
expect_length(                  
  wichtel_unglueck(10,2,100),
  1
)
#Test, ob ein richtiger Funktionsaufruf einen Error ausgibt
expect_no_error(               
  wichtel_unglueck(10,4)
)

#3.5
#Vereichnis richtig setzen
setwd('D:/Universität/Computergestützte Methoden')            
#Importieren der Datei
daten <- read.csv(file='bike_sharing_data_(with_NAs).csv', header=TRUE, sep=',')
#gucken ob es als data.frame abgespeichert wurde, Antwort: Ja
str(daten)                          
#3.5.1
#gucken ob in der group spalte ein NA ist
anyNA(daten$group)             
#da nicht, filtern wo unsere Gruppe ist (Gruppe 11)
i <- which(daten$group == 11)         
#unsere Daten in neuen data.frame abgespeichert
data <- daten[i,]                     
#3.5.2
#Es fällt auf, dass 2 Tage des Jahres fehlen, was wir später berücksichtigen müssen
nrow(data)                           
#gucken wie viele Na's wir haben: Antwort: 12
sum(is.na(data))                     
#gucken in welchen spalten und welche Einträge NA's sind
for (i in 1:length(data[1,])) {      
  print(which(is.na(data[,i])))
}
#Na's in Spalte 4 entfernen
#Aufgefallen: es ist der Tag im Jahr, also normalerweise aufsteigene Zahlen
#Da wir nicht 365 Einträge haben, fehlen welche, also falls so ein Fall Eintritt,
for (i in which(is.na(data[,4]))) {             
  if(data[i+1,4]-data[i-1,4] != 2){             
    #wo der Vor- und Nachtag vom NA Eintrag nicht sich um 2 unterschieden,
    #kommt Fehlermeldung, dass man selber nachschauen muss was der richtige Wert ist.
    print("Muss man sich selber angucken")      
    next                                        
  }                                             
  #Falls aber alles "normal" ist, einfach den Jahrestag vom Vortag um 1 erhöhen
  data[i,4] <- data[i-1,4] + 1                  
} 
#Na's in Spalte 5 entfernen (Wochentage)
for (i in which(is.na(data[,5]))) {             
  #Wenn Wochentag 2-6 fehlt, dann einfach den Vorherigen Wochentag um einen erhöhen
  if(data[i+1,5]-data[i-1,5] == 2){             
    data[i,5] <- data[i-1,5] + 1
  }
  #Wenn es ein 7. Wochentag wäre, dann 7 Eintragen
  else if (data[i-1,5] == 6 && data[i+1,5] == 1) { 
    data[i,5] <- 7
  }
  #Wenn es ein 1. Wochentag wäre, dann 1 Eintragen
  else if (data[i-1,5] == 7 && data[i+1,5] == 2) { 
    data[i,5] <- 1
  }
  #Wenn Tag davor oder danach fehlt, muss man sich es selber anschauen
  else {                                          
    print("Muss man sich selber angucken")
  }
}
#NA's in Spalte 6 entfernen (Monat des Jahres)
for (i in which(is.na(data[,6]))) {            
  #Wenn der Vor- und Nachtag im selben Monat sind,
  #Dann den Monat für das NA übernehmen
  if (data[i-1,6] == data[i+1,6]) {            
    data[i,6] <- data[i-1,6]                   
  }
  #Ansonsten muss man sich es angucken (Ende des Monate, Anfang des Monats)
  else {                                       
    print("Muss man sich selber anschauen")
  }
}
#NA Spalte 7 entfernen,
mean_precipitation <- round(mean(data$precipitation, na.rm = TRUE), digits = 2)
#Mittelwert der Spalte verwenden, da relativ zufällig unabhänig vom Zeitpunkt des Tages
data[which(is.na(data[,7])),7] <- mean_precipitation                   
#NA Spalte 8 entfernen
mean_windspeed <- round(mean(data$windspeed, na.rm = TRUE), digits = 2)
#Mittelwert der Spalte verweden, da relativ zufällig unabhänig vom Zeitpunkt des Tages
data[which(is.na(data[,8])),8] <- mean_windspeed                      
#NA Spalte 9,10,11,12 zusammen entfernen, da Schema der Imputationsverfahren gleich ist
for (j in 9:12) {                                                       
  for (i in which(is.na(data[,j]))) {                                         
    #Wert durch Mittelwert von 5 Tagen davor bis 5 Tage danach ersetzten
    #(Temperatur und ausgeliehende Fahrräder sind Jahreszeitabhängig)
    data[i,j] <- round(mean(data[(i-5):(i+5),j], na.rm = TRUE), digits = 0)    
  }
}
#Alle NA's wurden entfernt
anyNA(data)                                     
#3.5.3
#Datenanomalien
#Spannweite jeder Spalte angeguckt
for (i in 1:12) {                         
  print(c(min(data[,i]),max(data[,i])))
}
#unnormle Werte (<0 oder >5)
for (i in c(which(data$precipitation <0),which(data$precipitation >5))) {   
  #durch Mittelwert ersetzen
  data[i,7] <- mean_precipitation                                           
}
#unnormle Werte (<0)
for (i in which(data$windspeed <0)) {                                   
  #durch Mittelwert ersetzen
  data[i,8] <- mean_windspeed                                           
}
#Spalten 9 bis 12 unnormale Werte (<0), Temperatur <0 sehr unwahrschenlich,
#Ausgeliehende Fahrräder <0 auch
for (j in 9:12) {                                                 
  for (i in which(data[,j] < 0)) {
    #Imputierten durch Mittelwert von 5 Tagen davor bis 5 Tage dannach
    data[i,j] <- round(mean(data[(i-5):(i+5),j]), digits = 0)     
  }
}
#Temperatur umrechnen
#Spalte 9 bis 11 sind betroffen
for (i in 9:11) {                                             
  #Umrechnungsformel
  data[,i] <- round((data[,i]-32)*(5/9), digits = 0)          
}
#3.5.4
#Vektor für die 12 Monate erstellen
x <- 1:12                                                   
#Für jeden Monat
#count aufsummieren und in x Speichern
for (i in 1:12) {                                            
  x[i] <-  sum(data$count[data$month_of_year == i])         
}
#Monat mit der höchsten Gesamtanzahl ausgeliehender Fahrräder
which(x == max(x))                                       
#zugehöriger Wert
max(x)                                                    

# Aufgabe 4
# 4.2
ggplot(data = data) +
  geom_point(aes(x = average_temperature, y = count)) +
  xlab("Durchschnittstemperatur (in °C)") +
  ylab("Anzahl ausgeliehener Fahrräder") +
  ggtitle("Zusammenhang ausgeliehener Fahrräder und Durchschnittstemperatur (in °C)") 
ggsave(filename = "./Grafik4.2.1.pdf", width = 8, height = 5)
ggplot(data = data) +
  geom_point(aes(x = precipitation, y = count)) +
  xlab("Niederschlagsmenge") +
  ylab("Anzahl ausgeliehener Fahrräder") +
  ggtitle("Zusammenhang ausgeliehener Fahrräder und Niederschlagsmenge")
ggsave(filename = "./Grafik4.2.2.pdf", width = 8, height = 5)
ggplot(data = data) +
  geom_point(aes(x = windspeed, y = count)) +
  xlab("Windgeschwindigkeit (in km/h)") +
  ylab("Anzahl ausgeliehener Fahrräder") +
  ggtitle("Zusammenhang ausgeliehener Fahrräder und Windgeschwindigkeit (in km/h)")
ggsave(filename = "./Grafik4.2.3.pdf", width = 8, height = 5)
ggplot(data = data) +
  geom_point(aes(x = date, y = count)) +
  xlab("Tage (in 2023)") +
  ylab("Anzahl ausgeliehener Fahrräder") +
  ggtitle("Zusammenhang ausgeliehener Fahrräder und Zeit (Jahr 2023)")
ggsave(filename = "./Grafik4.2.4.pdf", width = 8, height = 5)

# 4.3
ggplot(data = filter(data, precipitation > 0)) +
  geom_point(aes(x = average_temperature, y = count)) +
  xlab("Durchschnittstemperatur (in °C)") +
  ylab("Anzahl ausgeliehener Fahrräder") +
  ggtitle("Zusammenhang ausgeliehener Fahrräder und Durchschnittstemperatur (an Tagen mit Regen)") 
ggsave(filename = "./Grafik4.3.1.pdf", width = 8, height = 5)
ggplot(data = filter(data, precipitation == 0)) +
  geom_point(aes(x = average_temperature, y = count)) +
  xlab("Durchschnittstemperatur (in °C)") +
  ylab("Anzahl ausgeliehener Fahrräder") +
  ggtitle("Zusammenhang ausgeliehener Fahrräder und Durchschnittstemperatur (an Tagen ohne Regen)") 
ggsave(filename = "./Grafik4.3.2.pdf", width = 8, height = 5)
# 4.4
ggplot(data = data) +
  geom_histogram(aes(x = count, y = ..density..)) +
  xlab("Anzahl ausgeliehener Fahrräder") +
  ylab("relative Häufigkeit") +
  ggtitle("Verteilung der ausgeliehenen Fahrräder") 
ggsave(filename = "./Grafik4.4.1.pdf", width = 8, height = 5)
ggplot(data = data) +
  geom_histogram(aes(x = average_temperature, y = ..density..)) +
  xlab("Durchschnittstemperatur (in °C)") +
  ylab("relative Häufigkeit") +
  ggtitle("Verteilung der Durchschnittstemperatur (in °C)") 
ggsave(filename = "./Grafik4.4.2.pdf", width = 8, height = 5)
ggplot(data = data) +
  geom_histogram(aes(x = precipitation, y = ..density..)) +
  xlab("Niederschlagsmenge") +
  ylab("relative Häufigkeit") +
  ggtitle("Verteilung der Niederschlagsmenge") 
ggsave(filename = "./Grafik4.4.3.pdf", width = 8, height = 5)
ggplot(data = data) +
  geom_histogram(aes(x = windspeed, y = ..density..)) +
  xlab("Windgeschwindigkeit (in km/h)") +
  ylab("relative Häufigkeit") +
  ggtitle("Verteilung der Windgeschwindigkeit (in km/h)") 
ggsave(filename = "./Grafik4.4.4.pdf", width = 8, height = 5)
# 4.5
#Jahreszeit hinzugefügt
data$season <- 1                                     
for (i in 1:length(data$season)) {
  if (data$month_of_year[i] %in% c(12,1,2)) {
      data$season[i] <- "Winter"
    }
    else if (data$month_of_year[i] %in% c(3,4,5)) {
      data$season[i] <- "Frühling"
    }
    else if (data$month_of_year[i] %in% c(6,7,8)) {
      data$season[i] <- "Sommer"
    }
    else if (data$month_of_year[i] %in% c(9,10,11)) {
      data$season[i] <- "Herbst"
    }
}
ggplot(data = data) +
  geom_density(aes(x = count, fill = season), alpha = 0.5) +
  scale_fill_manual(
    values = c(
      "Winter" = "white",
      "Frühling" = "yellow",
      "Sommer" = "blue",
      "Herbst" = "red"
    )
  ) +
  xlab("Anzahl ausgeliehener Fahrräder") +
  ylab("relative Häufigkeit") +
  labs( fill = "Jahreszeit") +
  ggtitle("Verteilung der ausgeliehenen Fahrräder nach Jahreszeit")
ggsave(filename = "./Grafik4.5.1.pdf", width = 8, height = 5)
# 4.6
Grafik <- plot_ly(data = data, x = ~average_temperature, y = ~windspeed, 
            z = ~count, type = "scatter3d", mode = "markers",
            marker = list(size = 6, opacity = 0.7), color = ~count)
Grafik %>% layout(title = "Zusammenhang Durchschnittstemperatur (in °C),
              Windgeschwindigkeit (in km/h) und Anzahl ausgeliehener Fahrräder",
              scene = list(xaxis = list(title = "Durchschnittstemperatur (in °C)"),
              yaxis = list(title = "Windgeschwindigkeit (in km/h)"),
              zaxis = list(title = "Anzahl ausgeliehener Fahrräder")),
              font = list(size = 10))
