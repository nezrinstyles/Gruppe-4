## Aufgabe 1: Nazrin Azizova und Sissi Chen



# set.seed fuer Wiederholbarkeit
set.seed(1234)

# simulierter Datensatz
id <- c(1:100)
alter <- round(rnorm(100, mean = 25, sd = 2))
studienfach <- sample(c("Statistik", "Data Science", "Mathe", "Informatik"), size =  100,
                      replace= T, prob= c(0.35, 0.35, 0.1, 0.2))
skala <- c(7:1)     ## Skala, wobei 1 = sehr geringe Interesse und 7 = sehr hohe Interesse


# Interesse an Mathe
int.math <- c()   ## leerer Vektor für die Ergebnisse
# for - loop, um Zusammenhang zu erzeugen
for(i in 1:100){
  output <- 
    if(studienfach[i] == "Mathe"){
      sample(skala, 1, replace = T, prob = c(0.4, 0.35, 0.15, 0.1, 0, 0, 0))
    } else {
      sample(skala, 1, replace = T, prob = c(0.15, 0.15, 0.2, 0.2, 0.1, 0.1, 0.1))                 
    }
  
  int.math <- c(int.math, output)
}



# dataframe erstellen
df <- data.frame(id, alter, studienfach, int.math)


