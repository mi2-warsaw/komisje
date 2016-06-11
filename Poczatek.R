# Początki

# 1. Wczytanie potrzebnych pakietów
# 2. Wczytanie stenogramu
# 3. Podzielenie na akapity
# 4. Wyodrębnienie konkretnych akapitów

## 4.1. Akapity, które zawierają uczestników posiedzenia
### a) Podzielenie akapitów na słowa
### b) Wyodrębnienie z nich imion i nazwisk

## 4.2. Akapit, który zawiera powód zwołania komisji
### a) Wydobycie powodu zwołania obrad

# 5. Wydobycie numeru kadencji sejmu
# 6. Wydobycie numeru posiedzenia komisji

# .
# .
# .

# Przedostatni punkt. Stworzenie ramki danych
# Ostatni punkt. Przetworzenie ramki danych na plik csv 

# Przykład:

# 1.
# Wczytajmy pakiety, które pomogą nam zdoyć środek html, jak również inne, które pomogą nam w późniejszych analizach

library(RCurl)
library(XML)
library(ggplot2)

# 2.
# Potrzebny kod do wczytywania stenogramów! - skorzystanie z RCurl i XML

stenogram <- "W posiedzeniu udział wzięli: Konstanty Radziwiłł minister zdrowia i Jarosław Pinkas sekretarz stanu w Ministerstwie Zdrowia ze współpracownikami, Andrzej Jacyna p.o. prezesa Narodowego Funduszu Zdrowia ze współpracownikiem, Krystyna Kozłowska rzecznik praw pacjenta ze współpracownikiem, Bartosz Sowiera dyrektor gabinetu Rzecznika w Biurze Rzecznika Praw Dziecka, Maciej Szustowicz wicedyrektor Departamentu Zdrowia Najwyższej Izby Kontroli, Dorota Budarz członek Rady Krajowej, Marcelina Zawisza członek Zarządu Krajowego i Marta Nowak rzecznik prasowy Partii Razem, Zdzisław Bujas wiceprzewodniczący Zarządu Krajowego Ogólnopolskiego Związku Zawodowego Pielęgniarek i Położnych, Wanda Fidelus-Ninkiewicz dyrektor Biura Naczelnej Izby Lekarskiej ze współpracownikiem, Jan Kowalczuk członek Zarządu Ogólnopolskiego Związku Zawodowego Lekarzy wraz ze współpracownikami, Zofia Małas prezes Naczelnej Rady Pielęgniarek i Położnych wraz ze współpracownikami Elżbieta Piotrowska-Rutkowska prezes Naczelnej Rady Aptekarskiej oraz Mateusz Moksik asystent przewodniczącego Komisji
W posiedzeniu udział wzięli pracownicy Kancelarii Sejmu: Longina Grzegrzułka, Małgorzata Siedlecka-Nowak, Monika Żołnierowicz-Kasprzyk – z sekretariatu Komisji w Biurze Komisji Sejmowych.
Komisja Zdrowia, obradująca pod przewodnictwem posła Bartosza Arłukowicza (PO), przewodniczącego Komisji oraz posła Tomasza Latosa (PiS), zastępcy przewodniczącego Komisji, rozpatrzyła:
– informację na temat aktualnej sytuacji w Szpitalu Instytucie „Pomnik – Centrum Zdrowia Dziecka”."

# 3.
# Nowe linie(entery) R traktuje jako napis "\n", więc pozbędźmy się ich - strsplit z sapply
# Od razu da nam to podzielenie tekstu na akapity

akapity <- sapply(strsplit(stenogram,"\n"), as.character)

# 4.
# Każdy stenogram posiada akapit/y "W posiedzeniu udział [...]" , wybierzmy więc je - subset z grepl
# - || - akapit i jemu następny np. " Komisja Zdrowia [...], rozpatrzyła: \n – informację na temat aktualnej sytuacji w Szpitalu Instytucie „Pomnik – Centrum Zdrowia Dziecka” "

## 4.1.

udzial <- subset(akapity,grepl("W posiedzeniu udział",akapity))

### a)
# Podzielmy dane akapity na słowa - sapply z strsplit 

slowa <- sapply(strsplit(udzial," "), as.character)

### b)
# Stwórzmy wektor imion i przyłóżmy go, żeby wydobyć imiona.
# Jako że po imionach występuje nazwisko dodajmy również kolejny wyraz - Pętla przeszukująca słowa w akapitach


pomocniczy <- ""
imiona <- c("Maciej","Mateusz","Jan","Wanda")
for(i in 1:length(slowa)){
  for(j in 1:length(slowa[[i]])){
    if(slowa[[i]][j] %in% imiona){
      pomocniczy <- paste(pomocniczy," ",slowa[[i]][j],slowa[[i]][j+1])
    }
  }
}

obecni <- sapply(strsplit(pomocniczy,"   "), as.character)[-1]

## 4.2.
### a)

sprawa <- akapity[grep("rozpatrzyła:",akapity)+1]

# 5.
# Spójrzmy na dwa linki z różnych kadencji
# http://sejm.gov.pl/Sejm8.nsf/biuletyn.xsp?skrnr=ZDR-24
# http://www.sejm.gov.pl/sejm7.nsf/PosKomZrealizowane.xsp?komisja=ZDR
# Widać od razu różnicę, w /Sejm"nr".nsf/ , podzielmy więc link jako string, a potem zróbmy prosty warunek

url <- "http://sejm.gov.pl/Sejm8.nsf/biuletyn.xsp?skrnr=ZDR-24"
urlp <- sapply(strsplit(url,"/"), as.character)
sejm <- subset(urlp,grepl(".nsf",urlprzerobiony))

if(grepl("8",sejm)){
  nr_kadencji <- 8
}else{
  nr_kadnecji <- 7
}

# 6.
# Spójrzmy jeszcze raz na przykładowy link http://sejm.gov.pl/Sejm8.nsf/biuletyn.xsp?skrnr=ZDR-24
# Pod koniec po "-" widnieje numer posiedzenia komisji, podzielmy więc odpowiednio url, żeby to wydobyć

url <- "http://sejm.gov.pl/Sejm8.nsf/biuletyn.xsp?skrnr=ZDR-24"
urln <- sapply(strsplit(url,"-"), as.character)
nr_posiedzenia <- as.numeric(urln[length(urln)]) # Jako, że numer występuje na końcu
