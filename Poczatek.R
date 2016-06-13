# Początki

# 1. Wczytanie potrzebnych pakietów
# 2. Wczytanie stenogramu
# 3. Wyodrębnienie konkretnych akapitów

## 3.1. Akapity, które zawierają uczestników posiedzenia
### a) Podzielenie akapitów na słowa
### b) Wyodrębnienie z nich imion i nazwisk

## 3.2. Akapit, który zawiera powód zwołania komisji
### a) Wydobycie powodu zwołania obrad

# 4. Wydobycie numeru kadencji sejmu
# 5. Wydobycie numeru posiedzenia komisji
# 6. Wydobycie nazwy Komisji

# .
# .
# .

# Przedostatni punkt. Stworzenie ramki danych
# Ostatni punkt. Przetworzenie ramki danych na plik csv 

# Przykład:

# 1.
# Wczytajmy pakiety, które pomogą nam w późniejszych analizach

library(ggplot2)

# 2.
# Funkcja do wczytywania stenogramów

url <- "http://sejm.gov.pl/Sejm8.nsf/biuletyn.xsp?skrnr=ZDR-24"
stenogram_tresc <- function(x=url){
  test <<- readLines(x)
  Encoding(test) <<- "UTF-8"
  poczatek <- grep("Zapis przebiegu posiedzenia komisji",test)[length(grep("Zapis przebiegu posiedzenia komisji",test))]
  koniec <- grep("amykam posiedzenie Komisji.",test)
  test <- test[poczatek:koniec]
  test <- subset(test,grepl("<p>.*?</p>",test)) # Wydobądźmy wszystkie akapity 
  test <- gsub("<.*?>"," ",test) # Usuńmy kodowanie html
  test <<- gsub("  "," ",test) # Usuńmy podwójne spacje
}
stenogram_tresc()

# 3.
# Każdy stenogram posiada akapity "W posiedzeniu udział [...]" i "- sprawa zwołania posiedzenia", wydobądźmy je - subset z grepl

## 3.1.

udzial <- subset(test,grepl("W posiedzeniu udział",test))

### a)
# Podzielmy dane akapity na słowa - sapply z strsplit 

slowa <- sapply(strsplit(udzial," "), as.character)

### b)
# Każde imię i nazwisko jest pogrubione, jak również jest po nim afiliacja, podzielmy więc odpowiednio konkretne akapity

udzial <- subset(test,grepl("W posiedzeniu udział",test))
lista_osob <- function(){
pogrubione <- sapply(strsplit(udzial,"</font><font face=\"Arial\"><b>"),as.character) # Dzielimy pogrubienia od lewej
pogrubione <- unlist(pogrubione)  # Z powodu dwóch tekstów robimy wektor              
pogrubione <- pogrubione[-c(grep("W posiedzeniu udział",pogrubione))] # Usuwamy elementy wektora bez obserwacji
pogrubione <- sapply(strsplit(pogrubione,"</b></font><font face=\"Arial\">"),as.character) # Dzielimy pogrubienia od prawej strony
pogrubione <- unlist(pogrubione) # Wszystkie modyfikacje dają nam wektor, którego nieparzysty element to osoba, a parzysty jego afiliacja
osoby <<- pogrubione[seq(1,length(pogrubione),2)] 
afiliacje <<- pogrubione[seq(2,length(pogrubione),2)]
}
lista_osob()

## 3.2.
### a)
# Pierwsze akapity poprzedzone myślnikiem, pokazują nam sprawy zwołania komisji
# Akapit następny po powodach zorganizowania spotkania, to akapit mówiący kto brał udział w posiedzeniu.
# Weźmy więc te akapity, które są przed nim

nr_akapitow <- grep(" – ",test)
nr_akapitu <- grep("W posiedzeniu udział",test)[1]
sprawy <- nr_akapitow[nr_akapitow < nr_akapitu]

# 4.
# Spójrzmy na dwa linki z różnych kadencji
# http://sejm.gov.pl/Sejm8.nsf/biuletyn.xsp?skrnr=ZDR-24
# http://www.sejm.gov.pl/sejm7.nsf/PosKomZrealizowane.xsp?komisja=ZDR
# Widać od razu różnicę, w /Sejm"nr".nsf/ , podzielmy więc link jako string, a potem zróbmy prosty warunek

url <- "http://sejm.gov.pl/Sejm8.nsf/biuletyn.xsp?skrnr=ZDR-24"
urlp <- sapply(strsplit(url,"/"), as.character)
sejm <- subset(urlp,grepl(".nsf",urlp))

if(grepl("8",sejm)){
  nr_kadencji <- 8
}else{
  nr_kadnecji <- 7
}

# 5.
# Spójrzmy jeszcze raz na przykładowy link http://sejm.gov.pl/Sejm8.nsf/biuletyn.xsp?skrnr=ZDR-24
# Pod koniec po "-" widnieje numer posiedzenia komisji, podzielmy więc odpowiednio url, żeby to wydobyć

url <- "http://sejm.gov.pl/Sejm8.nsf/biuletyn.xsp?skrnr=ZDR-24"
urln <- sapply(strsplit(url,"-"), as.character)
nr_posiedzenia <- as.numeric(urln[length(urln)]) # Jako, że numer występuje na końcu

# 6.
# Patrząc na linki http://sejm.gov.pl/Sejm8.nsf/biuletyn.xsp?skrnr=ZDR-24 i http://sejm.gov.pl/Sejm8.nsf/biuletyn.xsp?skrnr=PET-22
# widzimy, że każda komisja ma swój własny skrót, podzielmy odpowiednio link - sapply z strsplit

url <- "http://sejm.gov.pl/Sejm8.nsf/biuletyn.xsp?skrnr=ZDR-24"
urln <- sapply(strsplit(url,".*="), as.character)[-1] # Podzielmy string tak, żeby podzielnikiem było to co stoi przed skrótem
komisja <- sapply(strsplit(urln,"-.*"), as.character)[-2] # - || - to co stoi po skrócie

## Notka ##
# Punkty 5. i 6. da się zrobić jednocześnie
url <- "http://sejm.gov.pl/Sejm8.nsf/biuletyn.xsp?skrnr=ZDR-24"
urln <- sapply(strsplit(url,".*="), as.character)[-1]
urlnn <- sapply(strsplit(urln,"-"), as.character)
komisja <- urlnn[1]
nr_posiedzenia <- urlnn[2]

# ====================================================Podsumowanie========================================================== #

# Dane do wczytania
url <- "http://sejm.gov.pl/Sejm8.nsf/biuletyn.xsp?skrnr=ZDR-24"
# Wydobycie interesującej treści
stenogram_tresc <- function(x=url){
  test <<- readLines(x)
  Encoding(test) <<- "UTF-8"
  poczatek <- grep("Zapis przebiegu posiedzenia komisji",test)[length(grep("Zapis przebiegu posiedzenia komisji",test))]
  koniec <- grep("amykam posiedzenie Komisji.",test)
  test <- test[poczatek:koniec]
  test <- subset(test,grepl("<p>.*?</p>",test)) # Wydobądźmy wszystkie akapity 
  test <- gsub("<.*?>"," ",test) # Usuńmy kodowanie html
  test <<- gsub("  "," ",test) # Usuńmy podwójne spacje
}
# Wydobycie listy osób i ich przynależności
lista_osob <- function(){
pogrubione <- sapply(strsplit(udzial,"</font><font face=\"Arial\"><b>"),as.character) # Dzielimy pogrubienia od lewej
pogrubione <- unlist(pogrubione)  # Z powodu możliwości dwóch lub więcej tekstów robimy wektor              
pogrubione <- pogrubione[-c(grep("W posiedzeniu udział",pogrubione))] # Usuwamy elementy wektora bez obserwacji
pogrubione <- sapply(strsplit(pogrubione,"</b></font><font face=\"Arial\">"),as.character) # Dzielimy pogrubienia od prawej strony
pogrubione <- unlist(pogrubione) # Wszystkie modyfikacje dają nam wektor, którego nieparzysty element to osoba, a parzysty jego afiliacja
osoby <<- pogrubione[seq(1,length(pogrubione),2)] 
afiliacje <<- pogrubione[seq(2,length(pogrubione),2)]
}
# Wydobycie wszystkich informacji
informacje <- function(x=url){
  urlp <- sapply(strsplit(x,"/"), as.character)
  sejm <- subset(urlp,grepl(".nsf",urlp))
  if(grepl("8",sejm)){
    nr_kadencji <<- 8
  }else{
    nr_kadnecji <<- 7
  }
  urln <- sapply(strsplit(x,".*="), as.character)[-1]
  urlnn <- sapply(strsplit(urln,"-"), as.character) # Dzieląc tak tekst dostajemy dwu elementowy wektor, pierwszym stringiem jest skrót komisji, drugim jest jej numer
  komisja <<- urlnn[1]
  nr_posiedzenia <<- urlnn[2]
  stenogram_tresc(x)
  udzial <- subset(test,grepl("W posiedzeniu udział",test))
  slowa <- sapply(strsplit(udzial," "), as.character)
  lista_osob()
  nr_akapitow <- grep(" – ",test)
  nr_akapitu <- grep("W posiedzeniu udział",test)[1]
  sprawy <<- nr_akapitow[nr_akapitow < nr_akapitu]
  informacjew <<- c(nr_kadencji,komisja,nr_posiedzenia,sprawy,osoby,afiliacje,)
}
informacje()

# Uwaga - Afiliacje do poprawy

# ====================================================Podsumowanie========================================================== #
