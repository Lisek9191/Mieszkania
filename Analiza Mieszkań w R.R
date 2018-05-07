# Wczytanie bibliotek
library(RMySQL)
library(dbConect)
library(ggplot2)
library(dplyr)
library(gridExtra)


# Łączenie z bazą
con=dbConnect(MySQL(), user='root', password='1234', dbname="moje_m", host="localhost")
dbListTables(con)
MyQuery="select * from mieszkania;"
WyQuery="select * from wynajem"
Mieszkania<-dbGetQuery(con, MyQuery)
Wynajem<-dbGetQuery(con, WyQuery)


# Mogłem to zrobić za pomocą colnames(Mieszkania)<-c("xxx","yyy",...) ale moim zdaniem metoda poniżej jest bardziej przejrzysta
colnames(Mieszkania)[1]<-"Id"
colnames(Mieszkania)[2]<-"Dzielnica"
colnames(Mieszkania)[3]<-"Cena"
colnames(Mieszkania)[4]<-"Powierzchnia"
colnames(Mieszkania)[5]<-"Piętro"
colnames(Mieszkania)[6]<-"Pokoje"
colnames(Mieszkania)[7]<-"Rynek"
colnames(Mieszkania)[8]<-"Rodzaj_zabudowy"
colnames(Mieszkania)[9]<-"Materiał_budynku"
colnames(Mieszkania)[10]<-"Okna"
colnames(Mieszkania)[11]<-"Ogrzewanie"
colnames(Mieszkania)[12]<-"Rok_Budowy"
colnames(Mieszkania)[13]<-"Stan_Wykończenia"
colnames(Mieszkania)[14]<-"Czynsz"
colnames(Mieszkania)[15]<-"Forma_Własności"
colnames(Mieszkania)[16]<-"Miejsce_Parkingowe"
colnames(Mieszkania)[17]<-"Balkon"
colnames(Mieszkania)[18]<-"Piwnica"

colnames(Wynajem)[1]<-"Dzielnica"
colnames(Wynajem)[2]<-"Cena"
colnames(Wynajem)[3]<-"Powierzchnia"
colnames(Wynajem)[4]<-"Piętro"
colnames(Wynajem)[5]<-"Pokoje"
colnames(Wynajem)[6]<-"Czynsz"
colnames(Wynajem)[7]<-"Kaucja"
colnames(Wynajem)[8]<-"Rodzaj_zabudowy"
colnames(Wynajem)[9]<-"Ogrzewanie"

# Odkodowanie znaków UTF8
Encoding(Mieszkania$Dzielnica)<-"UTF-8"
Encoding(Mieszkania$Rynek)<-"UTF-8"
Encoding(Mieszkania$Rodzaj_zabudowy)<-"UTF-8"
Encoding(Mieszkania$Materiał_budynku)<-"UTF-8"
Encoding(Mieszkania$Okna)<-"UTF-8"
Encoding(Mieszkania$Ogrzewanie)<-"UTF-8"
Encoding(Mieszkania$Stan_Wykończenia)<-"UTF-8"
Encoding(Mieszkania$Forma_Własności)<-"UTF-8"
Encoding(Mieszkania$Pokoje)<-"UTF-8"

Encoding(Wynajem$Dzielnica)<-"UTF-8"
Encoding(Wynajem$Cena)<-"UTF-8"
Encoding(Wynajem$Powierzchnia)<-"UTF-8"
Encoding(Wynajem$Piętro)<-"UTF-8"
Encoding(Wynajem$Pokoje)<-"UTF-8"
Encoding(Wynajem$Czynsz)<-"UTF-8"
Encoding(Wynajem$Kaucja)<-"UTF-8"
Encoding(Wynajem$Rodzaj_zabudowy)<-"UTF-8"
Encoding(Wynajem$Ogrzewanie)<-"UTF-8"

################################################################## Uporzadkowanie danych cen nieruchomości
ggplot(Mieszkania, aes(x=Cena, y=Powierzchnia))+geom_point()
Mlodz<-Mieszkania
Mlodz$Dzielnica<-reorder(Mlodz$Dzielnica, Mlodz$Cena, median)
# Podział danych na mniejsze partie

MSameOsiedla<-Mlodz%>%filter(Dzielnica=="Bałuty" | Dzielnica=="Widzew" | Dzielnica=="Polesie" | Dzielnica=="Górna" | Dzielnica=="Śródmieście")

Bałuty<-MSameOsiedla %>% 
    filter(Dzielnica == "Bałuty")
Widzew<-MSameOsiedla %>% 
    filter(Dzielnica == "Widzew")
Śródmieście<-MSameOsiedla %>% 
    filter(Dzielnica == "Śródmieście")
Górna<-MSameOsiedla %>% 
    filter(Dzielnica == "Górna")
Polesie<-MSameOsiedla %>% 
    filter(Dzielnica == "Polesie")



####### Histogram Cen
ggplot(Mlodz, aes(x=Cena, fill=Dzielnica))+geom_histogram(color="white")+geom_vline(xintercept = median(Mlodz$Cena)) +
geom_vline(xintercept = mean(Mlodz$Cena), color="red")+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))

# Histogram Cen Bałut
ggplot(Bałuty, aes(x=Cena))+geom_histogram(color="black", fill="yellow")+geom_vline(xintercept=median(Bałuty$Cena))+
geom_vline(xintercept=mean(Bałuty$Cena), color="red") + scale_x_continuous(breaks=c(seq(0,1300000,100000)),labels=function(x) format(x, scientific=FALSE))

# Histogram Cen Widzewa
ggplot(Widzew, aes(x=Cena))+geom_histogram(color="black", fill="yellow")+geom_vline(xintercept=median(Widzew$Cena))+geom_vline(xintercept=mean(Widzew$Cena), color="red")+
scale_x_continuous(breaks=c(seq(0,1300000,100000)), labels=function(x) format(x, scientific=FALSE))

# Histogram Cen Polesia
ggplot(Polesie, aes(x=Cena))+geom_histogram(color="black", fill="yellow")+geom_vline(xintercept=median(Polesie$Cena))+geom_vline(xintercept=mean(Polesie$Cena), color="red")+
scale_x_continuous(breaks=c(seq(0,1300000,100000)), labels=function(x) format(x, scientific=FALSE))

# Histogram Śródmieścia
ggplot(Śródmieście, aes(x=Cena))+geom_histogram(color="black", fill="yellow")+geom_vline(xintercept=median(Śródmieście$Cena))+geom_vline(xintercept=mean(Śródmieście$Cena), color="red")+
scale_x_continuous(breaks=c(seq(0, 1300000, 100000)), labels=function(x) format(x, scientific=FALSE))

# Histogram Górna
ggplot(Górna, aes(x=Cena))+geom_histogram(color="black", fill="yellow")+geom_vline(xintercept=median(Górna$Cena))+geom_vline(xintercept=mean(Górna$Cena), color="red")+
scale_x_continuous(breaks=c(seq(0,1300000,100000)), labels=function(x) format(x, scientific=FALSE))


####### Histogram powierzchni
ggplot(Mlodz, aes(x=Powierzchnia))+geom_histogram(color="black", fill="green", binwidth=5)+geom_vline(xintercept=median(Mlodz$Powierzchnia))+geom_vline(xintercept=mean(Mlodz$Powierzchnia), color="red")+
scale_x_continuous(breaks=c(seq(0,250, 10)), labels=function(x) format(x, scientific=FALSE))

ggplot(Bałuty, aes(x=Powierzchnia))+geom_histogram(color="black", fill="green", binwidth=5)+geom_vline(xintercept=median(Bałuty$Powierzchnia))+geom_vline(xintercept=mean(Bałuty$Powierzchnia), color="red")+
scale_x_continuous(breaks=c(seq(0,250, 10)), labels=function(x) format(x, scientific=FALSE))

ggplot(Widzew, aes(x=Powierzchnia))+geom_histogram(color="black", fill="green", binwidth=5)+geom_vline(xintercept=median(Widzew$Powierzchnia))+geom_vline(xintercept=mean(Widzew$Powierzchnia), color="red")+
scale_x_continuous(breaks=c(seq(0, 250, 10)), labels=function(x) format(x, scientific=FALSE))

ggplot(Polesie, aes(x=Powierzchnia))+geom_histogram(color="black", fill="green", binwidth=5)+geom_vline(xintercept=median(Polesie$Powierzchnia))+geom_vline(xintercept=mean(Polesie$Powierzchnia), color="red")+
scale_x_continuous(breaks=c(seq(0, 250, 10)), labels=function(x) format(x, scientific=FALSE))

ggplot(Śródmieście, aes(x=Powierzchnia))+geom_histogram(color="black", fill="green", binwidth=5)+geom_vline(xintercept=median(Śródmieście$Powierzchnia))+geom_vline(xintercept=mean(Śródmieście$Powierzchnia), color="red")+
scale_x_continuous(breaks=c(seq(0,250,10)), labels=function(x) format(x, scientific=FALSE))

ggplot(Górna, aes(x=Powierzchnia))+geom_histogram(color="black", fill="green", binwidth=5)+geom_vline(xintercept=median(Górna$Powierzchnia))+geom_vline(xintercept=mean(Górna$Powierzchnia), color="red")+
scale_x_continuous(breaks=c(seq(0, 250, 10)), labels=function(x) format(x, scientific=FALSE))



####### Histogram cena/metr^2
# Nowa zmienna cena za metr kwadratowy
Mlodz<-Mlodz%>%mutate(cenazametr=round(Cena/Powierzchnia,0))
Bałuty<-Bałuty%>%mutate(cenazametr=round(Cena/Powierzchnia,0))
Widzew<-Widzew%>%mutate(cenazametr=round(Cena/Powierzchnia,0))
Polesie<-Polesie%>%mutate(cenazametr=round(Cena/Powierzchnia,0))
Śródmieście<-Śródmieście%>%mutate(cenazametr=round(Cena/Powierzchnia,0))
Górna<-Górna%>%mutate(cenazametr=round(Cena/Powierzchnia,0))

ggplot(Mlodz, aes(x=cenazametr))+geom_histogram(color="black", fill="green", binwidth=100)+geom_vline(xintercept=median(Mlodz$cenazametr))+geom_vline(xintercept=mean(Mlodz$cenazametr), color="red")+
scale_x_continuous(breaks=c(seq(0,250, 10)), labels=function(x) format(x, scientific=FALSE))

ggplot(Bałuty, aes(x=cenazametr))+geom_histogram(color="black", fill="green", binwidth=100)+geom_vline(xintercept=median(Bałuty$cenazametr))+geom_vline(xintercept=mean(Bałuty$cenazametr), color="red")+
scale_x_continuous(breaks=c(seq(0,250, 10)), labels=function(x) format(x, scientific=FALSE))

ggplot(Widzew, aes(x=cenazametr))+geom_histogram(color="black", fill="green", binwidth=100)+geom_vline(xintercept=median(Widzew$cenazametr))+geom_vline(xintercept=mean(Widzew$cenazametr), color="red")+
scale_x_continuous(breaks=c(seq(0, 250, 10)), labels=function(x) format(x, scientific=FALSE))

ggplot(Polesie, aes(x=cenazametr))+geom_histogram(color="black", fill="green", binwidth=100)+geom_vline(xintercept=median(Polesie$cenazametr))+geom_vline(xintercept=mean(Polesie$cenazametr), color="red")+
scale_x_continuous(breaks=c(seq(0, 250, 10)), labels=function(x) format(x, scientific=FALSE))

ggplot(Śródmieście, aes(x=cenazametr))+geom_histogram(color="black", fill="green", binwidth=100)+geom_vline(xintercept=median(Śródmieście$cenazametr))+geom_vline(xintercept=mean(Śródmieście$cenazametr), color="red")+
scale_x_continuous(breaks=c(seq(0,250,10)), labels=function(x) format(x, scientific=FALSE))

ggplot(Górna, aes(x=cenazametr))+geom_histogram(color="black", fill="green", binwidth=100)+geom_vline(xintercept=median(Górna$cenazametr))+geom_vline(xintercept=mean(Górna$cenazametr), color="red")+
scale_x_continuous(breaks=c(seq(0, 250, 10)), labels=function(x) format(x, scientific=FALSE))


####### Wykres barowy  rodzaji budynku

ggplot(Mlodz, aes(x=Rodzaj_zabudowy, fill=Dzielnica))+geom_bar(position="fill")
ggplot(Mlodz, aes(x=Rodzaj_zabudowy, fill=Dzielnica))+geom_bar()

ggplot(Bałuty, aes(x=Rodzaj_zabudowy, fill=Dzielnica))+geom_bar()

ggplot(Widzew, aes(x=Rodzaj_zabudowy, fill=Dzielnica))+geom_bar()

ggplot(Polesie, aes(x=Rodzaj_zabudowy, fill=Dzielnica))+geom_bar()

ggplot(Śródmieście, aes(x=Rodzaj_zabudowy, fill=Dzielnica))+geom_bar()

ggplot(Górna, aes(x=Rodzaj_zabudowy, fill=Dzielnica))+geom_bar()


####### Wykres barowy pokoi

ggplot(Mlodz, aes(x=Pokoje, fill=Dzielnica))+geom_bar(position="fill")
ggplot(Mlodz, aes(x=Pokoje, fill=Dzielnica))+geom_bar()

ggplot(Bałuty, aes(x=Pokoje, fill=Dzielnica))+geom_bar()

ggplot(Widzew, aes(x=Pokoje, fill=Dzielnica))+geom_bar()

ggplot(Polesie, aes(x=Pokoje, fill=Dzielnica))+geom_bar()

ggplot(Śródmieście, aes(x=Pokoje, fill=Dzielnica))+geom_bar()

ggplot(Górna, aes(x=Pokoje, fill=Dzielnica))+geom_bar()


###### Wykres barowy Pieter
ggplot(Mlodz, aes(x=Piętro, fill=Dzielnica))+geom_bar(position="fill")
ggplot(Mlodz, aes(x=Piętro, fill=Dzielnica))+geom_bar()

ggplot(Bałuty, aes(x=Piętro, fill=Dzielnica))+geom_bar()

ggplot(Widzew, aes(x=Piętro, fill=Dzielnica))+geom_bar()

ggplot(Polesie, aes(x=Piętro, fill=Dzielnica))+geom_bar()

ggplot(Śródmieście, aes(x=Piętro, fill=Dzielnica))+geom_bar()

ggplot(Górna, aes(x=Piętro, fill=Dzielnica))+geom_bar()

###### Wykres barowy rodzaji ogrzewań
ggplot(Mlodz, aes(x=Ogrzewanie, fill=Dzielnica))+geom_bar(position="fill")
ggplot(Mlodz, aes(x=Ogrzewanie, fill=Dzielnica))+geom_bar()

ggplot(Bałuty, aes(x=Ogrzewanie, fill=Dzielnica))+geom_bar()

ggplot(Widzew, aes(x=Ogrzewanie, fill=Dzielnica))+geom_bar()

ggplot(Polesie, aes(x=Ogrzewanie, fill=Dzielnica))+geom_bar()

ggplot(Śródmieście, aes(x=Ogrzewanie, fill=Dzielnica))+geom_bar()

ggplot(Górna, aes(x=Ogrzewanie, fill=Dzielnica))+geom_bar()

##### Rynek

ggplot(Mlodz, aes(x=Rynek, fill=Dzielnica))+geom_bar(position="fill")
ggplot(Mlodz, aes(x=Rynek, fill=Dzielnica))+geom_bar()

ggplot(Bałuty, aes(x=Rynek, fill=Dzielnica))+geom_bar()

ggplot(Widzew, aes(x=Rynek, fill=Dzielnica))+geom_bar()

ggplot(Polesie, aes(x=Rynek, fill=Dzielnica))+geom_bar()

ggplot(Śródmieście, aes(x=Rynek, fill=Dzielnica))+geom_bar()

ggplot(Górna, aes(x=Rynek, fill=Dzielnica))+geom_bar()


## Ostateczne czyszczenie tabeli:
# Usuwamy mieszkania z liczbą pokoi większa niż 5.
# Usuwamy domy wolnostojące, lofty, szeregowce, NA, plomby(choć wiekszość pewnie moglibyśmy zakwalifikować jako kamienice, nie chcę przypadkiem ubrudzić danych).
# Mieszkania z metrażem większym niż 100metrów.
# Usuwamy oferty z ceną powyżej 1mln zł i poniżej 50tys zł.
# Od 2018 r. wszystkie nowo budowane mieszkania muszą mieć minimum 25 m² powierzchni użytkowej – tak wynika z nowelizacji rozporządzenia w sprawie warunków technicznych, jakim powinny odpowiadać budynki i ich usytuowanie.

MSameOsiedla<-MSameOsiedla %>% filter(Pokoje<5, Rodzaj_zabudowy=='apartamentowiec'| Rodzaj_zabudowy=='blok' | Rodzaj_zabudowy=='kamienica', Powierzchnia<101, Powierzchnia>24.99, Cena>49999, Cena<1000001)
MSameOsiedla<-MSameOsiedla%>%mutate(cenazametr=round(Cena/Powierzchnia,0))
Bałuty<-MSameOsiedla %>% 
    filter(Dzielnica == "Bałuty")
Widzew<-MSameOsiedla %>% 
    filter(Dzielnica == "Widzew")
Śródmieście<-MSameOsiedla %>% 
    filter(Dzielnica == "Śródmieście")
Górna<-MSameOsiedla %>% 
    filter(Dzielnica == "Górna")
Polesie<-MSameOsiedla %>% 
    filter(Dzielnica == "Polesie")

####### Wykres punktowy pietro do ceny za metr kwadtarowy


## Sprawdzmy o jakim metrazu sa najtańsze mieszkania jesli wezmiemy pod uwage cene za metr



MSameOsiedla %>% 
   ggplot() +
   geom_point(aes(Powierzchnia, cenazametr), alpha=0.1) +
   geom_smooth(aes(Powierzchnia, cenazametr)) +
   labs(x="Powierzchnia", y="Cena za m2")


MSameOsiedla %>% 
   ggplot() +
   geom_point(aes(Powierzchnia, cenazametr), alpha=0.1) +
   geom_smooth(aes(Powierzchnia, cenazametr, col=Dzielnica)) +
   coord_cartesian(xlim=c(25, 80), ylim=c(4100, 6100)) +
   labs(x="Powierzchnia", y="Cena za m2")+
   scale_x_continuous(breaks=c(seq(25,80,2.5)))+
   scale_y_continuous(breaks=c(seq(4100,6100,100)))+
   theme(text = element_text(size=10))


MSameOsiedla %>% 
   ggplot() +
   geom_point(aes(Powierzchnia, cenazametr), alpha=0.1) +
   geom_smooth(aes(Powierzchnia, cenazametr, col=Rodzaj_zabudowy)) +
   coord_cartesian(xlim=c(25, 80), ylim=c(3100, 6100)) +
   labs(x="Powierzchnia", y="Cena za m2")+
   scale_x_continuous(breaks=c(seq(25,80,2.5)))+
   scale_y_continuous(breaks=c(seq(3100,6100,200)))+
   theme(text = element_text(size=10))

MSameOsiedla %>% 
   ggplot() +
   geom_point(aes(Powierzchnia, cenazametr), alpha=0.1) +
   geom_smooth(aes(Powierzchnia, cenazametr, col=Ogrzewanie)) +
   coord_cartesian(xlim=c(25, 100), ylim=c(2000, 7100)) +
   labs(x="Powierzchnia", y="Cena za m2")+
   scale_x_continuous(breaks=c(seq(25,100,2.5)))+
   scale_y_continuous(breaks=c(seq(2000,6100,200)))+
   theme(text = element_text(size=10))

MSameOsiedla %>% 
   ggplot() +
   geom_point(aes(Powierzchnia, cenazametr), alpha=0.1) +
   geom_smooth(aes(Powierzchnia, cenazametr, col=Piętro)) +
   coord_cartesian(xlim=c(25, 100), ylim=c(2000, 7100)) +
   labs(x="Powierzchnia", y="Cena za m2")+
   scale_x_continuous(breaks=c(seq(25,100,2.5)))+
   scale_y_continuous(breaks=c(seq(2000,6100,200)))+
   theme(text = element_text(size=10))


MSameOsiedla %>% 
   ggplot() +
   geom_point(aes(Powierzchnia, cenazametr), alpha=0.1) +
   geom_smooth(aes(Powierzchnia, cenazametr, col=Rynek)) +
   coord_cartesian(xlim=c(25, 100), ylim=c(4200, 6000)) +
   labs(x="Powierzchnia", y="Cena za m2")+
   scale_x_continuous(breaks=c(seq(25,100,2.5)))+
   scale_y_continuous(breaks=c(seq(2000,6100,200)))+
   theme(text = element_text(size=10))


################################################################## Uporzadkowanie danych cen nieruchomości
Wlodz<-Wynajem
Wlodz$Dzielnica<-reorder(Wlodz$Dzielnica, Wlodz$Cena, median)

WSameOsiedla<-Wynajem%>%filter(Dzielnica=="Bałuty" | Dzielnica=="Widzew" | Dzielnica=="Polesie" | Dzielnica=="Górna" | Dzielnica=="Śródmieście")

WBałuty<-WSameOsiedla %>% 
    filter(Dzielnica == "Bałuty")
WWidzew<-WSameOsiedla %>% 
    filter(Dzielnica == "Widzew")
WŚródmieście<-WSameOsiedla %>% 
    filter(Dzielnica == "Śródmieście")
WGórna<-WSameOsiedla %>% 
    filter(Dzielnica == "Górna")
WPolesie<-WSameOsiedla %>% 
    filter(Dzielnica == "Polesie")

####### Histogram Cen

ggplot(Wlodz, aes(x=Cena, fill=Dzielnica))+geom_histogram(color="white")+geom_vline(xintercept = median(Wlodz$Cena)) +
geom_vline(xintercept = mean(Wlodz$Cena), color="red")+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))+labs(x="Cena najmu Łodzi", y="ilość mieszkań do najmu Łodzi")

# Histogram Cen Bałut
ggplot(WBałuty, aes(x=Cena))+geom_histogram(color="black", fill="yellow")+geom_vline(xintercept=median(WBałuty$Cena))+
geom_vline(xintercept=mean(WBałuty$Cena), color="red") + scale_x_continuous(breaks=c(seq(0,10000,500)),labels=function(x) format(x, scientific=FALSE))+labs(x="Cena najmu na Bałutach", y="ilość mieszkań do najmu na Bałutach")

# Histogram Cen Widzewa
ggplot(WWidzew, aes(x=Cena))+geom_histogram(color="black", fill="yellow")+geom_vline(xintercept=median(WWidzew$Cena))+geom_vline(xintercept=mean(WWidzew$Cena), color="red")+
scale_x_continuous(breaks=c(seq(0,10000,500)), labels=function(x) format(x, scientific=FALSE))+labs(x="Cena najmu na Widzewie", y="ilość mieszkań do najmu na Widzewie")

# Histogram Cen Polesia
ggplot(WPolesie, aes(x=Cena))+geom_histogram(color="black", fill="yellow")+geom_vline(xintercept=median(WPolesie$Cena))+geom_vline(xintercept=mean(WPolesie$Cena), color="red")+
scale_x_continuous(breaks=c(seq(0,10000,500)), labels=function(x) format(x, scientific=FALSE))+labs(x="Cena najmu na Polesiu", y="ilość mieszkań do najmu na Polesiu")

# Histogram Śródmieścia
ggplot(WŚródmieście, aes(x=Cena))+geom_histogram(color="black", fill="yellow")+geom_vline(xintercept=median(WŚródmieście$Cena))+geom_vline(xintercept=mean(WŚródmieście$Cena), color="red")+
scale_x_continuous(breaks=c(seq(0,10000,500)), labels=function(x) format(x, scientific=FALSE))+labs(x="Cena najmu na Śródmieściu", y="ilość mieszkań do najmu na Śródmieściu")

# Histogram Górna
ggplot(WGórna, aes(x=Cena))+geom_histogram(color="black", fill="yellow")+geom_vline(xintercept=median(WGórna$Cena))+geom_vline(xintercept=mean(WGórna$Cena), color="red")+
scale_x_continuous(breaks=c(seq(0,10000,500)), labels=function(x) format(x, scientific=FALSE))+labs(x="Cena najmu na Górnej", y="ilość mieszkań do najmu na Górnej")

####### Histogram powierzchni
ggplot(Wlodz, aes(x=Powierzchnia))+geom_histogram(color="black", fill="green", binwidth=5)+geom_vline(xintercept=median(Wlodz$Powierzchnia))+geom_vline(xintercept=mean(Wlodz$Powierzchnia), color="red")+
scale_x_continuous(breaks=c(seq(0,250, 10)), labels=function(x) format(x, scientific=FALSE))+labs(x="Powierzchnia najmu Łodzi", y="ilość mieszkań do najmu Łodzi")

ggplot(WBałuty, aes(x=Powierzchnia))+geom_histogram(color="black", fill="green", binwidth=5)+geom_vline(xintercept=median(WBałuty$Powierzchnia))+geom_vline(xintercept=mean(WBałuty$Powierzchnia), color="red")+
scale_x_continuous(breaks=c(seq(0,250, 10)), labels=function(x) format(x, scientific=FALSE))+labs(x="Powierzchnia najmu na Bałutach", y="ilość mieszkań do najmu na Bałutach")

ggplot(WWidzew, aes(x=Powierzchnia))+geom_histogram(color="black", fill="green", binwidth=5)+geom_vline(xintercept=median(WWidzew$Powierzchnia))+geom_vline(xintercept=mean(WWidzew$Powierzchnia), color="red")+
scale_x_continuous(breaks=c(seq(0, 250, 10)), labels=function(x) format(x, scientific=FALSE))+labs(x="Powierzchnia najmu na Widzewie", y="ilość mieszkań do najmu na Widzewie")

ggplot(WPolesie, aes(x=Powierzchnia))+geom_histogram(color="black", fill="green", binwidth=5)+geom_vline(xintercept=median(WPolesie$Powierzchnia))+geom_vline(xintercept=mean(WPolesie$Powierzchnia), color="red")+
scale_x_continuous(breaks=c(seq(0, 250, 10)), labels=function(x) format(x, scientific=FALSE))+labs(x="Powierzchnia najmu na Polesiu", y="ilość mieszkań do najmu na Polesiu")

ggplot(WŚródmieście, aes(x=Powierzchnia))+geom_histogram(color="black", fill="green", binwidth=5)+geom_vline(xintercept=median(WŚródmieście$Powierzchnia))+geom_vline(xintercept=mean(WŚródmieście$Powierzchnia), color="red")+
scale_x_continuous(breaks=c(seq(0,250,10)), labels=function(x) format(x, scientific=FALSE))+labs(x="Powierzchnia najmu na Śródmieściu", y="ilość mieszkań do najmu na Śródmieściu")

ggplot(WGórna, aes(x=Powierzchnia))+geom_histogram(color="black", fill="green", binwidth=5)+geom_vline(xintercept=median(WGórna$Powierzchnia))+geom_vline(xintercept=mean(WGórna$Powierzchnia), color="red")+
scale_x_continuous(breaks=c(seq(0, 250, 10)), labels=function(x) format(x, scientific=FALSE))+labs(x="Powierzchnia najmu na Górnej", y="ilość mieszkań do najmu na Górnej")

####### Histogram cena/metr^2
# Nowa zmienna cena za metr kwadratowy
Wlodz<-Wlodz%>%mutate(cenazametr=round(Cena/Powierzchnia,0))
WBałuty<-WBałuty%>%mutate(cenazametr=round(Cena/Powierzchnia,0))
WWidzew<-WWidzew%>%mutate(cenazametr=round(Cena/Powierzchnia,0))
WPolesie<-WPolesie%>%mutate(cenazametr=round(Cena/Powierzchnia,0))
WŚródmieście<-WŚródmieście%>%mutate(cenazametr=round(Cena/Powierzchnia,0))
WGórna<-WGórna%>%mutate(cenazametr=round(Cena/Powierzchnia,0))

ggplot(Wlodz, aes(x=cenazametr))+geom_histogram(color="black", fill="green")+geom_vline(xintercept=median(Wlodz$cenazametr))+geom_vline(xintercept=mean(Wlodz$cenazametr), color="red")+
scale_x_continuous(breaks=c(seq(0,250, 5)), labels=function(x) format(x, scientific=FALSE))+labs(x="Cena za metr najmu Łodzi", y="ilość mieszkań do najmu Łodzi")

ggplot(WBałuty, aes(x=cenazametr))+geom_histogram(color="black", fill="green")+geom_vline(xintercept=median(WBałuty$cenazametr))+geom_vline(xintercept=mean(WBałuty$cenazametr), color="red")+
scale_x_continuous(breaks=c(seq(0,250, 5)), labels=function(x) format(x, scientific=FALSE))+labs(x="Cena za metr najmu na Bałutach", y="ilość mieszkań do najmu na Bałutach")

ggplot(WWidzew, aes(x=cenazametr))+geom_histogram(color="black", fill="green")+geom_vline(xintercept=median(WWidzew$cenazametr))+geom_vline(xintercept=mean(WWidzew$cenazametr), color="red")+
scale_x_continuous(breaks=c(seq(0,250, 5)), labels=function(x) format(x, scientific=FALSE))+labs(x="Cena za metr najmu na Widzewie", y="ilość mieszkań do najmu na Widzewie")

ggplot(WPolesie, aes(x=cenazametr))+geom_histogram(color="black", fill="green")+geom_vline(xintercept=median(WPolesie$cenazametr))+geom_vline(xintercept=mean(WPolesie$cenazametr), color="red")+
scale_x_continuous(breaks=c(seq(0,250, 5)), labels=function(x) format(x, scientific=FALSE))+labs(x="Cena za metr najmu na Polesiu", y="ilość mieszkań do najmu na Polesiu")

ggplot(WŚródmieście, aes(x=cenazametr))+geom_histogram(color="black", fill="green")+geom_vline(xintercept=median(WŚródmieście$cenazametr))+geom_vline(xintercept=mean(WŚródmieście$cenazametr), color="red")+
scale_x_continuous(breaks=c(seq(0,250, 5)), labels=function(x) format(x, scientific=FALSE))

ggplot(WGórna, aes(x=cenazametr))+geom_histogram(color="black", fill="green")+geom_vline(xintercept=median(WGórna$cenazametr))+geom_vline(xintercept=mean(WGórna$cenazametr), color="red")+
scale_x_continuous(breaks=c(seq(0,250, 5)), labels=function(x) format(x, scientific=FALSE))+labs(x="Cena za metr najmu na Górnej", y="ilość mieszkań do najmu na Górnej")



####### Wykres barowy  rodzaji budynku

ggplot(Mlodz, aes(x=Rodzaj_zabudowy, fill=Dzielnica))+geom_bar(position="fill")
ggplot(Mlodz, aes(x=Rodzaj_zabudowy, fill=Dzielnica))+geom_bar()

ggplot(Bałuty, aes(x=Rodzaj_zabudowy, fill=Dzielnica))+geom_bar()

ggplot(Widzew, aes(x=Rodzaj_zabudowy, fill=Dzielnica))+geom_bar()

ggplot(Polesie, aes(x=Rodzaj_zabudowy, fill=Dzielnica))+geom_bar()

ggplot(Śródmieście, aes(x=Rodzaj_zabudowy, fill=Dzielnica))+geom_bar()

ggplot(Górna, aes(x=Rodzaj_zabudowy, fill=Dzielnica))+geom_bar()



###### Wykres barowy Pieter
ggplot(Wlodz, aes(x=Piętro, fill=Dzielnica))+geom_bar(position="fill")
ggplot(Wlodz, aes(x=Piętro, fill=Dzielnica))+geom_bar()

ggplot(WBałuty, aes(x=Piętro, fill=Dzielnica))+geom_bar()

ggplot(WWidzew, aes(x=Piętro, fill=Dzielnica))+geom_bar()

ggplot(WPolesie, aes(x=Piętro, fill=Dzielnica))+geom_bar()

ggplot(WŚródmieście, aes(x=Piętro, fill=Dzielnica))+geom_bar()

ggplot(WGórna, aes(x=Piętro, fill=Dzielnica))+geom_bar()

###### Wykres barowy Pieter
ggplot(Wlodz, aes(x=Piętro, fill=Dzielnica))+geom_bar(position="fill")
ggplot(Wlodz, aes(x=Piętro, fill=Dzielnica))+geom_bar()

ggplot(WBałuty, aes(x=Piętro, fill=Dzielnica))+geom_bar()

ggplot(WWidzew, aes(x=Piętro, fill=Dzielnica))+geom_bar()

ggplot(WPolesie, aes(x=Piętro, fill=Dzielnica))+geom_bar()

ggplot(WŚródmieście, aes(x=Piętro, fill=Dzielnica))+geom_bar()

ggplot(WGórna, aes(x=Piętro, fill=Dzielnica))+geom_bar()

###### Wykres barowy rodzaji ogrzewań
ggplot(Wlodz, aes(x=Ogrzewanie, fill=Dzielnica))+geom_bar(position="fill")
ggplot(Wlodz, aes(x=Ogrzewanie, fill=Dzielnica))+geom_bar()

ggplot(WBałuty, aes(x=Ogrzewanie, fill=Dzielnica))+geom_bar()

ggplot(WWidzew, aes(x=Ogrzewanie, fill=Dzielnica))+geom_bar()

ggplot(WPolesie, aes(x=Ogrzewanie, fill=Dzielnica))+geom_bar()

ggplot(WŚródmieście, aes(x=Ogrzewanie, fill=Dzielnica))+geom_bar()

ggplot(WGórna, aes(x=Ogrzewanie, fill=Dzielnica))+geom_bar()


t<-MSameOsiedla %>% 
   ggplot() +
   geom_point(aes(Powierzchnia, cenazametr), alpha=0.1) +
   geom_smooth(aes(Powierzchnia, cenazametr, col=Dzielnica)) +
   coord_cartesian(xlim=c(25, 80), ylim=c(4100, 6100)) +
   labs(x="Powierzchnia", y="Cena za m2")+
   scale_x_continuous(breaks=c(seq(25,80,2.5)))+
   scale_y_continuous(breaks=c(seq(4100,6100,100)))+
   theme(text = element_text(size=10))

k<-Wlodz %>% 
   ggplot() +
   geom_point(aes(Powierzchnia, cenazametr), alpha=0.1) +
   geom_smooth(aes(Powierzchnia, cenazametr, col=Dzielnica)) +
   coord_cartesian(xlim=c(25, 80), ylim=c(10, 40)) +
   labs(x="Powierzchnia", y="Cena za m2")+
   scale_x_continuous(breaks=c(seq(25,80,2.5)))+
   scale_y_continuous(breaks=c(seq(10,40,2)))+
   theme(text = element_text(size=10))

WBałuty30<-WSameOsiedla %>% 
    filter(Powierzchnia>24.99 & Powierzchnia<30 & Dzielnica=="Bałuty")

MBałuty30<-MSameOsiedla %>% 
    filter(Powierzchnia>24.99 & Powierzchnia<30 & Dzielnica=="Bałuty")

WBałuty35<-WSameOsiedla %>% 
    filter(Powierzchnia>29.99 & Powierzchnia<35 & Dzielnica=="Bałuty")

MBałuty35<-MSameOsiedla %>% 
    filter(Powierzchnia>29.99 & Powierzchnia<35 & Dzielnica=="Bałuty")

WBałuty40<-WSameOsiedla %>% 
    filter(Powierzchnia>34.99 & Powierzchnia<40 & Dzielnica=="Bałuty")

MBałuty40<-MSameOsiedla %>% 
    filter(Powierzchnia>34.99 & Powierzchnia<40 & Dzielnica=="Bałuty")

WBałuty45<-WSameOsiedla %>% 
    filter(Powierzchnia>39.99 & Powierzchnia<45 & Dzielnica=="Bałuty")

MBałuty45<-MSameOsiedla %>% 
    filter(Powierzchnia>39.99 & Powierzchnia<45 & Dzielnica=="Bałuty")

WBałuty50<-WSameOsiedla %>% 
    filter(Powierzchnia>44.99 & Powierzchnia<50 & Dzielnica=="Bałuty")

MBałuty50<-MSameOsiedla %>% 
    filter(Powierzchnia>44.99 & Powierzchnia<50 & Dzielnica=="Bałuty")

WBałuty55<-WSameOsiedla %>% 
    filter(Powierzchnia>49.99 & Powierzchnia<55 & Dzielnica=="Bałuty")

MBałuty55<-MSameOsiedla %>% 
    filter(Powierzchnia>49.99 & Powierzchnia<55 & Dzielnica=="Bałuty")

WBałuty60<-WSameOsiedla %>% 
    filter(Powierzchnia>54.99 & Powierzchnia<60 & Dzielnica=="Bałuty")

MBałuty60<-MSameOsiedla %>% 
    filter(Powierzchnia>54.99 & Powierzchnia<60 & Dzielnica=="Bałuty")

WBałuty65<-WSameOsiedla %>% 
    filter(Powierzchnia>59.99 & Powierzchnia<65 & Dzielnica=="Bałuty")

MBałuty65<-MSameOsiedla %>% 
    filter(Powierzchnia>59.99 & Powierzchnia<65 & Dzielnica=="Bałuty")

WBałuty70<-WSameOsiedla %>% 
    filter(Powierzchnia>64.99 & Powierzchnia<70 & Dzielnica=="Bałuty")

MBałuty70<-MSameOsiedla %>% 
    filter(Powierzchnia>64.99 & Powierzchnia<70 & Dzielnica=="Bałuty")

WBałuty75<-WSameOsiedla %>% 
    filter(Powierzchnia>69.99 & Powierzchnia<75 & Dzielnica=="Bałuty")

MBałuty75<-MSameOsiedla %>% 
    filter(Powierzchnia>69.99 & Powierzchnia<75 & Dzielnica=="Bałuty")

WBałuty80<-WSameOsiedla %>% 
    filter(Powierzchnia>74.99 & Powierzchnia<80 & Dzielnica=="Bałuty")

MBałuty80<-MSameOsiedla %>% 
    filter(Powierzchnia>74.99 & Powierzchnia<80 & Dzielnica=="Bałuty")


WWidzew30<-WSameOsiedla %>% 
    filter(Powierzchnia>24.99 & Powierzchnia<30 & Dzielnica=="Widzew")

MWidzew30<-MSameOsiedla %>% 
    filter(Powierzchnia>24.99 & Powierzchnia<30 & Dzielnica=="Widzew")

WWidzew35<-WSameOsiedla %>% 
    filter(Powierzchnia>29.99 & Powierzchnia<35 & Dzielnica=="Widzew")

MWidzew35<-MSameOsiedla %>% 
    filter(Powierzchnia>29.99 & Powierzchnia<35 & Dzielnica=="Widzew")

WWidzew40<-WSameOsiedla %>% 
    filter(Powierzchnia>34.99 & Powierzchnia<40 & Dzielnica=="Widzew")

MWidzew40<-MSameOsiedla %>% 
    filter(Powierzchnia>34.99 & Powierzchnia<40 & Dzielnica=="Widzew")

WWidzew45<-WSameOsiedla %>% 
    filter(Powierzchnia>39.99 & Powierzchnia<45 & Dzielnica=="Widzew")

MWidzew45<-MSameOsiedla %>% 
    filter(Powierzchnia>39.99 & Powierzchnia<45 & Dzielnica=="Widzew")

WWidzew50<-WSameOsiedla %>% 
    filter(Powierzchnia>44.99 & Powierzchnia<50 & Dzielnica=="Widzew")

MWidzew50<-MSameOsiedla %>% 
    filter(Powierzchnia>44.99 & Powierzchnia<50 & Dzielnica=="Widzew")

WWidzew55<-WSameOsiedla %>% 
    filter(Powierzchnia>49.99 & Powierzchnia<55 & Dzielnica=="Widzew")

MWidzew55<-MSameOsiedla %>% 
    filter(Powierzchnia>49.99 & Powierzchnia<55 & Dzielnica=="Widzew")

WWidzew60<-WSameOsiedla %>% 
    filter(Powierzchnia>54.99 & Powierzchnia<60 & Dzielnica=="Widzew")

MWidzew60<-MSameOsiedla %>% 
    filter(Powierzchnia>54.99 & Powierzchnia<60 & Dzielnica=="Widzew")

WWidzew65<-WSameOsiedla %>% 
    filter(Powierzchnia>59.99 & Powierzchnia<65 & Dzielnica=="Widzew")

MWidzew65<-MSameOsiedla %>% 
    filter(Powierzchnia>59.99 & Powierzchnia<65 & Dzielnica=="Widzew")

WWidzew70<-WSameOsiedla %>% 
    filter(Powierzchnia>64.99 & Powierzchnia<70 & Dzielnica=="Widzew")

MWidzew70<-MSameOsiedla %>% 
    filter(Powierzchnia>64.99 & Powierzchnia<70 & Dzielnica=="Widzew")

WWidzew75<-WSameOsiedla %>% 
    filter(Powierzchnia>69.99 & Powierzchnia<75 & Dzielnica=="Widzew")

MWidzew75<-MSameOsiedla %>% 
    filter(Powierzchnia>69.99 & Powierzchnia<75 & Dzielnica=="Widzew")

WWidzew80<-WSameOsiedla %>% 
    filter(Powierzchnia>74.99 & Powierzchnia<80 & Dzielnica=="Widzew")

MWidzew80<-MSameOsiedla %>% 
    filter(Powierzchnia>74.99 & Powierzchnia<80 & Dzielnica=="Widzew")

WPolesie30<-WSameOsiedla %>% 
    filter(Powierzchnia>24.99 & Powierzchnia<30 & Dzielnica=="Polesie")

MPolesie30<-MSameOsiedla %>% 
    filter(Powierzchnia>24.99 & Powierzchnia<30 & Dzielnica=="Polesie")

WPolesie35<-WSameOsiedla %>% 
    filter(Powierzchnia>29.99 & Powierzchnia<35 & Dzielnica=="Polesie")

MPolesie35<-MSameOsiedla %>% 
    filter(Powierzchnia>29.99 & Powierzchnia<35 & Dzielnica=="Polesie")

WPolesie40<-WSameOsiedla %>% 
    filter(Powierzchnia>34.99 & Powierzchnia<40 & Dzielnica=="Polesie")

MPolesie40<-MSameOsiedla %>% 
    filter(Powierzchnia>34.99 & Powierzchnia<40 & Dzielnica=="Polesie")

WPolesie45<-WSameOsiedla %>% 
    filter(Powierzchnia>39.99 & Powierzchnia<45 & Dzielnica=="Polesie")

MPolesie45<-MSameOsiedla %>% 
    filter(Powierzchnia>39.99 & Powierzchnia<45 & Dzielnica=="Polesie")

WPolesie50<-WSameOsiedla %>% 
    filter(Powierzchnia>44.99 & Powierzchnia<50 & Dzielnica=="Polesie")

MPolesie50<-MSameOsiedla %>% 
    filter(Powierzchnia>44.99 & Powierzchnia<50 & Dzielnica=="Polesie")

WPolesie55<-WSameOsiedla %>% 
    filter(Powierzchnia>49.99 & Powierzchnia<55 & Dzielnica=="Polesie")

MPolesie55<-MSameOsiedla %>% 
    filter(Powierzchnia>49.99 & Powierzchnia<55 & Dzielnica=="Polesie")

WPolesie60<-WSameOsiedla %>% 
    filter(Powierzchnia>54.99 & Powierzchnia<60 & Dzielnica=="Polesie")

MPolesie60<-MSameOsiedla %>% 
    filter(Powierzchnia>54.99 & Powierzchnia<60 & Dzielnica=="Polesie")

WPolesie65<-WSameOsiedla %>% 
    filter(Powierzchnia>59.99 & Powierzchnia<65 & Dzielnica=="Polesie")

MPolesie65<-MSameOsiedla %>% 
    filter(Powierzchnia>59.99 & Powierzchnia<65 & Dzielnica=="Polesie")

WPolesie70<-WSameOsiedla %>% 
    filter(Powierzchnia>64.99 & Powierzchnia<70 & Dzielnica=="Polesie")

MPolesie70<-MSameOsiedla %>% 
    filter(Powierzchnia>64.99 & Powierzchnia<70 & Dzielnica=="Polesie")

WPolesie75<-WSameOsiedla %>% 
    filter(Powierzchnia>69.99 & Powierzchnia<75 & Dzielnica=="Polesie")

MPolesie75<-MSameOsiedla %>% 
    filter(Powierzchnia>69.99 & Powierzchnia<75 & Dzielnica=="Polesie")

WPolesie80<-WSameOsiedla %>% 
    filter(Powierzchnia>74.99 & Powierzchnia<80 & Dzielnica=="Polesie")

MPolesie80<-MSameOsiedla %>% 
    filter(Powierzchnia>74.99 & Powierzchnia<80 & Dzielnica=="Polesie")

WŚródmieście30<-WSameOsiedla %>% 
    filter(Powierzchnia>24.99 & Powierzchnia<30 & Dzielnica=="Śródmieście")

MŚródmieście30<-MSameOsiedla %>% 
    filter(Powierzchnia>24.99 & Powierzchnia<30 & Dzielnica=="Śródmieście")

WŚródmieście35<-WSameOsiedla %>% 
    filter(Powierzchnia>29.99 & Powierzchnia<35 & Dzielnica=="Śródmieście")

MŚródmieście35<-MSameOsiedla %>% 
    filter(Powierzchnia>29.99 & Powierzchnia<35 & Dzielnica=="Śródmieście")

WŚródmieście40<-WSameOsiedla %>% 
    filter(Powierzchnia>34.99 & Powierzchnia<40 & Dzielnica=="Śródmieście")

MŚródmieście40<-MSameOsiedla %>% 
    filter(Powierzchnia>34.99 & Powierzchnia<40 & Dzielnica=="Śródmieście")

WŚródmieście45<-WSameOsiedla %>% 
    filter(Powierzchnia>39.99 & Powierzchnia<45 & Dzielnica=="Śródmieście")

MŚródmieście45<-MSameOsiedla %>% 
    filter(Powierzchnia>39.99 & Powierzchnia<45 & Dzielnica=="Śródmieście")

WŚródmieście50<-WSameOsiedla %>% 
    filter(Powierzchnia>44.99 & Powierzchnia<50 & Dzielnica=="Śródmieście")

MŚródmieście50<-MSameOsiedla %>% 
    filter(Powierzchnia>44.99 & Powierzchnia<50 & Dzielnica=="Śródmieście")

WŚródmieście55<-WSameOsiedla %>% 
    filter(Powierzchnia>49.99 & Powierzchnia<55 & Dzielnica=="Śródmieście")

MŚródmieście55<-MSameOsiedla %>% 
    filter(Powierzchnia>49.99 & Powierzchnia<55 & Dzielnica=="Śródmieście")

WŚródmieście60<-WSameOsiedla %>% 
    filter(Powierzchnia>54.99 & Powierzchnia<60 & Dzielnica=="Śródmieście")

MŚródmieście60<-MSameOsiedla %>% 
    filter(Powierzchnia>54.99 & Powierzchnia<60 & Dzielnica=="Śródmieście")

WŚródmieście65<-WSameOsiedla %>% 
    filter(Powierzchnia>59.99 & Powierzchnia<65 & Dzielnica=="Śródmieście")

MŚródmieście65<-MSameOsiedla %>% 
    filter(Powierzchnia>59.99 & Powierzchnia<65 & Dzielnica=="Śródmieście")

WŚródmieście70<-WSameOsiedla %>% 
    filter(Powierzchnia>64.99 & Powierzchnia<70 & Dzielnica=="Śródmieście")

MŚródmieście70<-MSameOsiedla %>% 
    filter(Powierzchnia>64.99 & Powierzchnia<70 & Dzielnica=="Śródmieście")

WŚródmieście75<-WSameOsiedla %>% 
    filter(Powierzchnia>69.99 & Powierzchnia<75 & Dzielnica=="Śródmieście")

MŚródmieście75<-MSameOsiedla %>% 
    filter(Powierzchnia>69.99 & Powierzchnia<75 & Dzielnica=="Śródmieście")

WŚródmieście80<-WSameOsiedla %>% 
    filter(Powierzchnia>74.99 & Powierzchnia<80 & Dzielnica=="Śródmieście")

MŚródmieście80<-MSameOsiedla %>% 
    filter(Powierzchnia>74.99 & Powierzchnia<80 & Dzielnica=="Śródmieście")

WGórna30<-WSameOsiedla %>% 
    filter(Powierzchnia>24.99 & Powierzchnia<30 & Dzielnica=="Górna")

MGórna30<-MSameOsiedla %>% 
    filter(Powierzchnia>24.99 & Powierzchnia<30 & Dzielnica=="Górna")

WGórna35<-WSameOsiedla %>% 
    filter(Powierzchnia>29.99 & Powierzchnia<35 & Dzielnica=="Górna")

MGórna35<-MSameOsiedla %>% 
    filter(Powierzchnia>29.99 & Powierzchnia<35 & Dzielnica=="Górna")

WGórna40<-WSameOsiedla %>% 
    filter(Powierzchnia>34.99 & Powierzchnia<40 & Dzielnica=="Górna")

MGórna40<-MSameOsiedla %>% 
    filter(Powierzchnia>34.99 & Powierzchnia<40 & Dzielnica=="Górna")

WGórna45<-WSameOsiedla %>% 
    filter(Powierzchnia>39.99 & Powierzchnia<45 & Dzielnica=="Górna")

MGórna45<-MSameOsiedla %>% 
    filter(Powierzchnia>39.99 & Powierzchnia<45 & Dzielnica=="Górna")

WGórna50<-WSameOsiedla %>% 
    filter(Powierzchnia>44.99 & Powierzchnia<50 & Dzielnica=="Górna")

MGórna50<-MSameOsiedla %>% 
    filter(Powierzchnia>44.99 & Powierzchnia<50 & Dzielnica=="Górna")

WGórna55<-WSameOsiedla %>% 
    filter(Powierzchnia>49.99 & Powierzchnia<55 & Dzielnica=="Górna")

MGórna55<-MSameOsiedla %>% 
    filter(Powierzchnia>49.99 & Powierzchnia<55 & Dzielnica=="Górna")

WGórna60<-WSameOsiedla %>% 
    filter(Powierzchnia>54.99 & Powierzchnia<60 & Dzielnica=="Górna")

MGórna60<-MSameOsiedla %>% 
    filter(Powierzchnia>54.99 & Powierzchnia<60 & Dzielnica=="Górna")

WGórna65<-WSameOsiedla %>% 
    filter(Powierzchnia>59.99 & Powierzchnia<65 & Dzielnica=="Górna")

MGórna65<-MSameOsiedla %>% 
    filter(Powierzchnia>59.99 & Powierzchnia<65 & Dzielnica=="Górna")

WGórna70<-WSameOsiedla %>% 
    filter(Powierzchnia>64.99 & Powierzchnia<70 & Dzielnica=="Górna")

MGórna70<-MSameOsiedla %>% 
    filter(Powierzchnia>64.99 & Powierzchnia<70 & Dzielnica=="Górna")

WGórna75<-WSameOsiedla %>% 
    filter(Powierzchnia>69.99 & Powierzchnia<75 & Dzielnica=="Górna")

MGórna75<-MSameOsiedla %>% 
    filter(Powierzchnia>69.99 & Powierzchnia<75 & Dzielnica=="Górna")

WGórna80<-WSameOsiedla %>% 
    filter(Powierzchnia>74.99 & Powierzchnia<80 & Dzielnica=="Górna")

MGórna80<-MSameOsiedla %>% 
    filter(Powierzchnia>74.99 & Powierzchnia<80 & Dzielnica=="Górna")



Wlodz30<-Wlodz %>% 
    filter(Powierzchnia>24.99 & Powierzchnia<30)

Mlodz30<-Mlodz %>% 
    filter(Powierzchnia>24.99 & Powierzchnia<30)

Wlodz35<-Wlodz %>% 
    filter(Powierzchnia>29.99 & Powierzchnia<35)

Mlodz35<-Mlodz %>% 
    filter(Powierzchnia>29.99 & Powierzchnia<35)

Wlodz40<-Wlodz %>% 
    filter(Powierzchnia>34.99 & Powierzchnia<40)

Mlodz40<-Mlodz %>% 
    filter(Powierzchnia>34.99 & Powierzchnia<40)

Wlodz45<-Wlodz %>% 
    filter(Powierzchnia>39.99 & Powierzchnia<45)

Mlodz45<-Mlodz %>% 
    filter(Powierzchnia>39.99 & Powierzchnia<45)

Wlodz50<-Wlodz %>% 
    filter(Powierzchnia>44.99 & Powierzchnia<50)

Mlodz50<-Mlodz %>% 
    filter(Powierzchnia>44.99 & Powierzchnia<50)

Wlodz55<-Wlodz %>% 
    filter(Powierzchnia>49.99 & Powierzchnia<55)

Mlodz55<-Mlodz %>% 
    filter(Powierzchnia>49.99 & Powierzchnia<55)

Wlodz60<-Wlodz %>% 
    filter(Powierzchnia>54.99 & Powierzchnia<60)

Mlodz60<-Mlodz %>% 
    filter(Powierzchnia>54.99 & Powierzchnia<60)

Wlodz65<-Wlodz %>% 
    filter(Powierzchnia>59.99 & Powierzchnia<65)

Mlodz65<-Mlodz %>% 
    filter(Powierzchnia>59.99 & Powierzchnia<65)

Wlodz70<-Wlodz %>% 
    filter(Powierzchnia>64.99 & Powierzchnia<70)

Mlodz70<-Mlodz %>% 
    filter(Powierzchnia>64.99 & Powierzchnia<70)

Wlodz75<-Wlodz %>% 
    filter(Powierzchnia>69.99 & Powierzchnia<75)

Mlodz75<-Mlodz %>% 
    filter(Powierzchnia>69.99 & Powierzchnia<75)

Wlodz80<-Wlodz %>% 
    filter(Powierzchnia>74.99 & Powierzchnia<80)

Mlodz80<-Mlodz %>% 
    filter(Powierzchnia>74.99 & Powierzchnia<80)

B30<-round(12*mean(WBałuty30$Cena)/mean(MBałuty30$Cena)*100,1)

B35<-round(12*mean(WBałuty35$Cena)/mean(MBałuty35$Cena)*100,1)

B40<-round(12*mean(WBałuty40$Cena)/mean(MBałuty40$Cena)*100,1)

B45<-round(12*mean(WBałuty45$Cena)/mean(MBałuty45$Cena)*100,1)

B50<-round(12*mean(WBałuty50$Cena)/mean(MBałuty50$Cena)*100,1)

B55<-round(12*mean(WBałuty55$Cena)/mean(MBałuty55$Cena)*100,1)

B60<-round(12*mean(WBałuty60$Cena)/mean(MBałuty60$Cena)*100,1)

B65<-round(12*mean(WBałuty65$Cena)/mean(MBałuty65$Cena)*100,1)

B70<-round(12*mean(WBałuty70$Cena)/mean(MBałuty70$Cena)*100,1)

B75<-round(12*mean(WBałuty75$Cena)/mean(MBałuty75$Cena)*100,1)

B80<-round(12*mean(WBałuty80$Cena)/mean(MBałuty80$Cena)*100,1)



W30<-round(12*mean(WWidzew30$Cena)/mean(MWidzew30$Cena)*100,1)

W35<-round(12*mean(WWidzew35$Cena)/mean(MWidzew35$Cena)*100,1)

W40<-round(12*mean(WWidzew40$Cena)/mean(MWidzew40$Cena)*100,1)

W45<-round(12*mean(WWidzew45$Cena)/mean(MWidzew45$Cena)*100,1)

W50<-round(12*mean(WWidzew50$Cena)/mean(MWidzew50$Cena)*100,1)

W55<-round(12*mean(WWidzew55$Cena)/mean(MWidzew55$Cena)*100,1)

W60<-round(12*mean(WWidzew60$Cena)/mean(MWidzew60$Cena)*100,1)

W65<-round(12*mean(WWidzew65$Cena)/mean(MWidzew65$Cena)*100,1)

W70<-round(12*mean(WWidzew70$Cena)/mean(MWidzew70$Cena)*100,1)

W75<-round(12*mean(WWidzew75$Cena)/mean(MWidzew75$Cena)*100,1)

W80<-round(12*mean(WWidzew80$Cena)/mean(MWidzew80$Cena)*100,1)


P30<-round(12*mean(WPolesie30$Cena)/mean(MPolesie30$Cena)*100,1)

P35<-round(12*mean(WPolesie35$Cena)/mean(MPolesie35$Cena)*100,1)

P40<-round(12*mean(WPolesie40$Cena)/mean(MPolesie40$Cena)*100,1)

P45<-round(12*mean(WPolesie45$Cena)/mean(MPolesie45$Cena)*100,1)

P50<-round(12*mean(WPolesie50$Cena)/mean(MPolesie50$Cena)*100,1)

P55<-round(12*mean(WPolesie55$Cena)/mean(MPolesie55$Cena)*100,1)

P60<-round(12*mean(WPolesie60$Cena)/mean(MPolesie60$Cena)*100,1)

P65<-round(12*mean(WPolesie65$Cena)/mean(MPolesie65$Cena)*100,1)

P70<-round(12*mean(WPolesie70$Cena)/mean(MPolesie70$Cena)*100,1)

P75<-round(12*mean(WPolesie75$Cena)/mean(MPolesie75$Cena)*100,1)

P80<-round(12*mean(WPolesie80$Cena)/mean(MPolesie80$Cena)*100,1)


Ś30<-round(12*mean(WŚródmieście30$Cena)/mean(MŚródmieście30$Cena)*100,1)

Ś35<-round(12*mean(WŚródmieście35$Cena)/mean(MŚródmieście35$Cena)*100,1)

Ś40<-round(12*mean(WŚródmieście40$Cena)/mean(MŚródmieście40$Cena)*100,1)

Ś45<-round(12*mean(WŚródmieście45$Cena)/mean(MŚródmieście45$Cena)*100,1)

Ś50<-round(12*mean(WŚródmieście50$Cena)/mean(MŚródmieście50$Cena)*100,1)

Ś55<-round(12*mean(WŚródmieście55$Cena)/mean(MŚródmieście55$Cena)*100,1)

Ś60<-round(12*mean(WŚródmieście60$Cena)/mean(MŚródmieście60$Cena)*100,1)

Ś65<-round(12*mean(WŚródmieście65$Cena)/mean(MŚródmieście65$Cena)*100,1)

Ś70<-round(12*mean(WŚródmieście70$Cena)/mean(MŚródmieście70$Cena)*100,1)

Ś75<-round(12*mean(WŚródmieście75$Cena)/mean(MŚródmieście75$Cena)*100,1)

Ś80<-round(12*mean(WŚródmieście80$Cena)/mean(MŚródmieście80$Cena)*100,1)


G30<-round(12*mean(WGórna30$Cena)/mean(MGórna30$Cena)*100,1)

G35<-round(12*mean(WGórna35$Cena)/mean(MGórna35$Cena)*100,1)

G40<-round(12*mean(WGórna40$Cena)/mean(MGórna40$Cena)*100,1)

G45<-round(12*mean(WGórna45$Cena)/mean(MGórna45$Cena)*100,1)

G50<-round(12*mean(WGórna50$Cena)/mean(MGórna50$Cena)*100,1)

G55<-round(12*mean(WGórna55$Cena)/mean(MGórna55$Cena)*100,1)

G60<-round(12*mean(WGórna60$Cena)/mean(MGórna60$Cena)*100,1)

G65<-round(12*mean(WGórna65$Cena)/mean(MGórna65$Cena)*100,1)

G70<-round(12*mean(WGórna70$Cena)/mean(MGórna70$Cena)*100,1)

G75<-round(12*mean(WGórna75$Cena)/mean(MGórna75$Cena)*100,1)

G80<-round(12*mean(WGórna80$Cena)/mean(MGórna80$Cena)*100,1)


Ł30<-round(12*mean(Wlodz30$Cena)/mean(Mlodz30$Cena)*100,1)

Ł35<-round(12*mean(Wlodz35$Cena)/mean(Mlodz35$Cena)*100,1)

Ł40<-round(12*mean(Wlodz40$Cena)/mean(Mlodz40$Cena)*100,1)

Ł45<-round(12*mean(Wlodz45$Cena)/mean(Mlodz45$Cena)*100,1)

Ł50<-round(12*mean(Wlodz50$Cena)/mean(Mlodz50$Cena)*100,1)

Ł55<-round(12*mean(Wlodz55$Cena)/mean(Mlodz55$Cena)*100,1)

Ł60<-round(12*mean(Wlodz60$Cena)/mean(Mlodz60$Cena)*100,1)

Ł65<-round(12*mean(Wlodz65$Cena)/mean(Mlodz65$Cena)*100,1)

Ł70<-round(12*mean(Wlodz70$Cena)/mean(Mlodz70$Cena)*100,1)

Ł75<-round(12*mean(Wlodz75$Cena)/mean(Mlodz75$Cena)*100,1)

Ł80<-round(12*mean(Wlodz80$Cena)/mean(Mlodz80$Cena)*100,1)


Stopa_zwrotu<-data.frame(
    powierzchnia=c(30,35,40,45,50,55,60,65,70,75,80),
    Stopa_zwrotu_z_Łodzi=c(Ł30,Ł35,Ł40,Ł45,Ł50,Ł55,Ł60,Ł65,Ł70,Ł75,Ł80),
    Stopa_zwrotu_z_Bałut=c(B30,B35,B40,B45,B50,B55,B60,B65,B70,B75,B80),
    Stopa_zwrotu_z_Widzewa=c(W30,W35,W40,W45,W50,W55,W60,W65,W70,W75,W80),
    Stopa_zwrotu_z_Polesia=c(P30,P35,P40,P45,P50,P55,P60,P65,P70,P75,P80),
    Stopa_zwrotu_z_Śródmiescia=c(Ś30,Ś35,Ś40,Ś45,Ś50,Ś55,Ś60,Ś65,Ś70,Ś75,Ś80),
    Stopa_zwrotu_z_Górnej=c(G30,G35,G40,G45,G50,G55,G60,G65,G70,G75,G80)
    )
ggplot()
tt<-ttheme_default(base_size = 8)
grid.table(Stopa_zwrotu, theme=tt)
