# Wczytanie bibliotek
library(RMySQL)
library(dbConect)
library(ggplot2)
library(dplyr)

# Łączenie z bazą
con=dbConnect(MySQL(), user='root', password='1234', dbname="moje_m", host="localhost")
dbListTables(con)
MyQuery="select * from mieszkania;"

df<-dbGetQuery(con, MyQuery)



# Mogłem to zrobić za pomocą colnames(df)<-c("xxx","yyy",...) ale moim zdaniem metoda poniżej jest bardziej przejrzysta
colnames(df)[1]<-"Id"
colnames(df)[2]<-"Osiedle"
colnames(df)[3]<-"Cena"
colnames(df)[4]<-"Powierzchnia"
colnames(df)[5]<-"Piętro"
colnames(df)[6]<-"Pokoje"
colnames(df)[7]<-"Rynek"
colnames(df)[8]<-"Rodzaj_zabudowy"
colnames(df)[9]<-"Materiał_budynku"
colnames(df)[10]<-"Okna"
colnames(df)[11]<-"Ogrzewanie"
colnames(df)[12]<-"Rok_Budowy"
colnames(df)[13]<-"Stan_Wykończenia"
colnames(df)[14]<-"Czynsz"
colnames(df)[15]<-"Forma_Własności"
colnames(df)[16]<-"Miejsce_Parkingowe"
colnames(df)[17]<-"Balkon"
colnames(df)[18]<-"Piwnica"

# Odkodowanie znaków UTF8
Encoding(df$Osiedle)<-"UTF-8"
Encoding(df$Rynek)<-"UTF-8"
Encoding(df$Rodzaj_zabudowy)<-"UTF-8"
Encoding(df$Materiał_budynku)<-"UTF-8"
Encoding(df$Okna)<-"UTF-8"
Encoding(df$Ogrzewanie)<-"UTF-8"
Encoding(df$Stan_Wykończenia)<-"UTF-8"
Encoding(df$Forma_Własności)<-"UTF-8"
Encoding(df$Pokoje)<-"UTF-8"

# Eliminacja wartości odstających i uporzadkowanie
ggplot(df, aes(x=Cena, y=Powierzchnia))+geom_point()
lodz<-df%>% filter(Cena>49999 & Cena<1100000)
lodz<-lodz%>% filter(Powierzchnia<300)
lodz$Osiedle<-reorder(lodz$Osiedle, lodz$Cena, median)
# Podział danych na mniejsze partie

SameOsiedla<-lodz%>%filter(Osiedle=="Bałuty" | Osiedle=="Widzew" | Osiedle=="Polesie" | Osiedle=="Górna" | Osiedle=="Śródmieście")

Bałuty<-SameOsiedla %>% 
    filter(Osiedle == "Bałuty")
Widzew<-SameOsiedla %>% 
    filter(Osiedle == "Widzew")
Śródmieście<-SameOsiedla %>% 
    filter(Osiedle == "Śródmieście")
Górna<-SameOsiedla %>% 
    filter(Osiedle == "Górna")
Polesie<-SameOsiedla %>% 
    filter(Osiedle == "Polesie")
NieznaneOś<-SameOsiedla %>% 
    filter(Osiedle == "Nieznane")

# KIlka wykresów
ggplot(lodz, aes(x=Powierzchnia, y=Cena, col=Osiedle))+geom_point()+geom_smooth(se=FALSE, size=2, method="lm", formula=y~poly(x,2))

####### Histogram Cen
ggplot(lodz, aes(x=Cena, fill=Osiedle))+geom_histogram(color="white")+geom_vline(xintercept = median(lodz$Cena)) +
geom_vline(xintercept = mean(lodz$Cena), color="red")+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))

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
ggplot(lodz, aes(x=Powierzchnia))+geom_histogram(color="black", fill="green", binwidth=5)+geom_vline(xintercept=median(lodz$Powierzchnia))+geom_vline(xintercept=mean(lodz$Powierzchnia), color="red")+
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
lodz<-lodz%>%mutate(cenazametr=round(Cena/Powierzchnia,0))
Bałuty<-Bałuty%>%mutate(cenazametr=round(Cena/Powierzchnia,0))
Widzew<-Widzew%>%mutate(cenazametr=round(Cena/Powierzchnia,0))
Polesie<-Polesie%>%mutate(cenazametr=round(Cena/Powierzchnia,0))
Śródmieście<-Śródmieście%>%mutate(cenazametr=round(Cena/Powierzchnia,0))
Górna<-Górna%>%mutate(cenazametr=round(Cena/Powierzchnia,0))

ggplot(lodz, aes(x=cenazametr))+geom_histogram(color="black", fill="green", binwidth=100)+geom_vline(xintercept=median(lodz$cenazametr))+geom_vline(xintercept=mean(lodz$cenazametr), color="red")+
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


####### Wykres barowy cen za rodzaj budynku

ggplot(lodz, aes(x=Rodzaj_zabudowy, fill=Osiedle))+geom_bar(position="fill")
ggplot(lodz, aes(x=Rodzaj_zabudowy, fill=Osiedle))+geom_bar()

ggplot(Bałuty, aes(x=Rodzaj_zabudowy, fill=Osiedle))+geom_bar(position="fill")
ggplot(Bałuty, aes(x=Rodzaj_zabudowy, fill=Osiedle))+geom_bar()

ggplot(Widzew, aes(x=Rodzaj_zabudowy, fill=Osiedle))+geom_bar(position="fill")
ggplot(Widzew, aes(x=Rodzaj_zabudowy, fill=Osiedle))+geom_bar()

ggplot(Polesie, aes(x=Rodzaj_zabudowy, fill=Osiedle))+geom_bar(position="fill")
ggplot(Polesie, aes(x=Rodzaj_zabudowy, fill=Osiedle))+geom_bar()

ggplot(Śródmieście, aes(x=Rodzaj_zabudowy, fill=Osiedle))+geom_bar(position="fill")
ggplot(Śródmieście, aes(x=Rodzaj_zabudowy, fill=Osiedle))+geom_bar()

ggplot(Górna, aes(x=Rodzaj_zabudowy, fill=Osiedle))+geom_bar(position="fill")
ggplot(Górna, aes(x=Rodzaj_zabudowy, fill=Osiedle))+geom_bar()


####### Wykres barowy pokoi

ggplot(lodz, aes(x=Pokoje, fill=Osiedle))+geom_bar(position="fill")
ggplot(lodz, aes(x=Pokoje, fill=Osiedle))+geom_bar()

ggplot(Bałuty, aes(x=Pokoje, fill=Osiedle))+geom_bar(position="fill")
ggplot(Bałuty, aes(x=Pokoje, fill=Osiedle))+geom_bar()

ggplot(Widzew, aes(x=Pokoje, fill=Osiedle))+geom_bar(position="fill")
ggplot(Widzew, aes(x=Pokoje, fill=Osiedle))+geom_bar()

ggplot(Polesie, aes(x=Pokoje, fill=Osiedle))+geom_bar(position="fill")
ggplot(Polesie, aes(x=Pokoje, fill=Osiedle))+geom_bar()

ggplot(Śródmieście, aes(x=Pokoje, fill=Osiedle))+geom_bar(position="fill")
ggplot(Śródmieście, aes(x=Pokoje, fill=Osiedle))+geom_bar()

ggplot(Górna, aes(x=Pokoje, fill=Osiedle))+geom_bar(position="fill")
ggplot(Górna, aes(x=Pokoje, fill=Osiedle))+geom_bar()



####### Wykres punktowy pietro do ceny za metr kwadtarowy

ggplot(lodz, aes(x=Piętro, y=cenazametr, col=Piętro))+geom_point()





























