# Created by Tomasz Puncewicz.


# Programy powstały w celach demonstracyjnych. Zakazuje się wykorzystywania go w jakichkolwiek celach, zarówno w komerycjnych i niekomerycjnych.


# Projekt ma na celu analizę rynku mieszkań w Łodzi.

# Plan Działania:
# 1) Ściągnie podstron programem "Scraping podstron"
# 2) Wklejenie wyników do zmiennej "strony" w pliku "ściąganie informacji na temat mieszkań" oraz skasowanie ostatniego przecinka
# 3) Uruchomienie programu "ściąganie informacji na temat mieszkań" (czas trwania kilka godzin)
# 4) Tworzenie bazy danych poprzez wpisanie
#     CREATE TABLE mieszkania (
      osiedle Varchar(30),
      cena varchar(50),
      powierzchnia varchar(10),
      pietro varchar(10),
      pokoje varchar(10),
      informacje varchar(500),
      informacje_dodatkowe0 varchar(500),
      informacje_dodatkowe1  varchar(500)
      ) ENGINE=MyISAM DEFAULT CHARSET=utf8;
      
# oraz wklejenie wyników drugiego programu. Nalezy usunąć przecinek oraz dodać ";"
# 5) Zmiana struktury bazy, usuwanie złych rekordów
# 6) Analiza danych w programie MySql i Ms Excel.
