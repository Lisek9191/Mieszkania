-- MySQL dump 10.13  Distrib 5.7.17, for Win64 (x86_64)
--
-- Host: localhost    Database: lista_grzesia
-- ------------------------------------------------------
-- Server version	5.7.21-log

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `moje_kontakty`
--

DROP TABLE IF EXISTS `moje_kontakty`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `moje_kontakty` (
  `id_kontaktu` int(11) NOT NULL AUTO_INCREMENT,
  `nazwisko` varchar(30) DEFAULT NULL,
  `imie` varchar(20) DEFAULT NULL,
  `email` varchar(50) DEFAULT NULL,
  `plec` char(1) DEFAULT NULL,
  `data_urodzenia` date DEFAULT NULL,
  `zawod` varchar(50) DEFAULT NULL,
  `lokalizacja` varchar(50) DEFAULT NULL,
  `miasto` varchar(20) DEFAULT NULL,
  `stan` varchar(20) DEFAULT NULL,
  `zainteresowania` varchar(100) DEFAULT NULL,
  `szuka` varchar(100) DEFAULT NULL,
  PRIMARY KEY (`id_kontaktu`)
) ENGINE=InnoDB AUTO_INCREMENT=17 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `moje_kontakty`
--

LOCK TABLES `moje_kontakty` WRITE;
/*!40000 ALTER TABLE `moje_kontakty` DISABLE KEYS */;
INSERT INTO `moje_kontakty` VALUES (1,'Kowalska','Juliana','ju.ka@pizza-nzk.com.pl','K','1980-09-05','Autorka tekstów technicznych','Warszawa, MZ','Warszawa','panna','kajakarstwo, gady','związków, przyjaciół'),(2,'Kubaski','Andrzej','Kubaski_Andrzej@pizza-nzk.com.pl','M','1974-01-10','kierownik','Gliwice, SL','Gliwice','rozwiedziony','kobiety','kobiety na randki'),(3,'Symonowicz','Anna','anna_tymonowicz@pizza-nzk.com.pl','K','1966-01-23','przedszkolanka','Wroclaw, DS','Wroclaw','panna','żeglarstwo, łowienie ryb','partnera do niezobowiązującego związku'),(4,'Nowak','Joanna','nowjn@op.pl','K','1977-04-28','sprzedwczyni oprogramowania','Krakow, MP','Krakow','zamężna','kino, drinki','nowej pracy'),(5,'Oczkiewicz','Adam','oczam@zygzyg.eu','M','1964-09-10','administrator systemów komputerowych','Elbląg, WM','Elbląg','żonaty','RPG','niczego'),(6,'Karczynska','Marta','karmar@interia.pl','K','1962-07-01','właścicielka księgarni','Katowice, SL','Katowice','zamężna','kolekcjonowanie książek, nurkowanie','nowych przyjaciół'),(7,'Zefirek','Monika','moniaze@pizza-nzk.com.pl','K','1976-12-03','bezrobotna','Tychy, SL','Tychy','rozwiedziona','gotowanie','pracy'),(8,'Harmonia','Ania','a_harmonia@mojamuza.pl','K','1979-08-19','administratorka systemów UNIX','Wroclaw, DS','Wroclaw','zamężna','aktorstwo, taniec','nowej pracy'),(9,'Mikulski','Janek','mikunek@wp.pl','M','1967-01-23','programista','Poznan, WP','Poznan','żonaty','RGP, kreskówki','przyjaciół'),(10,'Tkacz','Martyna','mptkacz@o2.eu','K','1963-04-18','akwizytorka','Wroclaw, DS','Wroclaw','zamężna','poezja, książki','niczego'),(11,'Kowalska','Zosia','zetka@op.pl','K','1969-11-18','artystka','Katowice, SL','Katowice',NULL,NULL,NULL),(12,'Zofik','Tadeusz','t.zofik@pizza-nzk.com.pl','M','1977-08-09','piekarz','Mikolow, SL','Mikolow',NULL,NULL,NULL),(13,'Kubaski','Milosz','dj_milo@nfala.fm','M','1963-04-18','nauczyciel','Warszawa, MZ','Warszawa',NULL,NULL,NULL),(14,'Kowalska','Magda','miziak@op.pl','K','1983-01-10','studentka','Warszawa, MZ','Warszawa',NULL,NULL,NULL),(15,'Tarczynski','Piotr','p.tarczynski@ynteria.pl','M','1959-10-09','projektant stron WWW','Poznan, DS','Poznan',NULL,NULL,NULL),(16,'Maliniak','Piotr','p_malinka@op.pl','M','1968-02-05','programista','Katowice, SL','Katowice',NULL,NULL,NULL);
/*!40000 ALTER TABLE `moje_kontakty` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2018-03-16 22:03:40
