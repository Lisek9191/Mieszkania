# Created by Tomasz Puncewicz

from urllib.request import Request, urlopen
from bs4 import BeautifulSoup as soup
from time import sleep
from random import randint

for numer in range(1,102)
	req=Request('https://www.otodom.pl/sprzedaz/mieszkanie/lodz/?search%5Bdist%5D=0&search%5Bsubregion_id%5D=127&search%5Bcity_id%5D=1004&page='+str(numer), headers={'User-Agent': 'Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:47.0) Gecko/20100101 Firefox/47.0'})


	sleep(randint(2,4))
	page=urlopen(req).read()

	urlopen(req).close()


	page_soup=soup(page,'html.parser')

	urls=page_soup.findAll("div", {"class":"offer-item-details"})


	filename='adresy_url_mieszkań.csv'
	f=open(filename,'w',encoding="utf-8")
	headers='adress\n'
	f.write(headers)

	for url in urls:
	    adress=url.a['href']
	    

	    print("'"+adress+"'"+",")
	    f.write(adress+'\n')

f.close

