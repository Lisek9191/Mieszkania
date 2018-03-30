#Plik ściąga dane do wklepania od razu do bazy danych. Prawdopodobnie należy dodać na samym końcu wyjściowego pliku ";"

from urllib.request import Request, urlopen
from bs4 import BeautifulSoup as soup

filename='mieszkanie.txt'
f=open(filename,'w',encoding="utf-8")

#wklej do "strony" wyniki z programu 'scrapowanie podstron'

strony=['https://www.otodom.pl/oferta/wlasnosciowe-spoldzielcze-37-20-m-bezposrednio-ID3Athp.html#173da81aa9',
'https://www.otodom.pl/oferta/mieszkanie-3-pokojowe-od-dewelopera-pod-klucz-ID3riOM.html#173da81aa9']
for strona in strony:
    
    req=Request(strona, headers={'User-Agent': 'Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:47.0) Gecko/20100101 Firefox/47.0'})
    page=urlopen(req).read()

    urlopen(req).close()


    page_soup=soup(page,'html.parser')


    try:
        for z in page_soup('span',{"class":"percentile-location-text"}):
            osiedle=z.text
    except IndexError:
        osiedle=""
        
    try:
        cena=page_soup.findAll('li',{'class':'param_price'})[0].strong.text
    except IndexError:
        cena=""

    try:
        powierzchnia=page_soup.findAll('li',{'class':'param_m'})[0].strong.text
    except IndexError:
        powierzchnia=""
        
    try:
        pietro=page_soup.findAll('li',{'class':'param_floor_no'})[0].strong.text
    except IndexError:
        pietro=""

    try:
        for li in page_soup.find_all('li'):
            if 'Liczba pokoi' in li.text:
                pokoje = li.text.replace('Liczba pokoi ', '')
    except IndexError:
            pokoje=""

    try:
        informacje=page_soup.findAll('ul',{'class':'sub-list'})[0].text
    except IndexError:
        informacje=""

    try:
        informacje_dodatkowe0=page_soup.findAll('ul',{'class':'dotted-list'})[0].text
    except IndexError:
            informacje_dodatkowe0=""

    try:           
        informacje_dodatkowe1=page_soup.findAll('ul',{'class':'dotted-list'})[1].text
    except IndexError:
            informacje_dodatkowe1=""
                
    print(osiedle)
    print(cena)
    print(powierzchnia)
    print(pietro)
    print(pokoje)
    print(informacje)
    print(informacje_dodatkowe0)
    print(informacje_dodatkowe1)

    f.write('INSERT INTO mieszkania VALUES (')
    f.write("'"+osiedle+"'"+","+"'"+cena+"'"+","+"'"+powierzchnia+"'"+","+"'"+pietro+"'"+","+"'"+pokoje+"'"+","+"'"+informacje+"'"+","+"'"+informacje_dodatkowe0+"'"+","+"'"+informacje_dodatkowe1+"'"+"\n")
    f.write('); ')
    
f.close()


