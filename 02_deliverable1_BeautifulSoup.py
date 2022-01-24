# -*- coding: utf-8 -*-
"""deliverable1-beautifulsoup.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1I1P9GX_226Z9y58XCL4BSDSkXiGYXCw0
"""

#importing packages
import requests
from bs4 import BeautifulSoup as bs4
import pandas 

#put link inside quotations
page = requests.get("https://en.wikipedia.org/wiki/Donald_Trump")
webpage = page.content
page.close()
soup = bs4(webpage, 'html.parser')
print(soup)

print(page)
#Doing a bit of research, this response code means that it was successful

print(soup.h1.text)

print(soup.prettify())

info_box = soup.find("table",{"class": "infobox vcard"})
info_box.prettify()


bioinfo = [["Politician"],[info_box.div.text]]
biotags = info_box.findAll("th",{"class": "infobox-label"})
biodata = info_box.findAll("td",{"class": "infobox-data"})
x=3
while x < 13:
    biotags1=biotags[x].text
    biodata1=biodata[x].text.replace('\n','').replace(',',':')
    
    x=x+1
    bioinfo[0].append(biotags1)
    bioinfo[1].append(biodata1)
bioinfo[1][1]= bioinfo[1][1].replace('(age\xa075)','(age 75) ')
bioinfo[1][4]= bioinfo[1][4].replace('\u200b \u200b(m.\xa01977; div.\xa01992)\u200b',', ').replace('\u200b \u200b(m.\xa01993; div.\xa01999)\u200b',', ').replace('\u200b(m.\xa02005)\u200b','')
bioinfo[1][5]= bioinfo[1][5].replace('Donald Jr.IvankaEricTiffanyBarron','Donald Jr., Ivanka, Eric, Tiffany, Barron')
bioinfo[1][6]= bioinfo[1][6].replace('TrumpMary','Trump, Mary')
bioinfo[1][10]= bioinfo[1][10].replace('Politicianbusinessmantelevision presenter','Politician, businessman, television presenter')
bioinfo[1][4] = bioinfo[1][4].replace('\u010d','')
filename = 'bioinfo.csv'
f = open(filename, 'w')
f.write(str(bioinfo[0]))
f.write(str(bioinfo[1]))
f.close

links = ["https://en.wikipedia.org/wiki/Category:21st-century_presidents_of_the_United_States","https://en.wikipedia.org/wiki/Category:21st-century_vice_presidents_of_the_United_States","https://en.wikipedia.org/wiki/Category:20th-century_presidents_of_the_United_States","https://en.wikipedia.org/wiki/Category:20th-century_vice_presidents_of_the_United_States","https://en.wikipedia.org/wiki/Category:20th-century_vice_presidents_of_the_United_States","https://en.wikipedia.org/wiki/Category:19th-century_vice_presidents_of_the_United_States"]
Politician_herf = []
for i in range(len(links)):
    y=0
    page = requests.get(links[i])
    webpage = page.content
    page.close()
    soup = bs4(webpage, 'html.parser')
    Politicians = soup.find("div",{"class": "mw-content-ltr"})
    #this statement gets just the herfs, I was having trouble doing it the other way so I figured out a differant way
    for herfs in Politicians.findAll("a",href=True):
        #The reason for this is because for some reason it included a bunch of links that I couldn't see when I inspected the page, so I did a bit of testing by printing all of them out along with the length of them and found that the longest link that I actually wanted was 37 characters
        if len(herfs.get('href')) <= 37:
            #this just makes them the full links becouse the herfs only had the second halfs of the links 
            Politician_herf.append('https://en.wikipedia.org/' + herfs.get('href'))
bioinfo2 = []
for i2 in range(len(Politician_herf)):
    page2 = requests.get(Politician_herf[i2])
    webpage2 = page2.content
    page2.close()
    soup2 = bs4(webpage2, 'html.parser')
    info_box = soup2.find("table",{"class": "infobox vcard"})
    biotags2 = info_box.findAll("th",{"class": "infobox-label"})
    biodata2 = info_box.findAll("td",{"class": "infobox-data"}) 
    bioinfo2.append(["Politician"])
    bioinfo2.append([soup2.h1.text])
    start = '<th class="infobox-label" scope="row">Born</th>'
    end = '<th class="infobox-label" scope="row">Occupation</th>'
    for i3 in range(len(biotags2)):
        if start == str(biotags2[i3]):
            x=i3
        elif end == str(biotags2[i3]):
            x1=i3
        else:
            #5 was the highest number that worked in any case where the string didn't match, and I didn't want to make a ton of elif statements
            x1=x+5
    while x <= x1:
        biotags1=biotags2[x].text
        biodata1=biodata2[x].text.replace('\n','').replace(',',':')
        x=x+1
        bioinfo2[-2].append(biotags1)
        bioinfo2[-1].append(biodata1)
df = pandas.DataFrame(bioinfo2)
df

csvfile = df.to_csv('df.csv')
pandas.read_csv('df.csv')