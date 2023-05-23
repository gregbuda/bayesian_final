####################### WEB SCRAPING OF HABITACLIA ###################

from bs4 import BeautifulSoup
import requests
from fake_useragent import UserAgent
import re
import pandas as pd
import os
import numpy as np
#from selenium import webdriver
ua= UserAgent()
header = {'User-Agent':ua.random}


#Get number of pages
url = "https://www.habitaclia.com/viviendas-en-barcelones.htm"
response = requests.get(url, headers=header)
data = response.text
soup = BeautifulSoup(data, 'html.parser')
page_toggle=soup.find('li',{'class':'next'}).previous_sibling.previous_sibling
max_page=int(page_toggle.text.strip())

#Each page is a concatenation of the url and page number minus 1
first_page="https://www.habitaclia.com/viviendas-en-barcelones.htm"
other_pages=["https://www.habitaclia.com/viviendas-en-barcelones-"+str(i)+".htm" for i in range(1,max_page)]
all_pages=[first_page]+other_pages

#Save all data in this list
data_all=[] 

#Loop for all pages ############################################
for page in all_pages:
    response = requests.get(page, headers=header)
    data = response.text
    soup = BeautifulSoup(data, 'html.parser')
    
    #Get list of hrefs
    all_lists=soup.find_all('h3', {'class':'list-item-title'})
    hrefs=[]
    for lista in all_lists:
        hrefs.append(lista.a['href'])
    
    #Loop through each page's hrefs = 1 flat ############################
    for href in hrefs:
        #href='https://www.habitaclia.com/comprar-piso-de_3_dormitorios_con_terraza_de_6m2_en_venta_en_el_eixample_l_antiga_esquerra_de_l_eixample-barcelona-i4975004332902.htm?f=&geo=c&from=list&lo=55'
        ident+=1
        
        try:      
            #Reset key fields
            no_rooms=np.nan
            surface=np.nan
            bathroom=np.nan
    
            #Connect to server
            url = href
            response = requests.get(url, headers=header)
            data = response.text
            soup = BeautifulSoup(data, 'html.parser')
    
            #Find price, region...
            price = soup.find("span", {"itemprop": "price"})
            #Not always there         
            if not price is None:
                price=int(price.text.replace('€','').replace('.',''))
            else:
                price=np.nan
            
            barrio = soup.find("a", {"id": "js-ver-mapa-zona"})
            #Not always a good barrio description - make exception
            if not barrio is None:
                barrio=barrio.text.strip()
            else:
                barrio=np.nan
            #Save short description (in bald) and long description
            details=soup.find('section', {"class":"detail"})
            main_desc = details.h3.text
            long_desc = details.h3.next_sibling.next_sibling.text
            #main_desc = soup.body.h3.string
            #long_desc = soup.body.h3.next_sibling.next_sibling.string
            
            #Save some useful labels in a list
            label_list=[]
            
            #First section: extract the number of rooms, surface and no of bathrooms
            #Save other tags
            distribucion =details.h3.parent.next_sibling.next_sibling.ul
            if distribucion != None:
                for child in distribucion.children: #contents
                    if 'habitaciones' in child.text.lower():
                        no_rooms = int(child.text.split()[0])
                    elif 'superficie' in child.text.lower():
                        surface = int(child.text.split()[1])
                    elif ('baños' in child.text.lower()) | ('baño' in child.text.lower()):
                        bathroom = int(child.text.split()[0])
                    elif (child.text.lower()!='\n'):
                        if child.text.split()[0]!='Certificado':
                            label_list.append(child.text.strip())
                
                #Second section: save tags that are not related to electric certificate
                caracteristicas = distribucion.parent.next_sibling.next_sibling.ul
                if caracteristicas != None:
                    li_tags = caracteristicas.find_all('li')
                    for child in li_tags: #contents
                        if (child.text.lower()!='\n') & (child.text.split()[0]!='Certificado'):
                            label_list.append(child.text.strip())
                
                #Third section: save tags
                if caracteristicas != None:
                    equipamiento = caracteristicas.parent.next_sibling.next_sibling.ul
                    if equipamiento != None:
                        li_tags = equipamiento.find_all('li')
                        for child in li_tags: #contents
                            if (child.text.lower()!='\n'):
                                label_list.append(child.text.strip())
                
                #All the data together
                data_i = [ident, price, barrio, no_rooms, surface, bathroom, label_list, main_desc, long_desc, href]
                data_all.append(data_i)
        except:
            pass
        
        print(f"Done with flat {ident}")


############################################################################


data_all=pd.DataFrame(data_all)
data_all.columns=['id','price','barrio','no_rooms','surface','bathroom','labels','main_desc','long_desc','href']
data_all.to_excel('C:/Users/gregb/OneDrive/Asztali gép/UPC_master/BAY/Final Project/Webscrape/data2.xlsx',index=False)
data_all.to_csv('C:/Users/gregb/OneDrive/Asztali gép/UPC_master/BAY/Final Project/Webscrape/data2.csv',index=False)
data_all.to_pickle("C:/Users/gregb/OneDrive/Asztali gép/UPC_master/BAY/Final Project/Webscrape/data2.pkl")                    


