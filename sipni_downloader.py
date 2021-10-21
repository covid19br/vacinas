#!/usr/bin/python3
import os, sys
from datetime import date, timedelta, datetime
from lxml import html 
import requests 
import re
import locale
locale.setlocale(locale.LC_TIME, "pt_BR")

def get_file(download_address, output_file):
    r = requests.get(download_address, verify=False, allow_redirects=True,
                     stream = True, timeout=100)
    print(f"=== download size: {round(int(r.headers.get('content-length')) / (1024*1024))} M ===\n")
    with open(output_file, 'wb') as f:
        # 100M chunk size
        chunk_size = 100 * 1024 * 1024
        for chunk in r.iter_content(chunk_size=chunk_size):
            f.write(chunk)

def get_UF_file(index_page_address, UF, output_file):
    page = requests.get(index_page_address, verify=False, timeout=10)
    tree = html.fromstring(page.content)
    e = tree.xpath(f'.//a[text()="Dados {UF}"]')
    if len(e) == 0:
        return False
    get_file(e[0].attrib['href'], output_file)
    return True

def get_date(index_page_address):
    page = requests.get(index_page_address, verify=False, timeout=10)
    tree = html.fromstring(page.content)
    data = tree.xpath('.//th[text()="Dados atualizados pela última vez"]')[0].getparent().getchildren()[1].text
    return datetime.strptime(data, "%d/%B/%Y")

def check_for_new_file(index_page_address, last_date):
    page = requests.get(index_page_address, verify=False, timeout=10)
    tree = html.fromstring(page.content)
    resources = tree.xpath('//li[@class="resource-item"]')
    reg = re.compile(r".*SRAG (\d\d/\d\d/\d\d\d\d).*",
            re.DOTALL|re.MULTILINE|re.IGNORECASE)
    for item in resources:
        g = reg.match(item.text_content())
        if g:
            data_read = datetime.strptime(g.groups()[0], "%d/%m/%Y").date()
            if data_read > last_date:
                address = item.xpath('.//a[@class="resource-url-analytics"]')[0].attrib['href']
                return (data_read, address)
    return False

if __name__ == '__main__':
    index_page_address = "https://opendatasus.saude.gov.br/dataset/covid-19-vacinacao/resource/ef3bd0b8-b605-474b-9ae5-c97390c197a8"
    output_folder = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'dados/')
    estados = ["AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
            "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO",
            "RR", "RS", "SC", "SE", "SP", "TO"]

    data = get_date(index_page_address)
    print(f'data da última atualização: {data.strftime("%Y-%m-%d")}')
    if len(sys.argv) == 1:
        print("USO: sipni_downloader [UF1] [UF2] ... | [todas]")
    else:
        UFs = sys.argv[1:]
        if 'todas' in UFs:
            UFs = estados
        for UF in UFs:
            if UF not in estados:
                print(f'\n   "{UF}" não é uma UF válida\n')
            print(f'=== baixando base de {UF} ===\n')
            fname = f'dados_{data.strftime("%Y-%m-%d")}_{UF}.csv'
            output_file = os.path.join(output_folder, fname)
            get_UF_file(index_page_address, UF, output_file)

