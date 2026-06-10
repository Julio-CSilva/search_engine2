import os
from bs4 import BeautifulSoup

# Path to the local HTML file (BOLD classification-tree export)
file_path = os.path.join('results', 'ClassificationTree.html')

# Ler o conteúdo do arquivo HTML
with open(file_path, 'r', encoding='utf-8') as file:
    html_content = file.read()

# Analisar o HTML usando BeautifulSoup
soup = BeautifulSoup(html_content, 'html.parser')

# Encontrar todas as tags <a> com o atributo href
links = soup.find_all('a', href=True)

# Open the output file to save the names
arquivo = open(os.path.join('results', 'speciesNames.txt'), "w")

# Extrair e imprimir o conteúdo das tags <a> com href
for link in links:
    arquivo.write(link.text + "\n")
arquivo.close()