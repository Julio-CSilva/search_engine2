import argparse
import csv
import time
import os
from pathlib import Path
import requests
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry
from dotenv import load_dotenv, find_dotenv

# Load configuration from a local .env (searched from the repo root upward).
# Falls back to the legacy ../../samples/.env location for backward compatibility.
load_dotenv(find_dotenv(usecwd=True))
_legacy_env = os.path.join(os.path.dirname(__file__), '..', '..', 'samples', '.env')
if os.path.exists(_legacy_env):
    load_dotenv(_legacy_env, override=False)

# Endpoints base da API do BOLD Portal
BOLD_PREPROCESSOR_URL = "https://portal.boldsystems.org/api/query/preprocessor"
BOLD_SUMMARY_URL = "https://portal.boldsystems.org/api/summary"

def build_session() -> requests.Session:
    """Configura uma sessão HTTP com retentativas (retry)."""
    session = requests.Session()
    retry = Retry(
        total=3, 
        backoff_factor=1, 
        status_forcelist=[429, 500, 502, 503, 504]
    )
    adapter = HTTPAdapter(max_retries=retry)
    session.mount("http://", adapter)
    session.mount("https://", adapter)
    return session

def check_bold_portal(session: requests.Session, species: str) -> dict:
    """Consulta a API de summary do BOLD Portal para uma espécie."""
    
    # Etapa 1: Preprocessor (Opcional, mas recomendado pela documentação para validar a query)
    # A query procura no escopo taxonômico 'tax' no nível de 'species'
    raw_query = f"tax:species:{species}"
    
    # Etapa 2: Consulta o Summary
    # Buscamos os campos de interesse: specimens (contagem) e marker_code (genes)
    params = {
        "query": raw_query,
        "fields": "specimens,marker_code,species"
    }
    
    try:
        response = session.get(BOLD_SUMMARY_URL, params=params, timeout=15)
        response.raise_for_status()
        data = response.json()
        
        # A API retorna um dicionário vazio {} ou uma lista vazia [] se não encontrar nada?
        # Vamos assumir que ela retorna um JSON válido com contagens
        
        # Inicializa as variáveis padrão
        tem_coi = "não"
        total_specimens = 0
        marcadores_encontrados = []
        
        if data:
            # Precisamos extrair as informações baseadas na estrutura de resposta da API
            # Como não temos um exemplo do output exato no seu texto, 
            # esta lógica de extração pode precisar de ajustes dependendo do formato do JSON.
            
            # Tenta verificar se há espécimes listados
            if 'specimens' in data:
                # Pode ser um número total ou um dicionário de coleções
                if isinstance(data['specimens'], dict) and 'total' in data['specimens']:
                    total_specimens = data['specimens']['total']
                elif isinstance(data['specimens'], (int, float)):
                    total_specimens = data['specimens']
                elif isinstance(data['specimens'], list):
                    total_specimens = len(data['specimens'])
            
            # Verifica os marcadores presentes
            if 'marker_code' in data:
                # O BOLD usa 'COI-5P', 'COI', 'CO1'
                marcadores_encontrados = data['marker_code']
                
                # Se for uma lista de marcadores, verificamos se tem COI
                if isinstance(marcadores_encontrados, list):
                    if any("COI" in str(marker).upper() for marker in marcadores_encontrados):
                        tem_coi = "sim"
                # Se for um dicionário de contagem de marcadores (ex: {'COI-5P': 10})
                elif isinstance(marcadores_encontrados, dict):
                    if any("COI" in str(marker).upper() for marker in marcadores_encontrados.keys()):
                        tem_coi = "sim"
                        marcadores_encontrados = list(marcadores_encontrados.keys())

        return {
            "tem_coi": tem_coi,
            "total_specimens": total_specimens,
            "marcadores": ", ".join(map(str, marcadores_encontrados)) if marcadores_encontrados else "Nenhum",
            "status": "sucesso"
        }

    except requests.exceptions.JSONDecodeError:
         # O servidor não retornou um JSON válido (mesmo erro da v4)
         return {"tem_coi": "não", "total_specimens": 0, "marcadores": "N/A", "status": "não_encontrado"}
    except requests.exceptions.RequestException as e:
        print(f"Erro na requisição para '{species}': {e}")
        return {"tem_coi": "erro", "total_specimens": "erro", "marcadores": "erro", "status": "erro_conexão"}

def main(input_path: Path, output_path: Path):
    session = build_session()
    
    with open(input_path, "r", encoding="utf-8") as file:
        species_list = [line.strip() for line in file if line.strip()]
        
    print(f"Iniciando busca no BOLD Portal para {len(species_list)} espécies...")
    
    with open(output_path, "w", encoding="utf-8", newline="") as csvfile:
        writer = csv.writer(csvfile)
        # Cabeçalhos atualizados com mais detalhes extraídos do summary
        writer.writerow([
            "nome_especie", 
            "tem_coi_no_bold?", 
            "total_especimes_depositados", 
            "marcadores_disponiveis",
            "status_consulta"
        ])
        
        for species in species_list:
            resultado = check_bold_portal(session, species)
            writer.writerow([
                species, 
                resultado["tem_coi"], 
                resultado["total_specimens"], 
                resultado["marcadores"],
                resultado["status"]
            ])
            
            print(f"[{resultado['tem_coi'].upper()}] {species} | Espécimes: {resultado['total_specimens']}")
            time.sleep(1.5) # Respeitando a API
            
    print(f"\nBusca finalizada. Resultados salvos em: {output_path}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Busca a presença de sequências COI por espécie no BOLD Systems.")
    args = parser.parse_args()

    species_list = os.getenv("species_list_txt")
    path_results = os.getenv("PATH_RESULTS", "results")
    os.makedirs(path_results, exist_ok=True)
    output_path = os.path.join(path_results, "resultados_coi.csv")

    main(species_list, output_path)