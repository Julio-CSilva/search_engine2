import requests
import xml.etree.ElementTree as ET
import time
import os
import csv
import sys
import io
from dotenv import load_dotenv, find_dotenv

# Load configuration from a local .env (searched from the repo root upward).
# Falls back to the legacy ../../samples/.env location for backward compatibility.
load_dotenv(find_dotenv(usecwd=True))
_legacy_env = os.path.join(os.path.dirname(__file__), '..', '..', 'samples', '.env')
if os.path.exists(_legacy_env):
    load_dotenv(_legacy_env, override=False)
Species_list = os.getenv("sra_list_orderby_nameSpecies_az_txt")  # Path to the SRA list file
PATH_RESULTS = os.getenv("PATH_RESULTS", "results")

def formatar_bases(bases_str):
    """Converte o número total de bases para a escala (K, M, G)."""
    if not bases_str or bases_str == "N/A": return "N/A"
    try:
        bases = float(bases_str)
        if bases >= 1e9: return f"{bases/1e9:.1f}G"
        if bases >= 1e6: return f"{bases/1e6:.1f}M"
        if bases >= 1e3: return f"{bases/1e3:.1f}K"
        return str(int(bases))
    except ValueError:
        return bases_str

def formatar_tamanho_mb(size_mb_str):
    """Converte o tamanho em Megabytes retornado pelo RunInfo para Gb/Mb."""
    if not size_mb_str or size_mb_str == "N/A": return "N/A"
    try:
        size_mb = float(size_mb_str)
        # O NCBI na interface web usa o divisor de 1000 para formatar Gb.
        if size_mb >= 1000:
            return f"{size_mb/1000:.1f}Gb"
        return f"{size_mb:.1f}Mb"
    except ValueError:
        return size_mb_str

def buscar_dados_sra(sra_id):
    sra_id = sra_id.strip()
    
    # 1. Obter o ID interno do NCBI usando ESearch
    search_url = f"https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=sra&term={sra_id}&retmode=json"
    try:
        search_res = requests.get(search_url, timeout=10).json()
        id_list = search_res.get("esearchresult", {}).get("idlist", [])
        
        if not id_list:
            return {"SRA": sra_id, "Erro": "Não encontrado no NCBI"}
        internal_id = id_list[0]
    except Exception as e:
        return {"SRA": sra_id, "Erro": f"Erro na busca inicial: {e}"}

    # 2. Obter os metadados nativos e estruturados via EFetch (RunInfo)
    # O RunInfo retorna um CSV direto do banco de dados do NCBI
    fetch_url = f"https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=sra&id={internal_id}&rettype=runinfo&retmode=text"
    try:
        fetch_res = requests.get(fetch_url, timeout=15)
        
        # Lê a resposta como um arquivo CSV em memória
        leitor_csv = csv.DictReader(io.StringIO(fetch_res.text.strip()))
        
        # Captura a primeira linha de dados correspondente à run
        linha_runinfo = next(leitor_csv, None)
        
        if not linha_runinfo:
            return {"SRA": sra_id, "Erro": "Metadados de RunInfo vazios no NCBI"}
            
        # 3. Extrair os valores exatos
        # A coluna 'ReleaseDate' representa quando o dado se tornou público ("Published")
        dados = {
            "SRA": sra_id,
            "Species name": linha_runinfo.get("ScientificName", "N/A"),
            "BioProject": linha_runinfo.get("BioProject", "N/A"),
            "BioSample": linha_runinfo.get("BioSample", "N/A"),
            "# of Bases": formatar_bases(linha_runinfo.get("bases", "N/A")),
            "Size": formatar_tamanho_mb(linha_runinfo.get("size_MB", "N/A")),
            "Published": linha_runinfo.get("ReleaseDate", "N/A").split(" ")[0] if linha_runinfo.get("ReleaseDate") else "N/A"
        }
        
        return dados
        
    except Exception as e:
        return {"SRA": sra_id, "Erro": f"Erro de rede no EFetch: {e}"}

def processar_lista(arquivo_entrada, arquivo_saida):
    try:
        with open(arquivo_entrada, 'r') as f:
            sras = [linha.strip() for linha in f if linha.strip()]
    except FileNotFoundError:
        print(f"Erro: O arquivo '{arquivo_entrada}' não foi encontrado.")
        sys.exit(1)

    print(f"Encontrados {len(sras)} SRA(s) para processar. Iniciando busca segura...\n")

    fieldnames = ["SRA", "Species name", "BioProject", "BioSample", "# of Bases", "Size", "Published"]

    with open(arquivo_saida, 'w', newline='', encoding='utf-8') as f_out:
        writer = csv.DictWriter(f_out, fieldnames=fieldnames)
        writer.writeheader()

        for i, sra in enumerate(sras, 1):
            print(f"[{i}/{len(sras)}] Consultando: {sra}...")
            resultado = buscar_dados_sra(sra)
            
            if "Erro" in resultado:
                print(f"  -> Falha: {resultado['Erro']}")
                linha_erro = {key: (resultado['Erro'] if key != "SRA" else sra) for key in fieldnames}
                writer.writerow(linha_erro)
            else:
                print(f"  -> Sucesso: {resultado['Size']} | Publicado em: {resultado['Published']}")
                writer.writerow(resultado)
            
            # Pausa de 0.4s para respeitar o limite de 3 requisições/segundo da API
            time.sleep(0.4) 

    print(f"\nConcluído! Os resultados foram salvos em '{arquivo_saida}'.")

if __name__ == "__main__":
    output_dir = os.path.join(PATH_RESULTS, "Table")
    os.makedirs(output_dir, exist_ok=True)
    processar_lista(Species_list, os.path.join(output_dir, "resultados_infotable.csv"))