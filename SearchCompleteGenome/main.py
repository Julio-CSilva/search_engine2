import csv
import time
import os
import urllib.error
from Bio import Entrez
from dotenv import load_dotenv, find_dotenv


# Load configuration from a local .env (searched from the repo root upward).
# Falls back to the legacy ../../samples/.env location for backward compatibility.
load_dotenv(find_dotenv(usecwd=True))
_legacy_env = os.path.join(os.path.dirname(__file__), '..', '..', 'samples', '.env')
if os.path.exists(_legacy_env):
    load_dotenv(_legacy_env, override=False)

# NCBI requires a contact e-mail for excessive-use notices. Set ENTREZ_EMAIL in your .env.
Entrez.email = os.getenv("ENTREZ_EMAIL")
Species_list = os.getenv("species_list_txt")  # Path to the species list file
PATH_RESULTS = os.getenv("PATH_RESULTS", "results")

def buscar_mitocondria(especie):
    """
    Busca genomas mitocondriais completos de uma espécie no NCBI Nucleotide.
    """
    # Query otimizada buscando no título para evitar falsos positivos de genomas nucleares inteiros
    # que apenas 'contêm' a mitocôndria anotada.
    query = (
        f'"{especie}"[Organism] AND '
        f'(mitochondrion[Title] OR mitochondrial[Title]) AND '
        f'("complete genome"[Title] OR "complete sequence"[Title] OR "complete DNA"[Title])'
    )

    try:
        # 1. Fazer a busca para recuperar os IDs
        handle = Entrez.esearch(db="nucleotide", term=query, retmax=50) # Traz até 50 hits por espécie
        record = Entrez.read(handle)
        handle.close()

        id_list = record["IdList"]

        # Se não encontrou nada
        if not id_list:
            return {"ncbi": "Não", "data": "-", "accessions": "-"}

        # 2. Buscar os metadados (resumos) usando os IDs encontrados
        handle_summary = Entrez.esummary(db="nucleotide", id=",".join(id_list))
        summaries = Entrez.read(handle_summary)
        handle_summary.close()

        accessions_formatados = []
        datas_publicacao = []

        for docsum in summaries:
            # Extraindo informações do summary do NCBI
            acc = docsum.get("Caption", "")
            tamanho = docsum.get("Length", "")
            data = docsum.get("CreateDate", "") # Data de criação no banco YYYY/MM/DD

            if acc and tamanho:
                accessions_formatados.append(f"{acc}({tamanho}bp)")
            if data and data not in datas_publicacao:
                datas_publicacao.append(data)

        # Se houver múltiplas datas, pegamos a da publicação mais antiga (primeira vez publicado) 
        # ou a mais recente. Aqui estou pegando a primeira do array (geralmente a mais relevante do hit).
        data_final = datas_publicacao[0] if datas_publicacao else "-"

        return {
            "ncbi": "Sim",
            "data": data_final,
            "accessions": ";".join(accessions_formatados)
        }

    except urllib.error.HTTPError as e:
        print(f"Erro de conexão com NCBI para {especie}: {e}")
        return {"ncbi": "Erro", "data": "Erro HTTP", "accessions": "Erro"}
    except Exception as e:
        print(f"Erro inesperado ao buscar {especie}: {e}")
        return {"ncbi": "Erro", "data": "Erro", "accessions": "Erro"}
    finally:
        # Pausa obrigatória de 0.35s para não sobrecarregar o NCBI e evitar bloqueio de IP.
        # (O limite do NCBI é 3 requisições por segundo sem API Key)
        time.sleep(0.35)

def gerar_relatorio_csv(lista_especies, arquivo_saida):
    with open(arquivo_saida, mode='w', newline='', encoding='utf-8') as f:
        writer = csv.writer(f, delimiter=',')
        
        # Cabeçalho exigido
        writer.writerow(["Specie_name", "NCBI", "data de publicação", "lista de Accession"])

        for i, especie in enumerate(lista_especies, 1):
            print(f"[{i}/{len(lista_especies)}] Buscando: {especie}...")
            resultado = buscar_mitocondria(especie)
            
            writer.writerow([
                especie,
                resultado["ncbi"],
                resultado["data"],
                resultado["accessions"]
            ])
            
    print(f"\nBusca finalizada com sucesso! Relatório gerado: '{arquivo_saida}'.")

if __name__ == "__main__":
    # Lista de teste (incluindo alguns peixes amazônicos e um caso negativo)
    list_species = []
    with open(Species_list, "r") as file:
        for line in file:
            print(line)
            list_species.append(line.strip())

    # Output file (directory comes from PATH_RESULTS, defaults to ./results)
    os.makedirs(PATH_RESULTS, exist_ok=True)
    nome_csv = os.path.join(PATH_RESULTS, "resultado_mitocondrias.csv")

    gerar_relatorio_csv(list_species, nome_csv)