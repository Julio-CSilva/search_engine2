# search_engine

**Português** · [English](README.en.md)

Conjunto de ferramentas de bioinformática para **buscar e catalogar dados de sequências** em bancos
públicos (NCBI e BOLD Systems), com foco em **genomas e marcadores mitocondriais** de espécies
aquáticas amazônicas. Cada ferramenta é independente e resolve uma etapa da prospecção de dados.

## Ferramentas

| Ferramenta | Tipo | O que faz |
| --- | --- | --- |
| [Search-Engine](Search-Engine/README.md) | R + Python | Busca em massa no SRA e extração de metadados a partir do XML de experimentos. |
| [SearchCompleteGenome](SearchCompleteGenome/README.md) | Python (uv) | Procura genomas mitocondriais **completos** por espécie no NCBI Nucleotide. |
| [SearchBOLDSystems_COI](SearchBOLDSystems_COI/README.md) | Python (uv) | Verifica a presença do marcador **COI** por espécie no BOLD Systems. |
| [Search_Infotable](Search_Infotable/README.md) | Python (uv) | Monta uma tabela de metadados de runs do **SRA** (BioProject, bases, tamanho, data). |
| [SearchSeedsMitocondrialFish](SearchSeedsMitocondrialFish/README.md) | Bash | Baixa uma sequência **"seed"** mitocondrial por organismo (busca em cascata via Entrez Direct). |

## Pré-requisitos

Dependendo das ferramentas que for usar:

- **Python ≥ 3.12** e [uv](https://docs.astral.sh/uv/) — para as ferramentas em Python.
- **R** com os pacotes `rentrez` e `XML` — para o `Search-Engine/searchSRA.R`.
- **NCBI [Entrez Direct](https://www.ncbi.nlm.nih.gov/books/NBK179288/)** (`esearch`, `efetch`,
  `xtract`) — para o `SearchSeedsMitocondrialFish`.

## Configuração

As ferramentas em Python leem a configuração de um arquivo `.env`. Copie o modelo e preencha com os
seus valores:

```bash
cp .env.example .env
```

Variáveis disponíveis (veja [`.env.example`](.env.example)):

| Variável | Descrição |
| --- | --- |
| `ENTREZ_EMAIL` | E-mail de contato exigido pelo NCBI. |
| `ENTREZ_API_KEY` | (Opcional) Chave de API do NCBI; aumenta o limite para 10 req/s. |
| `species_list_txt` | Caminho do arquivo com a lista de espécies. |
| `sra_list_orderby_nameSpecies_az_txt` | Caminho do arquivo com a lista de acessos SRA. |
| `PATH_RESULTS` | Diretório base de saída (padrão: `results`). |

## Uso rápido

Cada ferramenta tem o seu próprio README com instruções detalhadas. Exemplo para as ferramentas
baseadas em `uv`:

```bash
cd SearchCompleteGenome
uv sync
uv run main.py
```

## Segurança

- **Nunca** faça commit do arquivo `.env`, de chaves de API ou de e-mails pessoais. O `.env` já está
  no [`.gitignore`](.gitignore).
- Não há credenciais no código: e-mails, chaves e caminhos vêm de variáveis de ambiente.
- Respeite os limites de uso das APIs do NCBI e do BOLD (as ferramentas já aplicam pausas entre as
  requisições).

## Licença

Distribuído sob a licença [MIT](LICENSE). © 2026 Júlio C. Silva.
