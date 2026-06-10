# SearchCompleteGenome

Finds **complete mitochondrial genomes** for a list of species in the
[NCBI Nucleotide](https://www.ncbi.nlm.nih.gov/nucleotide/) database, using Biopython's Entrez
interface.

For each species it runs a title-scoped query (to avoid false positives from whole nuclear genomes
that merely *contain* an annotated mitochondrion) and writes a CSV summarising whether a complete
mitochondrial genome exists, its publication date and the matching accessions.

> Part of the [search_engine](../README.md) toolkit.

## How it works

For every species in the input list it queries:

```text
"<species>"[Organism] AND (mitochondrion[Title] OR mitochondrial[Title])
AND ("complete genome"[Title] OR "complete sequence"[Title] OR "complete DNA"[Title])
```

It then fetches the summaries (up to 50 hits per species) and records the accessions and the
creation date. A mandatory `0.35 s` pause between requests respects the NCBI rate limit
(3 requests/second without an API key).

## Input

A plain-text file with **one species name per line**, e.g.:

```text
Colossoma macropomum
Arapaima gigas
Electrophorus electricus
```

The path is read from the `species_list_txt` environment variable.

## Output

`${PATH_RESULTS}/resultado_mitocondrias.csv` (default: `results/resultado_mitocondrias.csv`) with columns:

| Column | Description |
| --- | --- |
| `Specie_name` | Species queried |
| `NCBI` | `Sim` / `Não` / `Erro` — whether a complete mito genome was found |
| `data de publicação` | Creation date of the first hit (`YYYY/MM/DD`) |
| `lista de Accession` | `Accession(lengthbp)` entries, `;`-separated |

## Requirements

- Python ≥ 3.12
- [uv](https://docs.astral.sh/uv/)
- Dependencies (declared in `pyproject.toml`): `biopython`, `python-dotenv`

## Configuration

Create a `.env` at the repository root (see [`../.env.example`](../.env.example)):

| Variable | Required | Description |
| --- | --- | --- |
| `ENTREZ_EMAIL` | yes | Contact e-mail required by NCBI |
| `species_list_txt` | yes | Path to the species list file |
| `PATH_RESULTS` | no | Output directory (default `results`) |

## Usage

```bash
uv sync          # create the environment and install dependencies
uv run main.py   # run the search
```
