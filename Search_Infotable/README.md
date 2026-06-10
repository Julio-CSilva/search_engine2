# Search_Infotable

Builds an **information table** of NCBI **SRA** runs from a list of SRA accessions, using the NCBI
[E-utilities](https://www.ncbi.nlm.nih.gov/books/NBK25501/) (`esearch` + `efetch` with `runinfo`).

For each accession it resolves the internal NCBI UID and fetches the structured RunInfo record,
extracting the most relevant metadata into a single CSV.

> Part of the [search_engine](../README.md) toolkit.

## How it works

For each SRA accession it:

1. Resolves the internal UID via `esearch` (`db=sra`, JSON).
2. Fetches `rettype=runinfo` via `efetch` (a CSV straight from the SRA database).
3. Extracts the run fields and normalises base counts (`K/M/G`) and size (`Mb/Gb`).

A `0.4 s` pause between requests respects the NCBI rate limit (3 requests/second without an API key).

## Input

A plain-text file with **one SRA accession per line**, e.g.:

```text
SRR12345678
SRR23456789
ERR34567890
```

The path is read from the `sra_list_orderby_nameSpecies_az_txt` environment variable.

## Output

`${PATH_RESULTS}/Table/resultados_infotable.csv` (default: `results/Table/resultados_infotable.csv`)
with columns:

| Column | Description |
| --- | --- |
| `SRA` | Accession queried |
| `Species name` | Scientific name (`ScientificName`) |
| `BioProject` | BioProject accession |
| `BioSample` | BioSample accession |
| `# of Bases` | Total bases, human-readable (`K/M/G`) |
| `Size` | Download size (`Mb/Gb`) |
| `Published` | Release date (`YYYY-MM-DD`) |

Rows for accessions that fail are still written, with the error message in place of the values.

## Requirements

- Python ≥ 3.12
- [uv](https://docs.astral.sh/uv/)
- Dependencies (declared in `pyproject.toml`): `requests`, `python-dotenv`

## Configuration

Create a `.env` at the repository root (see [`../.env.example`](../.env.example)):

| Variable | Required | Description |
| --- | --- | --- |
| `sra_list_orderby_nameSpecies_az_txt` | yes | Path to the SRA accession list |
| `PATH_RESULTS` | no | Output base directory (default `results`) |

## Usage

```bash
uv sync          # create the environment and install dependencies
uv run main.py   # run the search
```
