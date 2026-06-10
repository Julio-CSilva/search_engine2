# SearchBOLDSystems_COI

Checks the availability of **COI barcode sequences** (and other markers) for a list of species in
[BOLD Systems](https://www.boldsystems.org/), via the BOLD Portal public API.

For each species it queries the BOLD summary endpoint and reports whether a COI marker is present,
how many specimens are deposited and which markers are available.

> Part of the [search_engine](../README.md) toolkit.

## How it works

For every species it issues a taxonomic-scope query (`tax:species:<species>`) against:

```text
https://portal.boldsystems.org/api/summary
```

requesting the `specimens`, `marker_code` and `species` fields. The HTTP session is configured with
automatic retries (for `429/500/502/503/504`) and a `1.5 s` pause between species to respect the API.

## Input

A plain-text file with **one species name per line**, e.g.:

```text
Colossoma macropomum
Arapaima gigas
Electrophorus electricus
```

The path is read from the `species_list_txt` environment variable.

## Output

`${PATH_RESULTS}/resultados_coi.csv` (default: `results/resultados_coi.csv`) with columns:

| Column | Description |
| --- | --- |
| `nome_especie` | Species queried |
| `tem_coi_no_bold?` | `sim` / `não` / `erro` — COI marker present in BOLD |
| `total_especimes_depositados` | Number of specimens deposited |
| `marcadores_disponiveis` | Markers found (comma-separated) |
| `status_consulta` | `sucesso` / `não_encontrado` / `erro_conexão` |

## Requirements

- Python ≥ 3.12
- [uv](https://docs.astral.sh/uv/)
- Dependencies (declared in `pyproject.toml`): `requests`, `python-dotenv`

## Configuration

Create a `.env` at the repository root (see [`../.env.example`](../.env.example)):

| Variable | Required | Description |
| --- | --- | --- |
| `species_list_txt` | yes | Path to the species list file |
| `PATH_RESULTS` | no | Output directory (default `results`) |

## Usage

```bash
uv sync          # create the environment and install dependencies
uv run main.py   # run the search
```

> **Note:** the BOLD API response schema can change. The marker/specimen extraction logic in
> `check_bold_portal()` is defensive but may need adjustment if the API format evolves.
