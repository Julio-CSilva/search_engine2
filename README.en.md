# search_engine

[Português](README.md) · **English**

A set of bioinformatics tools for **searching and cataloguing sequence data** in public databases
(NCBI and BOLD Systems), focused on **mitochondrial genomes and markers** of Amazonian aquatic
species. Each tool is self-contained and solves one step of the data-prospecting workflow.

## Tools

| Tool | Type | What it does |
| --- | --- | --- |
| [Search-Engine](Search-Engine/README.md) | R + Python | Bulk SRA search and metadata extraction from experiment XML. |
| [SearchCompleteGenome](SearchCompleteGenome/README.md) | Python (uv) | Finds **complete** mitochondrial genomes per species in NCBI Nucleotide. |
| [SearchBOLDSystems_COI](SearchBOLDSystems_COI/README.md) | Python (uv) | Checks **COI** marker availability per species in BOLD Systems. |
| [Search_Infotable](Search_Infotable/README.md) | Python (uv) | Builds a metadata table of **SRA** runs (BioProject, bases, size, date). |
| [SearchSeedsMitocondrialFish](SearchSeedsMitocondrialFish/README.md) | Bash | Downloads a mitochondrial **"seed"** sequence per organism (tiered Entrez Direct search). |

## Prerequisites

Depending on which tools you use:

- **Python ≥ 3.12** and [uv](https://docs.astral.sh/uv/) — for the Python tools.
- **R** with the `rentrez` and `XML` packages — for `Search-Engine/searchSRA.R`.
- **NCBI [Entrez Direct](https://www.ncbi.nlm.nih.gov/books/NBK179288/)** (`esearch`, `efetch`,
  `xtract`) — for `SearchSeedsMitocondrialFish`.

## Configuration

The Python tools read their configuration from a `.env` file. Copy the template and fill in your
values:

```bash
cp .env.example .env
```

Available variables (see [`.env.example`](.env.example)):

| Variable | Description |
| --- | --- |
| `ENTREZ_EMAIL` | Contact e-mail required by NCBI. |
| `ENTREZ_API_KEY` | (Optional) NCBI API key; raises the limit to 10 req/s. |
| `species_list_txt` | Path to the species list file. |
| `sra_list_orderby_nameSpecies_az_txt` | Path to the SRA accession list file. |
| `PATH_RESULTS` | Base output directory (default: `results`). |

## Quick start

Each tool has its own README with detailed instructions. Example for the `uv`-based tools:

```bash
cd SearchCompleteGenome
uv sync
uv run main.py
```

## Security

- **Never** commit the `.env` file, API keys or personal e-mails. `.env` is already in
  [`.gitignore`](.gitignore).
- No credentials live in the code: e-mails, keys and paths all come from environment variables.
- Respect the NCBI and BOLD API usage limits (the tools already throttle their requests).

## License

Released under the [MIT](LICENSE) license. © 2026 Júlio C. Silva.
