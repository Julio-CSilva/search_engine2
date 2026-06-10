# Search-Engine

The original **SRA search engine**: a small collection of R and Python scripts used to bulk-search
the NCBI **SRA** database, extract structured metadata from the raw experiment XML, and build
candidate tables for a target time window / location.

> Part of the [search_engine](../README.md) toolkit. Unlike the other tools, this folder is a set of
> standalone scripts (no `uv` project).

## Contents

| File | Language | Purpose |
| --- | --- | --- |
| `searchSRA.R` | R | Bulk SRA search via `rentrez`, parses each `EXPERIMENT_PACKAGE` from the result XML into a table and filters candidates by date. |
| `searchSRA.py` | Python | Minimal Biopython example of an SRA `esearch` query. |
| `species_scraping.py` | Python | Extracts species names from a saved BOLD *Classification Tree* HTML page. |
| `features_xml.ipynb` | Jupyter | Inspects the set of tags/values present in a result XML (for exploring the schema). |
| `results/` | — | Example outputs (CSV/XML/TXT). |

## `searchSRA.R`

Searches SRA (e.g. *Homo sapiens* samples from Brazil with mutation/variant terms), fetches the XML
for every hit using the web history, and parses fields such as city, primary id, collection date,
institute, instrument model, library strategy/source/selection into a data frame. It then writes the
full table plus a date-filtered "candidates" subset.

**Outputs** (in `$SEARCH_ENGINE_DIR`, default `results/`): `resultXML.xml`, `resultTable.csv`,
`candidatos.csv`.

**Requirements:** R with the `rentrez` and `XML` packages:

```r
install.packages(c("rentrez", "XML"))
```

**Configuration (environment variables):**

| Variable | Required | Description |
| --- | --- | --- |
| `ENTREZ_API_KEY` | no | NCBI API key; raises the rate limit to 10 req/s. |
| `SEARCH_ENGINE_DIR` | no | Output directory (default `results`). |

**Run:**

```bash
export ENTREZ_API_KEY="your_key_here"   # optional
Rscript searchSRA.R
```

## `searchSRA.py`

A minimal Biopython snippet that runs a single SRA `esearch` and prints the result. Reads the contact
e-mail from the `ENTREZ_EMAIL` environment variable.

```bash
pip install biopython
export ENTREZ_EMAIL="you@example.com"
python searchSRA.py
```

## `species_scraping.py`

Parses `results/ClassificationTree.html` (a saved BOLD classification-tree page) with BeautifulSoup
and writes every linked taxon name to `results/speciesNames.txt`.

```bash
pip install beautifulsoup4
python species_scraping.py
```

## `features_xml.ipynb`

A notebook helper that walks a result XML and collects the unique tags and values, useful for
understanding the SRA XML schema before writing new extraction logic.

## Security note

Earlier versions of `searchSRA.R` contained a hardcoded NCBI API key. It has been removed in favour
of the `ENTREZ_API_KEY` environment variable. **Never commit API keys.**
