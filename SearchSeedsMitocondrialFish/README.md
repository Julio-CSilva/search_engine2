# SearchSeedsMitocondrialFish

Downloads a **mitochondrial "seed" sequence** (FASTA) for each organism in a list, to be used as a
reference/bait for downstream mitogenome assembly (e.g. NOVOPlasty, GetOrganelle).

It uses NCBI [**Entrez Direct**](https://www.ncbi.nlm.nih.gov/books/NBK179288/)
(`esearch` / `efetch` / `xtract`) and a **tiered fallback strategy** so that, even when an exact
species has no data, a close relative is used instead.

> Although the folder name mentions *fish*, the script is taxon-agnostic and works for any organism
> (the bundled example list is made of plants).
>
> Part of the [search_engine](../README.md) toolkit.

## Search strategy (in order, stops at first hit)

1. **Complete mitochondrial genome for the species** — prefers a RefSeq (`NC_`) accession, otherwise
   the first available complete genome.
2. **Gene list for the species** — `COI`, `COX1`, `cytochrome b`, `cytb`, `ND2`, `NADH`,
   `16S`/`12S ribosomal RNA`, etc.
3. **Genus level** — repeats the mitogenome and gene searches for the genus of the species.
4. **Family level** — resolves the family via the taxonomy database and repeats the searches.

If nothing is found at any level, a final failure is logged for that organism. Organisms whose seed
file already exists and is non-empty are skipped, so the script is safe to re-run.

## Prerequisites

NCBI **Entrez Direct** must be installed and on your `PATH`. The script checks for `esearch`,
`efetch` and `xtract` and aborts if any is missing.

```bash
sh -c "$(curl -fsSL https://ftp.ncbi.nlm.nih.gov/entrez/entrezdirect/install-edirect.sh)"
```

(Optional) Set an NCBI API key to raise the rate limit:

```bash
export NCBI_API_KEY="your_key_here"
```

## Input

A plain-text file with **one organism per line**, genus and species joined by `_`, e.g.
([`specie_samplesList.txt`](specie_samplesList.txt)):

```text
Cenchrus_purpureus
Cyperus_haspan
Heliotropium_indicum
Lemna_valdiviana
Urospatha_sagittifolia
```

## Output

For each organism, written under the output directory you pass as the second argument:

- `<Organism>_seed.fa` — the seed sequence in FASTA format.
- `logs/<Organism>.log` — a per-organism log of every attempt.

The bundled [`seeds/`](seeds/) directory contains example outputs.

## Usage

```bash
bash seedSearch.sh <sample_file.txt> <output_directory>
```

Example:

```bash
bash seedSearch.sh specie_samplesList.txt seeds
```
