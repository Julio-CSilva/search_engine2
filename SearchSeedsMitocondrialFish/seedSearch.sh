#!/usr/bin/env bash

# Exit immediately if a command exits with a non-zero status or if a command in a pipe fails.
set -e -o pipefail

# --- Settings and Constants ---
# List of genes for the secondary search.
readonly GENES_LIST=(
    "COI"
    "cytochrome c oxidase subunit I"
    "COX1"
    "cytochrome b"
    "cytb"
    "ND2"
    "NADH"
    "16S ribosomal RNA"
    "12S ribosomal RNA"
)

# --- Logging Functions ---
# Global variable for the current genus's log file.
LOG_FILE=""

# Function to log INFO messages.
log_info() {
    # Prints to the console and appends to the sample's log file.
    echo "INFO: [$(date '+%Y-%m-%d %H:%M:%S')] $*" | tee -a "${LOG_FILE}"
}

# Function to log ERROR messages.
log_error() {
    # Prints to stderr and appends to the sample's log file.
    echo "ERROR: [$(date '+%Y-%m-%d %H:%M:%S')] $*" | tee -a "${LOG_FILE}" >&2
}
# --- End of Logging Functions ---


# Function to display usage and exit.
usage() {
    echo "ERROR: Invalid arguments."
    echo "Usage: $0 <sample_file.txt> <output_directory>"
    exit 1
}

# Function to check if dependencies (Entrez Direct) are installed.
check_dependencies() {
    local deps=("esearch" "efetch" "xtract")
    for dep in "${deps[@]}"; do
        if ! command -v "${dep}" &> /dev/null; then
            echo "ERROR: Dependency '${dep}' not found. Please install NCBI Entrez Direct."
            exit 1
        fi
    done
}

# Function to process seeds.
process_seeds() {
    local SEED="$1"
    mkdir -p "${OUTPUT_DIR}/logs"
    local LOGS_DIR="${OUTPUT_DIR}/logs"

    local SEED_FILE="${OUTPUT_DIR}/${SEED}_seed.fa"
    LOG_FILE="${LOGS_DIR}/${SEED}.log"
    
    # Clear the old log file if it exists, for a fresh log on each run.
    > "${LOG_FILE}"

    log_info "------------------------------------------------------------------"
    log_info "Processing SEED: ${SEED}"

    if [ -s "$SEED_FILE" ]; then
        log_info "Seed file already exists and is not empty. Skipping."
        return
    fi

    # ATTEMPT 1: Search for a complete mitochondrial genome for the SEED.
    log_info "--> Attempt 1: Searching for Complete Mitochondrial Genome [SEED: ${SEED}]"
    local MITO_QUERY="${SEED}[Organism] AND mitochondrion[All Fields] AND complete genome[All Fields] AND NC_0:NC_9[PACC]"
    # First, try to find a RefSeq (NC_). If not found, get the first result.

    local REFSEQ_ID
    REFSEQ_ID=$(esearch -db nuccore -query "${MITO_QUERY}" | efetch -format docsum | xtract -pattern DocumentSummary -element Caption)

    if [[ -n "$REFSEQ_ID" ]]; then
        log_info "--> Success! Reference genome (RefSeq ID: ${REFSEQ_ID}) found."
        efetch -db nuccore -id "${REFSEQ_ID}" -format fasta > "${SEED_FILE}"
    else
        MITO_QUERY="${SEED}[Organism] AND mitochondrion[All Fields] AND complete genome[All Fields]"
        log_info "--> No reference genome (NC_) found. Trying any complete genome."
        esearch -db nuccore -query "${MITO_QUERY}" | efetch -format fasta -stop 1 > "${SEED_FILE}"
    fi

    if [ -s "$SEED_FILE" ]; then
        log_info "--> SUCCESS! Mitochondrial Genome seed [SEED] found and saved."
        return
    fi
    log_info "--> Attempt 1 failed."


    # ATTEMPT 2: Search for the gene list for the SEED.
    log_info "--> Attempt 2: Searching by Gene List [SEED: ${SEED}]"
    for GENE in "${GENES_LIST[@]}"; do
        local GENS_QUERY="${SEED}[Organism] AND ${GENE}[title]"
        echo "${GENS_QUERY}"
        esearch -db nuccore -query "${GENS_QUERY}" | efetch -format fasta -stop 1 > "${SEED_FILE}"

        if [ -s "$SEED_FILE" ]; then
            log_info "--> SUCCESS! Gene seed [${SEED}] found and saved."
            return
        fi
    done
    log_info "--> Attempt 2 failed."


    # ATTEMPT 3: Search for the gene by genus.
    local GENUS
    GENUS=$(echo "${SEED}" | cut -d '_' -f1)
    
    local GENUS_MITO_QUERY="${GENUS}[Organism] AND mitochondrion[All Fields] AND complete genome[All Fields] AND NC_0:NC_9[PACC]"
    # First, try to find a RefSeq (NC_). If not found, get the first result.
    (esearch -db nuccore -query "${GENUS_MITO_QUERY}" | efetch -format fasta -stop 1 > "${SEED_FILE}") < /dev/null

    if [ -s "${SEED_FILE}" ]; then
        log_info "--> SUCCESS! Found mitochondrial genome from a close relative in Genus '${GENUS}'."
        return
    else
        GENUS_MITO_QUERY="${GENUS}[Organism] AND mitochondrion[All Fields] AND complete genome[All Fields]"
        log_info "--> No reference genome (NC_) found. Trying any complete genome."
        esearch -db nuccore -query "${GENUS_MITO_QUERY}" | efetch -format fasta -stop 1 > "${SEED_FILE}"
    fi

    if [ -s "${SEED_FILE}" ]; then
        log_info "--> SUCCESS! Mitochondrial Genome seed [SEED] found and saved."
        return
    fi

    for GENE in "${GENES_LIST[@]}"; do
        local GENS_QUERY="${GENUS}[Organism] AND ${GENE}[title]"
        echo "${GENS_QUERY}"
        esearch -db nuccore -query "${GENS_QUERY}" | efetch -format fasta -stop 1 > "${SEED_FILE}"

        if [ -s "${SEED_FILE}" ]; then
            log_info "--> SUCCESS! Gene seed [${SEED}] found and saved."
            return
        fi
    done
    log_info "--> Attempt 3 failed. Proceeding to search gen in family."


    # FALLBACK: Find the family and repeat the searches.
    local FAMILY
    FAMILY=$(esearch -db taxonomy -query "${SEED}" | efetch -format xml | xtract -pattern Taxon -block "*/Taxon" -if Rank -equals "family" -element ScientificName | head -n 1)

    if [[ -z "$FAMILY" ]]; then
        log_error "--> FINAL FAILURE: Could not determine Family for SEED '${SEED}'. No seed found."
        return
    fi
    
    log_info "--> Family found: ${FAMILY}. Repeating searches at the FAMILY level."

    # ATTEMPT 4: Search for a complete mitochondrial genome for the FAMILY.
    log_info "--> Attempt 4: Searching for Complete Mitochondrial Genome [Family: ${FAMILY}]"
    local mito_family_query="(\"${FAMILY}\"[Organism]) AND \"mitochondrial\"[All Fields] AND \"complete\"[All Fields]" < '/dev/null'
    (esearch -db nuccore -query "$mito_family_query" | efetch -format fasta -stop 1 > "${SEED_FILE}") < /dev/null

    if [ -s "${SEED_FILE}" ]; then
        log_info "--> SUCCESS! Mitochondrial Genome seed [Family] found and saved."
        return
    fi
    log_info "--> Attempt 4 failed."


    # ATTEMPT 5: Search for the gene list for the FAMILY.
    log_info "--> Attempt 5: Searching by Gene List [Family: ${FAMILY}]"
    local GEN_FAMILY_QUERY="(\"${FAMILY}\"[Organism]) AND (${GENE_QUERY_PART})"
    (esearch -db nuccore -query "${GEN_FAMILY_QUERY}" | efetch -format fasta -stop 1 > "${SEED_FILE}") < /dev/null
    
    if [ -s "${SEED_FILE}" ]; then
        log_info "--> SUCCESS! Gene seed [Family] found and saved."
    else
        log_error "--> FINAL FAILURE: No seed found for SEED '$SEED' or Family '${FAMILY}'."
    fi

    sleep 1 # Be kind to NCBI's servers.
}

main() {
    # Validate input arguments.
    if [[ $# -ne 2 ]]; then
        usage
    fi
    
    local SAMPLE_FILE="$1"
    local OUTPUT_DIR="$2"

    # Validate file Species.
    if [ ! -f "${SAMPLE_FILE}" ]; then
        echo "ERROR: Sample file '${SAMPLE_FILE}' not found."
        exit 1
    fi
    # The output directory is created if it doesn't exist, so no need to check.
    mkdir -p "${OUTPUT_DIR}"

    check_dependencies

    echo "Starting seed search..."
    echo "------------------------"

    # Loop through each SEED.
    while IFS= read -r SEED || [[ -n "$SEED" ]]; do
        # Skip blank lines.
        # if [[ -z "${SEED}" ]]; then continue; fi
        
        # Remove potential carriage return characters from Windows (\r).
        SEED=$(tr -d '\r' <<< "${SEED}")

        process_seeds "${SEED}" < /dev/null

    done < "${SAMPLE_FILE}"

    echo ""
    echo "Process complete!"
}

# Execute the main function with the arguments passed.
main "$@"