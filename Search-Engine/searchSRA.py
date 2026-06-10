import os
from Bio import Entrez
from Bio import SeqIO

# NCBI requires a contact e-mail. Set the ENTREZ_EMAIL environment variable
# (e.g. `export ENTREZ_EMAIL="you@example.com"`). Never hardcode it here.
Entrez.email = os.getenv("ENTREZ_EMAIL")

handle = Entrez.esearch(db="SRA", term="rnaseq mutation brazil cancer ", retmax="10") # retmax é uma string
record = Entrez.read(handle)
handle.close()

print(record)