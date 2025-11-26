# Read Ontogenies from Data

based on esqlabsR:::.readOntongeniesFromXLS

## Usage

``` r
readOntongenies(data)
```

## Arguments

- data:

  A data frame containing a column named "Protein Ontogenies".

## Value

A list of \`MoleculeOntogeny\` objects, each representing a protein and
its corresponding ontogeny. Returns NULL if the "Protein Ontogenies"
field is NA.

## Details

This function extracts protein ontogeny mappings from the provided data.
It splits the mappings into individual protein-ontogeny pairs and
validates the structure of each pair. Each valid pair is then converted
into a \`MoleculeOntogeny\` object.
