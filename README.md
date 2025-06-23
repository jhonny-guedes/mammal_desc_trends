# About this repository

## Motivation

This repository contains the data and code used in the manuscript "Historical shifts, geographic biases, and biological constraints shape mammal taxonomy". In this study, we explore how biological traits, geographic patterns, and taxonomic practices influence the way mammal species have been described from 1990 to 2022. We analyze trends in the comprehensiveness of species descriptions, the use of evidence types (e.g., molecular, morphological), authorship patterns, and potential phylogenetic structure in these patterns. All analyses were conducted in R and are fully reproducible through the materials provided here.

## How to use

### File structure

The repository contains three files:

-   **`Rcode_MammDescTrends.R`** – R script used for the analysis. The analysis script is organized into **10 main sections**, each addressing a key part of the study:

    1.  **Load and understand the dataset**

    2.  **Map mammal species described in the last three decades**

    3.  **Temporal trends in robustness of publications – based on annual means**

    4.  **Publication robustness by mammal order**

    5.  **Temporal trends in robustness of publications – based on GLMs**

    6.  **Create plots with model coefficients and confidence intervals**

    7.  **Check phylogenetic correlation in model residuals**

    8.  **Make phylogenetic correlograms**

    9.  **Explore temporal trends in the use of molecular data on mammal description**

    10. **Relationship between international description and taxonomy practices**

-   **`phylogeny/output.nex`** – Phylogeny used to generate the correlograms. The file represents 100 fully-sampled phylogenies for mammals ([Upham et al., 2019 PLoS Biology](https://doi.org/10.1371/journal.pbio.3000494)) available in Vertlife Data.

-   **`Dataset.RData`** – contains 30 columns, each explained below.

### Description

Here we add the description of each column present in the `Dataset.RData` file.

| **Column Name**          | **Description**                                                                                                                                             |
|--------------------|----------------------------------------------------|
| **SpeciesName**          | Binomial name.                                                                                                                                              |
| **Genus**                | Taxonomic genus to which a species belongs.                                                                                                                 |
| **Family**               | Taxonomic family to which a species belongs.                                                                                                                |
| **Order**                | Taxonomic order to which a species belongs ("Rodentia", "Chiroptera", "Primates", etc.).                                                                    |
| **Authority**            | Author(s) involved in the species description.                                                                                                              |
| **Year**                 | Year in which the species was formally described.                                                                                                           |
| **TaxonomicReview**      | Indicates if the description was based on a taxonomic review (0 = No, 1 = Yes).                                                                             |
| **N_authors**            | Number of authors per description.                                                                                                                          |
| **N.Countries**          | Number of countries (based on authors' affiliations) involved in the description.                                                                           |
| **Log10BodyMass_g**      | Maximum body mass per species (log10-transformed).                                                                                                          |
| **SppRichPerGenus**      | Per-genus species richness based on the year of each species' description.                                                                                  |
| **Morphometrics**        | Number of morphometric measurements used in the description.                                                                                                |
| **Osteology**            | Number of osteological measurements used in the description.                                                                                                |
| **Dentition**            | Binary variable indicating whether dentition data was provided in the description.                                                                          |
| **InternalAnatomy**      | Binary variable indicating whether data on internal anatomy was provided in the description.                                                                |
| **ShapeDescription**     | Binary variable indicating whether any aspects of the species' shape were described.                                                                        |
| **Trichology**           | Binary variable indicating whether trichology data was provided in the description.                                                                         |
| **Coloration**           | Binary variable indicating whether color data was provided in the description.                                                                              |
| **Karyotype**            | Binary variable indicating whether karyotype data was provided in the description.                                                                          |
| **Molecular**            | Binary variable indicating whether the authors used molecular data in the description.                                                                      |
| **MolMethod**            | Molecular method used in the description (e.g., mtDNA, nucDNA, multiLoci, SNPs).                                                                            |
| **N.Genes**              | Total number of genes sequenced when molecular data was used.                                                                                               |
| **N.Specimens**          | Number of specimens of the new species used in the description.                                                                                             |
| **TaxaComparedExamined** | Number of taxa analyzed/inspected for comparisons with the new species.                                                                                     |
| **TaxaCompared**         | Number of taxa mentioned in the text during comparisons with the new species.                                                                               |
| **N.Pages**              | Number of pages (METHODS and RESULTS sections only) of the article divided by the number of described species. One page consists of 4x0.25 parts.           |
| **N_evidencesI**         | Number of evidence types used in descriptions. Morphometrics, osteology, and genes sequenced are treated as continuous characters, while others are binary. |
| **N_evidencesII**        | Number of evidence types used in descriptions. All variables are treated as binary (0 or 1), giving them equal weight.                                      |

### Usage notes

R 4.4.1 software is required to open and ensure reproducibility of the `Rcode_MammDescTrends.R` and `Dataset.RData` scripts, as well as to read the phylogenetic trees available in the `phylogeny/output.nex` file.

## License

The code and data in this repository are provided for peer review and collaboration purposes only and **are not licensed for public use or redistribution** until the associated manuscript is formally published. If you are interested in using this material before publication, please contact the authors.
