# Mammal Description Trends

## Motivation
Approximately 1.8 million species have been described since the inception of the binomial nomenclature system by Linnaeus in 1758, yet most of the planet's species remain unknown to science. The description of a species is ultimately a scientific hypothesis, which can be revised, refuted, revalidated, and lead to changes in the prevailing taxonomy. While it is essential to accelerate species discovery to optimize conservation planning, the process of delimiting, naming, and classifying new species must be cautious to avoid unnecessary taxonomic instability. Mammals are relatively well-studied compared to amphibians and reptiles. Nevertheless, over 1,000 species of rodents and bats have been described since 1990. Understanding the factors that influence taxonomic robustness in newly discovered mammals can help establish quality taxonomic guidelines and reduce data (species) loss in conservation research.

## Description
This dataset contains **30 columns**, each explained below:

| **Column Name**         | **Description**                                                                                                           |
|--------------------------|---------------------------------------------------------------------------------------------------------------------------|
| **SpeciesName**          | Binomial name.                                                                                                           |
| **Genus**                | Taxonomic genus to which a species belongs.                                                                              |
| **Family**               | Taxonomic family to which a species belongs.                                                                             |
| **Order**                | Taxonomic order to which a species belongs ("Rodentia", "Chiroptera", "Primates", etc.).                          |
| **Authority**            | Author(s) involved in the species description.                                                                           |
| **Year**                 | Year in which the species was formally described.                                                                        |
| **TaxonomicReview**      | Indicates if the description was based on a taxonomic review (0 = No, 1 = Yes).                                          |
| **N_authors**            | Number of authors per description.                                                                                       |
| **N.Countries**          | Number of countries (based on authors' affiliations) involved in the description.                                        |
| **Log10BodyMass_g**      | Maximum body mass per species (log10-transformed).                                                                       |
| **SppRichPerGenus**      | Per-genus species richness based on the year of each species' description.                                               |
| **Morphometrics**        | Number of morphometric measurements used in the description.                                                             |
| **Osteology**            | Number of osteological measurements used in the description.                                                             |
| **Dentition**            | Binary variable indicating whether dentition data was provided in the description.                                       |
| **InternalAnatomy**      | Binary variable indicating whether data on internal anatomy was provided in the description.                             |
| **ShapeDescription**     | Binary variable indicating whether any aspects of the species' shape were described.                                     |
| **Trichology**           | Binary variable indicating whether trichology data was provided in the description.                                      |
| **Coloration**           | Binary variable indicating whether color data was provided in the description.                                           |
| **Karyotype**            | Binary variable indicating whether karyotype data was provided in the description.                                       |
| **Molecular**            | Binary variable indicating whether the authors used molecular data in the description.                                   |
| **MolMethod**            | Molecular method used in the description (e.g., mtDNA, nucDNA, multiLoci, SNPs).                                         |
| **N.Genes**              | Total number of genes sequenced when molecular data was used.                                                            |
| **N.Specimens**          | Number of specimens of the new species used in the description.                                                         |
| **TaxaComparedExamined** | Number of taxa analyzed/inspected for comparisons with the new species.                                                  |
| **TaxaCompared**         | Number of taxa mentioned in the text during comparisons with the new species.                                            |
| **N.Pages**              | Number of pages (METHODS and RESULTS sections only) of the article divided by the number of described species. One page consists of 4x0.25 parts. |
| **N_evidencesI**         | Number of evidence types used in descriptions. Morphometrics, osteology, and genes sequenced are treated as continuous characters, while others are binary. |
| **N_evidencesII**        | Number of evidence types used in descriptions. All variables are treated as binary (0 or 1), giving them equal weight.   |
