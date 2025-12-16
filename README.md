# README (Dansk / English)

---

# DK model-projects

Dette repository indeholder en videreudvikling af Vurderingsstyrelsens nuværende ejendomsværdi­model, baseret på en række simplifikationer i både dannelsen af datagrundlaget og selve den statistiske model.

## Indhold

Modellen køres i øjeblikket ved hjælp af følgende R Markdown-notebooks:

1. **Dataset.Rmd**  
   - Danner filen `dataset.csv`, som bruges til at fitte den statistiske model.  
   - Henter salgsejendomme fra 2020- og 2024-premodeldata.  
   - Samler dem til et datasæt af unikke salgsejendomme fra perioden **2014–2024**.  

2. **Notebook.Rmd**  
   - Indlæser `dataset.csv`.  
   - Fitter ejendomsværdimodellen.  
   - Indeholder analyser af modelkvalitet og performance.  

## Øvrige filer

- **dataset.csv**  
  - Inkluderet for, at man kan springe eksekveringen af `Dataset.Rmd` over.

- **coordinates_ocean_land.csv**  
  - Et prædannet grid over Danmark, der angiver hvilke koordinater der ligger på hav eller land.

- **prepare_figures.R**  
  - Et simpelt R-script til at generere figurer anvendt i rapporter, præsentationer eller publikationer.

---

---

# EN model-projects

This repository contains an improved version of the Danish Public Valuation Agency’s (Vurderingsstyrelsen’s) current property valuation model, based on a set of simplifications applied both to the construction of the underlying dataset and to the statistical model itself.

## Contents

At the time of writing, the model is executed using the following R Markdown notebooks:

1. **Dataset.Rmd**  
   - Produces `dataset.csv`, which is used to fit the statistical model.  
   - Imports property sales from the 2020 and 2024 pre-model datasets.  
   - Combines these into a dataset of unique property sales from **2014–2024**.

2. **Notebook.Rmd**  
   - Loads `dataset.csv`.  
   - Fits the property valuation model.  
   - Contains analyses evaluating model quality and performance.

## Other files

- **dataset.csv**  
  - Included to allow users to skip running `Dataset.Rmd`.

- **coordinates_ocean_land.csv**  
  - A pre-generated grid over Denmark indicating whether each coordinate lies on land or sea.

- **prepare_figures.R**  
  - A simple R script for generating figures used in publications, documentation, or presentations.

 


