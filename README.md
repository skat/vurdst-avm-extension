# README (Dansk / English)

---

# DK

Dette repository indeholder en forenklet og videreudviklet version af
Vurderingsstyrelsens nuværende modeller til ejendoms- og grundværdiansættelse.
Formålet er at give en transparent og reproducerbar implementering af
dataforberedelsen, de statistiske modeller og de tilhørende analyser.

Repositoryet indeholder både de datasæt, der anvendes i analysen, og den kode,
der dokumenterer, hvordan de er konstrueret. Den første dataudtrækning bygger
dog på interne Vurderingsstyrelsen-data og interne hjælpepakker og kan derfor
ikke reproduceres uden for Vurderingsstyrelsen. De resulterende datasæt er
inkluderet i repositoryet, så de efterfølgende trin kan reproduceres direkte.

## Workflow

Projektet består af tre hovedtrin:

### 1. load_data.R

Scriptet foretager det indledende dataudtræk fra Vurderingsstyrelsens interne
dataleverancer.

Analysen anvender dataleverancer for 1. januar 2020 og 1. januar 2024. Hver
leverance indeholder ejendomme på den pågældende vurderingstermin samt salg fra
de foregående seks år. De kombinerede salgsdata dækker derfor perioden
1. januar 2014 til 1. januar 2024.

Scriptet udtrækker og konstruerer:

- vurderingsejendomme.rds
  - danske vurderingsejendomme pr. 1. januar 2024;

- ejendomssalg.rds
  - danske ejendomssalg fra 1. januar 2014 til 1. januar 2024;

- grundsalg.rds
  - danske salg af ubebyggede grunde fra 1. januar 2014 til
    1. januar 2024.

Dubletter, som opstår på grund af overlappende dataleverancer, fjernes.
Salgsdata beriges desuden med salgsflag, og oplysninger om hav- og søudsigt
opdateres med information fra ejendomsdata pr. 1. januar 2024, hvor denne
information er mere opdateret. Endelig tilføjes udvalgte geografiske variable.

Scriptet afhænger af interne Vurderingsstyrelsen-pakker og interne datakilder
og er derfor primært inkluderet som dokumentation for den oprindelige
dataudtrækning.

### 2. Datasets.Rmd

Notebooken konstruerer det endelige analysesæt ud fra de tre datasæt fra
load_data.R.

Først afgrænses data til simple enfamiliesejendomme og ubebyggede grunde.
Komplekse ejendomsstrukturer og bunkesalg fjernes, og nested information om
deljordstykker og plandata foldes ud.

Herefter udvælges et ensartet udsnit af parcelhuse, parcelhussalg og grundsalg.
Udvælgelsen omfatter blandt andet krav til:

- ejendoms- og bygningsanvendelse;
- én vurderingsejendom, én delgrund, én boligenhed og én bygning;
- boligareal og grundareal;
- byggeår og ombygningsår;
- tag-, væg- og opvarmningstype;
- planforhold og zone;
- fravær af erhvervsareal og særlige ejendomsstrukturer;
- almindelige markedsmæssige salg uden udvalgte salgsflag.

Observationer med manglende værdier i centrale modelvariable fjernes, og salg
og ejendomme begrænses til rimelige intervaller for blandt andet salgspris,
grundareal og boligareal.

Hvis flere relevante plananvendelseskategorier findes for samme observation,
vælges én kategori med prioriteten 1110, derefter 1120 og til sidst 1100.

De tre rensede datasæt samles derefter til ét fælles analysesæt, hvor variable
omdøbes til engelske og mere direkte anvendelige navne. Kategoriske variable
som hustype, kommunegruppe, zone, tagmateriale, vægmateriale og varmekilde
omkodes til læsbare kategorier.

Notebooken genererer:

- model_dataset.rds
  - det endelige analysesæt anvendt i den statistiske analyse.

### 3. Analysis.Rmd

Notebooken indeholder den statistiske analyse baseret på
model_dataset.rds.

Den omfatter blandt andet:

- beskrivende analyse af datagrundlaget;
- estimation af hedoniske modeller;
- generaliserede additive modeller (GAMs);
- rumlige, tidslige og ikke-lineære effekter;
- krydsvalidering;
- sammenligning af modelspecifikationer;
- performance- og fejlmål;
- analyser af modelstabilitet og robusthed;
- ejendoms- og grundværdimodeller;
- figurer og tabeller anvendt i tilhørende artikler.

Analysen er relateret til følgende projekter/artikler:

1. *Generalized Additive Models for Mass Appraisal: Evidence from the Danish
   Public Property Valuation System*;
2. Property and Land Value Predictions with Generalized Additive Models;
3. On the Accuracy of Hedonic Real Estate Models.

## Datasæt

Repositoryet indeholder følgende fire `.rds`-filer:

### vurderingsejendomme.rds

Danske vurderingsejendomme pr. 1. januar 2024. Datasættet indeholder de
egenskaber, der anvendes til efterfølgende afgrænsning og konstruktion af
analysesættet.

### ejendomssalg.rds

Danske ejendomssalg fra 1. januar 2014 til 1. januar 2024. Datasættet er
konstrueret ved at kombinere salgsdata fra dataleverancerne for 2020 og 2024
og fjerne overlappende observationer.

### grundsalg.rds

Danske salg af ubebyggede grunde fra 1. januar 2014 til 1. januar 2024.
Datasættet er konstrueret på samme måde som ejendomssalgsdata.

### model_dataset.rds

Det endelige analysesæt med rensede parcelhuse, parcelhussalg og grundsalg.
Datasættet indeholder de variable, der anvendes direkte i Analysis.Rmd,
herunder:

- salgspris og salgsdato;
- kommune, landsdel og region;
- kommunegruppe og zone;
- koordinater;
- bygnings- og boligeigenskaber;
- grundareal og planforhold;
- afstand til kyst, sø, vandløb, vej, motorvej, station, jernbane og vindmølle;
- hav- og søudsigt;
- skovareal;
- observationstype.

De fire `.rds`-filer er inkluderet i repositoryet. Det er derfor ikke
nødvendigt at køre load_data.R for at reproducere den efterfølgende
dataforberedelse og analyse.

Hvis man alene ønsker at reproducere den statistiske analyse, kan man starte
direkte med Analysis.Rmd og den medfølgende fil model_dataset.rds.

## Øvrige filer

### geographical_subdivisions.csv

Indeholder geografiske klassifikationer for danske kommuner:

- region;
- landsdel;
- kommunenummer;
- kommunenavn;
- kommunegruppe.

Filen anvendes i load_data.R til at berige ejendoms- og salgsdata med
geografiske variable.

### coordinates_ocean_land.csv

Indeholder et koordinatgrid over Danmark med oplysninger om, hvorvidt
koordinater ligger på land eller hav, samt tilhørende geografiske
klassifikationer. Filen anvendes i den efterfølgende analyse af geografiske og
rumlige modelkomponenter.

### prepare_figures.R

R-script til at generere og klargøre endelige figurer til artikler og anden
formidling.

## Reproducerbarhed

Der er tre mulige startpunkter:

1. *Fuld intern reproduktion*
   - Kør load_data.R
   - Kør Datasets.Rmd
   - Kør Analysis.Rmd

   Dette kræver adgang til Vurderingsstyrelsens interne data og R-pakker.

2. *Reproduktion af datakonstruktionen*
   - Brug de inkluderede vurderingsejendomme.rds, ejendomssalg.rds og
     grundsalg.rds
   - Kør Datasets.Rmd
   - Kør Analysis.Rmd

3. *Reproduktion af den statistiske analyse*
   - Brug den inkluderede model_dataset.rds
   - Kør Analysis.Rmd

---

# EN

This repository contains a simplified and extended version of the Danish Public
Valuation Agency's current property and land valuation models. The aim is to
provide a transparent and reproducible implementation of the data preparation,
statistical modelling, and associated analyses.

The repository contains both the datasets used in the analysis and the code
documenting how they were constructed. The initial extraction step relies on
internal Danish Public Valuation Agency data and internal helper packages and
therefore cannot be reproduced outside the Agency. The resulting datasets are
included in the repository so that the subsequent data preparation and
statistical analysis can be reproduced directly.

## Workflow

The project consists of three main steps.

### 1. load_data.R

This script performs the initial data extraction from internal data deliveries
received by the Danish Public Valuation Agency.

The analysis uses data deliveries for 1 January 2020 and 1 January 2024. Each
delivery contains properties at the relevant valuation date together with sales
observed during the preceding six years. The combined sales population
therefore covers the period from 1 January 2014 to 1 January 2024.

The script extracts and constructs:

- vurderingsejendomme.rds
  - Danish properties as of 1 January 2024;

- ejendomssalg.rds
  - Danish property sales from 1 January 2014 to 1 January 2024;

- grundsalg.rds
  - Danish vacant-lot sales from 1 January 2014 to 1 January 2024.

Duplicate sales arising from overlapping data deliveries are removed. Sales
data are additionally enriched with sales flags, while ocean- and lake-view
variables are updated using information from the 2024 property data where this
provides more recent information. Selected geographical variables are also
added.

The script relies on internal Danish Public Valuation Agency packages and data
sources and is therefore primarily included as documentation of the original
data extraction.

### 2. Datasets.Rmd

This notebook constructs the final analysis dataset from the three datasets
generated by load_data.R.

The data are first restricted to relatively simple single-family properties and
vacant lots. Complex property structures and bulk sales are removed, and nested
information on land parcels and planning data is unfolded.

A consistent sample of single-family properties, property sales, and lot sales
is then selected. The restrictions include criteria related to:

- property and building use;
- one valuation property, one land parcel, one residential unit, and one
  building;
- living area and lot size;
- construction and renovation year;
- roof, wall, and heating type;
- zoning and planning characteristics;
- absence of commercial floor area and complex property structures;
- ordinary market transactions without selected sales flags.

Observations with missing values in variables required for the subsequent
models are removed. Sales and properties are also restricted to reasonable
ranges for variables including sales price, lot size, and living area.

Where several relevant planning-use categories are associated with the same
observation, a single category is selected with priority given to 1110,
followed by 1120, and then 1100.

The three cleaned datasets are subsequently combined into a common analysis
dataset. Variables are renamed to simpler English names, and categorical
variables such as house type, municipality type, zoning, roof material, wall
material, and heating type are recoded into interpretable categories.

The notebook generates:

- model_dataset.rds
  - the final dataset used in the statistical analysis.

### 3. Analysis.Rmd

This notebook contains the statistical analysis based on
model_dataset.rds.

It includes, among other things:

- descriptive analysis of the data;
- estimation of hedonic valuation models;
- generalized additive models (GAMs);
- spatial, temporal, and nonlinear effects;
- cross-validation;
- comparison of model specifications;
- prediction-error and performance measures;
- analyses of model stability and robustness;
- property- and land-value modelling;
- figures and tables used in the associated papers.

The notebook relies on packages including mgcv, gratia, ggplot2,
xgboost, caret, future, and furrr.

The analysis is related to the following projects/papers:

1. *Generalized Additive Models for Mass Appraisal: Evidence from the Danish
   Public Property Valuation System*;
2. Property and Land Value Predictions with Generalized Additive Models;
3. On the Accuracy of Hedonic Real Estate Models.

## Datasets

The repository contains the following four .rds files.

### vurderingsejendomme.rds

Danish valuation properties as of 1 January 2024. The dataset contains the
property information used in the subsequent sample selection and construction
of the final analysis dataset.

### ejendomssalg.rds

Danish property sales from 1 January 2014 to 1 January 2024. The dataset is
constructed by combining sales data from the 2020 and 2024 data deliveries and
removing overlapping observations.

### grundsalg.rds

Danish vacant-lot sales from 1 January 2014 to 1 January 2024. The dataset is
constructed in the same manner as the property-sales dataset.

### model_dataset.rds

The final analysis dataset containing cleaned single-family properties,
single-family property sales, and vacant-lot sales. It contains the variables
used directly in Analysis.Rmd, including:

- sales price and sales date;
- municipality, province, and region;
- municipality type and zoning;
- geographical coordinates;
- building and dwelling characteristics;
- lot size and planning characteristics;
- distances to the coast, lakes, streams, roads, highways, railway stations,
  railways, and wind turbines;
- ocean and lake views;
- forest area;
- observation type.

All four .rds files are included in the repository. Running load_data.R is
therefore not required to reproduce the subsequent data preparation and
analysis.

Users interested only in reproducing the statistical analysis can start
directly from Analysis.Rmd using the included model_dataset.rds.

## Additional files

### geographical_subdivisions.csv

Contains geographical classifications for Danish municipalities:

- region;
- province;
- municipality number;
- municipality name;
- municipality type.

The file is used in load_data.R to enrich the property and sales datasets
with geographical variables.

### coordinates_ocean_land.csv

Contains a coordinate grid covering Denmark, including an indicator of whether
each coordinate lies on land or ocean together with associated geographical
classifications. The file is used in subsequent analyses of geographical and
spatial model components.

### prepare_figures.R

R script used to generate and prepare final figures for the associated papers
and other outputs.

## Reproducibility

There are three possible starting points.

1. *Full internal reproduction*
   - Run load_data.R
   - Run Datasets.Rmd
   - Run Analysis.Rmd

   This requires access to the Danish Public Valuation Agency's internal data
   and R packages.

2. *Reproduction of dataset construction*
   - Use the included vurderingsejendomme.rds, ejendomssalg.rds, and
     grundsalg.rds
   - Run Datasets.Rmd
   - Run Analysis.Rmd

3. *Reproduction of the statistical analysis*
   - Use the included model_dataset.rds
   - Run Analysis.Rmd
