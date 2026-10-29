# EDIPE_shinyapp
The European Diatom Inventory for Palaeoecology and Ecology (EDIPE) is a a curated bibliographic resource of diatom studies encompassing sediment surface samples, top-bottom sample pairs, and sediment cores (1965-2025). 

🌐 The objective is to enhance diatom data discoverability, accessibility, and reuse across ecological and palaeoecological domains in continental and island environments (freshwater and brackish).

🖥 This R shiny app allows users to query the inventory via filtering and visualizing where the diatom study sites are and their temporal age span (from modern to >15,000 years), as well as  associated metadata of the collection units. 

🚀 Launch the app here: https://edipe.shinyapps.io/shiny/ 

---

## Features
- Search publications by site name, article title, ecosystem type, and dating method
- Filter collection units by:
  - Data source: European Diatom Database (EDDI), LakeCores metadatabase, and novel literature review PalaeOpen
  - Data type: sediment surface samples, top-bottom sample pairs, cores, and sections (i.e., geological deposits) 
  - Data available and repository
  - Temporal coverage

## Interactive visualizations
- Geographic distribution of studies
- Temporal distribution of collection units by time bins
- Raw data explorer
- Download filtered records as CSV 
- Export figures

---
  
## Running locally
  
1. If you have a [GitHub account](https://github.com/) and can use [git](https://git-scm.com/), you can [clone the repo](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository)
2. Download the full repository in your local machine: Code > Download ZIP. Unzip the file and open `EDIPE_shinyapp.Rproj`
3. Install required packages.

  ```r
if (!requireNamespace("pacman", quietly = TRUE)) {install.packages("pacman")} 
pacman::p_load(openxlsx, shiny, tidyverse, plotly, leaflet, DT, egg, shinyWidgets)
```

4. Open the `app.R` file and paste the command `shiny::runApp()` in your R console
  
---
  
## Data sources
  
The inventory integrates three major sources: 

1. **European Diatom Database (EDDI)** (Battarbee et al., 2001, ECRC Research Report No 81, University College London, London, p. 94)
2. **LakeCore metadatabase** (Battarbee, R. W., Morley, D., Bennion, H., Simpson, G. L., Hughes, M., and Bauere, V. (2011). A palaeolimnological meta-database for assessing the ecological status of lakes. J. Paleolimnol. 45, 405–414. [doi](https://doi.org/10.1007/s10933-010-9417-5) 
3. A novel **literature review** of European diatom studies published between 2007 and 2025

EDIPE includes 490 publications and 2807 collection units, defined as the physical entities sampled for diatom analyses. Each entry is described using 27 standardized metadata fields based on the [Neotoma Paleoecology Database](https://www.neotomadb.org), facilitating future data integration and reuse.

---
  
## Contributing
  
Suggestions, corrections, and new bibliographic records are welcome. Please contact 

---
  
## How to cite EDIPE?
  

---

## Contact
  
Email [**Xavier Benito**](mailto:xavier.benito@irta.cat), Marine and Continental Waters Programme, Institute of Agrifood Research and Technology (IRTA), Spain
Personal website: https://xbenitogranell.github.io
