# BES SIM 1
This repository contains all scripts and data used for analysis and figures for the paper **Pereira et al. (2024). Global trends and scenarios for terrestrial biodiversity and ecosystem services from 1900-2050. Science.**

This paper is the result of the BES SIM 1 project (Biodiversity and Ecosystem Services Scenario Inter-model Comparison).

## Authors
Henrique M. Pereira, Luise Quoß, Inês Martins

## Version history
Version 1.1, 26.4.2024  
Updated figured order and now uses color blind pallete for all figures.

Version 1.0, 25.4.2024  
https://doi.org/10.5281/zenodo.10703117

## Licence
This work is licensed under the Creative Commons Attribution 4.0 International License.
To view a copy of this license, visit http://creativecommons.org/licenses/by/4.0/ or send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

## Code folder
| Filename   | Description  |
| ---  | ---  |
|Figure1_and_S14.R | Create input data and plots for global biodiversity metrics |
|Figure2.R   | Create BD maps (species richness) per scenario, averaged over several models |
|Figure3.R   | Create input data and plots for global ecosystem service metrics |
|Figure4.R   | Create Input data and Map with regional plots for ES and BD |
|FigureS1.R  | Create global historical trends (1900-2015) in land-use and projected trends for each scenario (2015-2050) |
|FigureS2.R  | Create distribution maps of primary land (forest & non-forest) in 1900, historical changes (1900-2015) and future changes (2015-2050) in each scenario |
|FigureS3.R  | Create distribution maps of secondary land (forest & non-forest) in 1900, historical changes (1900-2015) and future changes (2015-2050) in each scenario |  
|FigureS4.R  | Create distribution maps of cropland (C3 & C4) in 1900, historical changes (1900-2015) and future changes (2015-2050) in each scenario  |
|FigureS5.R  | Create distribution maps of pasture and rangeland in 1900, historical changes (1900-2015) and future changes (2015-2050) in each scenario |
|FigureS6.R  | Create S2a: Global historical trends (1990-2015) in mean annual temperature and for each scenario (2015-2050) and create S2b-e: Spatial distribution maps of absolute changes in mean annual temperature in each scenario (2015-2050)| 
|FigureS7.R  | Create BD maps (species richness) per model for the regional rivalry scenario | 
|FigureS8.R  | Create BD maps (intactness) per scenario, averaged over several models | 
|FigureS9.R  | Create BD maps per scenario for the AIM model |
|FigureS10-S13.R | Create ES maps per scenario, some per model, some averaged over several models |



### Auxiliary files 
The files are here for documentation as the spatial statistics from the maps are already stored in the files in Data_tables.

| Filename   | Description     |
| ---    | ---  |
|BES-SIM_statistics_global.R | Calculate global statistics per IPBES region for all netCDFs|
|BES-SIM_statistics_ipbes-regions.R | Calculate zonal statistics per IPBES region for all netCDFs|

## Data_tables folder 
All tabular data from the biodiversity and ecosystem service model outputs are available in this folder. These files also include the spatial statistics from the maps. See README files on that folder for the metadata.

## IPBES_regions folder
A shapefile with the IPBES regions is available.

## Other spatial data
The remaining data can be downloaded from the following repositories.

### Biodiversity and ecosystem services models map outputs 
Download the datasets from the EBV Data Portal:
| ID  | Title | DOI |
| ---    | ---  | ---  |
|27 | Global trends in biodiversity (BES-SIM GLOBIO)| https://doi.org/10.25829/r7bt92 |
|28 | Global trends in biodiversity (BES-SIM PREDICTS)| https://doi.org/10.25829/vt7qk9 |
|29 | Global trends in biodiversity (BES-SIM cSAR-IIASA)| https://doi.org/10.25829/haq7d4 |
|30 | Global trends in biodiversity (BES-SIM cSAR-iDiv)| https://doi.org/10.25829/5zmy41 |
|31 | Global trends in biodiversity (BES-SIM AIM)| https://doi.org/10.25829/5wn357 |
|68 | Global trends in biodiversity (BES-SIM INSIGHTS) | https://doi.org/10.25829/h2evr2 |
|60 | Global trends in ecosystem services (BES-SIM LPJ-GUESS)| https://doi.org/10.25829/z5v9t2 |
|61 | Global trends in ecosystem services (BES-SIM LPJ)| https://doi.org/10.25829/xq7a86 |
|62 | Global trends in ecosystem services (BES-SIM CABLE POP)| https://doi.org/10.25829/ktnb68 |
|63 | Global trends in ecosystem services (BES-SIM InVEST)| https://doi.org/10.25829/zr4d27 |
|64 | Global trends in ecosystem services (BES-SIM GLOBIO-ES)| https://doi.org/10.25829/vqd4s4 |

### Land-use data
Download the LandUseHarmonization2 from their website: https://luh.umd.edu/data.shtml.

Data used for the historical maps: LUH2 v2h Release (10/14/16), file:  
- states.nc  

Data used for the scenario maps: LUH2 v2f Release (12/21/17), files:  
- multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-AIM-ssp370-2-1-f_gn_2015-2100.nc  
- multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-IMAGE-ssp126-2-1-f_gn_2015-2100.nc  
- multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-MAGPIE-ssp585-2-1-f_gn_2015-2100.nc  

### Climate data
Download the data from Dryad: https://doi.org/10.5061/dryad.3n5tb2rr6

### Global land map
Download 'Land' from NaturalEarth: https://www.naturalearthdata.com/downloads/110m-physical-vectors/  (ne_110m_land)
Version: 4.1.0 (The interface says Version 4.0.0 but the version information in the download files says 4.1.0 -> check both)

## Folder structure for code and data
All paths in the codes are relative. Place all spatial data in 'Data_geo' and all code can be run. The folders need the following names (except *your-folder-name*  which can be named for instance BES_SIM_1) and hierarchical structure:
	
	*
	├───Data_geo
	│ 	├───Climate_data
	│ 	├───ebv_cubes	
	│ 	├───LUH2
	│ 	├───ne_110m_land
	├───*your-folder-name, eg. "BES_SIM_1"*
	│ 	├───Code
	│ 	├───Data_tables
	|	  ├───IPBES_Regions
	│ 	├───Figures
	│ 	├───Outputs
	*

## Session Info for R 
Session Info of the R- and package-version(s):

	R version 4.3.2 (2023-10-31)
	Platform: x86_64-apple-darwin20 (64-bit)
	Running under: macOS Sonoma 14.2.1

	Matrix products: default
	BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
	LAPACK: /Library/Frameworks/R.framework/Versions/4.3-x86_64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0

	locale:
	[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

	time zone: Europe/Berlin
	tzcode source: internal

	attached base packages:
	[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

	other attached packages:
	 [1] gridExtra_2.3   raster_3.6-26   sp_2.1-3        readr_2.1.5     igraph_2.0.1.1  purrr_1.0.2     sf_1.0-15       rlang_1.1.3    
	 [9] ggpattern_1.0.1 readxl_1.4.3    ggpubr_0.6.0    Rmisc_1.5.1     plyr_1.8.9      lattice_0.22-5  dplyr_1.1.4     ggplot2_3.4.4  
	[17] classInt_0.4-10 terra_1.7-71    stringr_1.5.1   ebvcube_0.1.7  

	loaded via a namespace (and not attached):
	 [1] gtable_0.3.4        rstatix_0.7.2       rJava_1.0-11        rhdf5_2.46.1        tzdb_0.4.0          rhdf5filters_1.14.1
	 [7] vctrs_0.6.5         tools_4.3.2         generics_0.1.3      parallel_4.3.2      curl_5.2.0          tibble_3.2.1       
	[13] proxy_0.4-27        fansi_1.0.6         pkgconfig_2.0.3     KernSmooth_2.23-22  checkmate_2.3.1     lifecycle_1.0.4    
	[19] compiler_4.3.2      farver_2.1.1        munsell_0.5.0       codetools_0.2-19    carData_3.0-5       class_7.3-22       
	[25] crayon_1.5.2        pillar_1.9.0        car_3.1-2           tidyr_1.3.1         abind_1.4-5         tidyselect_1.2.0   
	[31] stringi_1.8.3       labeling_0.4.3      colorspace_2.1-0    cli_3.6.2           magrittr_2.0.3      xlsxjars_0.6.1     
	[37] utf8_1.2.4          broom_1.0.5         e1071_1.7-14        withr_3.0.0         scales_1.3.0        backports_1.4.1    
	[43] bit64_4.0.5         bit_4.0.5           ggsignif_0.6.4      cellranger_1.1.0    hms_1.1.3           memuse_4.2-3       
	[49] Rcpp_1.0.12         glue_1.7.0          DBI_1.2.1           xlsx_0.6.5          vroom_1.6.5         rstudioapi_0.15.0  
	[55] jsonlite_1.8.8      R6_2.5.1            Rhdf5lib_1.24.1     units_0.8-5

