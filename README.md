# Statistical Internship
## Multivariate Analysis of Climatological and Hydrological Low Water Drivers in Bavaria
<p align="center">
<img src="attic/images.png" width="100" height="100" class="center">
</p>

## Data

Simulated and measured data at 3 river gauges in hydrological
Bavaria between 1990 and 2020.
The target variable **NM7Q**: lowest 7-day runoff mean of a year (typical
low water characteristic)

## Research Interests

* How can the occurrence of low water events be explained/predicted?
* Which drivers are relevant? 
* Are drivers of an extreme event themselves extreme? Or is it a combination of moderately pronounced drivers that leads to extreme low water?


## Findings 
* How can the occurrence of low water events be explained/predicted?
  - GAMs with binomial distribution assumption
  - Modeling drivers as splines and individual explanatory interactions
  - Variable-specific quantile distribution analysis
  
* Which drivers are relevant? 
  - Groundwater level, precipitation group, and soilwater appear important
  - Clear differences between north and south, e.g., in influence of snow and air temperature
  - Grouping of southern and northern areas seems reasonable

* Are drivers of an extreme event themselves extreme? Or is it a combination of moderately pronounced drivers that leads to extreme low water?
  - Differences between variables
  - Extreme events seem relevant for: Precipitation, relative near-surface soil moisture, infiltration, and groundwater level.

## Contributing, reproducibility and setup
You will need access to the Climex-II data which is not publicly available. Access can be granted by the Geography Department from the LMU.
### Import the data
1. Download the Data from LRZ Sync + Share
2. Move the `data` folder to this cloned repository. (It should be as the same level as the R Project file)
3. Do not rename the folder or change the structure otherwise the setup will not work.

### Setup
The repository is set up with renv. To be all set paste the following chunk into the console and let it run. The setup file will take some time to run through as all data transformations, models and data generation is covered in there.

**First time setup**
```
renv::restore()
source('setup.R')
```
#### Folder structure explanation and dictionary
* added_data: Additonal data added such as geodata as well as generated data such as models
* attic: Folder for notes and pictures for github readme.
* data: Imported data folder. For more info see "Import the data" section
* Data_analysis.Rmd: File with all data analysis code. Sources `data_read.R`
* Data_modelling_final.Rmd: Final modelling approaches used in the report as well as in the Shiny app
* Data_modelling.Rmd: Includes several different modelling approaches (not just GAMs) and case studies with them for specific catchments
* Data_preparation: Prepares most of the data and saves it. Is wrapped by `setup.R`
* data_read.R: After the first time setup all the data can simply be read in via the `data_read.R` file or by calling:
```
source('data_read.R')
```
* meta: Meta data, description files for the data, protocols, presentation and other files used to report
* Model_Saver.Rmd: Generates just the models and saves them. Normally done already by `setup.R`
* observed_data: Folder with observed data later deployed. Includes a formatting file for the raw data. The data was not used in the analysis. 
* Plots: Folder for saved plots
* renv: Folder for renv package
* renv.lock: File for renv to document packages and versions. (Similar to Python's requirements.txt)
* setup.R: See "First time setup" above. Includes `Data_preparation.Rmd`, `Model_Saver.Rmd` and `setup.R`
* tables: Folder with tables for reporting



