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
