# Illinois-Community-Health-Dashboard
The Illinois Community Health Dashboard is a free web application that allows users to visually explore community health and environmental information for each census tract and county in Illinois. The web application can be accessed at:
 
**[bit.ly/illinois-health-dashboard](https://bit.ly/illinois-health-dashboard)**
 
---
 
## About
 
This place-based data platform is designed to support communities in understanding their neighborhood conditions in terms of climate and environmental burdens, healthcare access, physical and mental health conditions, and key demographic characteristics. We envision this web application as a resource to help communities become better informed and to support the development of relevant local initiatives.
 
Users can view and interact with maps and data directly in the browser. Key interactive functionalities include:
 
- **Location search:** Enter an address, ZIP Code, or place name to navigate the map.
- **Tract-level inspection:** Click on any census tract to display comprehensive information in a side panel, including state averages for comparison.
- **Indicator selection:** Use drop-down menus above the map to select different variables for display.
- **County-level exploration:** View county-level bar charts to compare conditions across counties.

---

## Demo

![cehdar](https://github.com/user-attachments/assets/d4c56782-6770-4a72-9ba3-3e59f7a1917f)

---

 ## Data Sources
 
This repository contains the application code and the processed data file needed to run the dashboard locally. All underlying data used in this application are derived from publicly available U.S. federal government sources and can be downloaded from the respective providers as detailed below. The primary data source is the Environmental Justice Index (EJI) 2024, published by the Centers for Disease Control and Prevention (CDC).
 
| Category | Source | Time Period |
|----------|--------|-------------|
| Population groups | US Census Bureau, American Community Survey (via EJI) | 2018–2022 |
| Potential lead exposure | US Census Bureau, American Community Survey (via EJI) | 2018–2022 |
| Extreme heat days | CDC (via EJI) | 2018–2022 |
| Wildfire smoky days | National Oceanic and Atmospheric Administration (via EJI) | 2013–2022 |
| Health conditions | CDC (via EJI) | 2024 |
| Air pollution | CDC | 2020 |
| Medical professional density | Federal Emergency Management Agency, Resilience Analysis and Planning Tool | 2019–2023 |
| Public/private insurance coverage | US Census Bureau, American Community Survey | 2018–2022 |
 
---
  
## Getting Started
 
### Prerequisites
 
- R (version 4.3 or later recommended)
- RStudio (recommended IDE)
 
### Setup
 
1. Clone the repository:
 
```bash
git clone https://github.com/CEHDAR/Illinois-Community-Health-Dashboard.git
```
 
2. Install required R packages:
 
```r
install.packages(c("shiny", "leaflet", "sf", "plotly", "dplyr",
                    "stringr", "bslib", "shinythemes", "leaflet.extras",
                    "viridisLite", "shinyjs", "shinycssloaders"))
```
 
3. Launch the application:
 
```r
setwd("~/Illinois-Community-Health-Dashboard")
shiny::runApp()
```
 
---
 
## License
 
Code in this repository is licensed under the [GNU Affero General Public License v3.0 (AGPL-3.0)](LICENSE)
 
---
 
## Citation
 
Gounder, B. & Cho, S. (2026). CEHDAR and the Illinois Community Health Dashboard: Communicating Environmental and Public Health Data with Interactive Geospatial Maps for Community Access. Zenodo. DOI
