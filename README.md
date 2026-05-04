# EVs and Emissions: The Untold Story

An R-based data analysis investigating whether rising EV adoption measurably 
reduces CO₂ emissions — or simply shifts pollution from roads to power plants.

## Research Questions

1. How has EV adoption grown globally across top markets (2015–2023)?
2. Does higher EV adoption correlate with lower CO₂ per capita?
3. Does grid cleanliness moderate the environmental benefit of EVs?
4. What variables best predict national CO₂ per capita?

## Datasets

| Dataset | Source |
|---|---|
| EV Sales by Country | [IEA Global EV Outlook](https://www.iea.org/data-and-statistics/data-product/global-ev-outlook-2024) |
| CO₂ Emissions | [OWID CO₂ Data](https://github.com/owid/co2-data) |
| Energy & Grid Mix | [OWID Energy Data](https://github.com/owid/energy-data) |

Download the CSV files and place them in a `data/` folder before running the script.

## Analysis Pipeline

### Stage 1 — Data Discovery
- Loaded 3 datasets, checked dimensions, year ranges, country coverage
- Identified and reported missing values on key variables

### Stage 2 — Data Preparation
- Harmonised country names across datasets (e.g. "USA" → "United States")
- Filtered to 2015–2023 and merged all 3 datasets on country + year
- Engineered features: EVs per million people, log-transformed EV sales, log CO₂

### Stage 3 — Visualisations (7 plots)

| Plot | Description |
|---|---|
| 01 | EV sales growth — top 10 countries (2015–2023) |
| 02 | Top 15 countries by EV sales in 2023 |
| 03 | Renewable electricity share — top 10 EV markets |
| 04 | EV adoption vs CO₂ per capita (coloured by renewables %) |
| 05 | Grid carbon intensity vs CO₂ per capita |
| 06 | Grid carbon intensity heatmap — top 25 EV markets |
| 07 | Correlation matrix of key variables |

### Stage 4 — Regression Models

5 linear regression models with year fixed effects:

| Model | Variables |
|---|---|
| A | EV adoption → CO₂ per capita |
| B | Renewable % → CO₂ per capita |
| C | Grid carbon intensity → CO₂ per capita |
| D | Multiple regression (A + B + C combined) |
| E ★ | EV × Grid cleanliness interaction (key model) |

**Key finding:** EVs alone show a weak relationship with CO₂ reduction. 
The interaction model (E) reveals that EVs reduce emissions significantly 
more in countries with cleaner electricity grids — confirming that EV 
adoption must go hand in hand with renewable energy investment.

## R Packages Used

tidyverse, ggplot2, ggcorrplot, ggrepel, viridis, scales, broom, patchwork

## How to Run

```r
# Install dependencies (auto-handled in script)
source("EVs_Analysis.R")
```
All 7 plots will be saved to the `plots/` folder.

## Author

Sinchana Ganapati Naik
