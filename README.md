# Code and data for the paper: "*Monkeypox in Latin America and the Caribbean: first response and challenge ahead after the first 12 weeks of the 2022 outbreak*"


The scripts written in R are of two types: (a) those that pre-process the data and (b) those that generate the figures (maps, timeline and line plot) and incidence table.

| Script | Purpose |
| ------ | ------- |
| 01-preproc-un-population-data.R | Download and pre-process the UN World Population Prospects data for 2022 |
| 02-preproc-wb-classification.R | Download and pre-process the WB Countrie's classification (FY 2023)  |
| 03-preproc-monkeypox-data.R | Download and pre-process the Monkeypox cases from the Global.healt repository  |
| fig01-monkeypox-map-current-incidence-country.R | The global incidence map for Monkeypox  |
| fig02-monkeypox-map-evolution-incidence-latamcarib.R | A small multiples graphic mapping the advance of Monkeypox in Latinamerica and the Caribbean |
| fig03-monkeypox-timeline.R | Create a timeline of the Monkeypox outbreak with specific milestones for the first 12 weeks  |
| fig04-monkeypox-plot-incidence-by-subregion.R | Trajectory of the incidence for all sub-regions, from the epidemiological week #20 until the week #31 of 2022  |
| tab01-monkeypox-tables-incidence.R | Table with the incidences per country in Latinamerica and the Caribbean, up to the epidemiological week #31 of 2022 |

The data from the original sources was last updated on 2022-08-11.

All the output artifacts (figures and table) can be found in the [output](output/) directory in this repository.

License: [MIT](LICENSE)
