# The talent paradox: Why is it fair to reward talent but not luck?

This repository provides data and code to replicate the analysis of the paper.

## Authors

- Björn Bartling, (https://www.econ.uzh.ch/en/people/faculty/bartling.html)
- Alexander W. Cappelen (https://sites.google.com/view/alexander-w-cappelen/home)
- Ingvild L. Skarpeid (https://sites.google.com/site/ingvildskarpeid)
- Erik Ø. Sørensen (https://www.statsokonomen.no/erik-o-sorensen-cv/)
- Bertil Tungodden (https://sites.google.com/view/bertiltungodden/home)

## Abstract

Meritocracy is a fundamental idea in a meritocratic society is that inequality 
reflecting differences in performance is fair, but not The paper investigates
the talent paradox in a large-scale study of the US population. We establish
that Americans differentiate significantly between inequalities due to luck and
inequalities due talent, even when controlling for beliefs about the extent to
which talent is under individual control. In an experiment, we study possible
explanations for the talent puzzle, where we find evidence that people reward
talent (but not luck) because a person must act on it to make it valuable. Our
findings provide novel evidence on why Americans tend to accept talented
people being richer than others.


# 1. Data Availability and Provenance Statement

## 1.1 Statement About Rights

## 1.2 License for Data

The data collected as part of the study are available 
in the public domain at Harvard Dataverse with a Creative Commons CC0 license.


## 1.3 Summary of Availability

All data are publicly available.

## 1.4 Details on each Data Source

| Data.Name       | Data.Files         | Location                | Provided | Citation                  |
|-----------------|--------------------|--------------------------|----------|----------------------------|
| Replication Data | mmtalent_df.dta       | Harvard Dataverse      | Yes      | Bartling et al. (2024)          |
| Education distribution | sc-est2017-alldata6.csv | raw-data/   | Yes | U.S. Census Bureau (2017) |
| Income distribution | finc_07.xls   | raw-data/                 | Yes | U.S. Census Bureau (2018a) |
| Geographic distribution | table-1-1.xlsx | raw-data/ | Yes | U.S. Census Bureau (2018b) |

# 2. Computational requirements

# 2.1 Software Requirements

The analysis is written in R, relying on the `targets` for automation, and
`renv` for controlling the version of packages. The exact version used of each
included package is documented in the `renv.lock` file managed by the `renv` package. 



# 2.2 Controlled Randomness

The analysis does not rely on any random number generator.

# 2.3 Memory, Runtime, Storage Requirements

The estimation of results require only minimal memory and computational
resources and should take well under 10 minutes and require only a few megabytes
of storage on any modern computer.

# 2.4. Description of programs/code

# 3. Instructions to Replicators




# 4. List of Display Items and Programs

| Display Item     | File name       | Vignette        | Chunk name     | Comment          |
|------------------|------------------|------------------|------------------|-------------------|
| Figure 1   |  A  | B     | C  | D  |
| Figure 2   |   |  |  |  | 
| Table | | | | | | |


# 5. References

- Bartling, Björn; Cappelen, Alexander W.; Skarpeid, Ingvild L.; Sørensen, Erik Ø.; Tungodden, Bertil, 2024, "Replication Data for: The talent paradox: Why is it fair to reward talent but not luck?", Harvard Dataverse, https://doi.org/10.7910/DVN/20CRBI
- U.S. Census Bureau (2017). "Educational attainment in the United States." Online, https://www.census.gov/data/tables/2017/demo/education-attainment/cps-detailed-tables.html
- U.S. Census Bureau (2018a). "Finc-07. Income distribution to $250,000 or more for families." Online, https://www.census.gov/data/tables/time-series/demo/income-poverty/cps-finc/finc-07.2017.html
- U.S. Census Bureau (2018b). "Population and housing unit estimates. Annual state resident population estimates for 6 race groups (6 race alone or in combination groups) by age, sex, and hispanic origin." Online, https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/sc-est2017-alldata6.csv
