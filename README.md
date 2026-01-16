# The talent paradox: Why is it fair to reward talent but not luck?

This repository provides data and code to replicate the analysis of the paper.

## Authors

- [Björn Bartling](https://www.econ.uzh.ch/en/people/faculty/bartling.html)
- [Alexander W. Cappelen](https://sites.google.com/view/alexander-w-cappelen/home)
- [Ingvild L. Skarpeid](https://sites.google.com/site/ingvildskarpeid)
- [Erik Ø. Sørensen](https://www.statsokonomen.no/erik-o-sorensen-cv/) (contact for questions on the data)
- [Bertil Tungodden](https://sites.google.com/view/bertiltungodden/home)

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

The authors have the rights to make the data from the experiment
available in the public domain. 

## 1.2 License for Data

The data collected as part of the study, `mmtalent_df.dta`, are available 
in the public domain at Harvard Dataverse, https://doi.org/10.7910/DVN/20CRBI,
with a Creative Commons CC0 license.


## 1.3 Summary of Availability

All data are publicly available and provided as part of this replication package.

## 1.4 Details on each Data Source

| Data.Name       | Data.Files         | Location                | Provided | Citation                  |
|-----------------|--------------------|--------------------------|----------|----------------------------|
| Replication Data | mmtalent_df.dta       | Harvard Dataverse      | Yes      | Bartling et al. (2024)          |
| Education distribution | sc-est2017-alldata6.csv | external-data/   | Yes | U.S. Census Bureau (2017) |
| Income distribution | finc_07.xls   | external-data/                 | Yes | U.S. Census Bureau (2018a) |
| Geographic distribution | table-1-1.xlsx | external-data/ | Yes | U.S. Census Bureau (2018b) |
| Comparisons to Almås et al. (2020)  | main_vars.dta | external-data/ | Yes | Almås, Cappelen, and Tungodden (2020) | 

# 2. Computational requirements

# 2.1 Software Requirements

The analysis is written in R, relying on the `targets` for automation, and
`renv` for controlling the version of packages. The exact version used of each
included package is documented in the `renv.lock` file managed by the `renv` package. 
For bootstrapping the `renv.lock` specified packages, the latest run
used:

- `R`, version 4.5.2 on Windows 11.
- `renv`, version 1.1.5.


# 2.2 Controlled Randomness

The analysis does not rely on any random number generator.

# 2.3 Memory, Runtime, Storage Requirements

The estimation of results require only minimal memory and computational
resources and should take well under 10 minutes and require only a few megabytes
of storage on any modern computer.


# 3. Instructions to Replicators

Replicators need to install `renv` and use the lock file to install 
other packages. In R, a `tar_make()` command will then calculate
all results and output all displays. The displays are created as
side-effects of creating the Vignettes, which also create html pages 
with narratives surrounding the displays. 

Sometimes it can be difficult to bootstrap renv and the packages. I've found
that it can help to manually install `renv`, and then, in Rstudio's terminal do:

```
R --vanilla -q -e "renv::restore(prompt = FALSE)"
```


# 4. List of Display Items and Programs

In main paper:

| Display Item     | File name       | Vignette        | Chunk name     | 
|------------------|------------------|------------------|------------------|
| Figure 1   | EEREV-D-24-01375_Figure_1.pdf          | descriptive_balance_tables.Rmd     | Consort diagram|   
| Figure 2   | EEREV-D-24-01375_Figure_2.pdf          | survey-results.Rmd | Fairness views and beliefs about control |  
| Figure 3   | EEREV-D-24-01375_Figure_3.pdf          | survey-results.Rmd | The talent paradox - heterogeneity analysis |
| Figure 4   | EEREV-D-24-01375_Figure_4.pdf          | experiment-results.Rmd |Dollars redistributed | 
| Figure 5   | EEREV-D-24-01375_Figure_5.pdf          | experiment-results.Rmd | Heterogeneity in the treatment effect |
| Table 1    | descriptive_table.tex                  | descriptive_balance_tables.Rmd | Descriptive Table | 
| Table 2    | background_balance_table.tex           | descriptive_balance_tables.Rmd | Balance Table | 
| Table 3    | attitudes_controlbelief_regression.tex | survey-results.Rmd | Attitudes Control Beliefs |
| Table 4    | inequality_treatment_regression.tex    | experiment-results.Rmd | Implemented Inequality regressions |
| Table 5    | policy_attitudes.tex                   | policy-results.Rmd | policy_attitudes | Policy Table |


In the supplementary material:


| Display Item     | File name       | Vignette        | Chunk name     | 
|------------------|------------------|------------------|------------------|
| Figure A1        | attitude_histograms.pdf | survey-results.Rmd |attitude_histograms |
| Figure A2        | attitudes_beliefs_by_subgroups.pdf  | survey-results.Rmd | subgroups |
| Figure A3        | attitudes_beliefs_by_subgroups_cross.pdf | survey-results.Rmd | subgroups_crossed |
| Figure A4        | experiment_heterogeneity_personal.pdf | experiment-results.Rmd | Heterogeneity figure personal |
| Figure A5        | cuddly_comparisons_graph.pdf | experiment-results.Rmd | Cuddly comparisons: graph  | 
| Table A1         | attitudes_controlbelief_regression_withT.tex | survey-results.Rmd | Controlbeliefs with treatment |
| Table A2         | inequality_treatment_regression_het3.tex | experiment-results.Rmd | Heterogeneity in treatment effects |
| Table A3         | inequality_treatment_regression_het4.tex | experiment-results.Rmd | Heterogeneity personal |
| Table A4         | decision_amount_regression.tex | experiment-results.Rmd | Decision amounts |

# 5. References

- Almås, Ingvild; Cappelen, Alexander W.; Tungodden, Bertil, 2020, [Supplemental material to:] Cutthroat Capitalism versus Cuddly Socialism: Are Americans More Meritocratic and Efficiency-Seeking than Scandinavians? Journal of Political Economy, 128(5): 1627-2018. https://doi.org/10.1086/705551
- Bartling, Björn; Cappelen, Alexander W.; Skarpeid, Ingvild L.; Sørensen, Erik Ø.; Tungodden, Bertil, 2024, "Replication Data for: The talent paradox: Why is it fair to reward talent but not luck?", Harvard Dataverse, https://doi.org/10.7910/DVN/20CRBI
- U.S. Census Bureau (2017). "Educational attainment in the United States." Online, https://www.census.gov/data/tables/2017/demo/education-attainment/cps-detailed-tables.html
- U.S. Census Bureau (2018a). "Finc-07. Income distribution to $250,000 or more for families." Online, https://www.census.gov/data/tables/time-series/demo/income-poverty/cps-finc/finc-07.2017.html
- U.S. Census Bureau (2018b). "Population and housing unit estimates. Annual state resident population estimates for 6 race groups (6 race alone or in combination groups) by age, sex, and hispanic origin." Online, https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/sc-est2017-alldata6.csv
