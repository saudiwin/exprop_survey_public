# Code and Data Repository for Why Corporate Political Connections Impede Investment

This repo hosts the code and data for:

Kubinec, Robert; Lee, Haillee and Tomashevskiy, Andrey. "Why Corporate Political Connections Impede Investment." *Working Paper*.

A brief description of the files is as follows:

-   `analyze_data.R`: loads survey data `data/qual_data.rds` and either estimates models or loads models from `data/`. Produces all plots in the paper.

-   `prepare_data_public.R`: takes the survey `data/qual_data.rds` and produces a data file with one row per experimental outcome.

-   `define_ord_betareg.R`: Definition of Kubinec (2020) ordinal beta regression model for use with `brms`.

-   `dag_depend.R`: Analysis of the DAG proposed in the paper.

-   `gen_tfp_survey.R`: generation of the simulated TFP data shown in the paper.

-   `beta_logit*.stan`: Stan files for the ordered beta regression model used in the analysis.

-   `preregr_exprop.Rmd`: Rmarkdown file containing code used to produce power analysis in the pre-registration.
