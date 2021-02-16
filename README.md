# Radiocarbon-dated Event Count (REC) Modelling
## Overview
This repo contains the R code used for the study presented in the following paper:

[Climate change, not human population growth, correlates with Late Quaternary megafauna declines in North America](https://www.nature.com/articles/s41467-021-21201-8). Please see the paper and associated SI for additional information (including the data required for replication efforts, a much clearer presentation of the analyses described in the paper, up-to-date scripts and a fully documented R markdown file for replication)

## Abstract
Chronological uncertainty complicates attempts to use radiocarbon dates as proxies for processes like human population growth/decline, forest fires, and marine ingression. Established approaches involve turning databases of radiocarbon-date densities into single summary proxies that cannot fully account for chronological uncertainty. Here, I use simulated data to explore an alternate Bayesian approach that instead models the data as what they are, namely radiocarbon-dated event-counts. The approach involves assessing possible event-count sequences by sampling radiocarbon date densities and then applying MCMC to estimate the parameters of an appropriate count-based regression model. The regressions based on individual sampled sequences were placed in a multilevel framework, which allowed for the estimation of hyperparameters that account for chronological uncertainty in individual event times. Two processes were used to produce simulated data. One represented a simple monotonic change in event-counts and the other was based on a real palaeoclimate proxy record. In both cases, the method produced estimates that had the correct sign and were consistently biased toward zero. These results indicate that the approach is widely applicable and could form the basis of a new class of quantitative models for use in exploring long-term human-environment interaction.

## Software
The R scripts contained in this repository are intended for replication efforts and to improve the transparency of my research. They are, of course, provided without warranty or technical support. That said, questions about the code can be directed to me, Chris Carleton, at ccarleton@protonmail.com.

### R
This analysis described in the associated manuscript was performed in R. Thus, you may need to download the latest version of [R](https://www.r-project.org/) in order to make use of the scripts described below.

### Nimble
This project made use of a Bayesian Analysis package called [Nimble](https://r-nimble.org/). See the Nimble website for documentation and a tutorial. Then, refer to the R scripts in this repo.

For a more detailed replication demonstration, see the [Wiki](https://github.com/wccarleton/megafauna-na/wiki) associated with this repo and---importantly---the supplementary information provided alongside [the paper](https://www.nature.com/articles/s41467-021-21201-8).
