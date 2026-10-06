# Expert Elicitation Shiny App: Version 1

## Overview

Version 1 of the Expert Elicitation Shiny App was developed to support structured expert-elicitation exercises for ecological decision-making under uncertainty.

The application accompanies a modified Delphi-based expert-elicitation approach designed for decision-support contexts where empirical information may be limited and expert judgement is used to characterize uncertainty.

Version 1 implements a three-step probabilistic elicitation framework in which experts provide:

1. a **lowest plausible estimate**;
2. a **best-guess estimate**; and
3. a **highest plausible estimate**.

These estimates are used to characterize individual expert judgements as probability distributions and subsequently aggregate judgements across experts.

## Elicitation and Aggregation

Individual expert responses are represented using **PERT distributions**, parameterized using each expert's lowest plausible, best-guess, and highest plausible estimates.

Individual distributions are then combined using an **equal-weight linear opinion pool**, in which each expert contributes equally to the aggregated distribution.

This approach preserves variation and disagreement among experts rather than requiring participants to reach consensus. The resulting pooled probability distribution represents the range and relative support of expert judgements at the group level.

## Application Features

The Shiny application supports the implementation and interpretation of the elicitation by providing tools to:

- import and process expert responses;
- summarize individual expert estimates;
- generate individual PERT distributions;
- aggregate expert distributions using an equal-weight linear opinion pool;
- visualize individual and aggregated expert judgements; and
- summarize uncertainty in the pooled distribution.

Although the approach was initially developed for conservation translocation feasibility assessments, the application can be adapted to other decision-support contexts requiring the structured elicitation and aggregation of expert judgement.

## Repository Structure

Version 1 is organized into three primary directories:
 
```text
App Version 1/
│
├── App/
│ ├── App-UI.R
│ ├── App-Server.R
│ └── Run-Application.R
│
├── Data/
│ └── demo_data.R
│
├── Functions and Helpers/
│ ├── Helper-function.R
│ ├── Packages-Themes.R
│ ├── Plotting-functions.R
│ └── Summarize-Pert.R
│
└── README.md
```
 
The `App` directory contains the Shiny application and scripts used to launch the application. The `Data` directory contains demonstration data, while `Functions and Helpers` contains supporting functions used for data processing, calculation, plotting, and application formatting.
 
## Running Version 1
 
To run the application locally:
 
1. Clone or download the repository.
2. Open the Version 1 application directory in R or RStudio.
3. Ensure that the required R packages are installed.
4. Run `Run-Application.R`.
 
The demonstration data included in the `Data` directory can be used to explore the application and its outputs.
 
## Version Status
 
Version 1 represents the original implementation of the expert-elicitation framework and is retained in this repository for reproducibility and comparison with subsequent versions. It can be cited as:
Lamothe, K.A. 2026. Implementing a Three-step Expert-elicitation Approach to Inform Ecological Decision-making. Can. Manuscr. Rep. Fish. Aquat. Sci. 3333: vii + 33 p. https://doi.org/10.60825/b7sr-pv60
 
Active development of the application occurs in **App Version 2**, which extends the elicitation framework to incorporate expert Degree of Belief.