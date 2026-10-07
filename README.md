# Expert Elicitation Shiny App

An R Shiny application developed to support structured expert elicitation for ecological decision-making under uncertainty.

The application provides tools for summarizing, aggregating, and visualizing expert judgements expressed as probabilistic estimates.

This repository contains two versions of the application:

- **Version 1:** the original expert-elicitation framework using equal-weight aggregation.
- **Version 2:** an expanded framework incorporating expert assessment confidence and optional multi-round elicitation.

## Application Versions

### Version 1: Base Expert Elicitation Model

`App Version 1` contains the original implementation of the expert elicitation application from Lamothe (2026):

Lamothe, K.A. 2026. Implementing a Three-step Expert-elicitation Approach to Inform Ecological Decision-making. Can. Manuscr. Rep. Fish. Aquat. Sci. 3333: vii + 33 p. https://doi.org/10.60825/b7sr-pv60

Experts provide three probability estimates for each question:

1. a **lowest plausible estimate**;
2. a **best-guess estimate**; and
3. a **highest plausible estimate**.

These estimates are used to construct individual PERT distributions. Individual expert distributions are then combined using an equal-weight linear opinion pool.

This version is retained to preserve the original implementation of the elicitation framework and support reproducibility and comparison with subsequent versions.

---

### Version 2: Assessment Confidence and Multi-Round Elicitation

`App Version 2` contains the current implementation of the application.

Version 2 extends the base framework in two primary ways.

#### Assessment Confidence

Experts can report an **Assessment Confidence (AC)** from 0 to 100 alongside their probability estimates.

Assessment confidence is used to modify each expert's relative contribution to a AC-weighted pooled distribution while leaving the expert's individual PERT distribution unchanged.

The application calculates both:

- an **equal-weight pooled distribution**; and
- a **assessment confidence-weighted pooled distribution**.

This allows the influence of expert confidence on the aggregated judgement to be evaluated directly.

#### Multi-Round Elicitation

Version 2 also supports an optional multi-round elicitation process.

A typical two-round workflow consists of:

1. **Round 1:** experts independently provide their initial estimates and assessment confidence;
2. **Discussion:** participants review and discuss the range of elicited judgements, assumptions, evidence, and uncertainty;
3. **Round 2:** experts independently retain or revise their estimates and assessment confidence; and
4. **Comparison:** Round 1 and Round 2 results are analyzed separately and compared.

The objective of the discussion is not to force consensus. Experts may retain their original estimates when discussion does not change their judgement.

The application provides Question × Round visualizations and summarizes changes between rounds, including:

- change in the equal-weight pooled mean;
- change in the assessment confidence-weighted pooled mean; and
- change in average assessment confidence.

Multi-round analysis is optional. Version 2 continues to support single-round datasets, including datasets without a Round variable.

## Repository Structure

```text
Expert-Elicitation-ShinyApp/
│
├── README.md
├── LICENSE
├── .gitignore
├── Expert-Elicitation-ShinyApp.Rproj
│
├── App Version 1/
│   ├── App/
│   ├── Data/
│   ├── Functions and Helpers/
│   └── README.md
│
└── App Version 2/
    ├── App-Server.R
    ├── App-UI.R
    ├── Run-Application.R
    ├── Data/
    ├── Functions and Helpers/
    ├── Google-Sheets-Setup.md
    └── README.md
```

## Version Overview

| Version | Description | Status |
|---------|-------------|--------|
| Version 1 | Original expert elicitation framework using equal-weight aggregation | Archived |
| Version 2 | Expert elicitation framework incorporating assessment confidence and optional multi-round elicitation | Active development |

## Running the Application

The applications are written in R using the Shiny framework.

To run an application locally:

1. Clone or download this repository.
2. Open `Expert-Elicitation-ShinyApp.Rproj` in RStudio.
3. Navigate to the desired application version.
4. Ensure that the required R packages are installed.
5. Run the corresponding `Run-Application.R` script.

Demonstration data are included with the applications to allow the analytical and visualization features to be explored without using data from an active expert elicitation exercise.

Additional information about the methodology, required inputs, application structure, and interpretation of outputs is provided in the README associated with each application version.

## Development

Current development is focused on Version 2.

Changes to the application are tracked using Git, allowing previous implementations of the model to remain available while development of the current version is documented through the repository's commit history.

## Author

**Karl Lamothe**; karl.lamothe@dfo-mpo.gc.ca

## License

This project is licensed under the MIT License. See ./LICENSE for the full license terms.

Copyright (c) 2026 His Majesty the King in Right of Canada, as represented by the Minister of Fisheries and Oceans.