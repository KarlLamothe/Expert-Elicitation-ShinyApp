# Expert Elicitation Shiny App

An R Shiny application developed to support structured expert elicitation by summarizing, aggregating, and visualizing expert judgments.

This repository contains two versions of the application. The original version provides the base expert elicitation framework, while the updated version extends the framework by incorporating expert **Degree of Belief (DoB)** into the aggregation of expert responses.

## Application Versions

### Version 1: Base Expert Elicitation Model

`App Version 1` contains the original implementation of the expert elicitation application from Lamothe (2026).

The application provides tools for:

- collecting and processing expert estimates;
- summarizing individual expert responses;
- aggregating responses across experts; and
- visualizing individual and aggregated expert judgments.

This version is retained to preserve the original implementation of the elicitation framework and allow results to be reproduced using the base model.

---

### Version 2: Degree of Belief Model

`App Version 2` contains the updated implementation of the application.

This version extends the base model by incorporating expert **Degree of Belief (DoB)** into the elicitation framework. Degree of Belief provides additional information about the confidence experts place in their own estimates and can be incorporated into the aggregation and interpretation of expert responses.

This is the **current version under active development**.

## Repository Structure

```text
Expert-Elicitation-ShinyApp/
│
├── App Version 1/
│   ├── App/
│   ├── Data/
│   └── Functions and Helpers/
│
├── App Version 2/
│   ├── App-Server.R
│   ├── App-UI.R
│   ├── Run-Application.R
│   ├── Data/
│   ├── Functions and Helpers/
│   └── README.md
│
└── README.md
```

## Running the Application

The applications are written in R using the Shiny framework.

To run a version of the application:

1. Clone or download this repository.
2. Open the desired application version in R or RStudio.
3. Install any required R packages that are not already installed.
4. Run the corresponding `Run-Application.R` script.

For the current application:

```r
source("Run-Application.R")
```

Additional information about model-specific requirements and use is provided within the README for each application version.

## Version Overview

| Version | Description | Status |
|---------|-------------|--------|
| Version 1 | Base expert elicitation model | Archived |
| Version 2 | Expert elicitation model incorporating Degree of Belief | Active development |

## Development

Current development is focused on Version 2.

Changes to the application are tracked using Git, allowing previous implementations of the model to remain available while updates to the current version are documented through the repository's commit history.

## Author

**Karl Lamothe**