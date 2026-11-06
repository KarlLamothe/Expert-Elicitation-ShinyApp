# Expert Elicitation Shiny App: Version 3

## Overview

Version 3 of the Expert Elicitation Shiny App extends the original expert-elicitation framework by supporting multiple elicitation question types while retaining assessment confidence weighting, optional multi-round elicitation, and automated facilitator reporting.

Version 3 supports four elicitation formats:

1. **Probability estimation**
2. **Numeric estimation**
3. **Yes/No questions**
4. **Ranking questions**

For probability and numeric estimation questions, experts characterize uncertainty by providing:

1. a **lowest plausible estimate**;
2. a **best-guess estimate**; and
3. a **highest plausible estimate**.

These three estimates are used to represent each expert's judgement as a modified PERT distribution.

For Yes/No questions, experts provide a binary response.

For ranking questions, experts rank a set of alternatives, management options, scenarios, species, threats, or other entities according to the elicitation objective.

Version 3 continues to support optional **Assessment Confidence (AC)** values ranging from 0 to 100. Assessment Confidence modifies the contribution of each expert to pooled summaries while leaving individual responses unchanged.

The application calculates both:

- equal-weight pooled results; and
- Assessment Confidence-weighted pooled results;

allowing the effect of confidence weighting to be evaluated directly.

Version 3 can be used in either:

- single-round elicitation exercises; or
- multi-round elicitation exercises incorporating structured discussion and response revision.

### Terminology Note

Version 2.0.0 used the term **Degree of Belief** for the self-reported confidence measure.

Subsequent development uses **Assessment Confidence** to distinguish this measure from other uses of degree of belief and subjective probability within the structured expert judgement literature.

Legacy datasets containing a Degree_of_Belief column remain supported.

---

# Application Features

Version 3 supports:

- demonstration datasets;
- CSV upload;
- Google Sheets integration;
- downloadable response templates;
- automatic column detection and mapping;
- input-data validation;
- single-round elicitation;
- multi-round elicitation;
- probability estimation questions;
- numeric estimation questions;
- Yes/No questions;
- ranking questions;
- individual expert estimate visualizations;
- pooled probability distributions;
- simulated pooled distributions;
- cumulative distribution functions;
- participant-level Yes/No response visualizations;
- pooled Yes/No summaries;
- participant-level ranking visualizations;
- pooled ranking summaries;
- equal-weight aggregation;
- Assessment Confidence-weighted aggregation;
- round-to-round comparison;
- participant tracking across rounds;
- automated facilitator reporting;
- executive summaries;
- diagnostic warnings;
- assessment confidence summaries;
- identification of influential weighting effects;
- identification of large post-discussion revisions;
- identification of divergent expert judgement;
- HTML facilitator report generation;
- plot export;
- CSV export; and
- ZIP export of complete analysis outputs.

---

# Getting Started: Running Version 3

To run Version 3 locally:

1. Clone or download the repository.
2. Open the repository as an RStudio Project.
3. Ensure all required packages are installed.
4. Run:

```r
source("Run-Application.R")
```

The demonstration dataset can be selected within the application to explore the available functionality.

---

# Input Methods

Version 3 supports multiple methods for supplying expert responses.

## Demonstration Data

The application includes an example dataset illustrating:

- multiple questions;
- multiple elicitation rounds;
- Assessment Confidence values;
- probability questions;
- numeric questions;
- Yes/No questions; and
- ranking questions.

The demonstration dataset can be used to explore all visualizations and reporting functions without requiring external files.

---

## CSV Upload

Expert responses can be supplied using a CSV file.

A template can be downloaded directly from the application.

After populating the template:

1. Save the file as CSV.
2. Select **Upload CSV**.
3. Upload the file.
4. Confirm column mappings.
5. Select questions of interest.
6. Select **Run / Refresh Analysis**.

---

## Google Sheets

Google Sheets may be used as a live response source during facilitated elicitation exercises.

Facilitators can refresh results during ongoing data collection and discussion.

Google Sheets integration supports:

- facilitator worksheets;
- participant worksheets;
- live response retrieval;
- response consolidation using `IMPORTRANGE()`; and
- repeated refreshes during workshops and meetings.

See:

```text
Google-Sheets-Setup.md
```

for setup instructions.

---

# Question Types

## Probability Questions

Probability questions require:

- Lowest Plausible Estimate (LPP)
- Best Guess (BGP)
- Highest Plausible Estimate (HPP)

Values must represent probabilities bounded between 0 and 1.

---

## Numeric Questions

Numeric questions also use:

- Lowest Plausible Estimate (LPP)
- Best Guess (BGP)
- Highest Plausible Estimate (HPP)

Values may represent abundance, biomass, mortality, growth rate, economic value, or other numerical quantities.

---

## Yes/No Questions

Experts provide a binary response:

```text
Yes
```

or

```text
No
```

---

## Ranking Questions

Experts rank alternatives, which may represent:

- management options;
- species priorities;
- threats;
- intervention strategies;
- research priorities; or
- competing scenarios.

---

# Input Data Structure

Version 3 supports multiple response formats depending on question type.

---

## Probability and Numeric Questions

Example:

```text
Question
Question_Type
Question_Text
Round
Participant
Lowest_Plausible_Pr
Best_Guess_Pr
Highest_Plausible_Pr
Assessment_Confidence
```

Example rows:

```text
Question  Round  Participant  LPP   BGP   HPP   AC
1         1      P1           0.20  0.40  0.60  70
1         1      P2           0.30  0.50  0.75  65
1         2      P1           0.25  0.50  0.70  85
1         2      P2           0.35  0.55  0.75  75
```

---

## Yes/No Questions

Example:

```text
Question
Question_Type
Question_Text
Round
Participant
Response
Assessment_Confidence
```

Example rows:

```text
Question  Participant  Response
3         P1           Yes
3         P2           No
3         P3           Yes
```

---

## Ranking Questions

Example:

```text
Question
Question_Type
Question_Text
Round
Participant
Option
Rank
```

Example rows:

```text
Question  Participant  Option         Rank
4         P1           Option A       1
4         P1           Option B       2
4         P1           Option C       3
```

---

# Question Text

The optional `Question_Text` field is strongly recommended.

When supplied, the application displays the full elicitation question within analysis panels whenever a single question is selected.

This improves interpretation of:

- visualizations;
- exports;
- reports; and
- summary tables.

---

# Input Data Validation

Version 3 validates submissions before analysis.

Invalid data stop the analysis and generate a validation message.

---

## Probability Validation

Probability questions must satisfy:

```math
0 < LPP ≤ BGP ≤ HPP < 1
```

and:

```math
LPP < HPP
```

The application checks for:

- missing values;
- non-numeric values;
- invalid probability bounds;
- implausible intervals; and
- duplicate responses.

---

## Numeric Validation

Numeric questions must satisfy:

```math
LPP ≤ BGP ≤ HPP
```

and:

```math
LPP < HPP
```

The application checks for:

- missing values;
- non-numeric values;
- inconsistent intervals; and
- duplicate responses.

---

## Yes/No Validation

Yes/No questions must contain valid responses.

Accepted values are:

```text
Yes
No
```

The application checks for:

- missing responses;
- invalid response categories; and
- duplicate responses.

---

## Ranking Validation

Ranking questions must contain valid ranks.

The application checks for:

- missing ranks;
- non-numeric ranks;
- duplicated ranks within respondents;
- incomplete rankings; and
- duplicate responses.

---

## Assessment Confidence Validation

Assessment Confidence values must satisfy:

```math
0 ≤ AC ≤ 100
```

The application checks for:

- values below 0;
- values above 100; and
- invalid numeric entries.

---

# Visualizations

Version 3 generates visualizations appropriate to each question type.

## Probability and Numeric Questions

Available visualizations include:

- Individual Expert Estimates;
- Pooled Distributions;
- Simulated Pooled Distributions;
- Cumulative Distribution Functions;
- Equal-weight distributions;
- Assessment Confidence-weighted distributions; and
- Beta approximations.

---

## Yes/No Questions

Available visualizations include:

- participant response heatmaps;
- pooled response summaries;
- equal-weight response proportions; and
- Assessment Confidence-weighted response proportions.

---

## Ranking Questions

Available visualizations include:

- participant ranking heatmaps;
- pooled ranking summaries;
- mean rankings;
- median rankings; and
- ranking variability.

---

# Multi-Round Elicitation

## Round 1

Experts independently provide responses.

The application summarizes:

- participant responses;
- pooled results; and
- Assessment Confidence.

---

## Structured Discussion

Participants review and discuss:

- assumptions;
- interpretations;
- evidence; and
- uncertainty.

The objective is not consensus.

---

## Round 2

Following discussion, participants submit revised responses.

Round 1 and Round 2 are analyzed separately.

---

## Round Comparisons

Version 3 calculates:

- change in pooled means;
- change in Assessment Confidence;
- changes in rankings;
- changes in Yes/No proportions; and
- changes in uncertainty.

Changes are calculated as:

```math
Δ = Round 2 − Round 1
```

---

# Statistical Methods

## Probability and Numeric Questions

Individual responses are represented using modified PERT distributions.

The shape parameters are:

```text
α = 1 + λ(m − a)/(b − a)
```

```text
β = 1 + λ(b − m)/(b − a)
```

where:

```text
λ = 4
```

by default.

---

## Equal-Weight Aggregation

Each expert receives:

```text
wᵢ = 1/n
```

The pooled density is:

```text
          n
f(x) = (1/n) Σ fᵢ(x)
         i=1
```
---

## Assessment Confidence-Weighted Aggregation

Weights are calculated as:

```text
wᵢ = ACᵢ / ΣAC
```

The weighted pool is:

```text
f(x) = Σ wᵢfᵢ(x)
```
---

## Yes/No Aggregation

The application calculates:

- equal-weight Yes proportions;
- equal-weight No proportions;
- Assessment Confidence-weighted Yes proportions; and
- Assessment Confidence-weighted No proportions.

---

## Ranking Aggregation

The application calculates:

- mean rank;
- median rank;
- rank standard deviation; and
- pooled ordering.

Lower mean rank indicates higher priority.

---

# Facilitator Reporting

Version 3 includes an automated facilitator report.

The report contains:

- Executive Summary;
- Key Findings;
- Participant Summary;
- Question Summary;
- Round Comparisons;
- Assessment Confidence Summary;
- Assessment Confidence Weighting Effects;
- Stability Following Discussion;
- Revision Following Discussion;
- Divergent Judgement;
- Ranking Results;
- Yes/No Results;
- Facilitator Warnings; and
- Diagnostic Visualizations.

Reports can be exported directly from the application as HTML documents.

---

# Repository Structure

```text
App Version 3/
│
├── App-Server.R
├── App-UI.R
├── Run-Application.R
│
├── Data/
│   └── demo_data.R
│
├── Functions and Helpers/
│   ├── Helper-function.R
│   ├── Packages-Themes.R
│   ├── Plotting-functions.R
│   └── Summarize-Pert.R
│
├── Reporting/
│   ├── Facilitator_report.Rmd
│   ├── Report_data.R
│   ├── Report_diagnostics.R
│   ├── Report_export.R
│   ├── Report_narrative.R
│   └── Report_plots.R
│
├── Google-Sheets-Setup.md
└── README.md
```

---

# Version Status

Version 3 is the current version of the Expert Elicitation Shiny App under active development.

Version 3 extends Version 2 by introducing support for:

- probability questions;
- numeric questions;
- binary Yes/No questions;
- ranking questions;
- question-type specific visualizations;
- question-type specific validation;
- enhanced exports; and
- expanded reporting.

Assessment Confidence weighting, multi-round elicitation, Google Sheets integration, facilitator reporting, and all Version 2 functionality remain supported.
