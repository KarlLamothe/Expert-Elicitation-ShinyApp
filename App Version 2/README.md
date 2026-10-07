# Expert Elicitation Shiny App: Version 2

## Overview

Version 2 of the Expert Elicitation Shiny App extends the original expert-elicitation framework by incorporating:

1. expert-reported **Assessment Confidence (AC)** into the aggregation of expert judgements; and
2. optional **multi-round elicitation**, allowing expert judgements to be compared before and after structured discussion.

As in Version 1, experts characterize uncertainty by providing:

1. a **lowest plausible estimate**;
2. a **best-guess estimate**; and
3. a **highest plausible estimate**.

These three estimates are used to represent each expert's judgement as a PERT distribution.

Version 2 additionally allows each expert to report an assessment confidence from **0 to 100**, representing the self-reported confidence the expert places in the elicited judgement. Assessment confidence modifies the relative contribution of each expert to the pooled distribution while leaving the expert's individual PERT distribution unchanged.

The application calculates both the original equal-weight aggregation and the self-reported assessment confidence-weighted aggregation, allowing the influence of Assessment Confidence on group-level results to be evaluated directly.

Version 2 can be used for either **single-round** or **multi-round** elicitation exercises.

Terminology note: Version 2.0.0 used the term Degree of Belief for the self-reported confidence measure. Subsequent development uses Assessment Confidence to distinguish this measure from other uses of “degree of belief” and interval confidence in the structured expert-judgement literature. Legacy datasets containing a Degree_of_Belief column remain supported.

## Multi-Round Elicitation

Version 2 supports an optional two-round elicitation workflow.

### Round 1

Experts independently provide their initial:

- lowest plausible estimate;
- best-guess estimate;
- highest plausible estimate; and
- assessment confidence.

Individual and pooled results can then be summarized and visualized to support structured discussion.

### Discussion

Following Round 1, participants can review and discuss the elicited judgements. Discussion allows participants to consider different interpretations, assumptions, evidence, and sources of uncertainty represented within the group.

The objective of the discussion is not to force consensus. Experts may retain or revise their individual judgements based on the information exchanged during discussion.

### Round 2

Following discussion, experts independently provide a second set of:

- lowest plausible estimates;
- best-guess estimates;
- highest plausible estimates; and
- assessment confidence values.

The application analyzes Round 1 and Round 2 separately and provides direct comparisons between rounds.

## Comparing Rounds

When multi-round data are supplied, the application provides separate results for each question and elicitation round.

Round-specific outputs include:

- individual expert estimates;
- individual expert PERT distributions;
- equal-weight pooled distributions;
- assessment confidence-weighted pooled distributions;
- simulated mixture distributions;
- cumulative distribution functions; and
- summary statistics.

The application also calculates changes between Round 1 and Round 2, including:

- change in the equal-weight pooled mean;
- change in the assessment confidence-weighted pooled mean; and
- change in average assessment confidence.

Changes are calculated as:

```math
\Delta = \mathrm{Round\ 2} - \mathrm{Round\ 1}
```

A positive value therefore represents an increase between rounds, while a negative value represents a decrease.

Round-specific visualizations are displayed in separate panels to allow changes in expert judgements and pooled distributions to be examined following discussion.

## Single-Round Compatibility

Multi-round elicitation is optional.

If no round variable is provided, the application operates as a single-round elicitation tool and retains the original workflow.

Datasets containing a Round variable but only Round 1 responses can also be analyzed. This allows the application to be used during an ongoing elicitation before post-discussion responses have been collected.

## Individual Expert Distributions

For each expert, the lowest plausible estimate (`a`), best-guess estimate (`m`), and highest plausible estimate (`b`) are used to parameterize a modified PERT distribution.

The Beta shape parameters are calculated as:

```math
\alpha = 1 + \lambda \frac{m-a}{b-a}
```

and

```math
\beta = 1 + \lambda \frac{b-m}{b-a}
```

where the default value of the PERT shape parameter is:

```math
\lambda = 4
```

The resulting Beta distribution is scaled to the expert's interval from `a` to `b`.

Assessment confidence does **not** alter an expert's individual PERT distribution. An expert providing the same lowest plausible, best-guess, and highest plausible estimates will therefore have the same individual PERT distribution regardless of the reported assessment confidence.

## Equal-Weight Aggregation

Version 2 retains the equal-weight linear opinion pool implemented in Version 1.

If there are `n` experts, each expert receives equal weight:

```math
w_i = \frac{1}{n}
```

The pooled probability density is:

```math
f_{\mathrm{equal}}(x)
=
\frac{1}{n}
\sum_{i=1}^{n} f_i(x)
```

where `f_i(x)` represents the PERT probability density for expert `i`.

The equal-weight pool provides a reference distribution representing the original aggregation approach.

## Assessment confidence-Weighted Aggregation

For the assessment confidence-weighted pool, each expert's reported assessment confidence is used as a relative aggregation weight.

For expert `i`, the normalized weight is:

```math
w_i =
\frac{\mathrm{AC}_i}
{\sum_{j=1}^{n} \mathrm{AC}_j}
```

The assessment confidence-weighted pooled density is:

```math
f_{\mathrm{AC}}(x)
=
\sum_{i=1}^{n} w_i f_i(x)
```

Experts reporting higher assessment confidence therefore contribute more strongly to the pooled distribution than experts reporting lower assessment confidence.

The weighting is relative within each question and round. For example, if three experts report Assessment Confidence values of 100, 50, and 50, their normalized contributions to the pooled distribution are:

- Expert 1: 0.50
- Expert 2: 0.25
- Expert 3: 0.25

### Equal Assessment Confidence

If all experts report the same assessment confidence, the normalized weights are equal. The assessment confidence-weighted pool is therefore equivalent to the equal-weight pool regardless of whether the common assessment confidence is high or low.

For example:

```text
20, 20, 20, 20
```

and:

```text
100, 100, 100, 100
```

both produce normalized weights of:

```text
0.25, 0.25, 0.25, 0.25
```

The current implementation therefore uses assessment confidence to represent **relative confidence among experts**. The absolute magnitude of assessment confidence does not independently increase or decrease the uncertainty of the pooled probability distribution.

### Zero Assessment Confidence

An individual expert can report an assessment confidence of zero. In this situation, the expert continues to contribute normally to the equal-weight pool but receives zero weight in the Assessment confidence-weighted pool.

If **all participants report an Assessment Confidence of zero for a question within a round**, a Assessment confidence-weighted distribution cannot be calculated because no relative weights can be assigned.

In this situation:

- the equal-weight results remain available;
- assessment confidence-weighted summaries are reported as unavailable; and
- the application displays a warning identifying the affected question and elicitation round.

## Missing Assessment Confidence

Assessment confidence values are constrained to the range from 0 to 100.

If an assessment confidence value is missing, or if no assessment confidence column is supplied, missing assessment confidence values are not imputed. The associated probability assessment remains included in the equal-weight pool but is excluded from the assessment confidence-weighted pool. Confidence-weighted aggregation is calculated only when at least two participants report assessment confidence and the sum of the reported values is greater than zero.

## Comparing Equal-Weight and AC-Weighted Results

For each question and elicitation round, the application calculates both:

- an equal-weight pooled distribution; and
- an assessment confidence-weighted pooled distribution.

For each distribution, the application calculates:

- mean;
- median;
- 5th percentile; and
- 95th percentile.

The influence of assessment confidence weighting on the pooled mean is calculated as:

```math
\mathrm{AC\ Effect}
=
\mathrm{Mean}_{\mathrm{AC}}
-
\mathrm{Mean}_{\mathrm{Equal}}
```

A positive value indicates that assessment confidence weighting shifts the pooled mean upward relative to equal weighting, while a negative value indicates a downward shift.

A value near zero indicates that assessment confidence weighting has little influence on the pooled mean. This can occur when experts report similar assessment confidence values or when differences in assessment confidence are not systematically associated with differences in the elicited estimates.

## Assessment Confidence Summaries

For each question and round, Version 2 reports:

- mean assessment confidence;
- median assessment confidence;
- minimum assessment confidence; and
- maximum assessment confidence.

For multi-round elicitation, the application additionally reports the change in average assessment confidence between rounds.

## Distribution Summaries

The application uses Monte Carlo simulation to summarize the equal-weight and assessment confidence-weighted mixtures.

By default:

```text
Nsim = 10,000
```

samples are generated for each aggregation approach.

The application additionally generates moment-matched Beta distributions based on the simulated mean and variance of the pooled distributions. These provide a simplified parametric representation of the corresponding mixture distributions.

## Application Features

Version 2 supports:

- selecting between demonstration data, CSV upload, and Google Sheets as input sources;
- downloading a CSV response template;
- validating submitted response data before analysis;
- importing and processing expert elicitation responses;
- optional single-round or multi-round elicitation;
- generating individual expert PERT distributions;
- calculating equal-weight linear opinion pools;
- calculating assessment confidence-weighted linear opinion pools;
- comparing equal-weight and assessment confidence-weighted results;
- comparing Round 1 and Round 2 elicitation results;
- summarizing assessment confidence across participants;
- calculating changes in average assessment confidence between rounds;
- displaying individual expert estimates by question and round;
- displaying pooled probability distributions by question and round;
- displaying simulated mixture distributions by question and round;
- displaying cumulative distribution functions by question and round;
- retrieving updated expert responses during an elicitation exercise;
- summarizing Round 1 versus Round 2 changes; and
- exporting summaries and visualizations.

## Input Methods

Version 2 supports multiple methods for supplying expert elicitation responses. Google Sheets is optional and is not required to use the application.

### Demonstration Data

Built-in demonstration data can be selected within the application to explore the available analyses and visualizations.

The demonstration dataset includes multiple questions, participants, assessment confidence values, and two elicitation rounds.

### CSV Upload

Expert responses can be supplied directly using a CSV file.

A response template can be downloaded from within the application using **Download Response Template**. The template can be opened and edited using spreadsheet software such as Microsoft Excel.

After completing the template:

1. Save the response data as a CSV file.
2. Select **Upload CSV** as the data source.
3. Upload the completed response file.
4. Confirm that the input columns have been mapped correctly.
5. Select the desired question or questions.
6. Select **Run / Refresh Analysis**.

The CSV workflow allows the application to be used without Google Sheets or Google authentication.

### Google Sheets

Google Sheets can optionally be used as a live data source during an elicitation exercise. This allows facilitators to refresh the analysis as participants submit or revise their responses.

For elicitation exercises where participants should not view one another's individual responses, separate participant Google spreadsheets can be dynamically linked to a consolidated facilitator spreadsheet.

Google Sheets requires initial authentication and configuration.

See the Google-Sheets-Setup.md for instructions covering:

- Google authentication and `gs4_auth()`;
- OAuth token handling;
- facilitator spreadsheet configuration;
- separate participant response spreadsheets;
- dynamically linking spreadsheets using `IMPORTRANGE()`; and
- recommended access and privacy practices.

## Input Data

The application is designed around input data containing one row for each expert response to a question within an elicitation round.

A multi-round dataset contains fields corresponding to:

```text
Question
Round
Participant
Lowest_Plausible_Pr
Best_Guess_Pr
Highest_Plausible_Pr
Assessment_confidence
```

For example:

```text
Question  Round  Participant  LPP   BGP   HPP   AC
1         1      P1           0.20  0.40  0.60  70
1         1      P2           0.30  0.50  0.75  65
1         2      P1           0.25  0.50  0.70  85
1         2      P2           0.35  0.55  0.75  75
```

The column-mapping controls within the application allow equivalent columns with different names to be assigned to the appropriate application variables.

The Round column is optional. If no Round column is selected, the data are analyzed using the single-round workflow.

## Input Data Validation

Before an analysis is run, Version 2 checks the submitted response data for common input problems.

For each submitted expert response, the probability estimates must satisfy:

```math
0 < \mathrm{LPP} \leq \mathrm{BGP} \leq \mathrm{HPP} < 1
```

The lowest and highest plausible probabilities must also define an interval with positive width:

```math
\mathrm{LPP} < \mathrm{HPP}
```

If assessment confidence is provided, valid values range from 0 to 100:

```math
0 \leq \mathrm{AC} \leq 100
```

A missing assessment confidence is permitted and is currently interpreted by the application as an assessment confidence of 100.

The application checks for:

- missing probability estimates within a submitted response;
- non-numeric probability estimates;
- lowest plausible probabilities that are less than or equal to 0;
- highest plausible probabilities that are greater than or equal to 1;
- best-guess probabilities that fall outside the corresponding lowest and highest plausible estimates;
- plausible intervals with zero or negative width;
- assessment confidence values outside the permitted range of 0 to 100;
- missing Question or Participant fields within submitted responses;
- missing Round values when a Round column is being used; and
- duplicate responses from the same participant for the same Question and Round.

Participants are **not required to respond to every question or participate in every elicitation round**. Differences in participant numbers among questions or rounds are therefore permitted.

When invalid data are identified, the analysis is stopped and the application displays a validation message identifying the affected **Question, Round, and response row**. Participant identifiers are not displayed in validation messages.

This allows problems in the source data to be corrected before analysis while avoiding the silent modification or reinterpretation of submitted expert judgements.

## Participant Identifiers

Participant identifiers are used internally where required for analysis, including distinguishing individual expert responses, detecting duplicate responses, matching responses across elicitation rounds, and determining participant numbers.

Participant identifiers are not displayed in group-facing visualizations or input-validation messages. Individual expert estimates and distributions can therefore be examined without directly identifying participants in the displayed outputs.

## Repository Structure

```text
App Version 2/
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
├── Google-Sheets-Setup.md
└── README.md
```

`App-UI.R` defines the application's user interface, while `App-Server.R` contains the server-side application logic.

The `Functions and Helpers` directory contains supporting functions for processing, aggregation, simulation, and visualization.

The `Data` directory contains demonstration data that can be used to explore both the assessment confidence and multi-round functionality without using responses from an active elicitation exercise.

## Running Version 2

To run Version 2 locally:

1. Clone or download this repository.
2. Open the repository as an RStudio Project.
3. Navigate to the `App Version 2` directory.
4. Ensure the required R packages are installed.
5. Run `Run-Application.R`.

The demonstration dataset can be selected within the application to explore the available analyses and visualizations.

## Version Status

Version 2 is the current version of the Expert Elicitation Shiny App under active development.

The original implementation is retained as Version 1 to support reproducibility and comparison with subsequent developments.

Version 2 currently extends the original framework through **Assessment confidence weighting** and **optional multi-round elicitation**, allowing changes in expert judgements and reported confidence following structured discussion to be examined explicitly.

Version 2 can do everything that Version 1 did, AND MORE! And its prettier. So that's cool too :)