# Expert Elicitation Shiny App: Version 2

## Overview

Version 2 of the Expert Elicitation Shiny App extends the original expert-elicitation framework by incorporating:

1. expert-reported **Degree of Belief (DoB)** into the aggregation of expert judgements; and
2. optional **multi-round elicitation**, allowing expert judgements to be compared before and after structured discussion.

As in Version 1, experts characterize uncertainty by providing:

1. a **lowest plausible estimate**;
2. a **best-guess estimate**; and
3. a **highest plausible estimate**.

These three estimates are used to represent each expert's judgement as a PERT distribution.

Version 2 additionally allows each expert to report a Degree of Belief from **0 to 100**, representing the confidence the expert places in the elicited judgement. Degree of Belief modifies the relative contribution of each expert to the pooled distribution while leaving the expert's individual PERT distribution unchanged.

The application calculates both the original equal-weight aggregation and the Degree-of-Belief-weighted aggregation, allowing the influence of Degree of Belief on group-level results to be evaluated directly.

Version 2 can be used for either **single-round** or **multi-round** elicitation exercises.

## Multi-Round Elicitation

Version 2 supports an optional two-round elicitation workflow.

### Round 1

Experts independently provide their initial:

- lowest plausible estimate;
- best-guess estimate;
- highest plausible estimate; and
- Degree of Belief.

Individual and pooled results can then be summarized and visualized to support structured discussion.

### Discussion

Following Round 1, participants can review and discuss the elicited judgements. Discussion allows participants to consider different interpretations, assumptions, evidence, and sources of uncertainty represented within the group.

The objective of the discussion is not to force consensus. Experts may retain or revise their individual judgements based on the information exchanged during discussion.

### Round 2

Following discussion, experts independently provide a second set of:

- lowest plausible estimates;
- best-guess estimates;
- highest plausible estimates; and
- Degree of Belief values.

The application analyzes Round 1 and Round 2 separately and provides direct comparisons between rounds.

## Comparing Rounds

When multi-round data are supplied, the application provides separate results for each question and elicitation round.

Round-specific outputs include:

- individual expert estimates;
- individual expert PERT distributions;
- equal-weight pooled distributions;
- Degree-of-Belief-weighted pooled distributions;
- simulated mixture distributions;
- cumulative distribution functions; and
- summary statistics.

The application also calculates changes between Round 1 and Round 2, including:

- change in the equal-weight pooled mean;
- change in the Degree-of-Belief-weighted pooled mean; and
- change in average Degree of Belief.

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

Degree of Belief does **not** alter an expert's individual PERT distribution. An expert providing the same lowest plausible, best-guess, and highest plausible estimates will therefore have the same individual PERT distribution regardless of the reported Degree of Belief.

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

## Degree-of-Belief-Weighted Aggregation

For the Degree-of-Belief-weighted pool, each expert's reported Degree of Belief is used as a relative aggregation weight.

For expert `i`, the normalized weight is:

```math
w_i =
\frac{\mathrm{DoB}_i}
{\sum_{j=1}^{n} \mathrm{DoB}_j}
```

The Degree-of-Belief-weighted pooled density is:

```math
f_{\mathrm{DoB}}(x)
=
\sum_{i=1}^{n} w_i f_i(x)
```

Experts reporting higher Degree of Belief therefore contribute more strongly to the pooled distribution than experts reporting lower Degree of Belief.

The weighting is relative within each question and round. For example, if three experts report Degree of Belief values of 100, 50, and 50, their normalized contributions to the pooled distribution are:

- Expert 1: 0.50
- Expert 2: 0.25
- Expert 3: 0.25

### Equal Degree of Belief

If all experts report the same Degree of Belief, the normalized weights are equal. The Degree-of-Belief-weighted pool is therefore equivalent to the equal-weight pool regardless of whether the common Degree of Belief is high or low.

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

The current implementation therefore uses Degree of Belief to represent **relative confidence among experts**. The absolute magnitude of Degree of Belief does not independently increase or decrease the uncertainty of the pooled probability distribution.

### Zero Degree of Belief

An individual expert can report a Degree of Belief of zero. In this situation, the expert continues to contribute normally to the equal-weight pool but receives zero weight in the Degree-of-Belief-weighted pool.

If **all participants report a Degree of Belief of zero for a question within a round**, a Degree-of-Belief-weighted distribution cannot be calculated because no relative weights can be assigned.

In this situation:

- the equal-weight results remain available;
- Degree-of-Belief-weighted summaries are reported as unavailable; and
- the application displays a warning identifying the affected question and elicitation round.

## Missing Degree of Belief

Degree of Belief values are constrained to the range from 0 to 100.

If a Degree of Belief value is missing, or if no Degree of Belief column is supplied, a value of **100** is assigned.

This implementation treats the absence of a Degree of Belief value as full acceptance of the expert's stated elicitation choices rather than interpreting a missing value as additional uncertainty.

## Comparing Equal-Weight and DoB-Weighted Results

For each question and elicitation round, the application calculates both:

- an equal-weight pooled distribution; and
- a Degree-of-Belief-weighted pooled distribution.

For each distribution, the application calculates:

- mean;
- median;
- 5th percentile; and
- 95th percentile.

The influence of Degree-of-Belief weighting on the pooled mean is calculated as:

```math
\mathrm{DoB\ Effect}
=
\mathrm{Mean}_{\mathrm{DoB}}
-
\mathrm{Mean}_{\mathrm{Equal}}
```

A positive value indicates that Degree-of-Belief weighting shifts the pooled mean upward relative to equal weighting, while a negative value indicates a downward shift.

A value near zero indicates that Degree-of-Belief weighting has little influence on the pooled mean. This can occur when experts report similar Degree of Belief values or when differences in Degree of Belief are not systematically associated with differences in the elicited estimates.

## Degree of Belief Summaries

For each question and round, Version 2 reports:

- mean Degree of Belief;
- median Degree of Belief;
- minimum Degree of Belief; and
- maximum Degree of Belief.

For multi-round elicitation, the application additionally reports the change in average Degree of Belief between rounds.

## Distribution Summaries

The application uses Monte Carlo simulation to summarize the equal-weight and Degree-of-Belief-weighted mixtures.

By default:

```text
Nsim = 40,000
```

samples are generated for each aggregation approach.

The application additionally generates moment-matched Beta distributions based on the simulated mean and variance of the pooled distributions. These provide a simplified parametric representation of the corresponding mixture distributions.

## Application Features

Version 2 supports:

- importing and processing expert elicitation responses;
- optional single-round or multi-round elicitation;
- generating individual expert PERT distributions;
- calculating equal-weight linear opinion pools;
- calculating Degree-of-Belief-weighted linear opinion pools;
- comparing equal-weight and Degree-of-Belief-weighted results;
- comparing Round 1 and Round 2 elicitation results;
- summarizing Degree of Belief across participants;
- calculating changes in average Degree of Belief between rounds;
- displaying individual expert estimates by question and round;
- displaying pooled probability distributions by question and round;
- displaying simulated mixture distributions by question and round;
- displaying cumulative distribution functions by question and round;
- retrieving updated expert responses during an elicitation exercise;
- summarizing Round 1 versus Round 2 changes; and
- exporting summaries and visualizations.

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
Degree_of_Belief
```

For example:

```text
Question  Round  Participant  LPP   BGP   HPP   DoB
1         1      P1           0.20  0.40  0.60  70
1         1      P2           0.30  0.50  0.75  65
1         2      P1           0.25  0.50  0.70  85
1         2      P2           0.35  0.55  0.75  75
```

The column-mapping controls within the application allow equivalent columns with different names to be assigned to the appropriate application variables.

The Round column is optional. If no Round column is selected, the data are analyzed using the single-round workflow.

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
└── README.md
```

`App-UI.R` defines the application's user interface, while `App-Server.R` contains the server-side application logic.

The `Functions and Helpers` directory contains supporting functions for processing, aggregation, simulation, and visualization.

The `Data` directory contains demonstration data that can be used to explore both the Degree-of-Belief and multi-round functionality without using responses from an active elicitation exercise.

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

Version 2 currently extends the original framework through **Degree-of-Belief weighting** and **optional multi-round elicitation**, allowing changes in expert judgements and reported confidence following structured discussion to be examined explicitly.