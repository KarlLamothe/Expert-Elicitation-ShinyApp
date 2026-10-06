# Expert Elicitation Shiny App: Version 2

## Overview

Version 2 of the Expert Elicitation Shiny App extends the original expert-elicitation framework by incorporating an expert-reported **Degree of Belief (DoB)** into the aggregation of expert judgements.

As in Version 1, experts characterize uncertainty by providing:

1. a **lowest plausible estimate**;
2. a **best-guess estimate**; and
3. a **highest plausible estimate**.

These three estimates are used to represent each expert's judgement as a PERT distribution.

Version 2 additionally allows each expert to report a Degree of Belief from **0 to 100**, representing the confidence the expert places in the elicited judgement. Degree of Belief is used to modify the relative contribution of each expert to the pooled distribution while leaving the expert's individual PERT distribution unchanged.

Both the original equal-weight aggregation and the Degree-of-Belief-weighted aggregation are calculated, allowing the influence of Degree of Belief on the group-level result to be evaluated directly.

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

where the default value of the PERT shape parameter is $\lambda = 4$. 

The resulting Beta distribution is scaled to the expert's interval from `a` to `b`.

Degree of Belief does **not** alter these individual distributions. An expert providing the same lowest plausible, best-guess, and highest plausible estimates will therefore have the same individual PERT distribution regardless of the reported Degree of Belief.

## Equal-Weight Aggregation

Version 2 retains the equal-weight linear opinion pool implemented in Version 1.

If there are $n$ experts, each expert receives equal weight:

```math
w_i = \frac{1}{n}
```

The pooled probability density is therefore:

```math
f_{\mathrm{equal}}(x)
=
\frac{1}{n}
\sum_{i=1}^{n} f_i(x)
```

where $f_i(x)$ is the PERT probability density for expert $i$.

The equal-weight pool provides a reference distribution representing the original aggregation approach.

## Degree-of-Belief-Weighted Aggregation

For the Degree-of-Belief-weighted pool, each expert's reported Degree of Belief is used as a relative aggregation weight.

For expert \(i\), the normalized weight is:

```math
w_i =
\frac{\mathrm{DoB}_i}
{\sum_{j=1}^{n} \mathrm{DoB}_j}
```

The Degree-of-Belief-weighted pooled density is then:

```math
f_{\mathrm{DoB}}(x)
=
\sum_{i=1}^{n} w_i f_i(x)
```

Consequently, experts reporting higher Degree of Belief contribute more strongly to the pooled distribution than experts reporting lower Degree of Belief.

The weighting is relative within each question. For example, if three experts report Degree of Belief values of 100, 50, and 50, their normalized contributions to the pooled distribution are:

- Expert 1: 0.50
- Expert 2: 0.25
- Expert 3: 0.25

Absolute Degree of Belief values therefore matter through their relative values among experts.

### Equal Degree of Belief

If all experts report the same Degree of Belief, the normalized weights are equal. In this situation, the Degree-of-Belief-weighted pool is equivalent to the equal-weight pool regardless of whether all experts report high or low Degree of Belief.

For example, Degree of Belief values of:

```text
20, 20, 20, 20
```

and:

```text
100, 100, 100, 100
```

both produce equal normalized weights of:

```text
0.25, 0.25, 0.25, 0.25
```

Thus, the current implementation uses Degree of Belief to represent **relative confidence among experts**, rather than using the absolute magnitude of Degree of Belief to increase or decrease overall uncertainty.

## Missing Degree of Belief

Degree of Belief values are constrained to the range from 0 to 100.

If a Degree of Belief value is missing, or if no Degree of Belief column is supplied, a value of **100** is assigned.

This implementation treats the absence of a Degree of Belief value as full acceptance of the expert's stated elicitation choices rather than interpreting a missing value as additional uncertainty.

## Comparing Equal-Weight and DoB-Weighted Results

The application calculates both aggregation approaches for each question:

- equal-weight pooled distribution; and
- Degree-of-Belief-weighted pooled distribution.

For each distribution, the application calculates:

- mean;
- median;
- 5th percentile; and
- 95th percentile.

The application also calculates a `DoB_Effect`:

```math
\mathrm{DoB\ Effect}
=
\mathrm{Mean}_{\mathrm{DoB}}
-
\mathrm{Mean}_{\mathrm{Equal}}
```

A positive value indicates that incorporating Degree of Belief shifts the pooled mean upward, while a negative value indicates a downward shift.

A value near zero indicates that Degree-of-Belief weighting has little influence on the pooled mean. This can occur when experts report similar Degree of Belief values or when differences in Degree of Belief are not systematically associated with differences in the elicited estimates.

## Degree of Belief Summaries

For each question, Version 2 also reports:

- mean Degree of Belief;
- median Degree of Belief;
- minimum Degree of Belief; and
- maximum Degree of Belief.

These measures provide context for interpreting the influence of Degree of Belief on the pooled results.

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

- processing expert elicitation responses;
- generating individual expert PERT distributions;
- calculating equal-weight linear opinion pools;
- calculating Degree-of-Belief-weighted linear opinion pools;
- comparing equal-weight and Degree-of-Belief-weighted results;
- summarizing Degree of Belief across participants;
- displaying individual and aggregated probability distributions;
- displaying cumulative distribution functions;
- retrieving updated expert responses during an elicitation exercise; and
- exporting summaries and visualizations.

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

`App-UI.R` defines the application's user interface, while `App-Server.R` contains the server-side application logic. Supporting functions for processing, summarizing, and plotting elicitation results are contained in `Functions and Helpers`.

## Running Version 2

To run Version 2 locally:

1. Download or clone the repository.
2. Open the `App Version 2` directory in R or RStudio.
3. Ensure the required R packages are installed.
4. Run `Run-Application.R`.

Demonstration data are provided in the `Data` directory for exploring the application without using responses from an active elicitation exercise.

## Version Status

Version 2 is the current version of the Expert Elicitation Shiny App under active development.

The original equal-weight implementation is retained as Version 1 to support reproducibility and comparison between the original and Degree-of-Belief-weighted approaches.