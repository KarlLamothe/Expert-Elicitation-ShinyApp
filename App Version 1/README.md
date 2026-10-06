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