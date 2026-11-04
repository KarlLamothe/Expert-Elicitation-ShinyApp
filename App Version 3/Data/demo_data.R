############################################################################
# Demonstration questions
############################################################################

question_texts <- c(
  "Assuming current environmental and management conditions continue, what is the probability that the target population will increase by at least 20% relative to its 2025 abundance within the next 10 years across the assessment area?",
  "Assuming current environmental and management conditions continue, what is the probability that the target population will decline by more than 30% relative to its 2025 abundance within the next 10 years across the assessment area?",
  "Assuming current environmental and management conditions continue, what is the probability that suitable habitat will increase by at least 10% within the next 10 years across the assessment area?",
  "Assuming current environmental and management conditions continue, what is the probability that the population will reach the recovery target within the next 20 years across the assessment area?")

############################################################################
# ROUND 1
############################################################################

demo_round1 <- data.frame(
  Question = rep(1:4, each = 6),
  Question_Text = rep(question_texts, each = 6),
  Participant = rep(c("P1", "P2", "P3", "P4", "P5", "P6"), times = 4),
  Lowest_Plausible_Pr = c(
    # Q1 - strong agreement
    0.30, 0.35, 0.32, 0.28, 0.34, 0.30,
    # Q2 - bimodal
    0.10, 0.15, 0.12, 0.60, 0.65, 0.55,
    # Q3 - high uncertainty
    0.05, 0.10, 0.01, 0.10, 0.15, 0.05,
    # Q4 - disagreement likely to converge
    0.20, 0.25, 0.30, 0.60, 0.55, 0.65),
  
  Best_Guess_Pr = c(
    0.45, 0.50, 0.47, 0.42, 0.48, 0.46,
    0.20, 0.25, 0.18, 0.75, 0.80, 0.70,
    0.30, 0.35, 0.25, 0.40, 0.45, 0.30,
    0.35, 0.40, 0.45, 0.75, 0.70, 0.80),
  
  Highest_Plausible_Pr = c(
    0.60, 0.65, 0.62, 0.58, 0.64, 0.61,
    0.40, 0.45, 0.35, 0.90, 0.95, 0.88,
    0.75, 0.80, 0.70, 0.85, 0.90, 0.75,
    0.60, 0.65, 0.70, 0.90, 0.88, 0.95),
  
  Assessment_Confidence = c(
    # Q1 - confident
    85, 90, 80, 88, 92, 87,
    # Q2 - moderate confidence despite disagreement
    70, 75, 72, 85, 88, 80,
    # Q3 - low confidence
    25, 35, 20, 30, 25, 40,
    # Q4 - moderately confident
    65, 70, 75, 78, 80, 72))

demo_round1$Round <- 1

############################################################################
# ROUND 2
############################################################################

demo_round2 <- demo_round1
demo_round2$Round <- 2

# Q1 - little change
q1 <- demo_round2$Question == 1
demo_round2$Best_Guess_Pr[q1] <-
  demo_round2$Best_Guess_Pr[q1] + c(0.01, 0.00, 0.02, 0.01, 0.00, 0.01)
demo_round2$Assessment_Confidence[q1] <-
  pmin(demo_round2$Assessment_Confidence[q1] + 5, 100)

# Q2 - bimodal pattern persists but narrows slightly
q2 <- demo_round2$Question == 2
demo_round2$Best_Guess_Pr[q2] <- c(0.25, 0.30, 0.28, 0.65, 0.70, 0.62)
demo_round2$Lowest_Plausible_Pr[q2] <- c(0.15, 0.20, 0.18, 0.50, 0.55, 0.48)
demo_round2$Highest_Plausible_Pr[q2] <- c(0.45, 0.50, 0.42, 0.80, 0.85, 0.78)
demo_round2$Assessment_Confidence[q2] <- c(80, 82, 78, 90, 92, 88)

# Q3 - confidence improves somewhat after discussion
q3 <- demo_round2$Question == 3
demo_round2$Assessment_Confidence[q3] <- c(45, 50, 40, 50, 45, 55)
demo_round2$Lowest_Plausible_Pr[q3] <- c(0.10, 0.15, 0.08, 0.15, 0.20, 0.10)
demo_round2$Highest_Plausible_Pr[q3] <- c(0.65, 0.70, 0.60, 0.75, 0.80, 0.65)

# Q4 - strong convergence after discussion
q4 <- demo_round2$Question == 4
demo_round2$Best_Guess_Pr[q4] <- c(0.55, 0.57, 0.58, 0.60, 0.59, 0.61)
demo_round2$Lowest_Plausible_Pr[q4] <- c(0.40, 0.42, 0.45, 0.48, 0.46, 0.50)
demo_round2$Highest_Plausible_Pr[q4] <- c(0.68, 0.70, 0.72, 0.74, 0.73, 0.75)
demo_round2$Assessment_Confidence[q4] <- c(85, 88, 90, 92, 90, 88)

############################################################################
# COMBINE ROUNDS
############################################################################

demo_df <- rbind(demo_round1, demo_round2)
demo_df <- demo_df[, c(
  "Question",
  "Question_Text",
  "Round",
  "Participant",
  "Lowest_Plausible_Pr",
  "Best_Guess_Pr",
  "Highest_Plausible_Pr",
  "Assessment_Confidence")]
row.names(demo_df) <- NULL
