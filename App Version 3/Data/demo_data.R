############################################################################
# DEMO DATASET
# Four question types:
#  Q1 = Probability
#  Q2 = Numeric
#  Q3 = Yes/No
#  Q4 = Ranking
############################################################################

question_texts <- c(
  "What is the probability that the population will increase by at least 20% within the next 10 years?",
  "What will the population abundance be in 2035?",
  "Should additional recovery actions be implemented within the next five years?",
  "Rank the following invasive carp species according to management priority."
)

############################################################################
# PROBABILITY QUESTION (Q1)
############################################################################

prob_round1 <- data.frame(
  Question = 1,
  Question_Type = "probability",
  Question_Text = question_texts[1],
  Round = 1,
  Participant = paste0("P", 1:6),
  Lowest_Plausible_Pr = c(0.30, 0.35, 0.32, 0.28, 0.34, 0.30),
  Best_Guess_Pr = c(0.45, 0.50, 0.47, 0.42, 0.48, 0.46),
  Highest_Plausible_Pr = c(0.60, 0.65, 0.62, 0.58, 0.64, 0.61),
  Assessment_Confidence = c(85, 90, 80, 88, 92, 87)
)

prob_round2 <- prob_round1
prob_round2$Round <- 2
prob_round2$Best_Guess_Pr <- c(0.50, 0.52, 0.51, 0.49, 0.53, 0.50)
prob_round2$Assessment_Confidence <- c(90, 95, 88, 92, 96, 91)

############################################################################
# NUMERIC QUESTION (Q2)
############################################################################

num_round1 <- data.frame(
  Question = 2,
  Question_Type = "numeric",
  Question_Text = question_texts[2],
  Round = 1,
  Participant = paste0("P", 1:6),
  Lowest_Plausible_Pr = c(2000, 2500, 1800, 2200, 2400, 2100),
  Best_Guess_Pr = c(4000, 5000, 3500, 4500, 4800, 4200),
  Highest_Plausible_Pr = c(7000, 8000, 6000, 7500, 7800, 7200),
  Assessment_Confidence = c(70, 75, 72, 78, 80, 74)
)

num_round2 <- num_round1
num_round2$Round <- 2
num_round2$Best_Guess_Pr <- c(3500, 4200, 3200, 4000, 4300, 3800)
num_round2$Assessment_Confidence <- c(78, 82, 80, 84, 86, 80)

############################################################################
# YES/NO QUESTION (Q3)
############################################################################

yesno_round1 <- data.frame(
  Question = 3,
  Question_Type = "yesno",
  Question_Text = question_texts[3],
  Round = 1,
  Participant = paste0("P", 1:6),
  Response = c("Yes", "Yes", "No", "Yes", "No", "No"),
  Assessment_Confidence = c(75, 80, 65, 85, 60, 70)
)

yesno_round2 <- yesno_round1
yesno_round2$Round <- 2
yesno_round2$Response <- c("Yes", "Yes", "No", "Yes", "Yes", "No")
yesno_round2$Assessment_Confidence <- c(85, 88, 75, 92, 80, 82)

############################################################################
# RANKING QUESTION (Q4)
############################################################################

ranking_round1 <- data.frame(
  Question = 4,
  Question_Type = "ranking",
  Question_Text = question_texts[4],
  Round = 1,
  
  Participant = c(
    rep("P1", 3),
    rep("P2", 3),
    rep("P3", 3),
    rep("P4", 3),
    rep("P5", 3),
    rep("P6", 3)
  ),
  
  Option = rep(
    c("Bighead Carp", "Grass Carp", "Silver Carp"),
    6
  ),
  
  Rank = c(
    1,2,3,  # P1
    1,2,3,  # P2
    2,1,3,  # P3
    3,1,2,  # P4
    3,1,2,  # P5
    2,3,1   # P6
  )
)

ranking_round2 <- data.frame(
  Question = 4,
  Question_Type = "ranking",
  Question_Text = question_texts[4],
  Round = 2,
  
  Participant = c(
    rep("P1", 3),
    rep("P2", 3),
    rep("P3", 3),
    rep("P4", 3),
    rep("P5", 3),
    rep("P6", 3)
  ),
  
  Option = rep(
    c("Bighead Carp", "Grass Carp", "Silver Carp"),
    6
  ),
  
  Rank = c(
    1,2,3,  # P1
    1,2,3,  # P2
    1,2,3,  # P3
    2,1,3,  # P4
    2,1,3,  # P5
    2,3,1   # P6
  )
)

############################################################################
# COMBINE ALL QUESTION TYPES
############################################################################

demo_df <- dplyr::bind_rows(
  prob_round1,
  prob_round2,
  num_round1,
  num_round2,
  yesno_round1,
  yesno_round2,
  ranking_round1,
  ranking_round2
)

row.names(demo_df) <- NULL