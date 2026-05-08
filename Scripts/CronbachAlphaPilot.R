###### Cronbach's alpha on pilot study questions
# this is just that class of grad students

# set up
setwd("~/Dropbox/UF/Research/Chapter 3 (R)/Data/EarlySurveys")
require(ltm)

# format and remove first two nonsense rows
pilotResults <- read.csv("20240409PilotResultsNumeric.csv", header = TRUE)
pilotResults <- pilotResults[-c(1:2),]

# pull out sections
ComputingSkillsBefore <- pilotResults[, c(
    "R.skills.1_1", "R.skills.1_2", "R.skills.1_3",
    "R.skills.1_4", "R.skills.1_5", "R.skills.1_6",
    "R.skills.1_7", "R.skills.1_8", "R.skills.1_9",
    "R.skills.1_10", "Computing.programs.1_1"
)]
ComputingSkillsAfter <- pilotResults[, c(
    "R.skills.2_1", "R.skills.2_2", "R.skills.2_3",
    "R.skills.2_4", "R.skills.2_5", "R.skills.2_6",
    "R.skills.2_7", "R.skills.2_8", "R.skills.2_9",
    "R.skills.2_10", "Computing.programs.2_1"
)]
MathSkillsBefore <- pilotResults[, c(
    "Math.skills.1_1", "Math.skills.1_2", "Math.skills.1_3",
    "Math.skills.1_4", "Math.skills.1_5", "Math.skills.1_6",
    "Math.skills.1_7", "Math.skills.1_8"
)]
MathSkillsAfter <- pilotResults[, c(
    "Math.skills.2_1", "Math.skills.2_2", "Math.skills.2_3",
    "Math.skills.2_4", "Math.skills.2_5", "Math.skills.2_6",
    "Math.skills.2_7", "Math.skills.2_8"
)]
MathSkills <- cbind(MathSkillsBefore, MathSkillsAfter)
ComputingSkills <- cbind(ComputingSkillsBefore, ComputingSkillsAfter)

# Cronbach's alpha on math skills
# want higher than 0.70
cronbach.alpha(MathSkillsBefore, standardized = FALSE, CI = TRUE)
cronbach.alpha(MathSkillsAfter, standardized = FALSE, CI = TRUE)
cronbach.alpha(MathSkills, standardized = FALSE, CI = TRUE)

# Cronbach's alpha on computing skills
# want higher than 0.70
cronbach.alpha(ComputingSkillsBefore, standardized = FALSE, CI = TRUE)
cronbach.alpha(ComputingSkillsAfter, standardized = FALSE, CI = TRUE)
cronbach.alpha(ComputingSkills, standardized = FALSE, CI = TRUE)

