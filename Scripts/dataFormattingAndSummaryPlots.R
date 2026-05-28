#### Looking at the R skills if students have taken a foundational class versus not

# packages
library(tidyverse)
library(ggplot2)
library(patchwork)
library(mice)
library(performance)
library(broom.mixed)
library(kableExtra)

# set up
setwd("~/Desktop/pedagogy/Data")
rm(list = ls())


################################################################################
############################### DATA CLEANING ##################################
################################################################################


# format and remove first two nonsense rows of the Likert/quantitative stuff
surveyValues <- read.csv("DataJan2026.csv", header = TRUE)
surveyValues <- surveyValues[-c(1:2),]
surveyLikert <- surveyValues[,27:87]
head(surveyLikert)
ncol(surveyLikert)

# format and remove first two nonsense rows of the labeled data
surveyText <- read.csv("DataJan2026_Text.csv", header = TRUE)
surveyText <- surveyText[-c(1:2),]
surveyTextMinusLikert <- surveyText[,c(1:26, 88:ncol(surveyText))]
head(surveyTextMinusLikert)
nrow(surveyTextMinusLikert)
ncol(surveyTextMinusLikert)

# put them together
survey <- cbind(surveyTextMinusLikert, surveyLikert)
head(survey)
nrow(survey)

# remove all rows where Participant.ID is blank
survey <- survey[!is.na(survey$Participant.ID) & survey$Participant.ID != "", ]
nrow(survey)

# clean column names
names(survey) <- gsub("#", "_", names(survey))
names(survey) <- gsub(" ", ".", names(survey))

# remove the following columns
cols_to_remove <- c(
    "EndDate", "StartDate", "Progress", "Duration..in.seconds.",
    "Finished", "RecipientLastName", "RecipientFirstName",
    "RecipientEmail", "ExternalReference", 
    "DistributionChannel", "UserLanguage", "Q1"
)
survey <- survey[, !(names(survey) %in% cols_to_remove)]
head(survey)

## ended up not needing this because I remove them later with the "Other" filter
# remove the rows from the pilot survey with the graduate class
# survey <- survey %>%
#   filter(as.Date(RecordedDate) > as.Date("2024-04-09"))
# nrow(survey)

# if column called "Current.classes" OR "Q35" contains "Computational Problem Solving in Wildlife Ecology Using R (WIS 4934)" anywhere, put a 1 in "Foundational" column
survey$Foundational <- NA
survey$Foundational <- ifelse(
  is.na(survey$Current.classes) & is.na(survey$Previous.classes),
  NA,
  ifelse(
    grepl("Computational Problem Solving in Wildlife Ecology Using R \\(WIS 4934\\)", 
          survey$Current.classes, fixed = FALSE) |
      grepl("Computational Problem Solving in Wildlife Ecology Using R \\(WIS 4934\\)", 
            survey$Previous.classes, fixed = FALSE),
    1, 0
  )
)

# if "Population Ecology" is mentioned anywhere in Current.classes or Previous.classes, put a 1 in "PopEco" column, otherwise 0
survey$PopEco <- NA
survey$PopEco <- ifelse(
  is.na(survey$Current.classes) & is.na(survey$Previous.classes),
  NA,
  ifelse(
    grepl("Population Ecology", survey$Current.classes, fixed = TRUE) |
      grepl("Population Ecology", survey$Previous.classes, fixed = TRUE),
    1, 0
  )
)

# "Quantitative Ecology" is mentioned anywhere in Current.classes or Previous.classes, put a 1 in "QuantEco" column, otherwise 0
survey$Quant <- NA
survey$Quant <- ifelse(
  is.na(survey$Current.classes) & is.na(survey$Previous.classes),
  NA,
  ifelse(
    grepl("Quantitative Wildlife Ecology", survey$Current.classes, fixed = TRUE) |
      grepl("Quantitative Wildlife Ecology", survey$Previous.classes, fixed = TRUE),
    1, 0
  )
)

# further cat
survey <- survey %>%
    mutate(
        # was the R course taken THIS semester specifically?
        Found_current = grepl("Computational Problem Solving in Wildlife Ecology Using R \\(WIS 4934\\)",
            Current.classes,
            fixed = FALSE
        ),
        # was any advanced course taken THIS semester?
        Advanced_current = grepl("Population Ecology", Current.classes, fixed = TRUE) |
            grepl("Quantitative Wildlife Ecology", Current.classes, fixed = TRUE),
        # was any advanced course taken in a PREVIOUS semester?
        Advanced_previous = grepl("Population Ecology", Previous.classes, fixed = TRUE) |
            grepl("Quantitative Wildlife Ecology", Previous.classes, fixed = TRUE)
    )

# group by Participant.ID and if there is a 1 for "Foundational" anywhere, set all to 1
survey <- survey %>%
    group_by(Participant.ID) %>%
    mutate(
        PopEco = ifelse(any(PopEco == 1), 1, PopEco),
        Quant = ifelse(any(Quant == 1), 1, Quant),
        Foundational = ifelse(any(Foundational == 1), 1, Foundational),
        # timing
        # concurrent = R class and advanced course in the same semester
        took_advanced_concurrent = any(Found_current & Advanced_current),
        # sequential = currently in an advanced course while R class is in Previous.classes
        # only catches the intended direction: R class first, then advanced course
        # students who did advanced before R class intentionally fall through to "None"
        took_R_before_advanced = any(
            Advanced_current &
            grepl("Computational Problem Solving in Wildlife Ecology Using R \\(WIS 4934\\)",
                  Previous.classes, fixed = FALSE)
        )
    ) %>%
    ungroup()

# sort into groups based on Foundational, PopEco, and Quant status
survey <- survey %>%
    mutate(
        group_5 = case_when(
            Foundational == 1 & Quant == 1 & PopEco == 1 ~ "R Class, Quant, and Pop Eco",
            Foundational == 1 & (Quant == 1 | PopEco == 1) ~ "R Class and Quant or Pop Eco",
            Foundational == 0 & Quant == 1 & PopEco == 1 ~ "Quant and Pop Eco",
            Foundational == 0 & (Quant == 1 | PopEco == 1) ~ "Quant or Pop Eco",
            Foundational == 1 & Quant == 0 & PopEco == 0 ~ "R Class only",
            TRUE ~ "None"
        ),
        group_3 = case_when(
            Foundational == 1 & (Quant == 1 | PopEco == 1) ~ "R Class and an advanced course",
            Foundational == 0 & (Quant == 1 | PopEco == 1) ~ "Quant and/or Pop Eco only",
            Foundational == 1 & Quant == 0 & PopEco == 0 ~ "R Class only",
            TRUE ~ "None"
        ),
        group_4 = case_when(
            # concurrent checked first — if they took R and advanced in the same semester,
            # that wins even if R also appears in a previous semester
            Foundational == 1 & (PopEco == 1 | Quant == 1) & took_advanced_concurrent ~ "R Class concurrent with advanced",
            # sequential — R class finished before the advanced course started
            Foundational == 1 & (PopEco == 1 | Quant == 1) & took_R_before_advanced   ~ "R Class before advanced",
            # R class only, no advanced
            Foundational == 1 & PopEco == 0 & Quant == 0                              ~ "R Class only",
            # advanced but no R class
            Foundational == 0 & (PopEco == 1 | Quant == 1)                            ~ "Advanced only",
            TRUE ~ "None"
        )
    )

# how many records of each Foundational status?
check <- survey[,c("Foundational", "Current.classes", "Previous.classes")]
table(check$Foundational)
length(unique(survey$Participant.ID)) # number of individuals

# how many individuals have taken Comp Class?
indivs <- survey %>%
  filter(Foundational == 1) %>%
  pull(Participant.ID) %>%
  unique()
length(indivs) # 26

# how many individuals per group
survey %>%
    distinct(Participant.ID, group_5) %>%
    count(group_5)
survey %>%
    distinct(Participant.ID, group_3) %>%
    count(group_3)
survey %>%
    distinct(Participant.ID, group_4) %>%
    count(group_4)


################################################################################
############################### AVERAGE SCORES #################################
################################################################################

# convert metric columns to numeric first
survey <- survey %>%
  mutate(across(starts_with(c("Anxiety", "R.skills", "R.use", "Math.skills", "Computing.programs")), 
                ~as.numeric(.)))

# calculate composite scores for each metric
# anxiety composite scores (average of items)
anxiety_pre_cols <- c("Anxiety.1_1", "Anxiety.1_2")
anxiety_post_cols <- c("Anxiety.2_1", "Anxiety.2_2")

survey$Anxiety_pre <- rowMeans(survey[, anxiety_pre_cols], na.rm = TRUE)
survey$Anxiety_post <- rowMeans(survey[, anxiety_post_cols], na.rm = TRUE)

# reflect anxiety so higher = better (1-5 scale, so 6 - x)
survey$Anxiety_pre <- 6 - survey$Anxiety_pre
survey$Anxiety_post <- 6 - survey$Anxiety_post

# # R skills composite scores
# r_skills_pre_cols <- grep("^R.skills.1_", names(survey), value = TRUE)
# r_skills_post_cols <- grep("^R.skills.2_", names(survey), value = TRUE)

# survey$R_skills_pre <- rowMeans(survey[, r_skills_pre_cols], na.rm = TRUE)
# survey$R_skills_post <- rowMeans(survey[, r_skills_post_cols], na.rm = TRUE)

# math skills composite scores
math_pre_cols <- grep("^Math.skills.1_", names(survey), value = TRUE)
math_post_cols <- grep("^Math.skills.2_", names(survey), value = TRUE)

survey$Math_skills_pre <- rowMeans(survey[, math_pre_cols], na.rm = TRUE)
survey$Math_skills_post <- rowMeans(survey[, math_post_cols], na.rm = TRUE)

# computing programs composite scores
r_use_pre_cols <- grep("^R.use.1_", names(survey), value = TRUE)
r_use_post_cols <- grep("^R.use.2_", names(survey), value = TRUE)
r_skills_pre_cols <- grep("^R.skills.1_", names(survey), value = TRUE)
r_skills_post_cols <- grep("^R.skills.2_", names(survey), value = TRUE)
computing_pre_cols <- c(r_skills_pre_cols, r_use_pre_cols, grep("^Computing.programs.1_", names(survey), value = TRUE)[1]) # only R 
computing_post_cols <- c(r_skills_post_cols, r_use_post_cols, grep("^Computing.programs.2_", names(survey), value = TRUE)[1]) # only R 

survey$Computing_pre <- rowMeans(survey[, computing_pre_cols], na.rm = TRUE)
survey$Computing_post <- rowMeans(survey[, computing_post_cols], na.rm = TRUE)

# calculate change scores
survey <- survey %>%
  mutate(
    Anxiety_change = Anxiety_post - Anxiety_pre,
    #R_skills_change = R_skills_post - R_skills_pre, # just lumped into computing
    Math_skills_change = Math_skills_post - Math_skills_pre,
    Computing_change = Computing_post - Computing_pre
  )




################################################################################
############################### MORE FORMATTING #################################
################################################################################


# determine which class number this is for each student
# this assumes students take classes sequentially!!!!!!!!!
survey <- survey %>%
  group_by(Participant.ID) %>%
  arrange(Participant.ID, RecordedDate) %>%
  mutate(class_number = row_number()) %>%
  ungroup()
nrow(survey)

# remove entomology students (aka Current.classes == "Other"); this takes out the pilot students too
survey <- survey %>%
  filter(Current.classes != "Other")
nrow(survey)

# convert empty strings to "No"
survey$High.school.particip <- ifelse(
    survey$High.school.particip == "" | is.na(survey$High.school.particip),
    "No",
    as.character(survey$High.school.particip)
)
# refactor to drop the empty level
survey$High.school.particip <- factor(survey$High.school.particip)


# first, rename columns to have a consistent pattern
data_for_long <- survey %>%
  dplyr::select(Participant.ID, class_number, Foundational, PopEco, Quant,
                group_5, group_3, group_4,
                Anxiety_pre, Anxiety_post, 
                #R_skills_pre, R_skills_post,
                Math_skills_pre, Math_skills_post,
                Computing_pre, Computing_post) %>%
  rename(
    Anxiety.pre = Anxiety_pre,
    Anxiety.post = Anxiety_post,
    #RSkills.pre = R_skills_pre,
    #RSkills.post = R_skills_post,
    MathSkills.pre = Math_skills_pre,
    MathSkills.post = Math_skills_post,
    Computing.pre = Computing_pre,
    Computing.post = Computing_post
  )

# now pivot with the corrected names
survey_long <- data_for_long %>%
  pivot_longer(
    cols = c(Anxiety.pre:Computing.post),
    names_to = c("metric", "time"),
    names_sep = "\\.",
    values_to = "score"
  ) %>%
  mutate(
    time = factor(time, levels = c("pre", "post")),
    time_numeric = as.numeric(time) - 1,  # 0 for pre, 1 for post
    metric = factor(metric, levels = c("Anxiety", "MathSkills", "Computing"),
                    labels = c("Anxiety", "Math Skills", "Computing")),
    Foundational = factor(Foundational, labels = c("No Foundation", "Foundation")),
    PopEco = factor(PopEco, labels = c("No PopEco", "PopEco")),
    Quant = factor(Quant, labels = c("No Quant", "Quant")),
    group_5 = factor(group_5, levels = c("None", "R Class only", "Quant or Pop Eco only", 
                                        "R Class and Quant or Pop Eco", "Quant and Pop Eco", 
                                        "R Class, Quant, and Pop Eco")),
    group_3 = factor(group_3, levels = c("None", "R Class only", "Quant and/or Pop Eco only", "R Class and an advanced course")),
    group_4 = factor(group_4, levels = c("None", "Advanced only", "R Class only", "R Class before advanced", "R Class concurrent with advanced")),
    student_id = as.factor(Participant.ID)
  ) %>%
  filter(!is.na(score))  # remove missing values


# save data
write.csv(survey_long, "formatted_survey_long.csv", row.names = FALSE)
write.csv(survey, "formatted_survey.csv", row.names = FALSE)
write.csv(survey[,c("ResponseId", "Participant.ID", "group_4", "EssayProduceAGraph", "EssayLearningR", "EssayRBenefits")], "essay_questions.csv", row.names = FALSE)


################################################################################
################################ SUMMARY STATS #################################
################################################################################

# summary stats by group and pre/post
summary_stats <- survey_long %>%
  group_by(Foundational, metric, time) %>%
  summarise(
    n = n(),
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    se = sd / sqrt(n),
    median = median(score, na.rm = TRUE),
    q1 = quantile(score, 0.25, na.rm = TRUE),
    q3 = quantile(score, 0.75, na.rm = TRUE),
    .groups = "drop"
  )
summary_stats

# summary by class sequence number
summary_by_class <- survey_long %>%
  group_by(Foundational, metric, time, class_number) %>%
  summarise(
    n = n(),
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    .groups = "drop"
  )
summary_by_class

# change scores summary
change_summary <- survey %>%
  filter(!is.na(Foundational)) %>%
  group_by(Foundational) %>%
  summarise(
    n_students = n_distinct(Participant.ID),
    across(ends_with("_change"), 
           list(mean = ~mean(., na.rm = TRUE),
                se = ~sd(., na.rm = TRUE) / sqrt(n_students),
                sd = ~sd(., na.rm = TRUE)),
           .names = "{.col}_{.fn}")
  )
change_summary


########## plot summary stats
summary_stats_nona <- summary_stats %>%
  filter(!is.na(metric), !is.na(time))

# plot mean plus/minus SE
ggplot(summary_stats_nona, aes(x = time, y = mean, fill = Foundational)) +
  geom_col(position = position_dodge(width = 0.7)) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    position = position_dodge(width = 0.7), width = 0.2
  ) +
  facet_wrap(~ metric, scales = "free_y") +
  labs(x = "Time", y = "Mean score ± SE", fill = "Foundational") +
  theme_minimal() +
  ggtitle("Mean Scores Pre- and Post-Class by Foundational Status")
ggsave("../Figures/MeanScoresPrePost_bars.png", width = 10, height = 6)

# violin plot
ggplot(survey_long, aes(x = time, y = score, fill = Foundational)) +
  geom_violin(position = position_dodge(width = 0.7), alpha = 0.5) +
  geom_boxplot(position = position_dodge(width = 0.7), width = 0.1, outlier.shape = NA) +
  facet_wrap(~ metric, scales = "free_y") +
  labs(x = "Time", y = "Score", fill = "Foundational") +
  theme_minimal() +
  ggtitle("Score Distributions Pre- and Post-Class by Foundational Status")

# boxplot
ggplot(survey_long, aes(x = time, y = score, fill = Foundational)) +
  geom_boxplot(position = position_dodge(width = 0.7)) +
  facet_wrap(~ metric, scales = "free_y") +
  labs(x = "Time", y = "Score", fill = "Foundational") +
  theme_minimal() +
  ggtitle("Score Distributions Pre- and Post-Class by Foundational Status")

# mean point with standard error
ggplot(summary_stats_nona, aes(x = time, y = mean, color = Foundational, group = Foundational)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(width = 0.3), width = 0.2) +
  geom_line(position = position_dodge(width = 0.3)) +
  facet_wrap(~ metric, scales = "free_y") +
  labs(x = "Time", y = "Mean score ± SE", color = "Foundational") +
  theme_minimal() +
  ggtitle("Mean Scores Pre- and Post-Class by Foundational Status") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)
  )
ggsave("../Figures/MeanScoresPrePost_points.png", width = 10, height = 6)

# mean point with 95% CI
ggplot(summary_stats_nona, aes(x = time, y = mean, color = Foundational, group = Foundational)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se),
                position = position_dodge(width = 0.3), width = 0.2) +
  geom_line(position = position_dodge(width = 0.3)) +
  facet_wrap(~ metric, scales = "free_y") +
  labs(x = "Time", y = "Mean score (95% CI)", color = "Foundational") +
  theme_minimal() +
  ggtitle("Mean Scores Pre- and Post-Class by Foundational Status") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)
  )
ggsave("../Figures/MeanScoresPrePost_CI_points.png", width = 10, height = 6)




################################################################################
############################## PLOT SUMMARIES ##################################
################################################################################

########## Plot change summary
change_summary_long <- change_summary %>%
  pivot_longer(
    cols = matches("_change_mean"),
    names_to = "metric",
    names_pattern = "(.*)_change_mean",
    values_to = "change_mean"
  ) %>%
  pivot_longer(
    cols = matches("_change_se"),
    names_to = "metric_se",
    names_pattern = "(.*)_change_se",
    values_to = "change_se"
  ) %>%
  filter(metric == metric_se) %>% 
  dplyr::select(-metric_se)

# clean metric labels a bit
change_summary_long <- change_summary_long %>%
  mutate(metric = recode(metric,
                         "Anxiety" = "Anxiety",
                         "R_skills" = "R skills",
                         "Math_skills" = "Math skills",
                         "Computing" = "Computing"))


# plot it
dodge <- 0.9
ggplot(change_summary_long, aes(x = metric, y = change_mean, fill = factor(Foundational))) +
  geom_col(position = position_dodge(width = dodge)) +
  geom_errorbar(aes(ymin = change_mean - change_se, ymax = change_mean + change_se),
                position = position_dodge(width = dodge), width = 0.2) +
  labs(x = "Metric", y = "Change in metric score (mean ± SE)", fill = "Foundational") +
  theme_minimal() +
  # title
  ggtitle("Mean Change in Scores from Pre- to Post-Class")
ggsave("../Figures/MeanChangeInScores.png", width = 8, height = 6)



################################################################################
################################ PLOT TABLES ###################################
################################################################################


# table 1: summary statistics by Group, Metric, and Time
summary_table <- summary_stats %>%
  mutate(
    mean_se = sprintf("%.2f (%.3f)", mean, se),  # cshanged to use se instead of sd
    median_iqr = sprintf("%.1f [%.1f, %.1f]", median, q1, q3)
  ) %>%
  dplyr::select(Foundational, metric, time, n, mean_se) %>%
  arrange(metric, Foundational, time)

kbl(summary_table, 
    col.names = c("Group", "Metric", "Time", "N", "Mean (SE)"), 
    caption = "Summary Statistics by Group, Metric, and Time",
    align = c("l", "l", "l", "c", "c", "c")) %>%
  kable_classic(font_size = 14, html_font = "TimesNewRoman") %>%
  row_spec(0, bold = TRUE) %>%
  collapse_rows(columns = 1:2, valign = "middle") %>%
  save_kable(file = "../Figures/Tables/summary_statistics.png", zoom = 2)

# table 2: change score summary by group
change_table <- survey %>%
  dplyr::filter(!is.na(Foundational)) %>%
  group_by(Foundational) %>%
  summarise(
    n = n_distinct(Participant.ID),
    # calculate SE instead of SD for each metric
    Anxiety = sprintf("%.2f (%.3f)", 
                      mean(Anxiety_change, na.rm = TRUE), 
                      sd(Anxiety_change, na.rm = TRUE) / sqrt(sum(!is.na(Anxiety_change)))),
    # `R Skills` = sprintf("%.2f (%.3f)", 
    #                      mean(R_skills_change, na.rm = TRUE), 
    #                      sd(R_skills_change, na.rm = TRUE) / sqrt(sum(!is.na(R_skills_change)))),
    `Math Skills` = sprintf("%.2f (%.3f)", 
                            mean(Math_skills_change, na.rm = TRUE), 
                            sd(Math_skills_change, na.rm = TRUE) / sqrt(sum(!is.na(Math_skills_change)))),
    Computing = sprintf("%.2f (%.3f)", 
                        mean(Computing_change, na.rm = TRUE), 
                        sd(Computing_change, na.rm = TRUE) / sqrt(sum(!is.na(Computing_change))))
  ) %>%
  mutate(Group = ifelse(Foundational == 0, "No Foundation", "Foundation")) %>%
  dplyr::select(Group, n, Anxiety, `Math Skills`, Computing)

kbl(change_table,
    caption = "Mean Change Scores (Post - Pre) by Group",
    align = c("l", "c", "c", "c", "c"),
    col.names = c("Group", "N", "Anxiety (SE)*", "Math Skills (SE)", "Computing (SE)")) %>%
  kable_classic(font_size = 14, html_font = "TimesNewRoman") %>%
  row_spec(0, bold = TRUE) %>%
  footnote(general = c("*Lower scores indicate improvement for Anxiety"),
           general_title = "Notes: ",
           footnote_as_chunk = TRUE) %>%
  save_kable(file = "../Figures/Tables/change_scores.png", zoom = 2)

# table 3: pre-post comparisons within groups
within_group_table <- survey_long %>%
  dplyr::filter(!is.na(Foundational)) %>%
  group_by(Foundational, metric, time) %>%
  summarise(
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),  # calculate standard error
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = time, 
              values_from = c(mean, sd, se, n)) %>%
  mutate(
    Pre = sprintf("%.2f (%.3f)", mean_pre, se_pre),  
    Post = sprintf("%.2f (%.3f)", mean_post, se_post),  
    Change = sprintf("%.2f", mean_post - mean_pre),
    `% Change` = sprintf("%.1f%%", ((mean_post - mean_pre) / mean_pre) * 100),
    `N (Pre/Post)` = sprintf("%d/%d", n_pre, n_post)
  ) %>%
  dplyr::select(Foundational, metric, Pre, Post, Change, `% Change`)

kbl(within_group_table,
    caption = "Pre-Post Comparisons Within Groups",
    align = c("l", "l", "c", "c", "c", "c", "c"),
    col.names = c("Group", "Metric", "Pre Mean (SE)", "Post Mean (SE)", "Change", "% Change")) %>%
  kable_classic(font_size = 14, html_font = "TimesNewRoman") %>%
  row_spec(0, bold = TRUE) %>%
  collapse_rows(columns = 1, valign = "middle") %>%
  save_kable(file = "../Figures/Tables/within_group_comparison.png", zoom = 2)