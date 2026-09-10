library(dplyr)

# setwd("Data")
rm(list = ls())

################################################################################
############################ DATA PREPARATION ##################################
################################################################################

survey <- read.csv("formatted_survey.csv")

survey$Computer.age <- factor(survey$Computer.age,
    levels = c("0 - 2 years old", "2 - 4 years old", "4 - 6 years old", "6+ years old"),
    ordered = TRUE
)
survey$Laptop.issues <- factor(survey$Laptop.issues,
    levels = c("Never", "Rarely", "Sometimes", "Often", "Always"),
    ordered = TRUE
)
survey$Gender <- factor(survey$Gender)
survey$Perseverance <- factor(survey$Perseverance, ordered = TRUE, levels = c(1:6))
survey$Effort <- factor(survey$Effort, ordered = TRUE, levels = c(1:5))
survey$High.school.particip <- factor(survey$High.school.particip)
survey$High.school.math <- factor(survey$High.school.math)
survey$Engagement <- factor(survey$Engagement, ordered = TRUE, levels = c(1:5))
survey$Race <- factor(survey$Race)
survey$Computer.savviness <- factor(survey$Computer.savviness, ordered = TRUE, levels = c(1:5))
survey$External.help_1 <- factor(survey$External.help_1, ordered = TRUE, levels = c(1:5))
survey$External.help_2 <- factor(survey$External.help_2, ordered = TRUE, levels = c(1:5))
survey$External.help_3 <- factor(survey$External.help_3, ordered = TRUE, levels = c(1:5))
survey$Dream.job <- factor(survey$Dream.job, ordered = TRUE, levels = c(1:5))
survey$Job.search <- factor(survey$Job.search, ordered = TRUE, levels = c(1:5))
survey$Academic.standing <- factor(survey$Academic.standing,
    ordered = TRUE,
    levels = c("Freshman", "Sophomore", "Junior", "Senior", "Graduate degree program")
)
survey$Age <- as.numeric(survey$Age)
survey[survey == ""] <- NA

# truncate ordered factor contrasts to linear + quadratic only
# prevents cubic and higher-order polynomial trends from entering the design matrix
ordered_vars <- c(
    "Computer.age", "Laptop.issues", "Perseverance", "Effort",
    "Engagement", "Computer.savviness", "External.help_1",
    "External.help_2", "External.help_3", "Dream.job",
    "Job.search", "Academic.standing"
)

for (v in ordered_vars) {
    k <- nlevels(survey[[v]])
    if (k > 2) {
        max_deg <- min(2, k - 1)
        contrasts(survey[[v]], how.many = max_deg) <- contr.poly(k)[, 1:max_deg, drop = FALSE]
    }
}

# after subsetting, some ordered factor levels may be empty
# droplevels() fixes that, but resets the contrast matrix
# this re-applies the L+Q truncation to a data frame's ordered columns
fix_ordered_contrasts <- function(dat) {
    dat <- droplevels(dat)
    for (v in ordered_vars) {
        if (v %in% names(dat) && is.ordered(dat[[v]])) {
            k <- nlevels(dat[[v]])
            if (k > 2) {
                max_deg <- min(2, k - 1)
                contrasts(dat[[v]], how.many = max_deg) <- contr.poly(k)[, 1:max_deg, drop = FALSE]
            }
        }
    }
    dat
}

# keep only each student's most recent survey entry
# collapse to one row per participant: the latest recorded survey
survey <- survey %>%
    mutate(RecordedDate = lubridate::mdy_hm(RecordedDate)) %>%
    group_by(Participant.ID) %>%
    slice_max(order_by = RecordedDate, n = 1, with_ties = FALSE) %>%
    ungroup()

# set Advanced only as baseline for group_4
survey$group_4 <- factor(survey$group_4, levels = c(
    "Advanced only",
    "Foundational only",
    "Foundational before advanced",
    "Foundational concurrent with advanced"
))

survey_long <- survey %>%
    dplyr::select(
        Participant.ID, class_number, group_4,
        Anxiety_pre, Math_skills_pre, Computing_pre,
        Anxiety_post, Math_skills_post, Computing_post,
        Age, Gender, Race, Ethnicity, Major, Academic.standing, High.school.math,
        High.school.particip, Computer.savviness, Effort, Engagement, Perseverance,
        External.help_1, External.help_2, External.help_3,
        Dream.job, Job.search, Computer.age, OS, Laptop.issues
    ) %>%
    rename(
        Anxiety.pre = Anxiety_pre,
        Anxiety.post = Anxiety_post,
        MathSkills.pre = Math_skills_pre,
        MathSkills.post = Math_skills_post,
        Computing.pre = Computing_pre,
        Computing.post = Computing_post
    ) %>%
    pivot_longer(
        cols = c(Anxiety.pre:Computing.post),
        names_to = c("Outcome", "Time"),
        names_sep = "\\.",
        values_to = "Score"
    ) %>%
    mutate(
        Time_numeric = ifelse(Time == "pre", 0, 1),
        Outcome = factor(Outcome),
        Time = factor(Time, levels = c("pre", "post"))
    )

# re-apply truncated contrasts after pivot (pivot can drop contrast attributes)
for (v in ordered_vars) {
    if (v %in% names(survey_long) && is.ordered(survey_long[[v]])) {
        k <- nlevels(survey_long[[v]])
        if (k > 2) {
            max_deg <- min(2, k - 1)
            contrasts(survey_long[[v]], how.many = max_deg) <- contr.poly(k)[, 1:max_deg, drop = FALSE]
        }
    }
}

survey_long$High.school.particip <- ifelse(
    survey_long$High.school.particip == "" | is.na(survey_long$High.school.particip),
    "No",
    as.character(survey_long$High.school.particip)
)
survey_long$High.school.particip <- factor(survey_long$High.school.particip)


# the two anxiety items at each timepoint, as administered (higher = more anxious).
# reverse-coding does not change a correlation, so raw items are fine here.
anx_items <- list(
    pre  = c("Anxiety.1_1", "Anxiety.1_2"),
    post = c("Anxiety.2_1", "Anxiety.2_2")
)

spearman_brown <- function(r) (2 * r) / (1 + r)   # 2-item Spearman-Brown

anxiety_reliability <- function(data, items) {
    x <- suppressWarnings(as.numeric(data[[items[1]]]))
    y <- suppressWarnings(as.numeric(data[[items[2]]]))
    ok <- stats::complete.cases(x, y)
    x <- x[ok]; y <- y[ok]

    r_pearson  <- cor(x, y, method = "pearson")
    r_spearman <- cor(x, y, method = "spearman")

    data.frame(
        n              = length(x),
        pearson_r      = round(r_pearson, 3),
        spearman_rho   = round(r_spearman, 3),
        # Spearman-Brown from each correlation (report whichever you prefer;
        # the Pearson-based value is the conventional one)
        SB_from_pearson  = round(spearman_brown(r_pearson), 3),
        SB_from_spearman = round(spearman_brown(r_spearman), 3)
    )
}

anxiety_rel <- bind_rows(
    pre  = anxiety_reliability(survey, anx_items$pre),
    post = anxiety_reliability(survey, anx_items$post),
    .id  = "timepoint"
)
print(anxiety_rel)

# pooled across both timepoints (treats each item rating as one observation)
pooled <- data.frame(
    item1 = c(suppressWarnings(as.numeric(survey[[anx_items$pre[1]]])),
              suppressWarnings(as.numeric(survey[[anx_items$post[1]]]))),
    item2 = c(suppressWarnings(as.numeric(survey[[anx_items$pre[2]]])),
              suppressWarnings(as.numeric(survey[[anx_items$post[2]]])))
)
pooled <- pooled[complete.cases(pooled), ]
cat("\nPooled inter-item Pearson r:", round(cor(pooled$item1, pooled$item2), 3),
    "| Spearman-Brown:", round(spearman_brown(cor(pooled$item1, pooled$item2)), 3), "\n")

