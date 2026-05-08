################# classifying students by final course reached #################

library(lme4)
library(lmerTest)
library(tidyverse)
library(emmeans)
library(ggplot2)
library(kableExtra)

setwd("~/Dropbox/UF/Research/Chapter 3 (R)/Data")


################################################################################
# ~*~AESTHETICS~*~
################################################################################

base_theme <- theme_bw(base_family = "Times New Roman") +
    theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 13),
        text = element_text(family = "Times New Roman"),
        panel.grid.major = element_line(color = "gray85"),
        panel.grid.minor = element_line(color = "gray95"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "gray95"),
        strip.text = element_text(size = 11)
    )

# color by final course reached
palette_fc <- c(
    "R Class" = "#56B4E9",
    "Quant" = "#E69F00",
    "Pop Eco" = "#009E73"
)

palette_4 <- c(
    "R Class only" = "#56B4E9",
    "Advanced only" = "#E69F00",
    "R Class before advanced" = "#009E73",
    "R Class concurrent with advanced" = "#CC79A7"
)

course_x_labels <- c(
    "R Class" = "R Class",
    "Quant" = "Quant Ecology",
    "Pop Eco" = "Pop Ecology"
)

metrics_plot <- c("Anxiety", "Math Skills", "Computing")

group_4_levels <- c(
    "R Class only",
    "Advanced only",
    "R Class before advanced",
    "R Class concurrent with advanced"
)


################################################################################
# load data
################################################################################

survey <- read.csv("formatted_survey.csv")

survey$group_4 <- factor(survey$group_4, levels = group_4_levels)
survey$Gender <- factor(survey$Gender)
survey$Race <- factor(survey$Race)
survey$Age <- as.numeric(survey$Age)
survey[survey == ""] <- NA


################################################################################
# helper functions
################################################################################

# pull coefficients 
extract_coefs_df <- function(model, metric) {
    ct <- summary(model)$coefficients
    data.frame(
        Metric = metric,
        Term = rownames(ct),
        Estimate = round(ct[, "Estimate"], 3),
        SE = round(ct[, "Std. Error"], 3),
        t_value = round(ct[, "t value"], 3),
        p_value = round(ct[, "Pr(>|t|)"], 4),
        Sig = case_when(
            ct[, "Pr(>|t|)"] < 0.001 ~ "***",
            ct[, "Pr(>|t|)"] < 0.01  ~ "**",
            ct[, "Pr(>|t|)"] < 0.05  ~ "*",
            ct[, "Pr(>|t|)"] < 0.1   ~ ".",
            TRUE ~ ""
        ),
        LCL = round(ct[, "Estimate"] - 1.96 * ct[, "Std. Error"], 3),
        UCL = round(ct[, "Estimate"] + 1.96 * ct[, "Std. Error"], 3),
        stringsAsFactors = FALSE,
        row.names = NULL
    )
}

# raw mean and 95% CI for any grouping
raw_ci <- function(df, group_vars, score_col = "Score") {
    df %>%
        group_by(across(all_of(group_vars))) %>%
        summarise(
            mean = mean(.data[[score_col]], na.rm = TRUE),
            n = sum(!is.na(.data[[score_col]])),
            se = sd(.data[[score_col]], na.rm = TRUE) / sqrt(n),
            ci_lower = mean - qt(0.975, pmax(n - 1, 1)) * se,
            ci_upper = mean + qt(0.975, pmax(n - 1, 1)) * se,
            .groups = "drop"
        )
}

# pin y-axis range across facets
shared_blank <- function(items, pad = 0.07,
                         metric_levels = metrics_plot,
                         y_col = "y") {
    ranges <- lapply(items, function(x) {
        x$df %>%
            group_by(Metric) %>%
            summarise(
                lo = min(.data[[x$lo]], na.rm = TRUE),
                hi = max(.data[[x$hi]], na.rm = TRUE),
                .groups = "drop"
            )
    })
    bind_rows(ranges) %>%
        group_by(Metric) %>%
        summarise(lo = min(lo), hi = max(hi), .groups = "drop") %>%
        mutate(p = (hi - lo) * pad, lo = lo - p, hi = hi + p) %>%
        pivot_longer(c(lo, hi), values_to = y_col) %>%
        dplyr::select(Metric, all_of(y_col)) %>%
        mutate(Metric = factor(Metric, levels = metric_levels))
}


################################################################################
# DATA PREP
################################################################################

# which course was each student's furthest
final_course_tbl <- survey %>%
    distinct(Participant.ID, Foundational, Quant, PopEco) %>%
    filter(!is.na(Foundational)) %>%
    mutate(
        final_course = factor(
            case_when(
                PopEco == 1       ~ "Pop Eco",
                Quant  == 1       ~ "Quant",
                Foundational == 1 ~ "R Class",
                TRUE              ~ "None"
            ),
            levels = c("R Class", "Quant", "Pop Eco")
        )
    ) %>%
    dplyr::select(Participant.ID, final_course)

# whether the student ever took the foundational R class
found_flag <- survey %>%
    distinct(Participant.ID, Foundational) %>%
    mutate(
        Found_label = factor(
            ifelse(Foundational == 1, "Took R Class", "No R Class"),
            levels = c("No R Class", "Took R Class")
        )
    ) %>%
    dplyr::select(Participant.ID, Found_label)

# tag each survey record with which course(s) were taken that semester
# students enrolled in two courses concurrently get two rows for that semester
survey_tagged_p1 <- survey %>%
    mutate(
        in_R_class = grepl(
            "Computational Problem Solving in Wildlife Ecology Using R \\(WIS 4934\\)",
            Current.classes),
        in_Quant   = grepl("Quantitative Wildlife Ecology", Current.classes),
        in_PopEco  = grepl("Population Ecology",           Current.classes)
    ) %>%
    pivot_longer(c(in_R_class, in_Quant, in_PopEco),
                 names_to = "course_flag", values_to = "took_this_sem") %>%
    filter(took_this_sem) %>%
    mutate(
        course_this_sem = factor(
            case_when(
                course_flag == "in_R_class" ~ "R Class",
                course_flag == "in_Quant"   ~ "Quant",
                course_flag == "in_PopEco"  ~ "Pop Eco"
            ),
            levels = c("R Class", "Quant", "Pop Eco")
        )
    ) %>%
    dplyr::select(-course_flag, -took_this_sem) %>%
    left_join(final_course_tbl, by = "Participant.ID") %>%
    left_join(found_flag,       by = "Participant.ID") %>%
    filter(!is.na(final_course), as.character(final_course) != "None",
           !is.na(Found_label))

# pivot to long format (pre and post as separate rows)
tagged_long_p1 <- survey_tagged_p1 %>%
    dplyr::select(
        Participant.ID, course_this_sem, final_course, Found_label,
        Anxiety_pre, Anxiety_post,
        Math_skills_pre, Math_skills_post,
        Computing_pre, Computing_post
    ) %>%
    rename(
        Anxiety.pre     = Anxiety_pre,
        Anxiety.post    = Anxiety_post,
        MathSkills.pre  = Math_skills_pre,
        MathSkills.post = Math_skills_post,
        Computing.pre   = Computing_pre,
        Computing.post  = Computing_post
    ) %>%
    pivot_longer(
        cols      = c(Anxiety.pre:Computing.post),
        names_to  = c("Metric", "Time"),
        names_sep = "\\.",
        values_to = "Score"
    ) %>%
    mutate(
        Time         = factor(Time, levels = c("pre", "post")),
        Metric       = factor(Metric,
                              levels = c("Anxiety", "MathSkills", "Computing"),
                              labels = metrics_plot),
        final_course = factor(final_course, levels = c("R Class", "Quant", "Pop Eco")),
        Found_label  = factor(Found_label,  levels = c("No R Class", "Took R Class"))
    ) %>%
    filter(!is.na(Score))

# valid combinations in the data; used later to drop impossible emmeans cells
# e.g., a student with no R Class cannot have R Class as their final course
valid_combos_p1 <- tagged_long_p1 %>%
    distinct(course_this_sem, final_course, Found_label)

# raw summaries per plotting cell
p1_raw <- raw_ci(tagged_long_p1,
                 c("course_this_sem", "final_course", "Found_label",
                   "Metric", "Time")) %>%
    semi_join(valid_combos_p1,
              by = c("course_this_sem", "final_course", "Found_label")) %>%
    mutate(
        Time         = factor(Time, levels = c("pre", "post")),
        Metric       = factor(Metric, levels = metrics_plot),
        final_course = factor(final_course, levels = c("R Class", "Quant", "Pop Eco")),
        Found_label  = factor(Found_label,  levels = c("No R Class", "Took R Class"))
    )


################################################################################
# MODELS
################################################################################

# one lmer per final_course group x metric, REML = FALSE
#
# R Class group: all students are foundational by definition since the R Class
#   is the foundational course itself, so Found_label has no variation and
#   would cause a singular fit. Only one course level exists in this group.
#   Formula: Score ~ Time + (1 | Participant.ID)
#
# Quant and Pop Eco groups: multiple course levels; Found_label does vary
#   across the advanced-course rows (some students took R first, some did not).
#   Formula: Score ~ course_this_sem * Time + Found_label + (1 | Participant.ID)

p1_models <- list()
p1_emmeans_all <- data.frame()
p1_coefs <- data.frame()

for (fc in c("R Class", "Quant", "Pop Eco")) {

    p1_models[[fc]] <- list()

    for (metric in metrics_plot) {

        cat("\nPart 1 lmer |", fc, "|", metric, "\n")

        mdata <- tagged_long_p1 %>%
            filter(final_course == fc, Metric == metric) %>%
            drop_na(Score, course_this_sem, Time, Found_label, Participant.ID)

        n_courses <- n_distinct(mdata$course_this_sem)

        model_formula <- if (n_courses > 1) {
            Score ~ course_this_sem * Time + Found_label + (1 | Participant.ID)
        } else {
            Score ~ Time + (1 | Participant.ID)
        }

        model <- tryCatch(
            lmer(model_formula, data = mdata, REML = FALSE),
            error = function(e) { message("lmer error: ", e$message); NULL }
        )
        if (is.null(model)) next
        p1_models[[fc]][[metric]] <- model

        p1_coefs <- rbind(p1_coefs,
            extract_coefs_df(model, metric) %>% mutate(Group = fc))

        # emmeans for Quant/Pop Eco: course x Time conditioned on Found_label
        # emmeans for R Class: Time only; labels assigned manually for plotting
        if (n_courses > 1) {
            em <- emmeans(model, ~ course_this_sem * Time | Found_label) %>%
                as.data.frame() %>%
                mutate(final_course = fc, Metric = metric)
        } else {
            em <- emmeans(model, ~ Time) %>%
                as.data.frame() %>%
                mutate(
                    course_this_sem = factor("R Class",
                                            levels = c("R Class", "Quant", "Pop Eco")),
                    Found_label = factor("Took R Class",
                                            levels = c("No R Class", "Took R Class")),
                    final_course = fc,
                    Metric = metric
                )
        }

        p1_emmeans_all <- rbind(p1_emmeans_all, em)

        cat("  fitted. students =", n_distinct(mdata$Participant.ID),
            "| obs =", nrow(mdata), "\n")
    }
}

# drop impossible cells from emmeans (same filter applied to raw data above)
p1_emmeans_all <- p1_emmeans_all %>%
    mutate(
        Time = factor(Time, levels = c("pre", "post")),
        Metric = factor(Metric, levels = metrics_plot),
        course_this_sem = factor(course_this_sem,
                                 levels = c("R Class", "Quant", "Pop Eco")),
        Found_label = factor(Found_label,
                                 levels = c("No R Class", "Took R Class")),
        final_course = factor(final_course,
                                 levels = c("R Class", "Quant", "Pop Eco"))
    ) %>%
    semi_join(valid_combos_p1,
              by = c("course_this_sem", "final_course", "Found_label"))


################################################################################
# PLOTS
################################################################################

# shared y-axis range across all four plots
p1_ylim_blank <- shared_blank(list(
    list(df = p1_raw,         lo = "ci_lower", hi = "ci_upper"),
    list(df = p1_emmeans_all, lo = "lower.CL", hi = "upper.CL")
))

# n per line for captions; one entry per final_course x Found_label combination
p1_n_per_line <- tagged_long_p1 %>%
    distinct(Participant.ID, final_course, Found_label) %>%
    count(final_course, Found_label, name = "n_students") %>%
    arrange(final_course, Found_label)

p1_n_caption <- p1_n_per_line %>%
    mutate(s = paste0(final_course, "/", Found_label, " n=", n_students)) %>%
    pull(s) %>% paste(collapse = "; ")

make_p1_plot <- function(dat, y_col, ymin_col, ymax_col,
                          title_str, y_lab, n_label = "") {
    ggplot(dat,
           aes(x = course_this_sem,
               y = .data[[y_col]],
               color = final_course,
               linetype = Found_label,
               group = interaction(final_course, Found_label))) +
        geom_blank(data = p1_ylim_blank, aes(y = y), inherit.aes = FALSE) +
        geom_line(position = position_dodge(width = 0.35),
                  linewidth = 0.9, na.rm = TRUE) +
        geom_errorbar(
            aes(ymin = .data[[ymin_col]], ymax = .data[[ymax_col]]),
            position = position_dodge(width = 0.35),
            width = 0.15, linewidth = 0.75
        ) +
        geom_point(aes(shape = Found_label),
                   position = position_dodge(width = 0.35), size = 3) +
        facet_wrap(~Metric, scales = "fixed") +
        scale_color_manual(values = palette_fc, name = "Final course reached") +
        scale_linetype_manual(
            values = c("No R Class" = "dashed", "Took R Class" = "solid"),
            name   = "Foundational status"
        ) +
        scale_shape_manual(
            values = c("No R Class" = 1, "Took R Class" = 16),
            name   = "Foundational status"
        ) +
        scale_x_discrete(labels = course_x_labels) +
        labs(
            title = title_str,
            x = "Course in sequence",
            y = y_lab,
            caption = if (nchar(n_label) > 0)
                          paste0("Students per line: ", n_label)
                      else NULL
        ) +
        base_theme +
        theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
        guides(
            color = guide_legend(
                override.aes = list(shape = c(16, 16, 16), linetype = "solid")),
            linetype = guide_legend(),
            shape = guide_legend()
        )
}

p1a <- make_p1_plot(
    filter(p1_raw, Time == "pre"),
    "mean", "ci_lower", "ci_upper",
    "Observed raw pre-semester scores by final course and foundational status",
    "Mean pre-semester score (95% CI)", p1_n_caption
)
ggsave("../Figures/Part1_Raw_Pre.png", p1a, width = 10, height = 6, bg = "white")

p1b <- make_p1_plot(
    filter(p1_raw, Time == "post"),
    "mean", "ci_lower", "ci_upper",
    "Observed raw post-semester scores by final course and foundational status",
    "Mean post-semester score (95% CI)", p1_n_caption
)
ggsave("../Figures/Part1_Raw_Post.png", p1b, width = 10, height = 6, bg = "white")

p1c <- make_p1_plot(
    filter(p1_emmeans_all, Time == "pre"),
    "emmean", "lower.CL", "upper.CL",
    "Model-predicted pre-semester scores by final course and foundational status",
    "Model-predicted pre-semester score (95% CI)", p1_n_caption
)
ggsave("../Figures/Part1_Predicted_Pre.png", p1c, width = 10, height = 6, bg = "white")

p1d <- make_p1_plot(
    filter(p1_emmeans_all, Time == "post"),
    "emmean", "lower.CL", "upper.CL",
    "Model-predicted post-semester scores by final course and foundational status",
    "Model-predicted post-semester score (95% CI)", p1_n_caption
)
ggsave("../Figures/Part1_Predicted_Post.png", p1d, width = 10, height = 6, bg = "white")


################################################################################
# n table and coefficient tables
################################################################################

# full n breakdown: unique students per (course x group x foundational status)
p1_n_counts <- tagged_long_p1 %>%
    distinct(Participant.ID, course_this_sem, final_course, Found_label) %>%
    count(course_this_sem, final_course, Found_label, name = "n_students") %>%
    arrange(final_course, course_this_sem, Found_label)

p1_n_counts %>%
    rename(
        "Course this semester" = course_this_sem,
        "Final course group" = final_course,
        "Foundational status" = Found_label,
        "n students" = n_students
    ) %>%
    kbl(caption = "Unique students per plotted cell") %>%
    kable_classic(font_size = 14, html_font = "Times New Roman") %>%
    row_spec(0, bold = TRUE) %>%
    collapse_rows(columns = c(1, 2), valign = "top") %>%
    footnote(
        general = paste0(
            "A student who appears in two course-this-semester cells ",
            "(e.g., took R Class and then Quant) is counted once in each cell."
        ),
        footnote_as_chunk = TRUE
    ) %>%
    save_kable("../Figures/Tables/Part1_n_counts.png", zoom = 2)

# coefficient tables, one per group x metric
for (fc in c("R Class", "Quant", "Pop Eco")) {
    for (metric in metrics_plot) {
        df <- p1_coefs %>% filter(Group == fc, Metric == metric)
        if (nrow(df) == 0) next
        n_str <- p1_n_per_line %>%
            filter(final_course == fc) %>%
            mutate(s = paste0(Found_label, " n=", n_students)) %>%
            pull(s) %>% paste(collapse = "; ")
        df %>%
            mutate(
                CI = paste0("[", LCL, ", ", UCL, "]"),
                Term = gsub("course_this_sem", "", Term) %>%
                       gsub(":Timepost", " x Post", .) %>%
                       gsub("Found_labelTook R Class", "Took R Class (Found.)", .) %>%
                       trimws()
            ) %>%
            dplyr::select(Term, Estimate, CI, t_value, p_value, Sig) %>%
            kbl(caption   = paste0(fc, " group | ", metric),
                col.names = c("Term", "B", "95% CI", "t", "p", "")) %>%
            kable_classic(font_size = 14, html_font = "Times New Roman") %>%
            row_spec(0, bold = TRUE) %>%
            footnote(
                general = paste0(
                    "Reference: R Class course-this-semester, pre-time",
                    ifelse(fc != "R Class", ", No R Class foundational", ""),
                    ". lmer REML=FALSE. ",
                    ". p<0.1  * p<0.05  ** p<0.01  *** p<0.001. ",
                    "Students: ", n_str
                ),
                footnote_as_chunk = TRUE
            ) %>%
            save_kable(
                file = paste0("../Figures/Tables/Part1_",
                              gsub(" ", "_", fc), "_",
                              gsub(" ", "_", metric), ".png"),
                zoom = 2
            )
    }
}


################################################################################
# DATA PREP (again)
################################################################################

# pull group_4 from each student's last (most complete) survey record
# early-semester rows may have NA or an incomplete label for students who
# were still mid-sequence, so joining from the last row avoids dropping records
group4_def <- survey %>%
    group_by(Participant.ID) %>%
    slice_max(order_by = class_number, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    filter(!is.na(group_4), as.character(group_4) != "None") %>%
    dplyr::select(Participant.ID, group_4) %>%
    rename(group_4_def = group_4)

# tag every survey row with which course was taken that semester
# same logic as Part 1; students in two courses concurrently get two rows
tag_course <- function(df) {
    df %>%
        mutate(
            in_R_class = grepl(
                "Computational Problem Solving in Wildlife Ecology Using R \\(WIS 4934\\)",
                Current.classes),
            in_Quant   = grepl("Quantitative Wildlife Ecology", Current.classes),
            in_PopEco  = grepl("Population Ecology", Current.classes)
        ) %>%
        pivot_longer(c(in_R_class, in_Quant, in_PopEco),
                     names_to = "course_flag", values_to = "took_this_sem") %>%
        filter(took_this_sem) %>%
        mutate(
            course_this_sem = factor(
                case_when(
                    course_flag == "in_R_class" ~ "R Class",
                    course_flag == "in_Quant"   ~ "Quant",
                    course_flag == "in_PopEco"  ~ "Pop Eco"
                ),
                levels = c("R Class", "Quant", "Pop Eco")
            )
        ) %>%
        dplyr::select(-course_flag, -took_this_sem)
}

# helper to pivot wide to long pre/post format, keeping course_this_sem
to_long_4 <- function(df) {
    df %>%
        dplyr::select(
            Participant.ID, group_4, course_this_sem,
            Anxiety_pre, Anxiety_post,
            Math_skills_pre, Math_skills_post,
            Computing_pre, Computing_post
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
            names_to = c("Metric", "Time"),
            names_sep = "\\.",
            values_to = "Score"
        ) %>%
        mutate(
            Time = factor(Time, levels = c("pre", "post")),
            Metric = factor(Metric,
                                     levels = c("Anxiety", "MathSkills", "Computing"),
                                     labels = metrics_plot),
            group_4 = factor(group_4, levels = group_4_levels),
            course_this_sem = factor(course_this_sem,
                                     levels = c("R Class", "Quant", "Pop Eco"))
        ) %>%
        filter(!is.na(Score))
}

# one survey record per student (their last semester)
# group_4 tells us about their full history; course_this_sem tells us which
# class this pre/post pair actually came from
last_survey_4 <- survey %>%
    group_by(Participant.ID) %>%
    slice_max(order_by = class_number, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    inner_join(group4_def, by = "Participant.ID") %>%
    mutate(group_4 = factor(group_4_def, levels = group_4_levels)) %>%
    dplyr::select(-group_4_def) %>%
    tag_course()

last_long_4 <- to_long_4(last_survey_4)

# all survey records per student; group_4 from definitive lookup
# "R Class before advanced" students appear at BOTH R Class and advanced
# course positions, which is the key structural difference from 2A
all_survey_4 <- survey %>%
    inner_join(group4_def, by = "Participant.ID") %>%
    mutate(group_4 = factor(group_4_def, levels = group_4_levels)) %>%
    dplyr::select(-group_4_def) %>%
    tag_course()

all_long_4 <- to_long_4(all_survey_4)

# valid combinations actually present in the data; used to filter emmeans
valid_combos_2a <- last_long_4 %>% distinct(group_4, course_this_sem)
valid_combos_2b <- all_long_4  %>% distinct(group_4, course_this_sem)

cat("\nPart 2A: observed group_4 x course_this_sem combinations (last survey):\n")
print(as.data.frame(valid_combos_2a %>% arrange(group_4, course_this_sem)))

cat("\nPart 2B: observed group_4 x course_this_sem combinations (all surveys):\n")
print(as.data.frame(valid_combos_2b %>% arrange(group_4, course_this_sem)))


################################################################################
# MODELS (again)
################################################################################

# 2A: Score ~ group_4 * Time + (1|Participant.ID)
# course_this_sem is not in the model because in 2A each group maps to at most
# one course position (last survey); including it would be collinear with group_4
# emmeans are requested by group_4 * Time, then course_this_sem is attached for
# plotting via the valid_combos_2a lookup

# 2B: Score ~ group_4 * course_this_sem * Time + (1|Participant.ID)
# now course_this_sem genuinely varies within some groups (e.g., R Class before
# advanced students contribute both an R Class row and an advanced course row),
# so it needs to be in the model to avoid conflating course effects with group effects


# --- 2A ---

res_2a_models  <- list()
res_2a_emmeans <- data.frame()
res_2a_coefs   <- data.frame()

for (metric in metrics_plot) {

    cat("\nPart 2A lmer | last survey |", metric, "\n")

    mdata <- last_long_4 %>%
        filter(Metric == metric) %>%
        drop_na(Score, group_4, Time, Participant.ID)

    model <- tryCatch(
        lmer(Score ~ group_4 * Time + (1 | Participant.ID),
             data = mdata, REML = FALSE),
        error = function(e) { message("lmer error: ", e$message); NULL }
    )
    if (is.null(model)) next
    res_2a_models[[metric]] <- model

    res_2a_coefs <- rbind(res_2a_coefs,
        extract_coefs_df(model, metric) %>% mutate(Model_label = "Last survey only"))

    em <- emmeans(model, ~ group_4 * Time) %>%
        as.data.frame() %>%
        # attach course_this_sem positions where each group's last surveys fell;
        # inner_join drops any group that has no observed course in 2A data
        inner_join(valid_combos_2a, by = "group_4") %>%
        mutate(Metric = metric, Model_label = "Last survey only")
    res_2a_emmeans <- rbind(res_2a_emmeans, em)

    cat("  fitted. students =", n_distinct(mdata$Participant.ID),
        "| obs =", nrow(mdata), "\n")
    print(summary(model)$coefficients)
}

# --- 2B ---

res_2b_models  <- list()
res_2b_emmeans <- data.frame()
res_2b_coefs   <- data.frame()

for (metric in metrics_plot) {

    cat("\nPart 2B lmer | all surveys |", metric, "\n")

    mdata <- all_long_4 %>%
        filter(Metric == metric) %>%
        drop_na(Score, group_4, course_this_sem, Time, Participant.ID)

    model <- tryCatch(
        lmer(Score ~ group_4 + course_this_sem + Time +
                     group_4:Time + course_this_sem:Time +
                     (1 | Participant.ID),
             data = mdata, REML = FALSE),
        error = function(e) { message("lmer error: ", e$message); NULL }
    )
    if (is.null(model)) next
    res_2b_models[[metric]] <- model

    res_2b_coefs <- rbind(res_2b_coefs,
        extract_coefs_df(model, metric) %>% mutate(Model_label = "All surveys"))

    # emmeans at every group x course x time cell that exists in the data;
    # averaging over the additive structure in the model
    em <- emmeans(model, ~ group_4 + course_this_sem + Time,
                  at = list(
                      group_4         = group_4_levels,
                      course_this_sem = c("R Class", "Quant", "Pop Eco"),
                      Time            = c("pre", "post")
                  )) %>%
        as.data.frame() %>%
        semi_join(valid_combos_2b, by = c("group_4", "course_this_sem")) %>%
        mutate(Metric = metric, Model_label = "All surveys")
    res_2b_emmeans <- rbind(res_2b_emmeans, em)

    cat("  fitted. students =", n_distinct(mdata$Participant.ID),
        "| obs =", nrow(mdata), "\n")
    print(summary(model)$coefficients)
}

# standardise factor levels and combine
tidy_em4 <- function(em_df) {
    em_df %>%
        mutate(
            Time = factor(Time, levels = c("pre", "post")),
            Metric = factor(Metric, levels = metrics_plot),
            group_4 = factor(group_4, levels = group_4_levels),
            course_this_sem = factor(course_this_sem,
                                     levels = c("R Class", "Quant", "Pop Eco")),
            Model_label = factor(Model_label,
                                     levels = c("Last survey only", "All surveys"))
        )
}

em_2a <- tidy_em4(res_2a_emmeans)
em_2b <- tidy_em4(res_2b_emmeans)
em_combined <- bind_rows(em_2a, em_2b)


################################################################################
# SUMMARIES (RAW)
################################################################################

raw_2a <- raw_ci(last_long_4,
                 c("group_4", "course_this_sem", "Time", "Metric")) %>%
    mutate(
        Time = factor(Time, levels = c("pre", "post")),
        Metric = factor(Metric, levels = metrics_plot),
        group_4 = factor(group_4, levels = group_4_levels),
        course_this_sem = factor(course_this_sem,
                                 levels = c("R Class", "Quant", "Pop Eco")),
        Model_label = factor("Last survey only",
                                 levels = c("Last survey only", "All surveys"))
    )

raw_2b <- raw_ci(all_long_4,
                 c("group_4", "course_this_sem", "Time", "Metric")) %>%
    mutate(
        Time = factor(Time, levels = c("pre", "post")),
        Metric = factor(Metric, levels = metrics_plot),
        group_4 = factor(group_4, levels = group_4_levels),
        course_this_sem = factor(course_this_sem,
                                 levels = c("R Class", "Quant", "Pop Eco")),
        Model_label = factor("All surveys",
                                 levels = c("Last survey only", "All surveys"))
    )

raw_combined <- bind_rows(raw_2a, raw_2b)


################################################################################
# PLOTS
################################################################################

# shared y-axis across all four Part 2 plots
p2_ylim_blank <- shared_blank(list(
    list(df = raw_2a, lo = "ci_lower", hi = "ci_upper"),
    list(df = raw_2b, lo = "ci_lower", hi = "ci_upper"),
    list(df = em_2a, lo = "lower.CL", hi = "upper.CL"),
    list(df = em_2b, lo = "lower.CL", hi = "upper.CL")
))

# n caption strings
last_n_str <- last_survey_4 %>%
    distinct(Participant.ID, group_4) %>%
    count(group_4) %>%
    mutate(s = paste0(group_4, " n=", n)) %>% pull(s) %>% paste(collapse = "; ")

all_n_str <- all_survey_4 %>%
    distinct(Participant.ID, group_4) %>%
    count(group_4) %>%
    mutate(s = paste0(group_4, " n=", n)) %>% pull(s) %>% paste(collapse = "; ")


# in 2A, each group appears at only one course position (their last class)
# in 2B, groups like "R Class before advanced" appear at multiple positions
make_p2_plot <- function(dat, y_col, ymin_col, ymax_col,
                          title_str, y_lab, caption_str) {
    ggplot(dat,
           aes(x = course_this_sem,
               y = .data[[y_col]],
               color = group_4,
               linetype = Model_label,
               shape = Model_label,
               group = interaction(group_4, Model_label))) +
        geom_blank(data = p2_ylim_blank, aes(y = y), inherit.aes = FALSE) +
        geom_line(position = position_dodge(width = 0.4),
                  linewidth = 0.9, na.rm = TRUE) +
        geom_errorbar(
            aes(ymin = .data[[ymin_col]], ymax = .data[[ymax_col]]),
            position = position_dodge(width = 0.4),
            width = 0.15, linewidth = 0.8
        ) +
        geom_point(position = position_dodge(width = 0.4), size = 3) +
        facet_wrap(~Metric, scales = "fixed") +
        scale_color_manual(values = palette_4, name = "Group") +
        scale_linetype_manual(
            values = c("Last survey only" = "dashed", "All surveys" = "solid"),
            name = "Data"
        ) +
        scale_shape_manual(
            values = c("Last survey only" = 1, "All surveys" = 16),
            name = "Data"
        ) +
        scale_x_discrete(labels = course_x_labels) +
        guides(
            color = guide_legend(ncol = 2),
            linetype = guide_legend(),
            shape = guide_legend()
        ) +
        ylim(0, 5) +
        labs(
            title = title_str,
            x = "Course",
            y = y_lab,
            caption = caption_str
        ) +
        base_theme +
        theme(axis.text.x = element_text(angle = 15, hjust = 1))
}

# raw pre
p2_raw_pre <- make_p2_plot(
    filter(raw_combined, Time == "pre"),
    "mean", "ci_lower", "ci_upper",
    "Observed raw pre-semester scores by 4-group classification",
    "Mean pre-semester score (95% CI)",
    paste0(
        "Dashed/open = last survey per student (", last_n_str, ").\n",
        "Solid/filled = all surveys (", all_n_str, ").\n",
        "Groups appear only at the course(s) they contributed data for."
    )
)
ggsave("../Figures/Part2_Raw_Pre.png",
       p2_raw_pre, width = 11, height = 6, bg = "white")

# raw post
p2_raw_post <- make_p2_plot(
    filter(raw_combined, Time == "post"),
    "mean", "ci_lower", "ci_upper",
    "Observed raw post-semester scores by 4-group classification",
    "Mean post-semester score (95% CI)",
    paste0(
        "Dashed/open = last survey per student (", last_n_str, ").\n",
        "Solid/filled = all surveys (", all_n_str, ").\n",
        "Groups appear only at the course(s) they contributed data for."
    )
)
ggsave("../Figures/Part2_Raw_Post.png",
       p2_raw_post, width = 11, height = 6, bg = "white")

# predicted pre
p2_em_pre <- make_p2_plot(
    filter(em_combined, Time == "pre"),
    "emmean", "lower.CL", "upper.CL",
    "Model-predicted pre-semester scores by 4-group classification",
    "Model-predicted pre-semester score (95% CI)",
    paste0(
        "Dashed/open = last survey lmer: Score ~ group_4 x Time + (1|ID) (", last_n_str, ").\n",
        "Solid/filled = all surveys lmer: Score ~ group_4 + course + Time + group_4:Time + course:Time + (1|ID) (",
        all_n_str, "). REML=FALSE."
    )
)
ggsave("../Figures/Part2_Predicted_Pre.png",
       p2_em_pre, width = 11, height = 6, bg = "white")

# predicted post
p2_em_post <- make_p2_plot(
    filter(em_combined, Time == "post"),
    "emmean", "lower.CL", "upper.CL",
    "Model-predicted post-semester scores by 4-group classification",
    "Model-predicted post-semester score (95% CI)",
    paste0(
        "Dashed/open = last survey lmer: Score ~ group_4 x Time + (1|ID) (", last_n_str, ").\n",
        "Solid/filled = all surveys lmer: Score ~ group_4 + course + Time + group_4:Time + course:Time + (1|ID) (",
        all_n_str, "). REML=FALSE."
    )
)
ggsave("../Figures/Part2_Predicted_Post.png",
       p2_em_post, width = 11, height = 6, bg = "white")



################################################################################
# Coefficient tables
################################################################################

save_p2_coef_table <- function(coef_df, label_str, model_str, n_str, file_prefix) {
    for (metric in metrics_plot) {
        coef_df %>%
            filter(Metric == metric) %>%
            mutate(
                CI = paste0("[", LCL, ", ", UCL, "]"),
                Term = gsub("group_4", "", Term) %>%
                       gsub("course_this_sem", "", .) %>%
                       gsub(":Timepost", " x Post", .) %>% trimws()
            ) %>%
            dplyr::select(Term, Estimate, CI, t_value, p_value, Sig) %>%
            kbl(caption   = paste0(label_str, ": ", metric),
                col.names = c("Term", "B", "95% CI", "t", "p", "")) %>%
            kable_classic(font_size = 14, html_font = "Times New Roman") %>%
            row_spec(0, bold = TRUE) %>%
            footnote(
                general = paste0(
                    "Reference: R Class only, pre-time. ",
                    "Model: ", model_str, ", REML=FALSE. ",
                    ". p<0.1  * p<0.05  ** p<0.01  *** p<0.001. ",
                    "Students: ", n_str
                ),
                footnote_as_chunk = TRUE
            ) %>%
            save_kable(
                file = paste0(file_prefix, gsub(" ", "_", metric), ".png"),
                zoom = 2
            )
    }
}

save_p2_coef_table(
    res_2a_coefs,
    label_str = "Part 2A lmer last survey",
    model_str = "Score ~ group_4 x Time + (1|ID)",
    n_str = last_n_str,
    file_prefix = "../Figures/Tables/Part2A_lmer_"
)

save_p2_coef_table(
    res_2b_coefs,
    label_str = "Part 2B lmer all surveys",
    model_str = "Score ~ group_4 + course + Time + group_4:Time + course:Time + (1|ID)",
    n_str = all_n_str,
    file_prefix = "../Figures/Tables/Part2B_lmer_"
)


################################################################################
# sample size summary
################################################################################

cat("\n============================================================\n")
cat("SAMPLE SIZE SUMMARY\n")
cat("============================================================\n")

cat("\n--- Part 1: unique students per line (final course x foundational status) ---\n")
print(as.data.frame(p1_n_per_line))

cat("\n--- Part 1: unique students per plotted cell ---\n")
cat("(course taken this semester x final course group x foundational status)\n")
print(as.data.frame(p1_n_counts))

cat("\n--- Part 2A: students per group_4 (last survey only) ---\n")
print(as.data.frame(last_survey_4 %>% distinct(Participant.ID, group_4) %>% count(group_4)))

cat("\n--- Part 2A: students per group_4 x course_this_sem (last survey only) ---\n")
print(as.data.frame(
    last_survey_4 %>%
        distinct(Participant.ID, group_4, course_this_sem) %>%
        count(group_4, course_this_sem) %>%
        arrange(group_4, course_this_sem)
))

cat("\n--- Part 2B: unique students per group_4 (all surveys) ---\n")
print(as.data.frame(
    all_survey_4 %>% distinct(Participant.ID, group_4) %>% count(group_4)
))

cat("\n--- Part 2B: unique students per group_4 x course_this_sem (all surveys) ---\n")
print(as.data.frame(
    all_survey_4 %>%
        distinct(Participant.ID, group_4, course_this_sem) %>%
        count(group_4, course_this_sem) %>%
        arrange(group_4, course_this_sem)
))

cat("\n--- Part 2B: total semester records per group_4 x course_this_sem ---\n")
print(as.data.frame(all_survey_4 %>% count(group_4, course_this_sem) %>%
    arrange(group_4, course_this_sem)))

cat("\n--- Part 2: group_4 definitive counts ---\n")
print(as.data.frame(group4_def %>% count(group_4_def)))

# citation stuff
print(R.version.string)
print(packageVersion("lme4"))
print(packageVersion("lmerTest"))
print(packageVersion("emmeans"))