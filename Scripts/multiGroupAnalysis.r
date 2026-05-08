################################################################################
# 3-group, 4-group, 5-group classifications + R -> Quant -> Pop Eco
################################################################################

################################################################################
############################ LOAD PACKAGES #####################################
################################################################################

library(lme4)  
library(lmerTest)  
library(tidyverse)  
library(kableExtra)  
library(dplyr)
library(emmeans)
library(ggplot2)

# set working directory
setwd("~/Dropbox/UF/Research/Chapter 3 (R)/Data")


################################################################################
################################ HELPERS #######################################
################################################################################

# clean metric labels for plots and tables
clean_metric <- function(x) {
    case_when(
        x == "MathSkills" ~ "Math Skills",
        x == "Computing"  ~ "Computing Skills",
        TRUE ~ x
    )
}

# extract fixed-effect coefficients from a fitted lmer model
extract_coefs <- function(model, scheme, metric) {
    coef_table <- summary(model)$coefficients
    df <- data.frame(
        Group_scheme = scheme,
        Metric = metric,
        Term = rownames(coef_table),
        Estimate = round(coef_table[, "Estimate"], 3),
        SE = round(coef_table[, "Std. Error"], 3),
        t_value = round(coef_table[, "t value"], 3),
        p_value = round(coef_table[, "Pr(>|t|)"], 4),
        Sig = case_when(
            coef_table[, "Pr(>|t|)"] < 0.001 ~ "***",
            coef_table[, "Pr(>|t|)"] < 0.01  ~ "**",
            coef_table[, "Pr(>|t|)"] < 0.05  ~ "*",
            coef_table[, "Pr(>|t|)"] < 0.1   ~ ".",
            TRUE ~ ""
        ),
        LCL = round(coef_table[, "Estimate"] - 1.96 * coef_table[, "Std. Error"], 3),
        UCL = round(coef_table[, "Estimate"] + 1.96 * coef_table[, "Std. Error"], 3),
        stringsAsFactors = FALSE
    )
    rownames(df) <- NULL
    return(df)
}

# build and save a kable coefficient table to PNG
save_coef_table <- function(data, scheme, filepath, footnote_text) {
    data %>%
        filter(Group_scheme == scheme) %>%
        mutate(
            Metric = clean_metric(Metric),
            CI_Lower = round(Estimate - 1.96 * SE, 3),
            CI_Upper = round(Estimate + 1.96 * SE, 3),
            CI = paste0("[", CI_Lower, ", ", CI_Upper, "]")
        ) %>%
        dplyr::select(Metric, Term, Estimate, CI, t_value, p_value, Sig) %>%
        kbl(col.names = c("Metric", "Term", "β", "95% CI", "t", "p", "")) %>%
        kable_classic(font_size = 14, html_font = "Times New Roman") %>%
        row_spec(0, bold = TRUE) %>%
        column_spec(1, italic = TRUE) %>%
        collapse_rows(columns = 1, valign = "top") %>%
        footnote(general = footnote_text, footnote_as_chunk = TRUE) %>%
        save_kable(file = filepath, zoom = 2)
}

# extract emmeans for a list of models and a group variable
extract_emmeans <- function(model_list, group_var, metrics) {
    result <- data.frame()
    for (metric in metrics) {
        model <- model_list[[metric]]
        if (is.null(model)) next
        formula_str <- as.formula(paste0("~ ", group_var, " * Time"))
        em <- emmeans(model, formula_str) %>%
            as.data.frame() %>%
            mutate(Metric = metric)
        result <- rbind(result, em)
    }
    result %>%
        filter(.data[[group_var]] != "None") %>%
        mutate(
            Metric = clean_metric(Metric),
            Time   = factor(Time, levels = c("pre", "post"))
        )
}

# summarize mean and 95% CI per grouping variable × time × metric
summarize_scores <- function(data, group_var) {
    data %>%
        filter(.data[[group_var]] != "None") %>%
        group_by(across(all_of(c(group_var, "Time", "Metric")))) %>%
        summarise(
            mean = mean(Score, na.rm = TRUE),
            sd = sd(Score, na.rm = TRUE),
            n = n(),
            se = sd / sqrt(n),
            ci_lower = mean - qt(0.975, n - 1) * se,
            ci_upper = mean + qt(0.975, n - 1) * se,
            .groups  = "drop"
        )
}


################################################################################
############################ SHARED AESTHETICS ################################
################################################################################

palette_3 <- c(
    "R Class only" = "#56B4E9",
    "Quant and/or Pop Eco only" = "#E69F00",
    "R Class and an advanced course" = "#009E73"
)

palette_4 <- c(
    "R Class only" = "#56B4E9",
    "Advanced only" = "#E69F00",
    "R Class before advanced" = "#009E73",
    "R Class concurrent with advanced" = "#CC79A7"
)

palette_5 <- c(
    "R Class only" = "#56B4E9",
    "Quant or Pop Eco" = "#E69F00",
    "Quant and Pop Eco" = "#D55E00",
    "R Class and Quant or Pop Eco" = "#009E73",
    "R Class, Quant, and Pop Eco" = "#CC79A7"
)

palette_found <- c(
    "No R Class" = "#56B4E9",
    "Took R Class" = "#E69F00"
)

# shared ggplot theme
base_theme <- theme_bw(base_family = "Times New Roman") +
    theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        text = element_text(family = "Times New Roman"),
        panel.grid.major = element_line(color = "gray85"),
        panel.grid.minor = element_line(color = "gray95"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "gray95")
    )

course_x_labels <- c(
    "R Class" = "R Class",
    "Quant"   = "Quant Ecology",
    "Pop Eco" = "Pop Ecology"
)


################################################################################
################# PART 1: GROUP-BASED ANALYSIS ################################
################################################################################

################################################################################
# DATA PREPARATION
################################################################################

survey <- read.csv("formatted_survey.csv")

# factor group variables with explicit level ordering for plots and models
survey$group_5 <- factor(survey$group_5, levels = c(
    "Quant or Pop Eco",
    "Quant and Pop Eco",
    "R Class only",
    "R Class and Quant or Pop Eco",
    "R Class, Quant, and Pop Eco"
))

survey$group_4 <- factor(survey$group_4, levels = c(
    "R Class only",
    "Advanced only",
    "R Class before advanced",
    "R Class concurrent with advanced"
))

survey$group_3 <- factor(survey$group_3, levels = c(
    "Quant and/or Pop Eco only",
    "R Class only",
    "R Class and an advanced course"
))

survey$Computer.age <- factor(survey$Computer.age,
    levels = c("0 - 2 years old", "2 - 4 years old",
               "4 - 6 years old", "6+ years old"),
    ordered = TRUE
)
survey$Gender <- factor(survey$Gender)
survey$Race <- factor(survey$Race)
survey$Age <- as.numeric(survey$Age)
survey[survey == ""] <- NA

# pivot to long format
survey_long_grp <- survey %>%
    dplyr::select(
        Participant.ID, Foundational, group_3, group_4, group_5, class_number,
        Anxiety_pre, Math_skills_pre, Computing_pre,
        Anxiety_post, Math_skills_post, Computing_post
    ) %>%
    rename(
        Anxiety.pre = Anxiety_pre,
        Anxiety.post = Anxiety_post,
        MathSkills.pre = Math_skills_pre,
        MathSkills.post = Math_skills_post,
        Computing.pre = Computing_pre,
        Computing.post = Computing_post
    ) %>%
    tidyr::pivot_longer(
        cols = c(Anxiety.pre:Computing.post),
        names_to = c("Outcome", "Time"),
        names_sep = "\\.",
        values_to = "Score"
    ) %>%
    dplyr::mutate(
        Time_numeric = ifelse(Time == "pre", 0, 1),
        Outcome = factor(Outcome),
        Time = factor(Time, levels = c("pre", "post")),
        group_3 = factor(group_3, levels = levels(survey$group_3)),
        group_4 = factor(group_4, levels = levels(survey$group_4)),
        group_5 = factor(group_5, levels = levels(survey$group_5))
    )


################################################################################
# GROUP × TIME BASE MODELS
################################################################################

metrics <- c("Anxiety", "MathSkills", "Computing")

base_group_results <- data.frame(
    Group_scheme = character(), Metric = character(), Term = character(),
    Estimate = numeric(), SE = numeric(), t_value = numeric(),
    p_value = numeric(), Sig = character(), LCL = numeric(), UCL = numeric(),
    stringsAsFactors = FALSE
)

base_models_3grp <- list()
base_models_4grp <- list()
base_models_5grp <- list()

for (metric in metrics) {

    cat("\n----------------------------------------\n")
    cat("Base group models for:", metric, "\n")
    cat("----------------------------------------\n")

    mdata <- survey_long_grp %>%
        filter(Outcome == metric) %>%
        drop_na(Score, Time, Participant.ID)

    # 3-group
    mdata_3 <- mdata %>% filter(group_3 != "None") %>% droplevels()
    model_3  <- tryCatch(
        lmer(Score ~ group_3 * Time + (1 | Participant.ID), data = mdata_3, REML = FALSE),
        error = function(e) { message("3-group error: ", e$message); NULL }
    )
    if (!is.null(model_3)) {
        base_models_3grp[[metric]] <- model_3
        base_group_results <- rbind(base_group_results, extract_coefs(model_3, "3-group", metric))
        cat("  3-group fitted. N =", nrow(mdata_3), "\n")
    }

    # 4-group
    mdata_4 <- mdata %>% filter(group_4 != "None") %>% droplevels()
    model_4  <- tryCatch(
        lmer(Score ~ group_4 * Time + (1 | Participant.ID), data = mdata_4, REML = FALSE),
        error = function(e) { message("4-group error: ", e$message); NULL }
    )
    if (!is.null(model_4)) {
        base_models_4grp[[metric]] <- model_4
        base_group_results <- rbind(base_group_results, extract_coefs(model_4, "4-group", metric))
        cat("  4-group fitted. N =", nrow(mdata_4), "\n")
    }

    # 5-group
    mdata_5 <- mdata %>% filter(group_5 != "None") %>% droplevels()
    model_5  <- tryCatch(
        lmer(Score ~ group_5 * Time + (1 | Participant.ID), data = mdata_5, REML = FALSE),
        error = function(e) { message("5-group error: ", e$message); NULL }
    )
    if (!is.null(model_5)) {
        base_models_5grp[[metric]] <- model_5
        base_group_results <- rbind(base_group_results, extract_coefs(model_5, "5-group", metric))
        cat("  5-group fitted. N =", nrow(mdata_5), "\n")
    }
}

as.data.frame(subset(base_group_results, Group_scheme == "3-group"))
as.data.frame(subset(base_group_results, Group_scheme == "4-group"))
as.data.frame(subset(base_group_results, Group_scheme == "5-group"))


################################################################################
# COEFFICIENT TABLES (PNG)
################################################################################

footnote_std <- "Reference level: Quant and/or Pop Eco only. . p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"

save_coef_table(base_group_results, "3-group",
                "../Figures/Tables/base_model_3group_coefs.png", footnote_std)
save_coef_table(base_group_results, "4-group",
                "../Figures/Tables/base_model_4group_coefs.png", footnote_std)
save_coef_table(base_group_results, "5-group",
                "../Figures/Tables/base_model_5group_coefs.png", footnote_std)


################################################################################
# PRE-POST SCORE SUMMARIES
################################################################################

score_summary_grp <- data.frame()

for (metric in metrics) {
    mdata <- survey_long_grp %>%
        filter(Outcome == metric, !is.na(Score)) %>%
        mutate(Metric = metric)

    score_summary_grp <- rbind(
        score_summary_grp,
        summarize_scores(mdata, "group_3") %>% rename(Group = group_3) %>% mutate(Scheme = "3-group"),
        summarize_scores(mdata, "group_4") %>% rename(Group = group_4) %>% mutate(Scheme = "4-group"),
        summarize_scores(mdata, "group_5") %>% rename(Group = group_5) %>% mutate(Scheme = "5-group")
    )
}

score_summary_grp <- score_summary_grp %>%
    mutate(Metric = clean_metric(Metric))


################################################################################
# SIGNIFICANCE BRACKETS FOR 3-GROUP PRE-POST PLOT
################################################################################

# tukey-adjusted pairwise comparisons between groups (collapsing over time)
grp3_pairs_brackets <- data.frame()
for (metric in metrics) {
    model <- base_models_3grp[[metric]]
    if (is.null(model)) next
    pairs_df <- pairs(emmeans(model, ~ group_3), adjust = "tukey") %>%
        as.data.frame() %>%
        mutate(
            Metric_clean = clean_metric(metric),
            Sig  = case_when(
                p.value < 0.001 ~ "***",
                p.value < 0.01  ~ "**",
                p.value < 0.05  ~ "*",
                TRUE ~ ""
            ),
            grp1 = trimws(sub(" - .*", "", contrast)),
            grp2 = trimws(sub(".* - ", "", contrast))
        ) %>%
        filter(p.value < 0.05)
    grp3_pairs_brackets <- rbind(grp3_pairs_brackets, pairs_df)
}

# post means per group × metric — used to set bracket y positions
post_means_3grp <- score_summary_grp %>%
    filter(Scheme == "3-group", Time == "post") %>%
    dplyr::select(Metric, Group, mean) %>%
    rename(Metric_clean = Metric)

# build bracket coordinate data frame
grp3_bracket_coords <- data.frame()
if (nrow(grp3_pairs_brackets) > 0) {
    grp3_bracket_coords <- grp3_pairs_brackets %>%
        left_join(post_means_3grp %>% rename(grp1 = Group, y1 = mean),
                  by = c("Metric_clean", "grp1")) %>%
        left_join(post_means_3grp %>% rename(grp2 = Group, y2 = mean),
                  by = c("Metric_clean", "grp2")) %>%
        group_by(Metric_clean) %>%
        mutate(bracket_rank = row_number()) %>%
        ungroup() %>%
        mutate(
            # stack brackets to the right of the post data point (x = 2 in discrete)
            x_bracket = 2.25 + (bracket_rank - 1) * 0.22,
            ymid = (y1 + y2) / 2,
            tick_len = 0.08
        ) %>%
        rename(Metric = Metric_clean)
}


################################################################################
# PLOT A: PRE-POST SCORES BY 3-GROUP
################################################################################

p_grp3 <- ggplot(
    filter(score_summary_grp, Scheme == "3-group"),
    aes(x = Time, y = mean, color = Group, group = Group)
) +
    geom_point(position = position_dodge(width = 0.3), size = 3) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
        position = position_dodge(width = 0.3), width = 0.15, linewidth = 0.8) +
    geom_line(position = position_dodge(width = 0.3), linewidth = 1) +
    facet_wrap(~Metric) +   # fixed scales (default) — same y across panels
    scale_color_manual(values = palette_3) +
    scale_x_discrete(
        labels  = c("pre" = "Pre", "post" = "Post"),
        expand  = expansion(add = c(0.5, 1.3))   # right space for brackets
    ) +
    labs(x = "Time", y = "Mean Score (95% CI)", color = NULL) +
    base_theme

# add brackets if any significant pairs exist
if (nrow(grp3_bracket_coords) > 0) {
    p_grp3 <- p_grp3 +
        # vertical bracket line connecting the two group post means
        geom_segment(data = grp3_bracket_coords,
                     aes(x = x_bracket, xend = x_bracket, y = y1, yend = y2),
                     inherit.aes = FALSE, color = "black", linewidth = 0.5) +
        # horizontal tick at group 1 y position
        geom_segment(data = grp3_bracket_coords,
                     aes(x = x_bracket, xend = x_bracket - tick_len, y = y1, yend = y1),
                     inherit.aes = FALSE, color = "black", linewidth = 0.5) +
        # horizontal tick at group 2 y position
        geom_segment(data = grp3_bracket_coords,
                     aes(x = x_bracket, xend = x_bracket - tick_len, y = y2, yend = y2),
                     inherit.aes = FALSE, color = "black", linewidth = 0.5) +
        # significance star at bracket midpoint
        geom_text(data = grp3_bracket_coords,
                  aes(x = x_bracket + 0.06, y = ymid, label = Sig),
                  inherit.aes = FALSE, color = "black", size = 4)
}

ggsave("../Figures/MeanScoresPrePost_3group.png",
       p_grp3, width = 11, height = 6, bg = "white")


################################################################################
# PLOT B: PRE-POST SCORES BY 4-GROUP
################################################################################

p_grp4 <- ggplot(
    filter(score_summary_grp, Scheme == "4-group"),
    aes(x = Time, y = mean, color = Group, group = Group)
) +
    geom_point(position = position_dodge(width = 0.3), size = 3) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
        position = position_dodge(width = 0.3), width = 0.15, linewidth = 0.8) +
    geom_line(position = position_dodge(width = 0.3), linewidth = 1) +
    facet_wrap(~Metric) +
    scale_color_manual(values = palette_4) +
    scale_x_discrete(labels = c("pre" = "Pre", "post" = "Post")) +
    guides(color = guide_legend(ncol = 2)) +
    labs(x = "Time", y = "Mean Score (95% CI)", color = NULL) +
    base_theme

ggsave("../Figures/MeanScoresPrePost_4group.png",
       p_grp4, width = 10, height = 6, bg = "white")


################################################################################
# PLOT C: PRE-POST SCORES BY 5-GROUP
################################################################################

p_grp5 <- ggplot(
    filter(score_summary_grp, Scheme == "5-group"),
    aes(x = Time, y = mean, color = Group, group = Group)
) +
    geom_point(position = position_dodge(width = 0.4), size = 3) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
        position = position_dodge(width = 0.4), width = 0.15, linewidth = 0.8) +
    geom_line(position = position_dodge(width = 0.4), linewidth = 1) +
    facet_wrap(~Metric) +
    scale_color_manual(values = palette_5) +
    scale_x_discrete(labels = c("pre" = "Pre", "post" = "Post")) +
    guides(color = guide_legend(ncol = 2)) +
    labs(x = "Time", y = "Mean Score (95% CI)", color = NULL) +
    base_theme

ggsave("../Figures/MeanScoresPrePost_5group.png",
       p_grp5, width = 10, height = 6, bg = "white")


################################################################################
# COEFFICIENT PLOTS (forest plots)
################################################################################

coef_plot_data <- base_group_results %>%
    filter(!grepl("Intercept", Term), Term != "Timepost") %>%
    mutate(
        Metric = clean_metric(Metric),
        Term = case_when(
            Group_scheme == "3-group" ~ gsub("group_3", "", Term),
            Group_scheme == "4-group" ~ gsub("group_4", "", Term),
            Group_scheme == "5-group" ~ gsub("group_5", "", Term)
        ) %>%
            gsub(":Timepost", " × Post", .) %>%
            trimws(),
        Term_type   = ifelse(grepl("×", Term), "Interaction", "Main effect"),
        Significant = ifelse(p_value < 0.05, "yes", "no")
    )

coef_plot_layers <- list(
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50"),
    geom_errorbarh(aes(xmin = LCL, xmax = UCL, alpha = Significant),
        height = 0.2, linewidth = 0.8),
    geom_point(aes(alpha = Significant), size = 3),
    scale_color_manual(values = c("Main effect" = "#56B4E9", "Interaction" = "#E69F00")),
    scale_alpha_manual(values = c("yes" = 1, "no" = 0.4), guide = "none"),
    labs(y = NULL, color = NULL),
    theme_bw(base_family = "Times New Roman"),
    theme(
        legend.position = "top",
        text = element_text(family = "Times New Roman"),
        panel.grid.major = element_line(color = "gray85"),
        panel.grid.minor = element_line(color = "gray95"),
        strip.background = element_rect(fill = "gray95")
    )
)

p_coef_3 <- ggplot(filter(coef_plot_data, Group_scheme == "3-group"),
    aes(x = Estimate, y = reorder(Term, Estimate), color = Term_type)) +
    coef_plot_layers + facet_wrap(~Metric, scales = "free_x") +
    labs(x = "Estimate (relative to reference group, pre-time)")

p_coef_4 <- ggplot(filter(coef_plot_data, Group_scheme == "4-group"),
    aes(x = Estimate, y = reorder(Term, Estimate), color = Term_type)) +
    coef_plot_layers + facet_wrap(~Metric, scales = "free_x") +
    labs(x = "Estimate (relative to R class only, pre-time)")

p_coef_5 <- ggplot(filter(coef_plot_data, Group_scheme == "5-group"),
    aes(x = Estimate, y = reorder(Term, Estimate), color = Term_type)) +
    coef_plot_layers + facet_wrap(~Metric, scales = "free_x") +
    labs(x = "Estimate (relative to reference group, pre-time)")

ggsave("../Figures/CoefPlot_3group.png", p_coef_3, width = 11, height = 6, bg = "white")
ggsave("../Figures/CoefPlot_4group.png", p_coef_4, width = 11, height = 6, bg = "white")
ggsave("../Figures/CoefPlot_5group.png", p_coef_5, width = 13, height = 7, bg = "white")


################################################################################
# EMMEANS PLOTS (model-predicted means)
################################################################################

emmeans_3grp <- extract_emmeans(base_models_3grp, "group_3", metrics)
emmeans_4grp <- extract_emmeans(base_models_4grp, "group_4", metrics)
emmeans_5grp <- extract_emmeans(base_models_5grp, "group_5", metrics)

p_emmeans_3 <- ggplot(emmeans_3grp,
    aes(x = Time, y = emmean, color = group_3, group = group_3)) +
    geom_point(position = position_dodge(width = 0.3), size = 3) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
        position = position_dodge(width = 0.3), width = 0.15, linewidth = 0.8) +
    geom_line(position = position_dodge(width = 0.3), linewidth = 1) +
    facet_wrap(~Metric) +
    scale_color_manual(values = palette_3) +
    scale_x_discrete(labels = c("pre" = "Pre", "post" = "Post")) +
    labs(x = "Time", y = "Model-predicted Mean Score (95% CI)", color = NULL) +
    base_theme

p_emmeans_4 <- ggplot(emmeans_4grp,
    aes(x = Time, y = emmean, color = group_4, group = group_4)) +
    geom_point(position = position_dodge(width = 0.3), size = 3) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
        position = position_dodge(width = 0.3), width = 0.15, linewidth = 0.8) +
    geom_line(position = position_dodge(width = 0.3), linewidth = 1) +
    facet_wrap(~Metric) +
    scale_color_manual(values = palette_4) +
    scale_x_discrete(labels = c("pre" = "Pre", "post" = "Post")) +
    guides(color = guide_legend(ncol = 2)) +
    labs(x = "Time", y = "Model-predicted Mean Score (95% CI)", color = NULL) +
    base_theme

p_emmeans_5 <- ggplot(emmeans_5grp,
    aes(x = Time, y = emmean, color = group_5, group = group_5)) +
    geom_point(position = position_dodge(width = 0.4), size = 3) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
        position = position_dodge(width = 0.4), width = 0.15, linewidth = 0.8) +
    geom_line(position = position_dodge(width = 0.4), linewidth = 1) +
    facet_wrap(~Metric) +
    scale_color_manual(values = palette_5) +
    scale_x_discrete(labels = c("pre" = "Pre", "post" = "Post")) +
    guides(color = guide_legend(ncol = 2)) +
    labs(x = "Time", y = "Model-predicted Mean Score (95% CI)", color = NULL) +
    base_theme

ggsave("../Figures/EmmeanPlot_3group.png", p_emmeans_3, width = 10, height = 6, bg = "white")
ggsave("../Figures/EmmeanPlot_4group.png", p_emmeans_4, width = 10, height = 6, bg = "white")
ggsave("../Figures/EmmeanPlot_5group.png", p_emmeans_5, width = 10, height = 6, bg = "white")


################################################################################
################# PART 2: COURSE SEQUENCE ANALYSIS ############################
################################################################################

################################################################################
# DATA PREPARATION
################################################################################

survey_tagged <- survey %>%
    mutate(
        # independent flags so concurrent students appear in both courses
        in_R_class = grepl(
            "Computational Problem Solving in Wildlife Ecology Using R \\(WIS 4934\\)",
            Current.classes),
        in_Quant   = grepl("Quantitative Wildlife Ecology", Current.classes),
        in_PopEco  = grepl("Population Ecology", Current.classes)
    ) %>%
    pivot_longer(
        cols      = c(in_R_class, in_Quant, in_PopEco),
        names_to  = "course_flag",
        values_to = "took_this_sem"
    ) %>%
    filter(took_this_sem) %>%
    mutate(
        course_this_sem = case_when(
            course_flag == "in_R_class" ~ "R Class",
            course_flag == "in_Quant"   ~ "Quant",
            course_flag == "in_PopEco"  ~ "Pop Eco"
        ),
        course_this_sem = factor(course_this_sem, levels = c("R Class", "Quant", "Pop Eco")),
        Found_label = factor(
            ifelse(Foundational == 1, "Took R Class", "No R Class"),
            levels = c("No R Class", "Took R Class")
        )
    ) %>%
    dplyr::select(-course_flag, -took_this_sem)

# sanity check
survey_tagged %>%
    distinct(Participant.ID, course_this_sem, Foundational) %>%
    count(course_this_sem, Foundational)


################################################################################
# PRE/POST LONG FORMAT
################################################################################

course_long_prepost <- survey_tagged %>%
    dplyr::select(
        Participant.ID, course_this_sem, Found_label,
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
                        labels = c("Anxiety", "Math Skills", "Computing"))
    ) %>%
    filter(!is.na(Score))


################################################################################
# CHANGE SCORE LONG FORMAT
################################################################################

course_long_change <- survey_tagged %>%
    dplyr::select(
        Participant.ID, course_this_sem, Found_label,
        Anxiety_pre, Anxiety_post,
        Math_skills_pre, Math_skills_post,
        Computing_pre, Computing_post
    ) %>%
    mutate(
        Anxiety_change = Anxiety_post    - Anxiety_pre,
        MathSkills_change = Math_skills_post - Math_skills_pre,
        Computing_change = Computing_post  - Computing_pre
    ) %>%
    dplyr::select(
        Participant.ID, course_this_sem, Found_label,
        Anxiety_change, MathSkills_change, Computing_change
    ) %>%
    pivot_longer(
        cols = c(Anxiety_change, MathSkills_change, Computing_change),
        names_to = "Metric",
        values_to = "Score"
    ) %>%
    mutate(
        Metric = factor(Metric,
                        levels = c("Anxiety_change", "MathSkills_change", "Computing_change"),
                        labels = c("Anxiety", "Math Skills", "Computing"))
    ) %>%
    filter(!is.na(Score))


################################################################################
# SUMMARIES
################################################################################

# pre/post: course × time × metric × Found_label
summary_prepost <- course_long_prepost %>%
    group_by(course_this_sem, Time, Metric, Found_label) %>%
    summarise(
        mean = mean(Score, na.rm = TRUE),
        sd = sd(Score, na.rm = TRUE),
        n = n(),
        se = sd / sqrt(n),
        ci_lower = mean - qt(0.975, n - 1) * se,
        ci_upper = mean + qt(0.975, n - 1) * se,
        .groups = "drop"
    )

# change score: course × metric × Found_label
summary_change <- course_long_change %>%
    group_by(course_this_sem, Metric, Found_label) %>%
    summarise(
        mean = mean(Score, na.rm = TRUE),
        sd = sd(Score, na.rm = TRUE),
        n = n(),
        se = sd / sqrt(n),
        ci_lower = mean - qt(0.975, n - 1) * se,
        ci_upper = mean + qt(0.975, n - 1) * se,
        .groups = "drop"
    )


################################################################################
# LMM: PRE/POST - Score ~ course × Time + Found_label + (1 | Participant.ID)
################################################################################

metrics_plot <- c("Anxiety", "Math Skills", "Computing")

prepost_model_results    <- data.frame()
prepost_emmeans_results  <- data.frame()
prepost_pairwise_results <- data.frame()
prepost_models <- list()

for (metric in metrics_plot) {

    cat("\n----------------------------------------\n")
    cat("Pre/post course model for:", metric, "\n")
    cat("----------------------------------------\n")

    mdata <- course_long_prepost %>%
        filter(Metric == metric) %>%
        drop_na(Score, course_this_sem, Time, Participant.ID, Found_label)

    model <- tryCatch(
        lmer(Score ~ course_this_sem * Time + Found_label + (1 | Participant.ID),
             data = mdata, REML = FALSE),
        error = function(e) { message("Model error: ", e$message); NULL }
    )
    if (is.null(model)) next
    prepost_models[[metric]] <- model
    cat("  Model fitted. N =", nrow(mdata), "\n")

    coef_table <- summary(model)$coefficients
    coef_df <- data.frame(
        Metric = metric,
        Term = rownames(coef_table),
        Estimate = round(coef_table[, "Estimate"], 3),
        SE = round(coef_table[, "Std. Error"], 3),
        t_value = round(coef_table[, "t value"], 3),
        p_value = round(coef_table[, "Pr(>|t|)"], 4),
        Sig = case_when(
            coef_table[, "Pr(>|t|)"] < 0.001 ~ "***",
            coef_table[, "Pr(>|t|)"] < 0.01  ~ "**",
            coef_table[, "Pr(>|t|)"] < 0.05  ~ "*",
            coef_table[, "Pr(>|t|)"] < 0.1   ~ ".",
            TRUE ~ ""
        ),
        LCL = round(coef_table[, "Estimate"] - 1.96 * coef_table[, "Std. Error"], 3),
        UCL = round(coef_table[, "Estimate"] + 1.96 * coef_table[, "Std. Error"], 3),
        stringsAsFactors = FALSE
    )
    rownames(coef_df) <- NULL
    prepost_model_results <- rbind(prepost_model_results, coef_df)

    # emmeans conditioned on Found_label -> separate trajectories per group
    em <- emmeans(model, ~ course_this_sem * Time | Found_label) %>%
        as.data.frame() %>%
        mutate(Metric = metric)
    prepost_emmeans_results <- rbind(prepost_emmeans_results, em)

    pairs_df <- pairs(emmeans(model, ~ course_this_sem | Time), adjust = "tukey") %>%
        as.data.frame() %>%
        mutate(Metric = metric)
    prepost_pairwise_results <- rbind(prepost_pairwise_results, pairs_df)

    cat("\n  Pairwise course comparisons (Tukey):\n")
    print(pairs_df)
}

prepost_pairwise_results <- prepost_pairwise_results %>%
    mutate(Sig = case_when(
        p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
        p.value < 0.05  ~ "*",   p.value < 0.1  ~ ".",
        TRUE ~ ""
    ))


################################################################################
# LMM: CHANGE SCORE - Score ~ course + Found_label + (1 | Participant.ID)
################################################################################

change_model_results <- data.frame()
change_emmeans_results <- data.frame()
change_pairwise_results <- data.frame()
change_models <- list()

for (metric in metrics_plot) {

    cat("\n----------------------------------------\n")
    cat("Change score course model for:", metric, "\n")
    cat("----------------------------------------\n")

    mdata <- course_long_change %>%
        filter(Metric == metric) %>%
        drop_na(Score, course_this_sem, Participant.ID, Found_label)

    model <- tryCatch(
        lmer(Score ~ course_this_sem + Found_label + (1 | Participant.ID),
             data = mdata, REML = FALSE),
        error = function(e) { message("Model error: ", e$message); NULL }
    )
    if (is.null(model)) next
    change_models[[metric]] <- model
    cat("  Model fitted. N =", nrow(mdata), "\n")

    coef_table <- summary(model)$coefficients
    coef_df <- data.frame(
        Metric = metric,
        Term = rownames(coef_table),
        Estimate = round(coef_table[, "Estimate"], 3),
        SE = round(coef_table[, "Std. Error"], 3),
        t_value = round(coef_table[, "t value"], 3),
        p_value = round(coef_table[, "Pr(>|t|)"], 4),
        Sig = case_when(
            coef_table[, "Pr(>|t|)"] < 0.001 ~ "***",
            coef_table[, "Pr(>|t|)"] < 0.01  ~ "**",
            coef_table[, "Pr(>|t|)"] < 0.05  ~ "*",
            coef_table[, "Pr(>|t|)"] < 0.1   ~ ".",
            TRUE ~ ""
        ),
        LCL = round(coef_table[, "Estimate"] - 1.96 * coef_table[, "Std. Error"], 3),
        UCL = round(coef_table[, "Estimate"] + 1.96 * coef_table[, "Std. Error"], 3),
        stringsAsFactors = FALSE
    )
    rownames(coef_df) <- NULL
    change_model_results <- rbind(change_model_results, coef_df)

    em <- emmeans(model, ~ course_this_sem | Found_label) %>%
        as.data.frame() %>%
        mutate(Metric = metric)
    change_emmeans_results <- rbind(change_emmeans_results, em)

    pairs_df <- pairs(emmeans(model, ~ course_this_sem), adjust = "tukey") %>%
        as.data.frame() %>%
        mutate(Metric = metric)
    change_pairwise_results <- rbind(change_pairwise_results, pairs_df)

    cat("\n  Pairwise course comparisons (Tukey):\n")
    print(pairs_df)
}

change_pairwise_results <- change_pairwise_results %>%
    mutate(Sig = case_when(
        p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
        p.value < 0.05  ~ "*",   p.value < 0.1  ~ ".",
        TRUE ~ ""
    ))


################################################################################
# SAVE PAIRWISE TABLES
################################################################################

save_pairwise_table <- function(pairs_df, filepath) {
    pairs_df %>%
        mutate(estimate = round(estimate, 3), SE = round(SE, 3),
               t.ratio  = round(t.ratio, 3), p.value = round(p.value, 4)) %>%
        dplyr::select(Metric, contrast, estimate, SE, t.ratio, p.value, Sig) %>%
        kbl(col.names = c("Metric", "Contrast", "Estimate", "SE", "t", "p", "")) %>%
        kable_classic(font_size = 14, html_font = "Times New Roman") %>%
        row_spec(0, bold = TRUE) %>%
        column_spec(1, italic = TRUE) %>%
        collapse_rows(columns = 1, valign = "top") %>%
        footnote(
            general = "Tukey-adjusted pairwise comparisons between courses, collapsing over time. . p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001",
            footnote_as_chunk = TRUE
        ) %>%
        save_kable(file = filepath, zoom = 2)
}

save_pairwise_table(prepost_pairwise_results, "../Figures/Tables/course_prepost_pairwise.png")
save_pairwise_table(change_pairwise_results,  "../Figures/Tables/course_change_pairwise.png")


################################################################################
# PLOT 1: RAW PRE/POST 
################################################################################

p_course_seq <- ggplot(
    summary_prepost,
    aes(x = course_this_sem, y = mean,
        color = Found_label, group = interaction(Found_label, Time))
) +
    geom_line(aes(linetype = Time),
              position = position_dodge(width = 0.4), linewidth = 0.9) +
    geom_errorbar(
        aes(ymin = ci_lower, ymax = ci_upper),
        position = position_dodge(width = 0.4),
        width = 0.15, linewidth = 0.8
    ) +
    geom_point(aes(shape = interaction(Found_label, Time)),
               position = position_dodge(width = 0.4), size = 3) +
    facet_wrap(~Metric) +
    scale_color_manual(values = palette_found) +
    scale_linetype_manual(
        values = c("pre" = "dashed", "post" = "solid"),
        labels = c("pre" = "Pre", "post" = "Post")
    ) +
    scale_shape_manual(
        values = c(
            "No R Class.pre" = 1,    # open circle
            "No R Class.post" = 16,   # filled circle
            "Took R Class.pre" = 2,    # open triangle
            "Took R Class.post" = 17   # filled triangle
        ),
        guide = "none"   # shape redundant with color + linetype; drop from legend
    ) +
    scale_x_discrete(labels = course_x_labels) +
    labs(
        x = "Course (intended sequence)",
        y = "Mean Score (95% CI)",
        color = NULL,
        linetype = "Time"
    ) +
    base_theme +
    theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave("../Figures/CourseSequence_RawMeans.png",
       p_course_seq, width = 11, height = 6, bg = "white")


################################################################################
# PLOT 2: CHANGE SCORES
################################################################################

p_course_change <- ggplot(
    summary_change,
    aes(x = course_this_sem, y = mean, color = Found_label, group = Found_label)
) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_line(position = position_dodge(width = 0.3), linewidth = 0.9) +
    geom_errorbar(
        aes(ymin = ci_lower, ymax = ci_upper),
        position = position_dodge(width = 0.3),
        width = 0.15, linewidth = 0.8
    ) +
    geom_point(aes(shape = Found_label),
               position = position_dodge(width = 0.3), size = 3) +
    facet_wrap(~Metric) +
    scale_color_manual(values = palette_found) +
    scale_shape_manual(values = c("No R Class" = 16, "Took R Class" = 17)) +
    scale_x_discrete(labels = course_x_labels) +
    labs(
        x     = "Course (intended sequence)",
        y     = "Mean Change Score, Post − Pre (95% CI)",
        color = NULL,
        shape = NULL
    ) +
    base_theme +
    theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave("../Figures/CourseSequence_ChangeScores.png",
       p_course_change, width = 11, height = 6, bg = "white")

################################################################################
# PLOT 3: EMMEANS PRE/POST 
################################################################################

prepost_emmeans_results <- prepost_emmeans_results %>%
    mutate(
        Time            = factor(Time, levels = c("pre", "post")),
        Metric          = factor(Metric, levels = c("Anxiety", "Math Skills", "Computing")),
        course_this_sem = factor(course_this_sem, levels = c("R Class", "Quant", "Pop Eco")),
        Found_label     = factor(Found_label, levels = c("No R Class", "Took R Class"))
    )

p_course_emmeans <- ggplot(
    prepost_emmeans_results,
    aes(x = Time, y = emmean, color = Found_label, group = Found_label)
) +
    geom_line(position = dodge, linewidth = 0.9) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
        position = dodge, width = 0.15, linewidth = 0.8) +
    geom_point(aes(shape = Found_label), position = dodge, size = 3) +
    facet_grid(Metric ~ course_this_sem,
               labeller = labeller(course_this_sem = course_x_labels)) +
    scale_color_manual(values = palette_found) +
    scale_shape_manual(values = c("No R Class" = 16, "Took R Class" = 17)) +
    scale_x_discrete(labels = c("pre" = "Pre", "post" = "Post")) +
    labs(x = "Time", y = "Model-predicted Mean Score (95% CI)",
         color = NULL, shape = NULL) +
    base_theme +
    guides(color = guide_legend(override.aes = list(shape = c(16, 17))))

ggsave("../Figures/CourseSequence_Emmeans.png",
       p_course_emmeans, width = 12, height = 8, bg = "white")


################################################################################
# PLOT 4: EMMEANS CHANGE SCORES; model-predicted, split by Found_label
################################################################################

change_emmeans_results <- change_emmeans_results %>%
    mutate(
        Metric = factor(Metric, levels = c("Anxiety", "Math Skills", "Computing")),
        course_this_sem = factor(course_this_sem, levels = c("R Class", "Quant", "Pop Eco")),
        Found_label = factor(Found_label, levels = c("No R Class", "Took R Class"))
    )

p_change_emmeans <- ggplot(
    change_emmeans_results,
    aes(x = course_this_sem, y = emmean, color = Found_label, group = Found_label)
) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_line(position = position_dodge(width = 0.3), linewidth = 0.9) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
        position = position_dodge(width = 0.3), width = 0.15, linewidth = 0.8) +
    geom_point(aes(shape = Found_label),
               position = position_dodge(width = 0.3), size = 3) +
    facet_wrap(~Metric) +
    scale_color_manual(values = palette_found) +
    scale_shape_manual(values = c("No R Class" = 16, "Took R Class" = 17)) +
    scale_x_discrete(labels = course_x_labels) +
    labs(x = "Course (intended sequence)",
         y = "Model-predicted Change Score (95% CI)",
         color = NULL, shape = NULL) +
    base_theme +
    theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave("../Figures/CourseSequence_ChangeEmmeans.png",
       p_change_emmeans, width = 11, height = 6, bg = "white")




################################################################################
################# PART 3: COURSE COMBINATION GROUP PLOTS ######################
################################################################################

# classify each student by their overall course combination

student_combo <- survey %>%
    group_by(Participant.ID) %>%
    summarise(
        took_R = any(Foundational == 1, na.rm = TRUE),
        took_Quant = any(Quant == 1, na.rm = TRUE),
        took_PopEco = any(PopEco == 1, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(
        combo_group = case_when(
            took_R & took_Quant & took_PopEco ~ "All 3",
            took_R & took_Quant & !took_PopEco ~ "R + Quant",
            took_R & !took_Quant & took_PopEco ~ "R + Pop Eco",
            !took_R & took_Quant & took_PopEco ~ "Quant + Pop Eco",
            took_R & !took_Quant & !took_PopEco ~ "R only",
            !took_R & took_Quant & !took_PopEco ~ "Quant only",
            !took_R & !took_Quant & took_PopEco ~ "Pop Eco only",
            TRUE ~ "None"
        ),
        combo_group = factor(combo_group, levels = c(
            "R only", "Quant only", "Pop Eco only",
            "R + Quant", "Quant + Pop Eco", "All 3"
        ))
    )

# groups requested for plotting
combo_groups_keep <- c("R only", "Quant only", "Pop Eco only",
                       "R + Quant", "Quant + Pop Eco", "All 3")

# join combo classification into the pre/post long course data
course_long_combo <- course_long_prepost %>%
    left_join(student_combo %>% dplyr::select(Participant.ID, combo_group),
              by = "Participant.ID") %>%
    filter(combo_group %in% combo_groups_keep) %>%
    droplevels()

##############################################################
# summarize: course × metric × combo_group × time
##############################################################
summary_combo <- course_long_combo %>%
    group_by(course_this_sem, Metric, combo_group, Time) %>%
    summarise(
        mean = mean(Score, na.rm = TRUE),
        sd = sd(Score, na.rm = TRUE),
        n = n(),
        se = sd / sqrt(n),
        ci_lower = mean - qt(0.975, n - 1) * se,
        ci_upper = mean + qt(0.975, n - 1) * se,
        .groups = "drop"
    )

# colorblind-friendly palette for 6 combo groups
palette_combo <- c(
    "R only" = "#56B4E9",   # blue
    "Quant only" = "#E69F00",   # orange
    "Pop Eco only" = "#009E73",   # green
    "R + Quant" = "#CC79A7",   # pink
    "Quant + Pop Eco" = "#D55E00",   # vermillion
    "All 3" = "#0072B2"    # dark blue
)

################################################################################
# HELPER: build one combo group plot for a given time point
################################################################################ 
plot_combo <- function(time_label) {
    ggplot(
        filter(summary_combo, Time == time_label),
        aes(x = course_this_sem, y = mean,
            color = combo_group, group = combo_group)
    ) +
        geom_line(position = position_dodge(width = 0.4),
                  linewidth = 0.8, na.rm = TRUE) +
        geom_errorbar(
            aes(ymin = ci_lower, ymax = ci_upper),
            position = position_dodge(width = 0.4),
            width = 0.15, linewidth = 0.8, na.rm = TRUE
        ) +
        geom_point(
            aes(shape = combo_group),
            position = position_dodge(width = 0.4),
            size = 3, na.rm = TRUE
        ) +
        facet_wrap(~Metric) +
        scale_color_manual(values = palette_combo) +
        scale_shape_manual(values = c(
            "R only" = 16, "Quant only" = 17, "Pop Eco only" = 15,
            "R + Quant" = 18, "Quant + Pop Eco" = 8, "All 3" = 7
        )) +
        scale_x_discrete(labels = course_x_labels) +
        labs(
            title    = paste0(ifelse(time_label == "pre", "Pre", "Post"),
                              "-Semester Scores by Course Combination"),
            x        = "Course (intended sequence)",
            y        = "Mean Score (95% CI)",
            color    = NULL,
            shape    = NULL
        ) +
        guides(color = guide_legend(ncol = 2),
               shape = guide_legend(ncol = 2)) +
        base_theme +
        theme(axis.text.x = element_text(angle = 15, hjust = 1),
              plot.title  = element_text(hjust = 0.5, size = 12))
}


################################################################################
# PLOT: PRE-SEMESTER SCORES BY COURSE COMBINATION
################################################################################

p_combo_pre <- plot_combo("pre")

ggsave("../Figures/CourseCombo_Pre.png",
       p_combo_pre, width = 11, height = 6, bg = "white")


################################################################################
# PLOT: POST-SEMESTER SCORES BY COURSE COMBINATION
################################################################################

p_combo_post <- plot_combo("post")

ggsave("../Figures/CourseCombo_Post.png",
       p_combo_post, width = 11, height = 6, bg = "white")





################################################################################
# PLOT: R+QUANT CONCURRENT vs SEQUENTIAL vs QUANT ONLY
################################################################################

################################################################################
# classify students by R+Quant timing using raw semester rows
# concurrent = both appear in Current.classes in the SAME semester row
################################################################################ 
rq_timing <- survey %>%
    mutate(
        has_R_this_sem = grepl(
            "Computational Problem Solving in Wildlife Ecology Using R \\(WIS 4934\\)",
            Current.classes
        ),
        has_Quant_this_sem = grepl("Quantitative Wildlife Ecology", Current.classes)
    ) %>%
    group_by(Participant.ID) %>%
    summarise(
        took_R = any(has_R_this_sem, na.rm = TRUE),
        took_Quant = any(has_Quant_this_sem, na.rm = TRUE),
        took_concurrent = any(has_R_this_sem & has_Quant_this_sem, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(
        rq_group = case_when(
            took_R & took_Quant & took_concurrent ~ "R + Quant concurrent",
            took_R & took_Quant & !took_concurrent ~ "R + Quant sequential",
            !took_R & took_Quant ~ "Quant only",
            TRUE ~ "None"
        ),
        rq_group = factor(rq_group, levels = c(
            "Quant only",
            "R + Quant sequential",
            "R + Quant concurrent"
        ))
    )

# sanity check
rq_timing %>% count(rq_group)

################################################################################
# join timing classification into pre/post long data
# keep only students in the three groups of interest
################################################################################ 
course_long_rq <- course_long_prepost %>%
    left_join(rq_timing %>% dplyr::select(Participant.ID, rq_group),
        by = "Participant.ID"
    ) %>%
    filter(rq_group != "None", !is.na(rq_group)) %>%
    droplevels()

################################################################################
# SUMMARIZE: course × time × metric × rq_group
################################################################################ 
summary_rq <- course_long_rq %>%
    group_by(course_this_sem, Time, Metric, rq_group) %>%
    summarise(
        mean = mean(Score, na.rm = TRUE),
        sd = sd(Score, na.rm = TRUE),
        n = n(),
        se = sd / sqrt(n),
        ci_lower = mean - qt(0.975, n - 1) * se,
        ci_upper = mean + qt(0.975, n - 1) * se,
        .groups = "drop"
    )

palette_rq <- c(
    "Quant only" = "#56B4E9", # blue
    "R + Quant sequential" = "#E69F00", # orange
    "R + Quant concurrent" = "#009E73" # green
)

################################################################################
# PLOT
################################################################################ 
p_rq_timing <- ggplot(
    summary_rq,
    aes(
        x = course_this_sem, y = mean,
        color = rq_group,
        group = interaction(rq_group, Time)
    )
) +
    geom_line(aes(linetype = Time),
        position = position_dodge(width = 0.4), linewidth = 0.9
    ) +
    geom_errorbar(
        aes(ymin = ci_lower, ymax = ci_upper),
        position = position_dodge(width = 0.4),
        width = 0.15, linewidth = 0.8
    ) +
    geom_point(
        aes(shape = interaction(rq_group, Time)),
        position = position_dodge(width = 0.4), size = 3
    ) +
    facet_wrap(~Metric) +
    scale_color_manual(values = palette_rq) +
    scale_linetype_manual(
        values = c("pre" = "dashed", "post" = "solid"),
        labels = c("pre" = "Pre", "post" = "Post")
    ) +
    scale_shape_manual(
        values = c(
            "Quant only.pre" = 1, # open circle
            "Quant only.post" = 16, # filled circle
            "R + Quant sequential.pre" = 2, # open triangle
            "R + Quant sequential.post" = 17, # filled triangle
            "R + Quant concurrent.pre" = 0, # open square
            "R + Quant concurrent.post" = 15 # filled square
        ),
        guide = "none"
    ) +
    scale_x_discrete(labels = course_x_labels) +
    labs(
        x = "Course (intended sequence)",
        y = "Mean Score (95% CI)",
        color = NULL,
        linetype = "Time"
    ) +
    base_theme +
    theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave("../Figures/RQ_Timing_Scores.png",
    p_rq_timing,
    width = 11, height = 6, bg = "white"
)




################################################################################
################# PART 4: EXPANDED COMBO PLOTS + STUDENT COUNTS ###############
################################################################################


################################################################################
# STUDENT COUNTS BY GROUPING SCHEME
################################################################################

cat("\n================================================================================\n")
cat("                         STUDENT COUNTS ACROSS ALL GROUPINGS\n")
cat("================================================================================\n")

# total unique students in the dataset
cat("\n--- TOTAL ---\n")
cat(
    "Total unique students (all records):",
    length(unique(survey$Participant.ID)), "\n"
)

################################################################################
# 1. FOUNDATIONAL STATUS
# included: all students with non-NA Foundational flag
# excluded: students where both Current.classes and Previous.classes are NA
################################################################################
cat("\n--- 1. FOUNDATIONAL STATUS ---\n")
cat("(included: all students with non-NA Foundational flag)\n")
cat("(excluded: students where course columns are both NA)\n")
found_counts <- survey %>%
    distinct(Participant.ID, Foundational) %>%
    filter(!is.na(Foundational)) %>%
    count(Foundational) %>%
    mutate(Label = ifelse(Foundational == 1,
        "Took R class at some point",
        "Never took R class"
    ))
print(as.data.frame(found_counts))

################################################################################
# 2. 3-GROUP SCHEMA
# included: students with a valid (non-"None") group_3 assignment
# excluded: students with NA course flags (group_3 == "None")
################################################################################
cat("\n--- 2. THREE-GROUP SCHEMA ---\n")
cat("(included: students with a valid group_3 assignment)\n")
cat("(excluded: students assigned 'None' due to missing course data)\n")
grp3_counts <- survey %>%
    distinct(Participant.ID, group_3) %>%
    filter(group_3 != "None", !is.na(group_3)) %>%
    count(group_3)
print(as.data.frame(grp3_counts))

################################################################################
# 3. 4-GROUP SCHEMA (R class timing relative to advanced courses)
# included: students with a valid group_4 assignment
# excluded: students with NA course flags (group_4 == "None")
################################################################################
cat("\n--- 3. FOUR-GROUP SCHEMA ---\n")
cat("(included: students with valid group_4 assignment)\n")
cat("(excluded: students assigned 'None')\n")
cat("(note: timing based on which semester each course appeared in Current.classes)\n")
grp4_counts <- survey %>%
    distinct(Participant.ID, group_4) %>%
    filter(group_4 != "None", !is.na(group_4)) %>%
    count(group_4)
print(as.data.frame(grp4_counts))

################################################################################
# 4. 5-GROUP SCHEMA
# included: students with a valid group_5 assignment
# excluded: students with NA course flags (group_5 == "None")
################################################################################
cat("\n--- 4. FIVE-GROUP SCHEMA ---\n")
cat("(included: students with valid group_5 assignment)\n")
cat("(excluded: students assigned 'None')\n")
grp5_counts <- survey %>%
    distinct(Participant.ID, group_5) %>%
    filter(group_5 != "None", !is.na(group_5)) %>%
    count(group_5)
print(as.data.frame(grp5_counts))

################################################################################
# 5. COURSE COMBINATION GROUPS (from student_combo)
# included: all students where course flags could be determined
# excluded: students assigned "None" (no courses identified)
################################################################################
cat("\n--- 5. COURSE COMBINATION GROUPS (ever took each course) ---\n")
cat("(included: all students with at least one identifiable course)\n")
cat("(excluded: students where no course strings matched in Current.classes)\n")
cat("(note: students may have taken courses not in the 3-course sequence;\n")
cat("       those are excluded as 'None')\n")
combo_counts <- student_combo %>%
    filter(combo_group != "None", !is.na(combo_group)) %>%
    count(combo_group)
print(as.data.frame(combo_counts))

################################################################################
# 6. R + QUANT TIMING (from rq_timing)
# included: students who took Quant at any point
################################################################################
cat("\n--- 6. R + QUANT TIMING GROUPS ---\n")
cat("(included: all students who took Quant at any point)\n")
cat("(excluded: students who never took Quant)\n")
cat("(note: concurrent = R and Quant in same semester's Current.classes)\n")
cat("(note: Pop Eco status is IRRELEVANT to this grouping — students\n")
cat("       who also took Pop Eco are included in all three groups)\n")
rq_counts <- rq_timing %>%
    filter(rq_group != "None", !is.na(rq_group)) %>%
    count(rq_group)
print(as.data.frame(rq_counts))

################################################################################
# 7. R CLASS + ANY ADVANCED COURSE (sequence irrelevant)
# included: students who took R class AND at least one of Quant or Pop Eco
# excluded: students who only took R class, only took advanced, or have NA flags
################################################################################
cat("\n--- 7. R CLASS + ANY ADVANCED COURSE (sequence irrelevant) ---\n")
cat("(included: students who took R AND at least Quant or Pop Eco)\n")
cat("(excluded: R class only students, advanced only students, NA)\n")
cat("(note: timing and sequence between courses is ignored)\n")
r_plus_advanced <- survey %>%
    distinct(Participant.ID, Foundational, Quant, PopEco) %>%
    filter(!is.na(Foundational), !is.na(Quant), !is.na(PopEco)) %>%
    mutate(
        r_and_advanced = Foundational == 1 & (Quant == 1 | PopEco == 1)
    ) %>%
    count(r_and_advanced) %>%
    mutate(Label = ifelse(r_and_advanced,
        "R class + at least one advanced course",
        "Did not take both R and an advanced course"
    ))
print(as.data.frame(r_plus_advanced))

################################################################################
# 8. COURSE SEQUENCE PLOT GROUPS (survey_tagged)
# included: students who appeared in at least one of R Class / Quant / Pop Eco
#           in Current.classes at any point; one row per student per course
################################################################################ 
cat("\n--- 8. COURSE SEQUENCE PLOT GROUPS (student-course rows) ---\n")
cat("(included: students appearing in at least one of the 3 target courses)\n")
cat("(excluded: students with no matching course strings in Current.classes)\n")
seq_counts <- survey_tagged %>%
    distinct(Participant.ID, course_this_sem, Found_label) %>%
    count(course_this_sem, Found_label)
print(as.data.frame(seq_counts))

# unique students per course (not inflated by concurrent)
cat("\n  Unique students per course:\n")
seq_uniq <- survey_tagged %>%
    distinct(Participant.ID, course_this_sem) %>%
    count(course_this_sem)
print(as.data.frame(seq_uniq))



################################################################################
# EXPANDED COMBO GROUPS
# R only | Quant only | R+Quant concurrent | R+Quant sequential | Quant+Pop Eco only
# n per group appended to legend labels
################################################################################

# build expanded group classification per student
expanded_combo <- student_combo %>%
    left_join(rq_timing %>% dplyr::select(Participant.ID, took_concurrent),
        by = "Participant.ID"
    ) %>%
    mutate(
        took_concurrent = replace_na(took_concurrent, FALSE),
        expanded_group = case_when(
            # R only: took R, never took Quant or Pop Eco
            combo_group == "R only" ~ "R only",
            # Quant only: took Quant, never took R or Pop Eco
            combo_group == "Quant only" ~ "Quant only",
            # R + Quant concurrent: took both, in the same semester
            # may also have taken Pop Eco 
            (combo_group %in% c("R + Quant", "All 3")) &
                took_concurrent ~ "R + Quant concurrent",
            # R + Quant sequential: took both, but never same semester
            # may also have taken Pop Eco 
            (combo_group %in% c("R + Quant", "All 3")) &
                !took_concurrent ~ "R + Quant sequential",
            # Quant + Pop Eco only: took Quant and Pop Eco, never took R
            combo_group == "Quant + Pop Eco" ~ "Quant + Pop Eco only",
            TRUE ~ "None"
        ),
        expanded_group = factor(expanded_group, levels = c(
            "R only",
            "Quant only",
            "R + Quant sequential",
            "R + Quant concurrent",
            "Quant + Pop Eco only"
        ))
    )

################################################################################
# count students per expanded group
################################################################################
expanded_n <- expanded_combo %>%
    filter(expanded_group != "None", !is.na(expanded_group)) %>%
    count(expanded_group, name = "n_students")

cat("--- EXPANDED COMBO GROUP COUNTS ---\n")
cat("(included: R only, Quant only, R+Quant concurrent, R+Quant sequential,\n")
cat("           Quant+Pop Eco only)\n")
cat("(excluded: students assigned 'None', R+Pop Eco only, All 3 not already\n")
cat("           captured by concurrent/sequential split)\n")
print(as.data.frame(expanded_n))

# build legend labels with n appended
expanded_labels <- expanded_n %>%
    mutate(label = paste0(expanded_group, " (n = ", n_students, ")")) %>%
    dplyr::select(expanded_group, label) %>%
    deframe() # named vector: group → label string

################################################################################
# join expanded group into pre/post long data
################################################################################
course_long_expanded <- course_long_prepost %>%
    left_join(expanded_combo %>% dplyr::select(Participant.ID, expanded_group),
        by = "Participant.ID"
    ) %>%
    filter(expanded_group != "None", !is.na(expanded_group)) %>%
    droplevels()


################################################################################
# summarize: course × time × metric × expanded_group
################################################################################ 
summary_expanded <- course_long_expanded %>%
    # R+Quant groups should only appear at R Class and Quant x positions
    # even if those students also took Pop Eco at some point
    filter(!(expanded_group %in% c("R + Quant sequential", "R + Quant concurrent") &
        course_this_sem == "Pop Eco")) %>%
    group_by(course_this_sem, Time, Metric, expanded_group) %>%
    summarise(
        mean = mean(Score, na.rm = TRUE),
        sd = sd(Score, na.rm = TRUE),
        n = n(),
        se = sd / sqrt(n),
        ci_lower = mean - qt(0.975, n - 1) * se,
        ci_upper = mean + qt(0.975, n - 1) * se,
        .groups = "drop"
    )

# for 5 expanded groups
palette_expanded <- c(
    "R only" = "#56B4E9", # blue
    "Quant only" = "#E69F00", # orange
    "R + Quant sequential" = "#009E73", # green
    "R + Quant concurrent" = "#CC79A7", # pink
    "Quant + Pop Eco only" = "#D55E00" # vermillion
)

shape_expanded <- c(
    "R only" = 16,
    "Quant only" = 17,
    "R + Quant sequential" = 15,
    "R + Quant concurrent" = 18,
    "Quant + Pop Eco only" = 8
)

################################################################################
# HELPER: build expanded combo plot for a given time point
################################################################################
plot_expanded_combo <- function(time_label) {
    ggplot(
        filter(summary_expanded, Time == time_label),
        aes(
            x = course_this_sem, y = mean,
            color = expanded_group,
            group = expanded_group
        )
    ) +
        geom_line(
            position = position_dodge(width = 0.4),
            linewidth = 0.8, na.rm = TRUE
        ) +
        geom_errorbar(
            aes(ymin = ci_lower, ymax = ci_upper),
            position = position_dodge(width = 0.4),
            width = 0.15, linewidth = 0.8, na.rm = TRUE
        ) +
        geom_point(
            aes(shape = expanded_group),
            position = position_dodge(width = 0.4),
            size = 3, na.rm = TRUE
        ) +
        facet_wrap(~Metric) +
        scale_color_manual(
            values = palette_expanded,
            labels = expanded_labels # appends n= to each legend entry
        ) +
        scale_shape_manual(
            values = shape_expanded,
            labels = expanded_labels
        ) +
        scale_x_discrete(labels = course_x_labels) +
        labs(
            title = paste0(
                ifelse(time_label == "pre", "Pre", "Post"),
                "-Semester Scores by Course Combination"
            ),
            x = "Course (intended sequence)",
            y = "Mean Score (95% CI)",
            color = NULL,
            shape = NULL
        ) +
        guides(
            color = guide_legend(ncol = 1),
            shape = guide_legend(ncol = 1)
        ) +
        base_theme +
        theme(
            axis.text.x = element_text(angle = 15, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 12),
            legend.text = element_text(size = 9)
        )
}


################################################################################
# PLOT: PRE-SEMESTER
################################################################################

p_expanded_pre <- plot_expanded_combo("pre")

ggsave("../Figures/CourseCombo_Expanded_Pre.png",
    p_expanded_pre,
    width = 12, height = 6, bg = "white"
)


################################################################################
# PLOT: POST-SEMESTER
################################################################################

p_expanded_post <- plot_expanded_combo("post")

ggsave("../Figures/CourseCombo_Expanded_Post.png",
    p_expanded_post,
    width = 12, height = 6, bg = "white"
)




################################################################################
# EMMEANS PLOTS: EXPANDED COMBO GROUPS
# Score ~ expanded_group * Time + (1 | Participant.ID)
# model-predicted pre/post means per group per metric
################################################################################

# join expanded group into pre/post long data and drop Pop Eco rows for R+Quant groups
course_long_expanded_model <- course_long_prepost %>%
    left_join(expanded_combo %>% dplyr::select(Participant.ID, expanded_group),
        by = "Participant.ID"
    ) %>%
    filter(expanded_group != "None", !is.na(expanded_group)) %>%
    # R+Quant groups only appear at R Class and Quant positions
    filter(!(expanded_group %in% c("R + Quant sequential", "R + Quant concurrent") &
        course_this_sem == "Pop Eco")) %>%
    droplevels()
course_long_expanded_model <- course_long_expanded_model %>%
    mutate(
        expanded_group = relevel(expanded_group, ref = "Quant + Pop Eco only")
    )

# fit LMM per metric
# Score ~ expanded_group * Time + (1 | Participant.ID)
# expanded_group * Time captures whether groups differ in their
# pre-to-post trajectory, consistent with the other group models
expanded_models <- list()
expanded_emmeans_pre <- data.frame()
expanded_emmeans_post <- data.frame()

for (metric in metrics_plot) {
    cat("\n----------------------------------------\n")
    cat("Expanded combo model for:", metric, "\n")
    cat("----------------------------------------\n")

    mdata <- course_long_expanded_model %>%
        filter(Metric == metric) %>%
        drop_na(Score, expanded_group, Time, Participant.ID)

    model <- tryCatch(
        lmer(Score ~ expanded_group * Time + (1 | Participant.ID),
            data = mdata, REML = FALSE
        ),
        error = function(e) {
            message("Model error: ", e$message)
            NULL
        }
    )
    print(summary(model))
    #tab_model(model)

    if (is.null(model)) next
    expanded_models[[metric]] <- model
    cat("  Model fitted. N =", nrow(mdata), "\n")
    #print(summary(model)$coefficients)

    # subset of data for each course separately
    for (course in c("R Class", "Quant")) {
        # which groups appear at this course position
        groups_at_course <- course_long_expanded_model %>%
            filter(Metric == metric, course_this_sem == course) %>%
            pull(expanded_group) %>%
            unique() %>%
            droplevels()

        if (length(groups_at_course) == 0) next

        mdata_course <- mdata %>%
            filter(
                course_this_sem == course,
                expanded_group %in% groups_at_course
            ) %>%
            droplevels()

        model_course <- tryCatch(
            lmer(Score ~ expanded_group * Time + (1 | Participant.ID),
                data = mdata_course, REML = FALSE
            ),
            error = function(e) {
                message("Course model error: ", e$message)
                NULL
            }
        )
        if (is.null(model_course)) next

        em <- emmeans(model_course, ~ expanded_group * Time) %>%
            as.data.frame() %>%
            mutate(Metric = metric, course_this_sem = course)

        expanded_emmeans_pre <- rbind(
            expanded_emmeans_pre,
            filter(em, Time == "pre")
        )
        expanded_emmeans_post <- rbind(
            expanded_emmeans_post,
            filter(em, Time == "post")
        )
    }

    # Pop Eco position: only Quant + Pop Eco only group appears here
    mdata_popeco <- mdata %>%
        filter(
            course_this_sem == "Pop Eco",
            expanded_group == "Quant + Pop Eco only"
        ) %>%
        droplevels()

    if (nrow(mdata_popeco) > 10) {
        model_popeco <- tryCatch(
            lmer(Score ~ Time + (1 | Participant.ID),
                data = mdata_popeco, REML = FALSE
            ),
            error = function(e) {
                message("Pop Eco model error: ", e$message)
                NULL
            }
        )
        if (!is.null(model_popeco)) {
            em_popeco <- emmeans(model_popeco, ~Time) %>%
                as.data.frame() %>%
                mutate(
                    Metric = metric,
                    course_this_sem = "Pop Eco",
                    expanded_group = "Quant + Pop Eco only"
                )
            expanded_emmeans_pre <- rbind(
                expanded_emmeans_pre,
                filter(em_popeco, Time == "pre")
            )
            expanded_emmeans_post <- rbind(
                expanded_emmeans_post,
                filter(em_popeco, Time == "post")
            )
        }
    }
}

# clean factor levels for plotting
clean_expanded_em <- function(df) {
    df %>%
        mutate(
            Metric = factor(Metric,
                levels = c("Anxiety", "Math Skills", "Computing")
            ),
            course_this_sem = factor(course_this_sem,
                levels = c("R Class", "Quant", "Pop Eco")
            ),
            expanded_group = factor(expanded_group, levels = c(
                "R only",
                "Quant only",
                "R + Quant sequential",
                "R + Quant concurrent",
                "Quant + Pop Eco only"
            )),
            Time = factor(Time, levels = c("pre", "post"))
        )
}

expanded_emmeans_pre <- clean_expanded_em(expanded_emmeans_pre)
expanded_emmeans_post <- clean_expanded_em(expanded_emmeans_post)


################################################################################
# HELPER: build emmeans plot for pre or post
################################################################################

plot_expanded_emmeans <- function(em_data, time_label) {
    ggplot(
        em_data,
        aes(
            x = course_this_sem, y = emmean,
            color = expanded_group, group = expanded_group
        )
    ) +
        geom_line(
            position = position_dodge(width = 0.4),
            linewidth = 0.8, na.rm = TRUE
        ) +
        geom_errorbar(
            aes(ymin = lower.CL, ymax = upper.CL),
            position = position_dodge(width = 0.4),
            width = 0.15, linewidth = 0.8, na.rm = TRUE
        ) +
        geom_point(
            aes(shape = expanded_group),
            position = position_dodge(width = 0.4),
            size = 3, na.rm = TRUE
        ) +
        facet_wrap(~Metric) +
        scale_color_manual(
            values = palette_expanded,
            labels = expanded_labels
        ) +
        scale_shape_manual(
            values = shape_expanded,
            labels = expanded_labels
        ) +
        scale_x_discrete(labels = course_x_labels) +
        labs(
            title = paste0(
                "Model-predicted ",
                ifelse(time_label == "pre", "Pre", "Post"),
                "-Semester Scores by Course Combination"
            ),
            x = "Course (intended sequence)",
            y = "Model-predicted Mean Score (95% CI)",
            color = NULL,
            shape = NULL
        ) +
        guides(
            color = guide_legend(ncol = 1),
            shape = guide_legend(ncol = 1)
        ) +
        base_theme +
        theme(
            axis.text.x = element_text(angle = 15, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 12),
            legend.text = element_text(size = 9)
        )
}


################################################################################
# PLOT: MODEL-PREDICTED PRE SCORES 
################################################################################

p_expanded_emmeans_pre <- plot_expanded_emmeans(expanded_emmeans_pre, "pre")

ggsave("../Figures/CourseCombo_Expanded_Emmeans_Pre.png",
    p_expanded_emmeans_pre,
    width = 12, height = 6, bg = "white"
)


################################################################################
# PLOT: MODEL-PREDICTED POST SCORES
################################################################################

p_expanded_emmeans_post <- plot_expanded_emmeans(expanded_emmeans_post, "post")

ggsave("../Figures/CourseCombo_Expanded_Emmeans_Post.png",
    p_expanded_emmeans_post,
    width = 12, height = 6, bg = "white"
)














################################################################################
############################ SESSION INFO ######################################
################################################################################

R.version.string
packageVersion("lme4")
packageVersion("lmerTest")
packageVersion("emmeans")