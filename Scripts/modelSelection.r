library(lme4)
library(lmerTest)
library(dplyr)
library(tidyr)
library(ggplot2)
library(kableExtra)
library(showtext)

font_add("Times New Roman", "C:/Windows/Fonts/times.ttf")
showtext_auto(FALSE)

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
    mutate(RecordedDate = as.POSIXct(RecordedDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
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


################################################################################
############################ SHARED AESTHETICS #################################
################################################################################

base_theme <- theme_bw(base_family = "Times New Roman") +
    theme(
        legend.position = "top",
        text = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 10),
        panel.grid.major = element_line(color = "gray85"),
        panel.grid.minor = element_line(color = "gray95"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "gray95")
    )

palette_4grp <- c(
    "Foundational only" = "#56B4E9",
    "Advanced only" = "#E69F00",
    "Foundational before advanced" = "#009E73",
    "Foundational concurrent with advanced" = "#CC79A7"
)

shape_4grp <- c(
    "Foundational only" = 16,
    "Advanced only" = 17,
    "Foundational before advanced" = 15,
    "Foundational concurrent with advanced" = 18
)

palette_model_type <- c("Block model" = "#E69F00", "Single model" = "#56B4E9")

palette_metric <- c(
    "Anxiety" = "#E69F00",
    "Math Skills" = "#56B4E9",
    "Computing" = "#009E73"
)

################################################################################
######################## COVARIATE BLOCKS ######################################
################################################################################

metrics <- c("Anxiety", "MathSkills", "Computing")

covariate_blocks <- list(
    Motivation = c("Perseverance", "Effort"),
    Prior_Preparation = c("High.school.particip", "Computer.savviness"),
    Resourcefulness = c("External.help_1", "External.help_2", "External.help_3"),
    Resource_Access = c("Computer.age", "Laptop.issues"),
    Future_Application = c("Dream.job", "Job.search"),
    Demographics = c("Gender", "Race", "Age")
)

all_covariates <- unlist(covariate_blocks, use.names = FALSE) %>% unique()

# reverse lookup: covariate name -> block name
covar_to_block <- stack(covariate_blocks) %>%
    rename(Covariate = values, Block = ind) %>%
    mutate(Block = as.character(Block))

# readable labels for covariates -- used in all plots and tables
covariate_labels <- c(
    "Perseverance" = "Perseverance",
    "Effort" = "Effort",
    "High.school.particip" = "Advanced math in high school",
    "Computer.savviness" = "Computer savviness",
    "External.help_1" = "Help from classmates",
    "External.help_2" = "Help from internet",
    "External.help_3" = "Help from R documentation",
    "Computer.age" = "Computer age",
    "Laptop.issues" = "Laptop issues",
    "Dream.job" = "Interest in a computational career",
    "Job.search" = "R skills helpful for job search",
    "Gender" = "Gender",
    "Race" = "Race",
    "Age" = "Age"
)

# look up clean label; fall back to raw name if not found
clean_covar_label <- function(x) {
    ifelse(x %in% names(covariate_labels), covariate_labels[x], x)
}


################################################################################
# MODEL FITTING
# all models: Score ~ group_4 * Time + covariate(s), lm, no random effects
################################################################################

block_results <- data.frame()
coefficient_results <- data.frame()
single_results <- data.frame()
single_fit_info <- data.frame()

for (metric in metrics) {
    cat("\n========================================\n")
    cat("Metric:", metric, "\n")
    cat("========================================\n")

    metric_data <- survey_long %>%
        filter(Outcome == metric) %>%
        filter(!is.na(Score), !is.na(group_4), !is.na(Time))

    # base model (no covariates); will be refit on each block's complete-case
    # subset for F-test; stored here for coefficient extraction only
    base_model_full <- lm(Score ~ group_4 * Time, data = metric_data)

    base_row <- data.frame(
        Metric = metric,
        Model = "0_Base",
        Block = "Base",
        Covariates = "(none)",
        N = nrow(metric_data),
        FTest_F = NA_real_,
        FTest_df1 = NA_integer_,
        FTest_df2 = NA_integer_,
        FTest_p = NA_real_,
        FTest_Sig = "",
        stringsAsFactors = FALSE
    )
    block_results <- rbind(block_results, base_row)

    ct <- summary(base_model_full)$coefficients
    coef_base <- data.frame(
        Metric = metric, Model = "0_Base", Block = "Base",
        Covariate = "(base)", Model_type = "block",
        Term = rownames(ct),
        Estimate = round(ct[, "Estimate"], 3),
        SE = round(ct[, "Std. Error"], 3),
        t_value = round(ct[, "t value"], 3),
        p_value = round(ct[, "Pr(>|t|)"], 4),
        Sig = case_when(
            ct[, "Pr(>|t|)"] < 0.001 ~ "***",
            ct[, "Pr(>|t|)"] < 0.01 ~ "**",
            ct[, "Pr(>|t|)"] < 0.05 ~ "*",
            ct[, "Pr(>|t|)"] < 0.1 ~ ".",
            TRUE ~ ""
        ),
        stringsAsFactors = FALSE
    )
    rownames(coef_base) <- NULL
    coefficient_results <- rbind(coefficient_results, coef_base)

    # --- block models ---
    for (block_name in names(covariate_blocks)) {
        block_covars <- covariate_blocks[[block_name]]
        covars_present <- block_covars[block_covars %in% names(metric_data)]
        if (length(covars_present) == 0) next

        block_data <- metric_data[complete.cases(metric_data[, covars_present, drop = FALSE]), ]
        block_data <- fix_ordered_contrasts(block_data)
        if (nrow(block_data) < 20) {
            cat("  Skipping block", block_name, "- N =", nrow(block_data), "\n")
            next
        }

        formula_str <- paste0("Score ~ group_4 * Time + ", paste(covars_present, collapse = " + "))
        model_name <- paste0(which(names(covariate_blocks) == block_name), "_", block_name)

        block_model <- tryCatch(
            lm(as.formula(formula_str), data = block_data),
            error = function(e) {
                message("Block model error: ", e$message)
                NULL
            }
        )
        if (is.null(block_model)) next

        # refit base model on the same complete-case subset for valid F-test
        base_on_block <- lm(Score ~ group_4 * Time, data = block_data)
        ftest <- anova(base_on_block, block_model)

        ftest_F <- round(ftest[2, "F"], 3)
        ftest_df1 <- ftest[2, "Df"]
        ftest_df2 <- ftest[2, "Res.Df"]
        ftest_p <- round(ftest[2, "Pr(>F)"], 4)
        ftest_sig <- case_when(
            ftest_p < 0.001 ~ "***",
            ftest_p < 0.01 ~ "**",
            ftest_p < 0.05 ~ "*",
            ftest_p < 0.1 ~ ".",
            TRUE ~ ""
        )

        cat(
            "  Block:", block_name, "| N =", nrow(block_data),
            "| F =", ftest_F, "| p =", ftest_p, ftest_sig, "\n"
        )

        fit_row <- data.frame(
            Metric = metric,
            Model = model_name,
            Block = block_name,
            Covariates = paste(covars_present, collapse = " + "),
            N = nrow(block_data),
            FTest_F = ftest_F,
            FTest_df1 = ftest_df1,
            FTest_df2 = ftest_df2,
            FTest_p = ftest_p,
            FTest_Sig = ftest_sig,
            stringsAsFactors = FALSE
        )
        block_results <- rbind(block_results, fit_row)

        ct <- summary(block_model)$coefficients
        coef_rows <- data.frame(
            Metric = metric, Model = model_name, Block = block_name,
            Covariate = "(block)", Model_type = "block",
            Term = rownames(ct),
            Estimate = round(ct[, "Estimate"], 3),
            SE = round(ct[, "Std. Error"], 3),
            t_value = round(ct[, "t value"], 3),
            p_value = round(ct[, "Pr(>|t|)"], 4),
            Sig = case_when(
                ct[, "Pr(>|t|)"] < 0.001 ~ "***",
                ct[, "Pr(>|t|)"] < 0.01 ~ "**",
                ct[, "Pr(>|t|)"] < 0.05 ~ "*",
                ct[, "Pr(>|t|)"] < 0.1 ~ ".",
                TRUE ~ ""
            ),
            stringsAsFactors = FALSE
        )
        rownames(coef_rows) <- NULL
        coefficient_results <- rbind(coefficient_results, coef_rows)
    }

    # --- single-covariate models ---
    for (covar in all_covariates) {
        if (!covar %in% names(metric_data)) next

        single_data <- metric_data[complete.cases(metric_data[, covar, drop = FALSE]), ]
        single_data <- fix_ordered_contrasts(single_data)
        if (nrow(single_data) < 20) next

        formula_str <- paste0("Score ~ group_4 * Time + ", covar)
        single_model <- tryCatch(
            lm(as.formula(formula_str), data = single_data),
            error = function(e) {
                message("Single model error: ", e$message)
                NULL
            }
        )
        if (is.null(single_model)) next

        # refit base on same data for F-test
        base_on_single <- lm(Score ~ group_4 * Time, data = single_data)
        ftest_s <- anova(base_on_single, single_model)
        ftest_p_s <- round(ftest_s[2, "Pr(>F)"], 4)
        ftest_sig_s <- case_when(
            ftest_p_s < 0.001 ~ "***",
            ftest_p_s < 0.01 ~ "**",
            ftest_p_s < 0.05 ~ "*",
            ftest_p_s < 0.1 ~ ".",
            TRUE ~ ""
        )

        info_row <- data.frame(
            Metric = metric,
            Covariate = covar,
            Block = covar_to_block$Block[covar_to_block$Covariate == covar][1],
            N = nrow(single_data),
            FTest_p = ftest_p_s,
            FTest_Sig = ftest_sig_s,
            stringsAsFactors = FALSE
        )
        single_fit_info <- rbind(single_fit_info, info_row)

        ct <- summary(single_model)$coefficients
        covar_terms <- rownames(ct)[grepl(covar, rownames(ct))]
        if (length(covar_terms) == 0) next

        for (term in covar_terms) {
            single_row <- data.frame(
                Metric = metric,
                Covariate = covar,
                Block = covar_to_block$Block[covar_to_block$Covariate == covar][1],
                Model_type = "single",
                Term = term,
                Estimate = round(ct[term, "Estimate"], 3),
                SE = round(ct[term, "Std. Error"], 3),
                t_value = round(ct[term, "t value"], 3),
                p_value = round(ct[term, "Pr(>|t|)"], 4),
                Sig = case_when(
                    ct[term, "Pr(>|t|)"] < 0.001 ~ "***",
                    ct[term, "Pr(>|t|)"] < 0.01 ~ "**",
                    ct[term, "Pr(>|t|)"] < 0.05 ~ "*",
                    ct[term, "Pr(>|t|)"] < 0.1 ~ ".",
                    TRUE ~ ""
                ),
                stringsAsFactors = FALSE
            )
            single_results <- rbind(single_results, single_row)
        }
    }
}

table(coefficient_results$Term[grepl("savviness", coefficient_results$Term)])

# rank block results by F-test p-value within each metric
block_results <- block_results %>%
    group_by(Metric) %>%
    arrange(FTest_p, .by_group = TRUE) %>%
    ungroup()

as.data.frame(block_results)


################################################################################
# COMPARE SINGLE VS BLOCK MODELS
################################################################################

block_coef_plot <- coefficient_results %>%
    filter(
        Block != "Base",
        Covariate == "(block)",
        !grepl("^\\(Intercept\\)|^group_4|^Time", Term)
    ) %>%
    mutate(
        Covariate = sapply(Term, function(t) {
            matched <- all_covariates[sapply(all_covariates, function(cv) grepl(cv, t, fixed = TRUE))]
            if (length(matched) == 0) NA_character_ else matched[1]
        }),
        Model_type = "Block model"
    ) %>%
    filter(!is.na(Covariate)) %>%
    dplyr::select(Metric, Block, Covariate, Term, Estimate, SE, p_value, Sig, Model_type)

single_coef_plot <- single_results %>%
    mutate(Model_type = "Single model") %>%
    dplyr::select(Metric, Block, Covariate, Term, Estimate, SE, p_value, Sig, Model_type)

forest_data <- rbind(block_coef_plot, single_coef_plot) %>%
    mutate(
        Metric = factor(Metric,
            levels = c("Anxiety", "MathSkills", "Computing"),
            labels = c("Anxiety", "Math Skills", "Computing")
        ),
        Model_type = factor(Model_type, levels = c("Block model", "Single model")),
        Term_clean = Term %>%
            gsub("\\.L$", " (linear)", .) %>%
            gsub("\\.Q$", " (quadratic)", .) %>%
            gsub(paste(all_covariates, collapse = "|"), "", .) %>%
            trimws(),
        Term_clean = ifelse(Term_clean == "", Covariate, paste0(Covariate, Term_clean)),
        Covar_label = clean_covar_label(Covariate),
        y_label = paste0("[", Block, "] ", Covar_label),
        Significant = ifelse(p_value < 0.05, "yes", "no")
    )


################################################################################
# PLOT: COMPARE SINGLE VS BLOCK MODELS
################################################################################

for (block_name in names(covariate_blocks)) {
    plot_data <- forest_data %>% filter(Block == block_name)
    if (nrow(plot_data) == 0) next

    p_forest <- ggplot(
        plot_data,
        aes(
            x = Estimate, y = y_label,
            color = Model_type, alpha = Significant
        )
    ) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
        geom_errorbarh(
            aes(xmin = Estimate - 1.96 * SE, xmax = Estimate + 1.96 * SE),
            height = 0.2, linewidth = 0.7,
            position = position_dodge(width = 0.5)
        ) +
        geom_point(size = 3, position = position_dodge(width = 0.5)) +
        facet_wrap(~Metric, scales = "free_x") +
        scale_color_manual(values = palette_model_type) +
        scale_alpha_manual(values = c("yes" = 1, "no" = 0.35), guide = "none") +
        labs(
            x = "Beta estimate (95% CI)",
            y = NULL,
            color = NULL,
            title = paste0("Covariate effects: ", block_name, " block")
        ) +
        base_theme

    ggsave(
        paste0("../Figures/BlockVsSingle_", block_name, ".png"),
        p_forest,
        width = 12, height = max(3, length(unique(plot_data$y_label)) * 0.5 + 2),
        bg = "white"
    )
    cat("Saved plot for block:", block_name, "\n")
}


################################################################################
# SIGNIFICANCE TABLE
################################################################################

# for each covariate x metric, pick the coefficient that best represents
# the dominant direction: prefer the linear trend (.L) if present, otherwise use the term with the smallest p-value
covar_direction <- coefficient_results %>%
    filter(Block != "Base", Covariate == "(block)") %>%
    mutate(
        Covar_matched = sapply(Term, function(t) {
            matched <- all_covariates[sapply(all_covariates, function(cv) grepl(cv, t, fixed = TRUE))]
            if (length(matched) == 0) NA_character_ else matched[1]
        })
    ) %>%
    filter(!is.na(Covar_matched)) %>%
    group_by(Metric, Covar_matched) %>%
    mutate(is_linear = grepl("\\.L$", Term)) %>%
    arrange(desc(is_linear), p_value) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(Direction = ifelse(Estimate > 0, "+", "-")) %>%
    dplyr::select(Metric, Covariate = Covar_matched, Direction)

sig_table_data <- single_fit_info %>%
    left_join(covar_direction, by = c("Metric", "Covariate")) %>%
    mutate(
        Sig_cell = ifelse(FTest_p < 0.05, Direction, ""),
        Sig_cell = ifelse(is.na(Sig_cell), "", Sig_cell),
        Covar_label = clean_covar_label(Covariate),
        Metric = factor(Metric,
            levels = c("Anxiety", "MathSkills", "Computing"),
            labels = c("Anxiety", "Math Skills", "Computing")
        )
    )

sig_wide <- sig_table_data %>%
    dplyr::select(Block, Covar_label, Metric, Sig_cell) %>%
    pivot_wider(names_from = Metric, values_from = Sig_cell, values_fill = "") %>%
    left_join(
        covar_to_block %>% mutate(Covar_label = clean_covar_label(Covariate)),
        by = c("Covar_label", "Block")
    ) %>%
    arrange(Block, Covar_label) %>%
    dplyr::select(Block, Covar_label, Anxiety, `Math Skills`, Computing)

as.data.frame(sig_wide)

sig_wide %>%
    kbl(
        format = "latex",
        booktabs = TRUE,
        col.names = c("Block", "Covariate", "Anxiety", "Math Skills", "Computing"),
        caption = "Covariates significant in single-covariate models (F-test vs. base model)"
    ) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(1, italic = TRUE) %>%
    collapse_rows(columns = 1, valign = "top", latex_hline = "major") %>%
    footnote(
        general = "+ / - = direction of dominant coefficient (linear trend preferred). F-test p < 0.05 vs. base model.",
        footnote_as_chunk = TRUE,
        threeparttable = TRUE
    ) %>%
    save_kable(file = "../Figures/Tables/covariate_significance_table.tex")


################################################################################
# MODEL COMPARISON TABLE
################################################################################

block_results %>%
    filter(Block != "Base") %>%
    mutate(
        Metric = factor(Metric,
            levels = c("Anxiety", "MathSkills", "Computing"),
            labels = c("Anxiety", "Math Skills", "Computing")
        ),
        FTest_p_fmt = ifelse(is.na(FTest_p), "", paste0(FTest_p, " ", FTest_Sig))
    ) %>%
    arrange(Metric, FTest_p) %>%
    dplyr::select(Metric, Block, Covariates, FTest_p, FTest_Sig) %>%
    kbl(
        format = "latex",
        booktabs = TRUE,
        col.names = c("Metric", "Block", "Covariates", "F-test p", ""),
        escape = FALSE
    ) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(1, italic = TRUE) %>%
    collapse_rows(columns = 1, valign = "top", latex_hline = "major") %>%
    footnote(
        general = "F-test comparing block model vs. base model. . p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001",
        footnote_as_chunk = TRUE,
        threeparttable = TRUE
    ) %>%
    save_kable(file = "../Figures/Tables/model_comparison_blocks.tex")


################################################################################
# BEST MODEL COEFFICIENTS TABLE
# best = lowest F-test p-value (most significant improvement over base)
################################################################################

best_models <- block_results %>%
    filter(Block != "Base", !is.na(FTest_p)) %>%
    group_by(Metric) %>%
    slice_min(FTest_p, n = 1, with_ties = FALSE) %>%
    ungroup()

as.data.frame(best_models)

clean_term <- function(term) {
    term %>%
        gsub("\\.L$", " (linear trend)", .) %>%
        gsub("\\.Q$", " (quadratic trend)", .) %>%
        gsub("Timepost", "Time: post", .) %>%
        gsub("High.school.participYes", "Advanced math in high school: Yes", .) %>%
        gsub("group_4Foundational only", "Foundational only", .) %>%
        gsub("group_4Foundational before advanced", "Foundational before advanced", .) %>%
        gsub("group_4Foundational concurrent with advanced", "Foundational concurrent with advanced", .) %>%
        gsub("Perseverance", "Perseverance", .) %>%
        gsub("Effort", "Effort required", .) %>%
        gsub("Computer.savviness", "Computer savviness", .) %>%
        gsub("External.help_1", "Help from classmates", .) %>%
        gsub("External.help_2", "Help from internet", .) %>%
        gsub("External.help_3", "Help from R documentation", .) %>%
        gsub("Computer.age", "Computer age", .) %>%
        gsub("Laptop.issues", "Laptop issues", .) %>%
        gsub("Dream.job", "Interest in computational career", .) %>%
        gsub("Job.search", "R skills for job search", .)
}

best_coef_combined <- data.frame()
for (metric in metrics) {
    best_name <- best_models %>%
        filter(Metric == metric) %>%
        pull(Model)
    rows <- coefficient_results %>%
        filter(Metric == metric, Model == best_name) %>%
        dplyr::select(Metric, Term, Estimate, SE, t_value, p_value, Sig) %>%
        mutate(
            Term = clean_term(Term),
            Metric = factor(Metric,
                levels = c("Anxiety", "MathSkills", "Computing"),
                labels = c("Anxiety", "Math Skills", "Computing")
            )
        )
    best_coef_combined <- rbind(best_coef_combined, rows)
}

# footnote: which block won per metric
best_block_footnote_coef <- best_models %>%
    mutate(Metric_label = case_when(
        Metric == "Anxiety" ~ "Anxiety",
        Metric == "MathSkills" ~ "Math Skills",
        Metric == "Computing" ~ "Computing"
    )) %>%
    mutate(s = paste0(Metric_label, ": ", Block, " (F = ", FTest_F, ", p = ", FTest_p, ")")) %>%
    pull(s) %>%
    paste(collapse = "; ")

best_coef_combined %>%
    kbl(
        format = "latex",
        booktabs = TRUE,
        col.names = c("Metric", "Term", "$\\beta$", "SE", "t", "p", ""),
        escape = FALSE
    ) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(1, italic = TRUE) %>%
    collapse_rows(columns = 1, valign = "top", latex_hline = "major") %>%
    footnote(
        general = paste0(
            "Reference level: Advanced only. Best block selected by lowest F-test p-value vs. base model. ",
            best_block_footnote_coef, ". . p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
        ),
        footnote_as_chunk = TRUE,
        threeparttable = TRUE
    ) %>%
    save_kable(file = "../Figures/Tables/best_model_coefficients.tex")


################################################################################
# LRT COMPARISON TABLE
################################################################################

block_ftest <- block_results %>%
    filter(Model != "0_Base") %>%
    dplyr::select(Metric, Block, FTest_p, FTest_Sig) %>%
    mutate(Model_type = "Block model", Label = Block)

single_ftest <- single_fit_info %>%
    mutate(Covar_label = clean_covar_label(Covariate)) %>%
    dplyr::select(Metric, Block, Covar_label, FTest_p, FTest_Sig) %>%
    mutate(Model_type = "Single model", Label = Covar_label) %>%
    dplyr::select(-Covar_label)

ftest_combined <- bind_rows(block_ftest, single_ftest) %>%
    mutate(
        Metric = factor(Metric,
            levels = c("Anxiety", "MathSkills", "Computing"),
            labels = c("Anxiety", "Math Skills", "Computing")
        ),
        Model_type = factor(Model_type, levels = c("Block model", "Single model")),
        Sig_flag = FTest_p < 0.05
    ) %>%
    arrange(Metric, FTest_p)

as.data.frame(ftest_combined)

ftest_combined %>%
    mutate(FTest_p_fmt = paste0(FTest_p, " ", FTest_Sig)) %>%
    dplyr::select(Metric, Model_type, Block, Label, FTest_p, FTest_Sig) %>%
    kbl(
        format = "latex",
        booktabs = TRUE,
        col.names = c("Metric", "Model type", "Block", "Model", "F-test p", ""),
        escape = FALSE
    ) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(1, italic = TRUE) %>%
    collapse_rows(columns = 1, valign = "top", latex_hline = "major") %>%
    footnote(
        general = "F-test vs. base model. Ranked by p-value within each metric. . p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001",
        footnote_as_chunk = TRUE,
        threeparttable = TRUE
    ) %>%
    save_kable(file = "../Figures/Tables/FTest_comparison_block_vs_single.tex")


################################################################################
# LIKERT PLOT: Job.search and Dream.job
################################################################################

job_search_labels <- c(
    "1" = "Not at all helpful",
    "2" = "Slightly helpful",
    "3" = "Moderately helpful",
    "4" = "Very helpful",
    "5" = "Extremely helpful"
)

dream_job_labels <- c(
    "1" = "Not interested",
    "2" = "Slightly interested",
    "3" = "Moderately interested",
    "4" = "Very interested",
    "5" = "Extremely interested"
)

likert_palette <- c(
    "1" = "#ccebc5",
    "2" = "#7fbf7b",
    "3" = "#41ab5d",
    "4" = "#006d2c",
    "5" = "#00441b"
)

js_summary <- survey %>%
    filter(!is.na(Job.search)) %>%
    count(Job.search, name = "n") %>%
    mutate(
        pct = n / sum(n),
        Response = as.character(Job.search),
        Question = "Do you think R coding skills will be\nhelpful in your future job search?"
    )

dj_summary <- survey %>%
    filter(!is.na(Dream.job)) %>%
    count(Dream.job, name = "n") %>%
    mutate(
        pct = n / sum(n),
        Response = as.character(Dream.job),
        Question = "How interested are you in a career\nthat requires computational skills?"
    )

likert_plot_data <- bind_rows(
    js_summary %>% mutate(Label = job_search_labels[Response]),
    dj_summary %>% mutate(Label = dream_job_labels[Response])
) %>%
    mutate(
        Question = factor(Question, levels = c(
            "Do you think R coding skills will be\nhelpful in your future job search?",
            "How interested are you in a career\nthat requires computational skills?"
        )),
        Response = factor(Response, levels = c("1", "2", "3", "4", "5")),
        pct_label = paste0(round(pct * 100), "%")
    )

p_likert <- ggplot(
    likert_plot_data,
    aes(x = Question, y = pct, fill = Response)
) +
    geom_col(position = "stack", width = 1, color = NA) +
    geom_text(
        aes(label = pct_label),
        position = position_stack(vjust = 0.5),
        color = "white",
        family = "Times New Roman",
        data = ~ filter(.x, pct > 0.04)
    ) +
    coord_flip() +
    scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        expand = expansion(add = c(0, 0.01))
    ) +
    scale_fill_manual(
        values = likert_palette,
        labels = c(
            "1" = "Not at all", "2" = "Slightly",
            "3" = "Moderately", "4" = "Very", "5" = "Extremely"
        ),
        name = NULL
    ) +
    labs(x = NULL, y = "Percentage of respondents") +
    theme_bw(base_family = "Times New Roman") +
    theme(
        legend.position = "right",
        text = element_text(family = "Times New Roman"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray95")
    ) +
    guides(fill = guide_legend(reverse = TRUE))

ggsave("../Figures/Likert_JobSearch_DreamJob.png",
    p_likert,
    width = 9, height = 4, bg = "white"
)


################################################################################
# EMMEANS PLOT: best covariate block model per metric (selected by F-test)
# Significance brackets: interaction contrasts (diff-in-diff, Sidak adjusted)
################################################################################

best_block_models <- list()
best_block_emmeans <- data.frame()
best_block_pairs <- data.frame()

for (metric in metrics) {
    best_row <- best_models %>% filter(Metric == metric)
    best_block <- best_row$Block
    best_covars <- best_row$Covariates

    cat("\n---", metric, "--- best block:", best_block, "\n")

    metric_data <- survey_long %>%
        filter(Outcome == metric) %>%
        filter(!is.na(Score), !is.na(group_4), !is.na(Time))

    covar_vec <- trimws(strsplit(best_covars, "\\+")[[1]])
    model_data <- metric_data[complete.cases(metric_data[, covar_vec, drop = FALSE]), ]
    model_data <- fix_ordered_contrasts(model_data)

    formula_str <- paste0("Score ~ group_4 * Time + ", paste(covar_vec, collapse = " + "))
    best_model <- tryCatch(
        lm(as.formula(formula_str), data = model_data),
        error = function(e) {
            message("Error: ", e$message)
            NULL
        }
    )
    if (is.null(best_model)) next
    best_block_models[[metric]] <- best_model

    em <- emmeans(best_model, ~ group_4 * Time) %>%
        as.data.frame() %>%
        mutate(Metric = metric, Best_block = best_block)
    best_block_emmeans <- rbind(best_block_emmeans, em)

    ic <- tryCatch(
        contrast(
            emmeans(best_model, ~ group_4 * Time),
            interaction = c(group_4 = "pairwise", Time = "consec"),
            adjust = "sidak"
        ) %>%
            as.data.frame() %>%
            mutate(
                Metric = metric,
                Sig = case_when(
                    p.value < 0.001 ~ "***",
                    p.value < 0.01 ~ "**",
                    p.value < 0.05 ~ "*",
                    TRUE ~ ""
                ),
                grp1 = trimws(sub(" - .*", "", group_4_pairwise)),
                grp2 = trimws(sub(".* - ", "", group_4_pairwise))
            ) %>%
            filter(p.value < 0.05),
        error = function(e) {
            message("Contrast error: ", e$message)
            data.frame()
        }
    )
    best_block_pairs <- rbind(best_block_pairs, ic)
}

# factor levels for plotting
best_block_emmeans <- best_block_emmeans %>%
    mutate(
        Time = factor(Time, levels = c("pre", "post")),
        Metric = factor(Metric,
            levels = c("Anxiety", "MathSkills", "Computing"),
            labels = c("Anxiety", "Math Skills", "Computing")
        ),
        group_4 = factor(group_4, levels = c(
            "Advanced only", "Foundational only",
            "Foundational before advanced", "Foundational concurrent with advanced"
        ))
    ) %>%
    filter(!is.na(group_4))

best_block_pairs <- best_block_pairs %>%
    mutate(Metric = factor(Metric,
        levels = c("Anxiety", "MathSkills", "Computing"),
        labels = c("Anxiety", "Math Skills", "Computing")
    ))

# sample sizes for group legend
group_n_counts <- survey %>%
    filter(group_4 != "None", !is.na(group_4)) %>%
    count(group_4, name = "n_students")

group_n <- group_n_counts %>%
    mutate(label = paste0(group_4, " (n = ", n_students, ")")) %>%
    dplyr::select(group_4, label) %>%
    deframe()

# caption footnote
best_block_footnote <- best_block_emmeans %>%
    distinct(Metric, Best_block) %>%
    left_join(
        best_models %>%
            mutate(Metric = factor(Metric,
                levels = c("Anxiety", "MathSkills", "Computing"),
                labels = c("Anxiety", "Math Skills", "Computing")
            )) %>%
            dplyr::select(Metric, Block, FTest_F, FTest_p, FTest_Sig),
        by = c("Metric", "Best_block" = "Block")
    ) %>%
    mutate(s = paste0(
        Metric, ": ", Best_block,
        " (F = ", FTest_F, ", p = ", FTest_p, FTest_Sig, ")"
    )) %>%
    pull(s) %>%
    paste(collapse = "; ")

# base plot
p_best_base <- ggplot(
    best_block_emmeans,
    aes(x = Time, y = emmean, color = group_4, group = group_4)
) +
    geom_line(position = position_dodge(width = 0.3), linewidth = 0.9) +
    geom_errorbar(
        aes(ymin = lower.CL, ymax = upper.CL),
        position = position_dodge(width = 0.3),
        width = 0.15, linewidth = 0.8
    ) +
    geom_point(
        aes(shape = group_4),
        position = position_dodge(width = 0.3), size = 3
    ) +
    facet_wrap(~Metric) +
    scale_color_manual(values = palette_4grp, labels = group_n) +
    scale_shape_manual(values = shape_4grp, labels = group_n) +
    scale_x_discrete(labels = c("pre" = "Pre", "post" = "Post")) +
    guides(color = guide_legend(ncol = 2), shape = guide_legend(ncol = 2)) +
    labs(
        x = "Time",
        y = "Model-predicted Mean Score (95% CI)",
        color = NULL,
        shape = NULL
        # caption = str_wrap(
        #     paste0(
        #         "Best covariate block (F-test): ", best_block_footnote,
        #         ". Reference level: Advanced only. Emmeans averaged over covariates at observed means."
        #     ),
        #     width = 145
        # )
    ) +
    base_theme +
    theme(plot.caption = element_text(hjust = 0, face = "italic"))

# extract actual dodged x positions for bracket placement
best_block_bracket_coords <- data.frame()

if (nrow(best_block_pairs) > 0) {
    pb <- ggplot_build(p_best_base)
    x_at_post <- pb$data[[3]] %>%
        filter(round(x) == 2) %>%
        group_by(colour) %>%
        summarise(x = mean(x), .groups = "drop") %>%
        left_join(
            tibble(colour = unname(palette_4grp), group_4 = names(palette_4grp)),
            by = "colour"
        ) %>%
        filter(!is.na(group_4)) %>%
        {
            setNames(.$x, .$group_4)
        }

    cat("\nPost x positions:\n")
    print(x_at_post)

    em_y_ceil <- best_block_emmeans %>%
        group_by(Metric) %>%
        summarise(y_max = max(upper.CL, na.rm = TRUE), .groups = "drop")

    best_block_bracket_coords <- best_block_pairs %>%
        mutate(
            x1 = x_at_post[grp1],
            x2 = x_at_post[grp2],
            span = abs(x2 - x1)
        ) %>%
        filter(!is.na(x1), !is.na(x2)) %>%
        left_join(em_y_ceil, by = "Metric") %>%
        group_by(Metric) %>%
        arrange(span, .by_group = TRUE) %>%
        mutate(
            bracket_rank = row_number(),
            y_bracket = y_max + 0.2 + (bracket_rank - 1) * 0.35,
            xmid = (x1 + x2) / 2,
            tick_len = 0.08
        ) %>%
        ungroup()
}

em_y_top <- if (nrow(best_block_bracket_coords) > 0) {
    max(best_block_bracket_coords$y_bracket) + 0.3
} else {
    max(best_block_emmeans$upper.CL, na.rm = TRUE) + 0.2
}

p_best_block_emmeans <- p_best_base +
    coord_cartesian(ylim = c(
        min(best_block_emmeans$lower.CL, na.rm = TRUE) - 0.1,
        em_y_top
    ))

if (nrow(best_block_bracket_coords) > 0) {
    p_best_block_emmeans <- p_best_block_emmeans +
        geom_segment(
            data = best_block_bracket_coords,
            aes(x = x1, xend = x2, y = y_bracket, yend = y_bracket),
            inherit.aes = FALSE, color = "black", linewidth = 0.5
        ) +
        geom_segment(
            data = best_block_bracket_coords,
            aes(x = x1, xend = x1, y = y_bracket, yend = y_bracket - tick_len),
            inherit.aes = FALSE, color = "black", linewidth = 0.5
        ) +
        geom_segment(
            data = best_block_bracket_coords,
            aes(x = x2, xend = x2, y = y_bracket, yend = y_bracket - tick_len),
            inherit.aes = FALSE, color = "black", linewidth = 0.5
        ) +
        geom_text(
            data = best_block_bracket_coords,
            aes(x = xmid, y = y_bracket + 0.08, label = Sig),
            inherit.aes = FALSE, color = "black"
        )
}

ggsave("../Figures/BestBlock_Emmeans_PrePost.png",
    p_best_block_emmeans,
    width = 12, height = 6, bg = "white"
)


################################################################################
# COEFFICIENT PLOT: significant single-covariate model betas
################################################################################

single_coef_plot_data <- single_results %>%
    filter(p_value < 0.05) %>%
    mutate(
        Metric = factor(Metric,
            levels = c("Anxiety", "MathSkills", "Computing"),
            labels = c("Anxiety", "Math Skills", "Computing")
        ),
        LCL = Estimate - 1.96 * SE,
        UCL = Estimate + 1.96 * SE
    )

single_coef_plot_data <- single_coef_plot_data %>%
    mutate(
        Term_suffix = mapply(function(term, covar) {
            suffix <- sub(paste0("^", covar), "", term)
            covar_clean <- clean_covar_label(covar)
            if (suffix == "") covar_clean else paste0(covar_clean, ": ", suffix)
        }, Term, Covariate),
        Term_suffix = trimws(Term_suffix)
    )

term_order <- single_coef_plot_data %>%
    distinct(Term_suffix, Covariate) %>%
    mutate(covar_order = match(Covariate, all_covariates)) %>%
    arrange(covar_order, Term_suffix) %>%
    pull(Term_suffix) %>%
    unique() %>%
    rev()

single_coef_plot_data <- single_coef_plot_data %>%
    mutate(Term_suffix = factor(Term_suffix, levels = term_order))

p_single_coef <- ggplot(
    single_coef_plot_data,
    aes(x = Estimate, y = Term_suffix)
) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(
        aes(xmin = LCL, xmax = UCL),
        height = 0.25, linewidth = 0.8, color = "#56B4E9"
    ) +
    geom_point(size = 3, color = "#56B4E9") +
    facet_wrap(~Metric, scales = "free_x") +
    labs(
        x = "Estimate \u03b2 (95% CI)",
        y = NULL
    ) +
    base_theme

ggsave("../Figures/SingleCovariate_SigCoefs.png",
    p_single_coef,
    width = 12, height = 6, bg = "white"
)


################################################################################
# COEFFICIENT PLOT: best block model betas per metric
################################################################################

block_coef_plot_data <- data.frame()

for (metric in metrics) {
    best_name <- best_models %>%
        filter(Metric == metric) %>%
        pull(Model)

    rows <- coefficient_results %>%
        filter(Metric == metric, Model == best_name) %>%
        distinct(Metric, Term, .keep_all = TRUE) %>%
        mutate(
            LCL = Estimate - 1.96 * SE,
            UCL = Estimate + 1.96 * SE,
            Significant = ifelse(p_value < 0.05, "yes", "no"),
            # clean labels to match Group4Plotting style
            Term_clean = Term %>%
                gsub(":Timepost", " \u00d7 Post", .) %>%
                gsub("group_4", "", .) %>%
                gsub("^Timepost$", "Post", .) %>%
                gsub("\\.L$", " (L)", .) %>%
                gsub("\\.Q$", " (Q)", .) %>%
                gsub("High.school.participYes", "Adv. math in HS: Yes", .) %>%
                gsub("Computer.savviness", "Computer savviness", .) %>%
                gsub("External.help_1", "Help: classmates", .) %>%
                gsub("External.help_2", "Help: internet", .) %>%
                gsub("External.help_3", "Help: R docs", .) %>%
                gsub("Computer.age", "Computer age", .) %>%
                gsub("Laptop.issues", "Laptop issues", .) %>%
                gsub("Dream.job", "Computational career", .) %>%
                gsub("Job.search", "R for job search", .) %>%
                gsub("Perseverance", "Perseverance", .) %>%
                gsub("Effort", "Effort", .) %>%
                gsub("Gender", "Gender: ", .) %>%
                gsub("Race", "Race: ", .) %>%
                gsub("\\(Intercept\\)", "Intercept", .) %>%
                trimws(),
            Term_clean = str_wrap(Term_clean, width = 25),
            Metric = factor(Metric,
                levels = c("Anxiety", "MathSkills", "Computing"),
                labels = c("Anxiety", "Math Skills", "Computing")
            )
        )
    block_coef_plot_data <- rbind(block_coef_plot_data, rows)
}

block_coef_plot_data <- block_coef_plot_data %>%
    group_by(Metric) %>%
    mutate(term_order = row_number()) %>%
    ungroup() %>%
    mutate(Term_clean = reorder(Term_clean, -term_order))

# wrap the caption to the figure width so it doesn't run off the right edge
coef_caption <- str_wrap(
    paste0(
        "Best covariate block (F-test): ", best_block_footnote_coef,
        ". Reference level: Advanced only, pre-time. L = linear trend, Q = quadratic trend."
    ),
    width = 135
)

# scale figure height to the densest panel so y-axis labels don't overlap
# (Demographics block has the most terms; ~0.45 in per row gives readable spacing)
n_terms_max <- block_coef_plot_data %>%
    dplyr::count(Metric) %>%
    dplyr::pull(n) %>%
    max()
coef_height <- max(7, n_terms_max * 0.45 + 2)

p_block_coef <- ggplot(
    block_coef_plot_data,
    aes(x = Estimate, y = Term_clean, color = Metric, alpha = Significant)
) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(
        aes(xmin = LCL, xmax = UCL),
        height = 0.25, linewidth = 0.8
    ) +
    geom_point(size = 3) +
    facet_wrap(~Metric, scales = "free") +
    scale_color_manual(values = palette_metric, guide = "none") +
    scale_alpha_manual(
        values = c("yes" = 1, "no" = 0.35),
        guide  = "none"
    ) +
    labs(
        x = "Estimate \u03b2 (95% CI)",
        y = NULL
        #caption = coef_caption
    ) +
    base_theme +
    theme(plot.caption = element_text(hjust = 0, face = "italic"))

ggsave("../Figures/CoefPlot_BestBlock.png",
    p_block_coef,
    width = 12, height = 9, bg = "white"
)

################################################################################
############################ RAW KEY RESULTS ###################################
################################################################################

cat("\n\n========== BLOCK RESULTS ==========\n")
as.data.frame(block_results)

cat("\n\n========== BEST MODELS ==========\n")
as.data.frame(best_models)

cat("\n\n========== SINGLE FIT INFO ==========\n")
as.data.frame(single_fit_info)

cat("\n\n========== SINGLE COVARIATE RESULTS ==========\n")
as.data.frame(single_results)

cat("\n\n========== LRT COMBINED ==========\n")
as.data.frame(ftest_combined)

cat("\n\n========== COVARIATE DIRECTION TABLE ==========\n")
as.data.frame(sig_wide)