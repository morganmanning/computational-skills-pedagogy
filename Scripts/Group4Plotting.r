
################################################################################
############################ LOAD PACKAGES #####################################
################################################################################

library(tidyverse)
library(dplyr)
library(emmeans)
library(ggplot2)
library(kableExtra)
library(showtext)

#setwd("Data")
rm(list = ls())

################################################################################
############################ SHARED AESTHETICS ################################
################################################################################

font_add("Times New Roman", "C:/Windows/Fonts/times.ttf")
showtext_auto(FALSE) # girl idk

base_theme <- theme_bw(base_family = "Times New Roman") +
    theme(
        legend.position = "top",
        text = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 10),
        panel.grid.major = element_line(color = "gray85"),
        panel.grid.minor = element_line(color = "gray95"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "gray95")
    )


# keeping this consistent with the palette_4 colors used elsewhere
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

palette_metric <- c(
    "Anxiety" = "#E69F00",
    "Math Skills" = "#56B4E9",
    "Computing" = "#009E73"
)


################################################################################
# DATA PREP
################################################################################

survey <- read.csv("formatted_survey.csv")

survey$Gender <- factor(survey$Gender)
survey$Race <- factor(survey$Race)
survey$Age <- as.numeric(survey$Age)
survey[survey == ""] <- NA

# only one per particip
survey <- survey %>%
    mutate(RecordedDate = as.POSIXct(RecordedDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
    group_by(Participant.ID) %>%
    slice_max(order_by = RecordedDate, n = 1, with_ties = FALSE) %>%
    ungroup()

################################################################################
# PIVOT LONG
################################################################################

survey_long <- survey %>%
    dplyr::select(
        Participant.ID, group_4,
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
            labels = c("Anxiety", "Math Skills", "Computing")
        )
    ) %>%
    filter(group_4 != "None", !is.na(group_4)) # drop unclassified rows

# reflect anxiety onto the 1-5 scale so higher = better, matching the other metrics
survey_long <- survey_long %>%
    mutate(Score = ifelse(Metric == "Anxiety", 6 - Score, Score))


################################################################################
# SAMPLE SIZES
################################################################################

group_n_counts <- survey %>%
    filter(group_4 != "None", !is.na(group_4)) %>%
    count(group_4, name = "n_students")

# named vector for scale_color_manual / scale_shape_manual labels
group_n <- group_n_counts %>%
    mutate(label = paste0(group_4, " (n = ", n_students, ")")) %>%
    dplyr::select(group_4, label) %>%
    deframe()


################################################################################
# LINEAR MODELS
# Score ~ group_4 * Time 
################################################################################

metrics_plot <- c("Anxiety", "Math Skills", "Computing")
lm_models <- list()
lm_coefs <- tibble()
emmeans_results <- tibble()
pairwise_results <- tibble()

for (metric in metrics_plot) {
    cat("\n----------------------------------------\n")
    cat(metric, "\n")
    cat("----------------------------------------\n")

    mdata <- survey_long %>%
        filter(Metric == metric) %>%
        drop_na(Score, group_4, Time)

    model <- lm(Score ~ group_4 * Time, data = mdata)
    lm_models[[metric]] <- model

    cat("  n observations:", nrow(mdata), "\n")
    print(summary(model)$coefficients)

    # pull coefficients into a tidy frame for later use
    ct <- summary(model)$coefficients
    coef_df <- data.frame(
        Metric = metric,
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
        LCL = round(ct[, "Estimate"] - 1.96 * ct[, "Std. Error"], 3),
        UCL = round(ct[, "Estimate"] + 1.96 * ct[, "Std. Error"], 3),
        stringsAsFactors = FALSE
    )
    rownames(coef_df) <- NULL
    lm_coefs <- bind_rows(lm_coefs, coef_df)

    # emmeans: model-predicted means at each group x time combination
    em <- emmeans(model, ~ group_4 * Time) %>%
        as.data.frame() %>%
        mutate(Metric = metric)
    emmeans_results <- bind_rows(emmeans_results, em)

    # tukey-adjusted pairwise comparisons between groups, collapsing over time
    pairs_df <- pairs(emmeans(model, ~group_4), adjust = "tukey") %>%
        as.data.frame() %>%
        mutate(
            Metric = metric,
            Sig = case_when(
                p.value < 0.001 ~ "***",
                p.value < 0.01 ~ "**",
                p.value < 0.05 ~ "*",
                p.value < 0.1 ~ ".",
                TRUE ~ ""
            )
        )
    pairwise_results <- bind_rows(pairwise_results, pairs_df)

    cat("\n  Tukey pairwise comparisons between groups:\n")
    print(as.data.frame(pairs_df))
}

# check the full coefficient table
as.data.frame(lm_coefs)

# save it
lm_coefs %>%
    mutate(
        CI = paste0("[", LCL, ", ", UCL, "]"),
        Term = gsub("group_4", "", Term) %>%
            gsub(":Timepost", " $\\\\times$ Post", .) %>%
            trimws()
    ) %>%
    dplyr::select(Metric, Term, Estimate, CI, t_value, p_value, Sig) %>%
    kbl(
        format    = "latex",
        booktabs  = TRUE,
        col.names = c("Metric", "Term", "$\\beta$", "95\\% CI", "t", "p", ""),
        align     = c("l", "l", "r", "r", "r", "r", "l"),
        escape    = FALSE
    ) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(1, italic = TRUE) %>%
    collapse_rows(columns = 1, valign = "top", latex_hline = "major") %>%
    footnote(
        general = "Reference level: Advanced only / Pre. . p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001",
        footnote_as_chunk = TRUE,
        threeparttable = TRUE
    ) %>%
    save_kable(file = "../Figures/Tables/4group_lm_coefs.tex")

# factor levels for plotting
emmeans_results <- emmeans_results %>%
    mutate(
        Time = factor(Time, levels = c("pre", "post")),
        Metric = factor(Metric, levels = c("Anxiety", "Math Skills", "Computing")),
        group_4 = factor(group_4, levels = c(
            "Advanced only",
            "Foundational only",
            "Foundational before advanced",
            "Foundational concurrent with advanced"
        ))
    )
emmeans_results <- emmeans_results[emmeans_results$group_4 != "None" & !is.na(emmeans_results$group_4), ] # drop unclassified rows


################################################################################
# RAW SCORE SUMMARIES
################################################################################

raw_summary <- survey_long %>%
    group_by(group_4, Time, Metric) %>%
    summarise(
        mean = mean(Score, na.rm = TRUE),
        sd = sd(Score, na.rm = TRUE),
        n = n(),
        se = sd / sqrt(n),
        ci_lower = mean - qt(0.975, n - 1) * se,
        ci_upper = mean + qt(0.975, n - 1) * se,
        .groups = "drop"
    )
raw_summary <- raw_summary[raw_summary$group_4 != "None", ] # drop unclassified rows

################################################################################
# PLOT 1: RAW MEANS
################################################################################

p_raw <- ggplot(
    raw_summary,
    aes(x = Time, y = mean, color = group_4, group = group_4)
) +
    geom_line(position = position_dodge(width = 0.3), linewidth = 0.9) +
    geom_errorbar(
        aes(ymin = ci_lower, ymax = ci_upper),
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
    guides(
        color = guide_legend(ncol = 2),
        shape = guide_legend(ncol = 2)
    ) +
    labs(
        x = "Time",
        y = "Mean Score (95% CI)",
        color = NULL,
        shape = NULL
    ) +
    base_theme

ggsave("../Figures/4Group_RawMeans_PrePost.png",
    p_raw,
    width = 11, height = 6, bg = "white"
)


################################################################################
# PLOT 2: EMMEANS: model-predicted means, same layout as Plot 1
################################################################################
emmeans_results <- emmeans_results %>%
    mutate(
        Metric = factor(
            Metric,
            levels = c("Anxiety", "Math Skills", "Computing")
        )
    )

p_emmeans <- ggplot(
    emmeans_results,
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
    guides(
        color = guide_legend(ncol = 2),
        shape = guide_legend(ncol = 2)
    ) +
    labs(
        x = "Time",
        y = "Model-predicted Mean Score (95% CI)",
        color = NULL,
        shape = NULL
    ) +
    base_theme

ggsave("../Figures/4Group_Emmeans_PrePost.png",
    p_emmeans,
    width = 11, height = 6, bg = "white"
)

################################################################################
# CHANGE SCORE SUMMARIES
################################################################################

# calculate change score per student per metric first, then summarize
change_summary <- survey_long %>%
    dplyr::select(Participant.ID, group_4, Metric, Time, Score) %>%
    filter(group_4 != "None", !is.na(group_4)) %>%
    group_by(Participant.ID, group_4, Metric, Time) %>%
    summarise(Score = mean(Score, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Time, values_from = Score) %>%
    mutate(change = post - pre) %>%
    filter(!is.na(change)) %>%
    group_by(group_4, Metric) %>%
    summarise(
        mean = mean(change, na.rm = TRUE),
        sd = sd(change, na.rm = TRUE),
        n = n(),
        se = sd / sqrt(n),
        ci_lower = mean - qt(0.975, n - 1) * se,
        ci_upper = mean + qt(0.975, n - 1) * se,
        .groups = "drop"
    )


################################################################################
# PLOT 3: CHANGE SCORES
################################################################################

p_change <- ggplot(
    change_summary,
    aes(x = group_4, y = mean, color = group_4, shape = group_4)
) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(
        aes(ymin = ci_lower, ymax = ci_upper),
        width = 0.15, linewidth = 0.8
    ) +
    geom_point(size = 3.5) +
    facet_wrap(~Metric) +
    scale_color_manual(values = palette_4grp, labels = group_n) +
    scale_shape_manual(values = shape_4grp,   labels = group_n) +
    scale_x_discrete(labels = group_n) +
    guides(
        color = guide_legend(ncol = 2),
        shape = guide_legend(ncol = 2)
    ) +
    labs(
        x = NULL,
        y = "Mean Change Score, Post \u2212 Pre (95% CI)",
        color = NULL,
        shape = NULL
    ) +
    base_theme +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())

ggsave("../Figures/4Group_ChangeScores.png",
       p_change, width = 11, height = 6, bg = "white")



################################################################################
# PAIRWISE INTERACTION CONTRASTS: which groups differ in pre-post change?
################################################################################

pairs_interaction <- tibble()
full_interaction <- tibble()

for (metric in metrics_plot) {
    model <- lm_models[[metric]]
    if (is.null(model)) next

    em <- emmeans(model, ~ group_4 * Time)

    # interaction contrast: pairwise across groups x consecutive across time (post - pre)
    # estimate = (post-pre) for group_i minus (post-pre) for group_j
    ic <- contrast(em, interaction = c(group_4 = "pairwise", Time = "consec", adjust = "tukey")) %>%
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
        filter(p.value < 0.05)
    
    full <- contrast(em, interaction = c(group_4 = "pairwise", Time = "consec", adjust = "tukey")) %>%
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
        )

    pairs_interaction <- bind_rows(pairs_interaction, ic)
    full_interaction <- bind_rows(full_interaction, full)
}

################################################################################
# BRACKET COORDINATES: RAW MEANS AND EMMEANS PLOTS
################################################################################

group_levels <- c(
    "Advanced only",
    "Foundational only",
    "Foundational before advanced",
    "Foundational concurrent with advanced"
)

# mimic position_dodge(width = 0.3)
dodge_width <- 0.3
offsets <- seq(
    -dodge_width / 2,
    dodge_width / 2,
    length.out = length(group_levels)
)

x_at_post_em <- setNames(2 + offsets, group_levels)
x_at_post_raw <- setNames(2 + offsets, group_levels)

print(x_at_post_em)

# helper: return named vector group -> x
deframe_xy <- function(df) {
    setNames(df$x, df$group_4)
}

x_at_post_raw <- get_post_x(p_raw, palette_4grp)
x_at_post_em <- get_post_x(p_emmeans, palette_4grp)

cat("Raw post x positions:\n")
print(x_at_post_raw)
cat("Emmeans post x positions:\n")
print(x_at_post_em)

raw_bracket_coords <- data.frame()
em_bracket_coords <- data.frame()

if (nrow(pairs_interaction) > 0) {
    raw_y_ceil <- raw_summary %>%
        group_by(Metric) %>%
        summarise(y_max = max(ci_upper, na.rm = TRUE), .groups = "drop")

    em_y_ceil <- emmeans_results %>%
        group_by(Metric) %>%
        summarise(y_max = max(upper.CL, na.rm = TRUE), .groups = "drop")

    raw_bracket_coords <- pairs_interaction %>%
        mutate(
            x1   = x_at_post_raw[grp1],
            x2   = x_at_post_raw[grp2],
            span = abs(x2 - x1)
        ) %>%
        filter(!is.na(x1), !is.na(x2)) %>%
        left_join(raw_y_ceil, by = "Metric") %>%
        group_by(Metric) %>%
        arrange(span, .by_group = TRUE) %>%
        mutate(
            bracket_rank = row_number(),
            y_bracket    = y_max + 0.2 + (bracket_rank - 1) * 0.35,
            xmid         = (x1 + x2) / 2,
            tick_len     = 0.08
        ) %>%
        ungroup()

    em_bracket_coords <- pairs_interaction %>%
        mutate(
            x1   = x_at_post_em[grp1],
            x2   = x_at_post_em[grp2],
            span = abs(x2 - x1)
        ) %>%
        filter(!is.na(x1), !is.na(x2)) %>%
        left_join(em_y_ceil, by = "Metric") %>%
        group_by(Metric) %>%
        arrange(span, .by_group = TRUE) %>%
        mutate(
            bracket_rank = row_number(),
            y_bracket    = y_max + 0.2 + (bracket_rank - 1) * 0.35,
            xmid         = (x1 + x2) / 2,
            tick_len     = 0.08
        ) %>%
        ungroup()
}

raw_y_top <- if (nrow(raw_bracket_coords) > 0) {
    max(raw_bracket_coords$y_bracket) + 0.3
} else {
    max(raw_summary$ci_upper, na.rm = TRUE) + 0.2
}

em_y_top <- if (nrow(em_bracket_coords) > 0) {
    max(em_bracket_coords$y_bracket) + 0.3
} else {
    max(emmeans_results$upper.CL, na.rm = TRUE) + 0.2
}


################################################################################
# BRACKET LAYER HELPER
################################################################################

bracket_layers <- function(coords) {
    list(
        geom_segment(
            data = coords,
            aes(x = x1, xend = x2, y = y_bracket, yend = y_bracket),
            inherit.aes = FALSE, color = "black", linewidth = 0.5
        ),
        geom_segment(
            data = coords,
            aes(x = x1, xend = x1, y = y_bracket, yend = y_bracket - tick_len),
            inherit.aes = FALSE, color = "black", linewidth = 0.5
        ),
        geom_segment(
            data = coords,
            aes(x = x2, xend = x2, y = y_bracket, yend = y_bracket - tick_len),
            inherit.aes = FALSE, color = "black", linewidth = 0.5
        ),
        geom_text(
            data = coords,
            aes(x = xmid, y = y_bracket + 0.08, label = Sig),
            inherit.aes = FALSE, color = "black", size = 4
        )
    )
}


################################################################################
# PLOT 1b: RAW MEANS WITH SIGNIFICANCE BRACKETS
################################################################################

p_raw_brackets <- p_raw +
    coord_cartesian(ylim = c(
        min(raw_summary$ci_lower, na.rm = TRUE) - 0.1,
        raw_y_top
    ))

if (nrow(raw_bracket_coords) > 0) {
    p_raw_brackets <- p_raw_brackets + bracket_layers(raw_bracket_coords)
}

ggsave("../Figures/4Group_RawMeans_PrePost_Brackets.png",
    p_raw_brackets,
    width = 11, height = 6, bg = "white"
)


################################################################################
# PLOT 2b: EMMEANS WITH SIGNIFICANCE BRACKETS
################################################################################

p_emmeans_brackets <- p_emmeans +
    coord_cartesian(ylim = c(
        min(emmeans_results$lower.CL, na.rm = TRUE) - 0.1,
        em_y_top
    ))

if (nrow(em_bracket_coords) > 0) {
    p_emmeans_brackets <- p_emmeans_brackets + bracket_layers(em_bracket_coords)
}

ggsave("../Figures/4Group_Emmeans_PrePost_Brackets.png",
    p_emmeans_brackets,
    width = 11, height = 6, bg = "white"
)


################################################################################
# COEFFICIENT PLOT: 4-group lm fixed effects
################################################################################

coef_plot_data <- lm_coefs %>%
    filter(!grepl("Intercept", Term)) %>%
    mutate(
        Term = gsub("group_4", "", Term) %>%
            gsub(":Timepost", " \u00d7 Post", .) %>%
            gsub("Timepost", "Post", .) %>%
            trimws(),
        Significant = ifelse(p_value < 0.05, "yes", "no"),
        Metric = factor(Metric, levels = c("Anxiety", "Math Skills", "Computing"))
    )

# preserve table order: reverse it so the first term in the table sits at top
term_order <- coef_plot_data %>%
    distinct(Term) %>%
    mutate(Term = factor(Term, levels = rev(unique(Term))))

coef_plot_data <- coef_plot_data %>%
    mutate(Term = factor(Term, levels = levels(term_order$Term)))

p_coef <- ggplot(
    coef_plot_data,
    aes(x = Estimate, y = Term, color = Metric, alpha = Significant)
) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(
        aes(xmin = LCL, xmax = UCL),
        height = 0.25, linewidth = 0.8
    ) +
    geom_point(size = 3) +
    facet_wrap(~Metric, scales = "free_x") +
    scale_color_manual(values = palette_metric, guide = "none") +
    scale_alpha_manual(
        values = c("yes" = 1, "no" = 0.35),
        guide  = "none"
    ) +
    labs(
        x = "Estimate \u03b2 (reference: Advanced only, pre-time)",
        y = NULL
    ) +
    base_theme

ggsave("../Figures/CoefPlot_4group_base.png",
    p_coef,
    width = 12, height = 6, bg = "white"
)



################################################################################
############################ RAW KEY RESULTS ###################################
################################################################################

cat("\n\n========== LM COEFFICIENTS ==========\n")
as.data.frame(lm_coefs)

cat("\n\n========== EMMEANS ==========\n")
as.data.frame(emmeans_results)

cat("\n\n========== PAIRWISE (TUKEY) ==========\n")
as.data.frame(pairwise_results)

cat("\n\n========== INTERACTION CONTRASTS (SIG ONLY) ==========\n")
as.data.frame(pairs_interaction)

cat("\n\n========== INTERACTION CONTRASTS (ALL) ==========\n")
as.data.frame(full_interaction)

cat("\n\n========== RAW SUMMARIES ==========\n")
as.data.frame(raw_summary)

cat("\n\n========== CHANGE SUMMARIES ==========\n")
as.data.frame(change_summary)


################################################################################
############################ SESSION INFO ######################################
################################################################################

R.version.string
packageVersion("emmeans")
