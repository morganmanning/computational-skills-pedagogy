# Computational Pedagogy in Wildlife Ecology

Code and survey data for a study on how undergraduate course pathway through R and quantitative coursework affects students' coding anxiety, perceived math skills, and perceived computing skills. Part of a dissertation chapter at the University of Florida.

## Study

Students in the wildlife ecology and conservation major take a sequence of courses with quantitative content: a foundational R class (WIS 4934), Quantitative Wildlife Ecology (WIS 4601), and Introduction to Wildlife Population Ecology (WIS 4501). The two advanced courses use R but assume students can write and adapt code on their own. The question is whether the foundational R class pays off in the advanced courses, and whether timing matters: does it help to take the R class first, alongside the advanced course, or is the advanced course on its own enough.

The analytic sample is $N = 50$ students sorted into four groups based on course pathway:

- *R Class only*
- *Advanced only*
- *R Class before advanced*
- *R Class concurrent with advanced*

Outcomes are pre/post change in coding anxiety, perceived math skills, and perceived computing skills. Open-ended responses cover how students would produce a graph, what challenges they have run into learning R, and what benefits they see in learning R.

## Repository layout

```
.
├── Data/
│   ├── DataJan2026.csv              # raw Qualtrics export, numeric
│   ├── DataJan2026_Text.csv         # raw Qualtrics export, labeled
│   ├── formatted_survey.csv         # cleaned and recoded
│   ├── essay_questions.csv          # open-ended responses
│   ├── essay_questions_coded.csv    # coded qualitative data
│   └── Pilot/                       # pilot data, not used in chapter
├── Scripts/
│   ├── dataFormattingAndSummaryPlots.R
│   ├── multiGroupAnalysis.r
│   ├── Group4Plotting.r
│   ├── modelSelection.r
│   └── final_class_classification.r
└── Figures/
    └── Tables/
```

## Running the analysis

Scripts assume the working directory is `Data/` and write outputs to `../Figures/` and `../Figures/Tables/`. Run them in this order:

1. `dataFormattingAndSummaryPlots.R`. Reads the raw Qualtrics CSVs, builds composite scores for each metric, classifies students into the group schemas, and writes summary tables.
2. `multiGroupAnalysis.r`. Group-based pre/post analyses across the 3-, 4-, and 5-group schemas, plus course-sequence and combo plots.
3. `Group4Plotting.r`. Main four-group results: raw means, emmeans, coefficient plots, and pairwise interaction contrasts (difference-in-differences with t-tests on the contrast estimates).
4. `modelSelection.r`. Block model comparisons. Each block adds a set of related covariates (motivation, prior preparation, resourcefulness, resource access, future application, demographics) on top of the base `Score ~ group_4 * Time` model. Comparisons use partial F-tests via `anova()` on nested `lm` objects.
5. `final_class_classification.r`. Last-survey-only analyses cross-tabulating final course reached against whether the student took the foundational R class.

Models are fit with base `lm()`. The analytic structure is one record per student per metric per time point, so partial F-tests on nested linear models do the nested-model comparison without random-effects machinery the data structure would not identify.

## Requirements

R (>= 4.2) with:

```
tidyverse, ggplot2, patchwork, emmeans, kableExtra,
mice, performance, broom.mixed, lme4, lmerTest
```

Tables are written via `kableExtra` to `.tex` (using `booktabs` and `threeparttable`) and `.png`. The LaTeX tables are pulled into the manuscript with `\input{}`.

## Qualitative data

Open-ended responses are in `essay_questions.csv`. The coded version (`essay_questions_coded.csv`) follows reflexive thematic analysis (Braun & Clarke 2006, 2022) using Robinson's (2022) Structured Tabular approach for short-answer data. Codes are attributed by participant ID and group label.

## Notes

- Group names are kept consistent across scripts and the manuscript: *R Class only*, *Advanced only*, *R Class before advanced*, *R Class concurrent with advanced*.
- Anxiety items are coded so that lower scores indicate improvement. Math and computing items are coded so that higher scores indicate higher perceived skill.
- The pilot data (`Data/Pilot/DataSep2025.csv`) is from an earlier round of the survey and is kept for reference but is not part of the chapter analyses.
