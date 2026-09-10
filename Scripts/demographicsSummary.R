# load in packages
library(tidyverse)
library(lubridate)

# 1. read the raw data
raw_names <- names(read_csv("Data/DataJan2026_Text.csv", n_max = 0, show_col_types = FALSE))
raw <- read_csv("Data/DataJan2026_Text.csv", skip = 3, col_names = raw_names,
                show_col_types = FALSE)

demo <- raw %>%
  select(pid = `Participant ID`, RecordedDate,
         Age, Gender, Race, Ethnicity, Major,
         `Academic standing`, `High school math`) %>%
  filter(!is.na(pid), pid != "") %>%
  mutate(Age = suppressWarnings(as.numeric(Age)),
         RecordedDate = mdy_hm(RecordedDate)) %>% 
  # one record per participant: keep the most recent 
  arrange(pid, desc(RecordedDate)) %>%
  distinct(pid, .keep_all = TRUE)

# 2. participant -> pathway crosswalk from the analytic sample 
groups <- read_csv("Data/formatted_survey_long.csv", show_col_types = FALSE) %>%
  distinct(pid = Participant.ID, group_4) %>%
  filter(group_4 != "None") %>%    
  mutate(pathway = recode(group_4,
           "Foundational only"                     = "Foundational only",
           "Advanced only"                         = "Advanced only",
           "Foundational before advanced"          = "Foundational before advanced",
           "Foundational concurrent with advanced" = "Foundational concurrent"),
         pathway = factor(pathway, levels = c(
           "Foundational only", "Advanced only",
           "Foundational before advanced", "Foundational concurrent")))

# 3. join demographics to the analytic sample 
df <- groups %>% left_join(demo, by = "pid")
df <- df %>%
  mutate(Major = case_when(
    str_detect(Major, regex("wildlife ecology|WEC|Human Dimensions", ignore_case = TRUE)) ~ "Wildlife Ecology and Conservation",
    str_detect(Major, regex("marine", ignore_case = TRUE)) ~ "Marine Sciences",
    str_detect(Major, regex("natural resource|NRC", ignore_case = TRUE)) ~ "Natural Resource Conservation",
    TRUE ~ Major))

cat("\n GROUP SIZES \n")
print(count(df, pathway, name = "n"))
cat("Total N =", nrow(df),
    "| missing demographic record:", sum(is.na(df$RecordedDate)), "\n")

# 4. summary tables 
cat_summary <- function(data, var) {
  data %>%
    mutate(level = replace_na(as.character(.data[[var]]), "(missing)")) %>%
    count(pathway, level, name = "n") %>%
    pivot_wider(names_from = pathway, values_from = n, values_fill = 0) %>%
    arrange(level)
}

cat("\n=== Gender ===\n"); print(cat_summary(df, "Gender"), 1000)
cat("\n=== Race (note: multi-select; some cells are comma-joined) ===\n")
print(cat_summary(df, "Race"), 1000)
cat("\n=== Ethnicity ===\n"); print(cat_summary(df, "Ethnicity"), 1000)
cat("\n=== Academic standing ===\n"); print(cat_summary(df, "Academic standing"), 1000)
cat("\n=== Major ===\n"); print(cat_summary(df, "Major"), 1000)
cat("\n=== High school offered advanced math (school-level availability) ===\n")
print(cat_summary(df, "High school math"), 1000)

cat("\n=== Age (years) ===\n")
df %>%
  group_by(pathway) %>%
  summarise(n = sum(!is.na(Age)),
            mean = round(mean(Age, na.rm = TRUE), 1),
            sd   = round(sd(Age,   na.rm = TRUE), 1),
            min  = suppressWarnings(min(Age, na.rm = TRUE)),
            max  = suppressWarnings(max(Age, na.rm = TRUE)),
            .groups = "drop") %>%
  print()
