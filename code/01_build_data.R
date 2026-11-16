# 01_build_data.R
# Goal: turn the raw survey file into variables we can analyze.
# Input:  data/ExperimentalData.dta  (from Armaly et al.'s Dataverse replication files)

library(haven)      # reads Stata (.dta) files
library(tidyverse)  # data cleaning tools

# STEP 1: load the data --------------------------------------------------
# zap_labels() removes Stata's value labels so every answer is a plain number
raw_data <- read_dta("data/ExperimentalData.dta") |>
  zap_labels()

# STEP 2: drop "preview" responses ---------------------------------------
# These are test responses from the research team, not real participants.
survey_data <- raw_data |>
  filter(distributionchannel != "preview")

stopifnot(nrow(survey_data) == 1136)   # stops the script if this is wrong

# STEP 3: Christian nationalism scale ------------------------------------
# Six items, each on a 1-5 agree scale. Item 3 asks about church-state
# separation, so agreeing means LESS Christian nationalism. Flipping it so
# that a high score means MORE Christian nationalism.
survey_data <- survey_data |>
  mutate(q196_3_flipped = 6 - q196_3)

cn_item_names <- c("q196_1", "q196_2", "q196_3_flipped",
                   "q196_4", "q196_5", "q196_6")

# Average the six items. na.rm = FALSE means anyone missing an item gets a
# missing score (mirroring Armaly et al. Stata code)
survey_data <- survey_data |>
  mutate(
    cn_average = rowMeans(across(all_of(cn_item_names)), na.rm = FALSE),
    # Rescale from the 1-5 range to 0-1 (mirroring Armaly et al. Stata code)
    christian_nationalism = (cn_average - 1) / 4
  )

print(summary(survey_data$christian_nationalism))
print(sum(is.na(survey_data$christian_nationalism)))

# STEP 4: outcome variables ----------------------------------------------
# In the raw data, 1 = "strongly agree" and 5 = "strongly disagree".
# Flipped and re-scaled so that 0 = strongly disagree and 1 = strongly agree.
# Higher values then mean MORE support for justices using that factor.
survey_data <- survey_data |>
  mutate(
    religious_support = (5 - q214_6) / 4,   # "using religious considerations"
    personal_support  = (5 - q214_7) / 4    # "using personal considerations"
  )

# STEP 5: experimental group ---------------------------------------------
# A person has a value in one of these columns only if they were shown that
# story
survey_data <- survey_data |>
  mutate(
    group = case_when(
      !is.na(q208_firstclick) ~ "control",   # read the bench-shape story
      !is.na(q204_firstclick) ~ "alito",     # read the Alito story
      TRUE                    ~ NA_character_
    ),
    # 0/1 version for the regression: 1 = saw the Alito story
    alito_story = case_when(
      group == "alito"   ~ 1,
      group == "control" ~ 0
    )
  )

# STEP 6: keep only control and Alito participants -------------------------
analysis_sample <- survey_data |>
  filter(!is.na(group))

# Safety checks (these should match the published paper)
stopifnot(nrow(analysis_sample) == 272)
stopifnot(sum(analysis_sample$alito_story == 0) == 133)
stopifnot(sum(analysis_sample$alito_story == 1) == 139)

print(table(analysis_sample$group))

print(summary(analysis_sample[, c("christian_nationalism",
                                  "religious_support",
                                  "personal_support",
                                  "alito_story")]))

# STEP 7: save for the next scripts --------------------------------------
saveRDS(analysis_sample, "data/analysis_sample.rds")
