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
    # Rescale from the 1-5 range to 0-1 (mirroring Armaly et al. State code)
    christian_nationalism = (cn_average - 1) / 4
  )

print(summary(survey_data$christian_nationalism))
print(sum(is.na(survey_data$christian_nationalism)))