library(haven)

raw_data <- read_dta("data/ExperimentalData.dta")
print(dim(raw_data))