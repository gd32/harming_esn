set.seed(3162023)

no_time_pressure = sample(21:50, 15)
sort(no_time_pressure)
time_pressure = setdiff(21:50, no_time_pressure)
sort(time_pressure)

library(tidyverse)

assignment = tibble(game = 21:50,
                    time_pressure_on = ifelse(game %in% time_pressure, 1, 0),
                    time_pressure_off = ifelse(game %in% no_time_pressure, 1, 0))

assignment
write_csv(assignment, file = "~/Documents/Projects/harming_esn/tp_assignment_exp4.csv")

