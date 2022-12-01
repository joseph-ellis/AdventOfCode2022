
# ADVENT OF CODE - DAY 1

library(tidyverse)

input <- read_csv(
  file = "input.csv",
  skip_empty_rows = FALSE
)

# PART 1 - find the elf with the most calories

# FUNCTION - splits a vector into a list on each NA entry
split_vec_na <- function(.vector) {
  is_sep <- is.na(.vector)
  split(.vector[!is_sep], cumsum(is_sep)[!is_sep])
}

# split the input into a list using the function defined above
elf_list <- split_vec_na(input)

# generate a new list with the total calories for each elf
elf_totals <- map(
  .x = elf_list,
  .f = sum
)

# get the maximum number of calories in the list
elf_max <- max(unlist(elf_totals))


# PART 2 - find the top three elves with the most calories
elf_sorted <- elf_totals %>%
  unlist() %>%
  as_tibble %>%
  arrange(desc(value)) %>%
  mutate(csum = cumsum(value)) %>%
  slice_head(n = 3)
