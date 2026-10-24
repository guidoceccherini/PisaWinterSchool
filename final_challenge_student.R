# =====================================================================
#  FINAL CHALLENGE
#  "From raw field data to a publication-ready figure"
#  Introduction to R: tidyverse & ggplot2 — Pisa Summer School
#  Guido Ceccherini & Federico Gianoli
# ---------------------------------------------------------------------
#  HOW IT WORKS
#  * Work in pairs. Replace every ___ with code, run, check the result.
#  * CORE tasks 1-6: everyone.  ADVANCED tasks 7-10: when you finish.
#    BONUS: a second dataset.
#  * Save this file as NameSurname.R and send it to us at the end.
#  * You may use any function from base R or the tidyverse, the help
#    pages (?function), the cheatsheets and the course slides.
#
#  THE DATA
#  penguins_raw (package palmerpenguins) is the same set of 344 penguins
#  we used all day, but exactly as it came from the field spreadsheets:
#  column names with spaces and brackets, long species names, sex in
#  capitals, egg-laying dates and stable isotope ratios.
# =====================================================================

library(tidyverse)
library(palmerpenguins)


# =====================================================================
# CORE
# =====================================================================

# Task 1 — Load and inspect ------------------------------------------
# Store penguins_raw in an object called raw. How many rows and columns?
# Which columns are numbers, which are text, which are dates?
raw <- penguins_raw
___(raw)          # one line per column
___(raw)          # rows and columns


# Task 2 — Select and rename -----------------------------------------
# Keep and rename these 12 columns. Names with spaces need `backticks`.
#   species, island, individual_id, clutch_completion, date_egg,
#   bill_length_mm (= Culmen Length), bill_depth_mm (= Culmen Depth),
#   flipper_length_mm, body_mass_g, sex,
#   d15n (= Delta 15 N), d13c (= Delta 13 C)
peng <- raw |>
  select(
    species           = Species,
    island            = Island,
    individual_id     = `Individual ID`,
    clutch_completion = ___,
    date_egg          = ___,
    bill_length_mm    = `Culmen Length (mm)`,
    bill_depth_mm     = ___,
    flipper_length_mm = ___,
    body_mass_g       = ___,
    sex               = ___,
    d15n              = `Delta 15 N (o/oo)`,
    d13c              = ___
  )
glimpse(peng)


# Task 3 — Clean the values ------------------------------------------
# species: "Adelie Penguin (Pygoscelis adeliae)" -> "Adelie"
#          hint: word(species, 1) keeps the first word (stringr)
# sex:     "MALE" -> "male"   hint: str_to_lower()
peng <- peng |>
  mutate(
    species = ___,
    sex     = ___
  )
count(peng, species)   # should show Adelie, Chinstrap, Gentoo
count(peng, sex)


# Task 4 — Missing values --------------------------------------------
# a) Count the NAs in every column (hint: summarise + across + is.na)
peng |>
  summarise(across(___, ~ sum(is.na(.x))))

# b) Drop the rows with missing body_mass_g, flipper_length_mm or sex.
#    How many rows did you lose?
peng_clean <- peng |>
  ___(body_mass_g, flipper_length_mm, sex)

nrow(peng)
nrow(peng_clean)


# Task 5 — Summary table ---------------------------------------------
# For each species and sex: number of penguins, mean and sd of body
# mass in KILOGRAMS, mean flipper length. Sort from heaviest to lightest.
# Which group is the heaviest?
summary_tab <- peng_clean |>
  group_by(___, ___) |>
  summarise(
    n            = ___,
    mass_kg_mean = mean(body_mass_g) / 1000,
    mass_kg_sd   = ___,
    flipper_mean = ___,
    .groups = "drop"
  ) |>
  arrange(___)
summary_tab


# Task 6 — The figure ------------------------------------------------
# Scatter plot: flipper length (x) vs body mass (y), coloured by species,
# with one linear trend line per species (geom_smooth, method = "lm"),
# axis labels with units, a legend title and a clean theme.
# Then save it as a 300 dpi PNG in the output folder, and save the
# summary table as a CSV.
p1 <- ggplot(peng_clean,
             aes(x = ___, y = ___, colour = ___)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = ___, se = FALSE) +
  labs(
    title  = ___,
    x      = "Flipper length (mm)",
    y      = ___,
    colour = ___
  ) +
  theme_minimal(base_size = 14)
p1

dir.create("output", showWarnings = FALSE)
ggsave("output/flipper_vs_mass.png", p1, width = 8, height = 5, dpi = ___)
write_csv(___, "output/summary_species_sex.csv")


# =====================================================================
# ADVANCED
# =====================================================================

# Task 7 — Dates with lubridate ---------------------------------------
# a) Add year and month (as a label: Nov, Dec) from date_egg.
# b) Within each year, compute laying_day = days since the first egg
#    of that season: as.numeric(date_egg - min(date_egg)).
# c) Which species lays its eggs earliest in the season?
peng_dates <- peng_clean |>
  mutate(
    year  = ___(date_egg),
    month = ___(date_egg, label = TRUE)
  ) |>
  group_by(___) |>
  mutate(laying_day = ___) |>
  ungroup()

peng_dates |> count(year, month)

peng_dates |>
  group_by(species) |>
  summarise(mean_laying_day = ___, .groups = "drop") |>
  arrange(mean_laying_day)


# Task 8 — Join ------------------------------------------------------
# a) Make a table with the mean body mass of each species.
# b) left_join() it back to peng_clean and compute
#    mass_anomaly_g = body_mass_g - species mean.
# c) Which 5 penguins are heaviest RELATIVE to their own species?
#    Look at individual_id: is it a unique key? (hint: N39A2)
species_means <- peng_clean |>
  group_by(species) |>
  summarise(mass_species_mean = ___, .groups = "drop")

peng_anomaly <- peng_clean |>
  ___(species_means, by = ___) |>
  mutate(mass_anomaly_g = ___) |>
  arrange(desc(mass_anomaly_g))

peng_anomaly |>
  select(individual_id, species, sex, body_mass_g, mass_anomaly_g) |>
  head(5)


# Task 9 — Reshape and facet ------------------------------------------
# Put the four measurements (bill_length_mm to body_mass_g) in long
# format, then draw one boxplot panel per measurement.
# Hint: facet_wrap(~ measure, scales = "free_y")
peng_long <- peng_clean |>
  select(species, bill_length_mm:body_mass_g) |>
  pivot_longer(
    cols      = ___,
    names_to  = ___,
    values_to = ___
  )

ggplot(peng_long, aes(x = species, y = value, fill = species)) +
  geom_boxplot() +
  ___ +
  theme_bw() +
  theme(legend.position = "none")


# Task 10 — Stable isotopes ------------------------------------------
# Plot d13c (x) against d15n (y), coloured by species, one panel per
# island. Some penguins have no isotope data: remove them first.
# Do the three species seem to feed differently?
peng_clean |>
  drop_na(___, ___) |>
  ggplot(aes(x = ___, y = ___, colour = ___)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ ___) +
  theme_minimal()


# =====================================================================
# BONUS — a new dataset: airquality (built into R)
# Daily air quality in New York, May to September 1973:
# Ozone (ppb), Solar.R, Wind (mph), Temp (°F), Month, Day.
# =====================================================================
# a) Turn it into a tibble and build a proper date column with
#    make_date(1973, Month, Day).
# b) How many NAs per column?
# c) Monthly mean Ozone (careful with NAs!), mean Temp and number of days.
# d) Long format with Ozone, Temp and Wind, then a time-series plot with
#    one panel per variable (facet_wrap(..., ncol = 1, scales = "free_y")).

aq <- as_tibble(airquality) |>
  mutate(date = ___)

# your code here


# =====================================================================
# Done? Save as NameSurname.R and send it. Well done!
# =====================================================================
