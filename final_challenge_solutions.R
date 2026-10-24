# =====================================================================
#  FINAL CHALLENGE — SOLUTIONS
#  "From raw field data to a publication-ready figure"
#  Introduction to R: tidyverse & ggplot2 — Pisa Summer School
#  Guido Ceccherini & Federico Gianoli
# =====================================================================

library(tidyverse)
library(palmerpenguins)

# ---------------------------------------------------------------------
# CORE (everyone)
# ---------------------------------------------------------------------

# Task 1 — Load and inspect the raw data -------------------------------
raw <- penguins_raw
glimpse(raw)
dim(raw)            # 344 rows, 17 columns
names(raw)          # spaces, brackets, capitals: not R-friendly!

# Task 2 — Select and rename the columns we need -------------------------
# Column names with spaces must be wrapped in backticks
peng <- raw |>
  select(
    species           = Species,
    island            = Island,
    individual_id     = `Individual ID`,
    clutch_completion = `Clutch Completion`,
    date_egg          = `Date Egg`,
    bill_length_mm    = `Culmen Length (mm)`,
    bill_depth_mm     = `Culmen Depth (mm)`,
    flipper_length_mm = `Flipper Length (mm)`,
    body_mass_g       = `Body Mass (g)`,
    sex               = Sex,
    d15n              = `Delta 15 N (o/oo)`,
    d13c              = `Delta 13 C (o/oo)`
  )
glimpse(peng)

# Task 3 — Clean the values ----------------------------------------------
# "Adelie Penguin (Pygoscelis adeliae)" -> "Adelie"; "MALE" -> "male"
peng <- peng |>
  mutate(
    species = word(species, 1),
    sex     = str_to_lower(sex)
  )
count(peng, species)
count(peng, sex)

# Task 4 — Missing values ----------------------------------------------
peng |>
  summarise(across(everything(), ~ sum(is.na(.x))))

peng_clean <- peng |>
  drop_na(body_mass_g, flipper_length_mm, sex)

nrow(peng)        # 344
nrow(peng_clean)  # 333

# Task 5 — Summary table -------------------------------------------------
summary_tab <- peng_clean |>
  group_by(species, sex) |>
  summarise(
    n            = n(),
    mass_kg_mean = mean(body_mass_g) / 1000,
    mass_kg_sd   = sd(body_mass_g) / 1000,
    flipper_mean = mean(flipper_length_mm),
    .groups = "drop"
  ) |>
  arrange(desc(mass_kg_mean))
summary_tab
# Heaviest group: male Gentoo

# Task 6 — The figure ----------------------------------------------------
p1 <- ggplot(peng_clean,
             aes(x = flipper_length_mm, y = body_mass_g, colour = species)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_colour_viridis_d(end = 0.85) +
  labs(
    title  = "Bigger flippers, heavier penguins",
    x      = "Flipper length (mm)",
    y      = "Body mass (g)",
    colour = "Species"
  ) +
  theme_minimal(base_size = 14)
p1

dir.create("output", showWarnings = FALSE)
ggsave("output/flipper_vs_mass.png", p1, width = 8, height = 5, dpi = 300)
write_csv(summary_tab, "output/summary_species_sex.csv")

# ---------------------------------------------------------------------
# ADVANCED
# ---------------------------------------------------------------------

# Task 7 — Dates with lubridate -----------------------------------------
peng_dates <- peng_clean |>
  mutate(
    year      = year(date_egg),
    month     = month(date_egg, label = TRUE)
  ) |>
  group_by(year) |>
  # days since the first egg recorded in that season
  mutate(laying_day = as.numeric(date_egg - min(date_egg))) |>
  ungroup()

peng_dates |> count(year, month)

peng_dates |>
  group_by(species) |>
  summarise(mean_laying_day = mean(laying_day), .groups = "drop") |>
  arrange(mean_laying_day)

# Task 8 — Join: how far is each penguin from its species average? -------
species_means <- peng_clean |>
  group_by(species) |>
  summarise(mass_species_mean = mean(body_mass_g), .groups = "drop")

peng_anomaly <- peng_clean |>
  left_join(species_means, by = "species") |>
  mutate(mass_anomaly_g = body_mass_g - mass_species_mean) |>
  arrange(desc(mass_anomaly_g))

peng_anomaly |>
  select(individual_id, species, sex, body_mass_g, mass_anomaly_g) |>
  head(5)

# Lookup table with scientific names: any species without a match?
species_info <- tibble(
  species    = c("Adelie", "Chinstrap", "Gentoo"),
  latin_name = c("Pygoscelis adeliae", "Pygoscelis antarcticus",
                 "Pygoscelis papua")
)
anti_join(peng_clean, species_info, by = "species") |> nrow()   # 0

# Task 9 — Reshape to long and facet -------------------------------------
peng_long <- peng_clean |>
  select(species, bill_length_mm:body_mass_g) |>
  pivot_longer(
    cols      = -species,
    names_to  = "measure",
    values_to = "value"
  )

p2 <- ggplot(peng_long, aes(x = species, y = value, fill = species)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.25, size = 0.8) +
  facet_wrap(~ measure, scales = "free_y") +
  scale_fill_viridis_d(end = 0.85) +
  labs(x = NULL, y = NULL, title = "Four measurements, three species") +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")
p2
ggsave("output/morphometrics_facets.png", p2, width = 9, height = 6, dpi = 300)

# Task 10 — Stable isotopes: who eats what? ------------------------------
p3 <- peng_clean |>
  drop_na(d15n, d13c) |>
  ggplot(aes(x = d13c, y = d15n, colour = species)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ island) +
  scale_colour_viridis_d(end = 0.85) +
  labs(x = expression(delta^13*C~"(\u2030)"),
       y = expression(delta^15*N~"(\u2030)"),
       colour = "Species") +
  theme_minimal(base_size = 13)
p3

# ---------------------------------------------------------------------
# BONUS — a second dataset: airquality (built into R)
# Daily air quality measurements, New York, May–September 1973
# ---------------------------------------------------------------------
aq <- as_tibble(airquality) |>
  mutate(date = make_date(1973, Month, Day))

aq |>
  summarise(across(everything(), ~ sum(is.na(.x))))

aq |>
  group_by(month = month(date, label = TRUE)) |>
  summarise(
    ozone_mean = mean(Ozone, na.rm = TRUE),
    temp_mean  = mean(Temp),
    n_days     = n(),
    .groups = "drop"
  )

aq |>
  select(date, Ozone, Temp, Wind) |>
  pivot_longer(-date, names_to = "variable", values_to = "value") |>
  ggplot(aes(x = date, y = value)) +
  geom_line(colour = "grey40") +
  geom_smooth(method = "loess", se = FALSE, colour = "#d9580f") +
  facet_wrap(~ variable, ncol = 1, scales = "free_y") +
  labs(x = NULL, y = NULL, title = "New York, summer 1973") +
  theme_minimal(base_size = 13)
