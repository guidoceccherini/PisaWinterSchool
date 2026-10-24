# Introduction to R: tidyverse & ggplot2 — all code from the slides
# Pisa Summer School — Guido Ceccherini
# Lines from exercise starters with ___ are commented out.

library(tidyverse)
library(palmerpenguins)

# ====================================================================
# Opening
# ====================================================================

# --- Setup ---
library(tidyverse)
library(palmerpenguins)

# ====================================================================
# Block 1 · Getting started
# ====================================================================

# --- R as a calculator ---
3 + 5 * 2       # 13: usual precedence
(3 + 5) * 2     # 16
2 ^ 10          # 1024
10 / 3          # 3.333333
10 %/% 3        # 3, integer division
10 %% 3         # 1, remainder
2 / 10000       # 2e-04, i.e. 2 x 10^-4
5e3             # 5000

# --- Comparing values ---
1 == 1          # TRUE  (two equals signs)
1 != 2          # TRUE  (not equal)
3 >= 4          # FALSE
0.1 + 0.2 == 0.3                    # FALSE !
isTRUE(all.equal(0.1 + 0.2, 0.3))   # TRUE

# --- Variables and assignment ---
x <- 1 / 40
x               # 0.025
log(x)          # use it in calculations
x <- 100        # overwrite it
x <- x + 1      # 101
y <- x * 2      # 202

# --- What is in each variable? (starter) ---
mass <- 47.5
age  <- 122
mass <- mass * 2.3
age  <- age - 20

# --- What is in each variable? ---
mass    # 109.25
age     # 102
mass > age   # TRUE

# --- Vectors: the basic building block ---
heights <- c(1.62, 1.75, 1.80, 1.68)   # c() = combine
heights * 100              # 162 175 180 168
length(heights)            # 4
mean(heights)              # 1.7125
1:10                       # 1 2 3 ... 10
seq(0, 1, by = 0.25)       # 0.00 0.25 0.50 0.75 1.00
heights[2]                 # 1.75, the 2nd element
heights[heights > 1.7]     # 1.75 1.80

# --- Every value has a type ---
class(3.14)        # "numeric"
class(1L)          # "integer"
class("banana")    # "character"
class(TRUE)        # "logical"
c(1, "a", TRUE)    # all become "character"!
as.numeric("3.5")  # 3.5
as.numeric("80 or 83")   # NA, with a warning

# --- Factors: categorical data ---
sizes <- c("low", "high", "medium", "low")
f <- factor(sizes, levels = c("low", "medium", "high"))
levels(f)                        # "low" "medium" "high"
table(f)
years <- factor(c("2007", "2009"))
as.numeric(years)                # 1 2  (the hidden codes!)
as.numeric(as.character(years))  # 2007 2009

# --- Data frames and tibbles ---
penguins                  # a tibble: 344 x 8
penguins$body_mass_g[1:5] # one column = one vector
glimpse(penguins)         # structure, one line per column
dim(penguins)             # 344 8
nrow(penguins)            # 344
names(penguins)           # column names
summary(penguins)         # quick statistics

# --- Functions, arguments and help ---
# ?mean
mean(c(2, 4, NA))                  # NA: missing is contagious
mean(c(2, 4, NA), na.rm = TRUE)    # 3
round(3.14159, digits = 2)         # 3.14
round(3.14159, 2)                  # same, by position

# --- Explore the penguins ---
dim(penguins)                    # 344 rows, 8 columns
class(penguins$flipper_length_mm) # "integer"
class(penguins$island)            # "factor"
sum(is.na(penguins$body_mass_g))  # 2
levels(penguins$species)  # "Adelie" "Chinstrap" "Gentoo"
slice_sample(penguins, n = 5)

# ====================================================================
# Block 2 · dplyr
# ====================================================================

# --- The pipe: then do this ---
x <- c(1, 4, 9, 16)

# Nested: read from the inside out
round(sqrt(mean(x)), 2)

# Piped: read left to right, step by step
x |> mean() |> sqrt() |> round(2)

# --- select(): keep the columns you need ---
penguins |>
  select(species, island, body_mass_g)

penguins |> select(species, starts_with("bill"))
penguins |> select(-year, -sex)        # drop columns
penguins |> select(species:bill_depth_mm)
penguins |> rename(mass_g = body_mass_g)

# --- filter(): keep the rows that match ---
penguins |> filter(island == "Torgersen")
penguins |> filter(species == "Gentoo", body_mass_g > 5000)
penguins |> filter(island %in% c("Dream", "Biscoe"))
penguins |> filter(bill_length_mm > 50 | body_mass_g > 6000)  # or
penguins |> filter(is.na(sex))                 # missing sex
penguins |> filter(!is.na(sex), year == 2009)  # ! = not

# --- Chain verbs: filter, then select ---
torgersen <- penguins |>
  filter(island == "Torgersen") |>
  select(year, sex, species)

torgersen     # 52 rows, 3 columns

# --- Penguins of Dream island ---
dream <- penguins |>
  filter(island == "Dream") |>
  select(year, sex, species)
nrow(dream)    # 124

# --- arrange(): sort the rows ---
penguins |> arrange(body_mass_g)          # lightest first
penguins |> arrange(desc(body_mass_g))    # heaviest first
penguins |> arrange(species, desc(flipper_length_mm))

# --- mutate(): create new columns ---
penguins |>
  mutate(
    body_mass_kg   = body_mass_g / 1000,
    bill_ratio     = bill_length_mm / bill_depth_mm,
    flipper_per_kg = flipper_length_mm / body_mass_kg
  ) |>
  select(species, body_mass_kg, bill_ratio, flipper_per_kg)

# --- Conditional columns: if_else() and case_when() ---
penguins |>
  mutate(
    heavy = if_else(body_mass_g > 4500,
                    "heavy", "light"),
    size  = case_when(
      body_mass_g <  3500 ~ "small",
      body_mass_g <  4500 ~ "medium",
      body_mass_g >= 4500 ~ "large"
    )
  ) |>
  count(size)

# --- Create and classify ---
penguins |>
  mutate(
    flipper_cm = flipper_length_mm / 10,
    bill_class = if_else(bill_length_mm >= 45,
                         "long", "short")
  ) |>
  count(species, bill_class)

# --- Watch out for NA ---
penguins |>
  group_by(island) |>
  summarise(mean_bill = mean(bill_length_mm))

penguins |>
  group_by(island) |>
  summarise(mean_bill = mean(bill_length_mm,
                             na.rm = TRUE))

# --- Several groups, several statistics ---
penguins |>
  group_by(species, sex) |>
  summarise(
    n         = n(),
    mass_mean = mean(body_mass_g, na.rm = TRUE),
    mass_sd   = sd(body_mass_g, na.rm = TRUE),
    bill_max  = max(bill_length_mm, na.rm = TRUE),
    .groups   = "drop"
  )

# --- count() and n() ---
penguins |> count(species)
penguins |> count(species, island, sort = TRUE)
penguins |> filter(year == 2007) |> count(sex)

penguins |>
  group_by(sex) |>
  summarise(se_bill = sd(bill_length_mm, na.rm = TRUE) / sqrt(n()))

# --- Summarise by group ---
penguins |>
  group_by(species, island) |>
  summarise(
    n            = n(),
    flipper_mean = mean(flipper_length_mm,
                        na.rm = TRUE),
    mass_mean    = mean(body_mass_g, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(mass_mean))

# --- Do the same thing to many columns: across() ---
# missing values in every column
penguins |>
  summarise(across(everything(), ~ sum(is.na(.x))))

# mean of every numeric column, per species
penguins |>
  group_by(species) |>
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

# --- Rule one: never summarise dirty data ---
penguins_clean <- penguins |>
  drop_na(bill_length_mm, bill_depth_mm, flipper_length_mm,
          body_mass_g, sex)

nrow(penguins)         # 344
nrow(penguins_clean)   # 333

# --- Putting it together: one readable pipeline ---
penguins_derived <- penguins_clean |>
  filter(body_mass_g > 3000) |>                      # rows
  mutate(bill_ratio = bill_length_mm / bill_depth_mm, # new variables
         flipper_mass = flipper_length_mm / body_mass_g * 1000) |>
  select(species, island, sex, year,                  # columns
         body_mass_g, bill_ratio, flipper_mass) |>
  arrange(desc(body_mass_g))                          # order

penguins_derived

# --- Your turn with a new dataset: mtcars ---
mtcars |>
  filter(hp > 100) |>
  mutate(kpl = mpg * 0.425) |>
  group_by(cyl, am) |>
  summarise(n = n(), kpl_mean = mean(kpl),
            .groups = "drop") |>
  arrange(desc(kpl_mean))

# ====================================================================
# Block 3 · Tidy data and joins
# ====================================================================

# --- Reading and writing files with readr ---
url <- paste0("https://raw.githubusercontent.com/allisonhorst/",
              "palmerpenguins/main/inst/extdata/penguins.csv")
peng_csv <- read_csv(url)        # from the web
spec(peng_csv)                   # which types were guessed?

dir.create("output", showWarnings = FALSE)
write_csv(penguins_clean, "output/penguins_clean.csv")
peng_back <- read_csv("output/penguins_clean.csv")

# --- pivot_wider(): long to wide ---
rain_long <- tibble(
  station = c("A", "A", "B", "B"),
  date    = ymd(c("2025-04-24", "2025-04-25",
                  "2025-04-24", "2025-04-25")),
  rain_mm = c(5.2, 0.0, 3.5, 1.2)
)

rain_wide <- rain_long |>
  pivot_wider(names_from = station,
              values_from = rain_mm)

# --- pivot_longer(): wide to long ---
rain_wide |>
  pivot_longer(cols = -date,            # every column but date
               names_to = "station",
               values_to = "rain_mm")

penguins_long <- penguins_clean |>
  select(species, island, bill_length_mm:body_mass_g) |>
  pivot_longer(cols = bill_length_mm:body_mass_g,
               names_to = "measure", values_to = "value")

# --- Reshape three stations (starter) ---
rain3 <- tibble(
  station = rep(c("A", "B", "C"), each = 3),
  date    = rep(ymd(c("2025-04-24", "2025-04-25", "2025-04-26")), 3),
  rain_mm = c(5.2, 0.0, 1.1, 3.5, 1.2, 0.0, 6.0, 2.0, 0.5)
)

# --- Reshape three stations ---
rain3 |>
  pivot_wider(names_from = station,
              values_from = rain_mm)

rain3 |>
  group_by(station) |>
  summarise(mean_rain = mean(rain_mm))

# --- left_join() and inner_join() in action ---
x <- tibble(key = c("A", "B", "C", "E"),
            value1 = c(8, 10, 7, 9))
y <- tibble(key = c("A", "B", "C", "D"),
            value2 = c(5, 2, 3, 4))

left_join(x, y, by = "key")
inner_join(x, y, by = "key")

# --- Predict before you run (starter) ---
customers <- tibble(id = c(1, 2, 3, 4),
                    name = c("Anna", "Luca", "Maria", "Giulia"))
orders <- tibble(id = c(2, 3, 5),
                 product = c("Book", "Pen", "Notebook"))

# --- Join answers ---
left_join(customers, orders, by = "id")
inner_join(customers, orders, by = "id")
full_join(customers, orders, by = "id")

# --- A realistic join: add metadata ---
species_info <- tibble(
  species    = c("Adelie", "Chinstrap", "Gentoo"),
  latin_name = c("Pygoscelis adeliae", "Pygoscelis antarcticus",
                 "Pygoscelis papua")
)

penguins_clean |>
  group_by(species) |>
  summarise(mass_mean = mean(body_mass_g)) |>
  left_join(species_info, by = "species")

# --- Dates: why lubridate? ---
# base R
as.Date("15/01/2025", format = "%d/%m/%Y")

# lubridate
ymd("2025-01-15")          # ISO, as in most databases
ymd("20250115")            # separators do not matter
dmy("15/01/2025")          # European
mdy("01/15/2025")          # US
dmy("15 January 2025")
dmy_hms("15/01/2025 14:30:00")

# --- Extract parts, do arithmetic ---
d <- ymd("2025-01-15")
year(d); month(d); day(d)
month(d, label = TRUE)          # Jan (ordered factor)
wday(d, label = TRUE)           # Wed
yday(d)                         # 15, day of the year
d + days(10)                    # 2025-01-25
d + months(2)                   # 2025-03-15
ymd("2025-06-20") - d           # Time difference of 156 days

# --- Monthly means per station (starter) ---
# temps <- tibble(
#   date_txt = c("10/01/2025", "15/01/2025", "20/01/2025", "05/02/2025",
#                "12/02/2025", "01/03/2025", "10/03/2025"),
#   station  = c("A", "A", "B", "A", "B", "A", "B"),
#   temp     = c(5.2, 4.8, 6.1, 7.3, 6.9, 9.5, 8.7))
# 
# temps |>
#   mutate(date = ___(date_txt), month = month(___, label = TRUE)) |>
#   group_by(___, ___) |>
#   summarise(temp_mean = mean(temp), n = ___, .groups = "drop")

# --- temps data ---
temps <- tibble(
  date_txt = c("10/01/2025", "15/01/2025", "20/01/2025", "05/02/2025",
               "12/02/2025", "01/03/2025", "10/03/2025"),
  station  = c("A", "A", "B", "A", "B", "A", "B"),
  temp     = c(5.2, 4.8, 6.1, 7.3, 6.9, 9.5, 8.7))

# --- Monthly means per station ---
temps |>
  mutate(date  = dmy(date_txt),
         month = month(date, label = TRUE)) |>
  group_by(station, month) |>
  summarise(temp_mean = mean(temp),
            n = n(),
            .groups = "drop")

# ====================================================================
# Block 4 · ggplot2
# ====================================================================

# --- Base R plot: quick, but limited ---
plot(penguins$flipper_length_mm,
     penguins$body_mass_g)

# --- ggplot() alone draws an empty canvas ---
ggplot(data = penguins,
       aes(x = bill_length_mm))

# --- Add a geom: a histogram ---
ggplot(data = penguins,
       aes(x = bill_length_mm)) +
  geom_histogram()

# --- Your first plot ---
ggplot(penguins,
       aes(x = flipper_length_mm)) +
  geom_histogram(binwidth = 5,
                 fill = "steelblue",
                 colour = "white")

# --- Two numeric variables: geom_point() ---
ggplot(penguins,
       aes(x = flipper_length_mm,
           y = body_mass_g)) +
  geom_point()

# --- Map a third variable to colour ---
ggplot(penguins,
       aes(x = flipper_length_mm,
           y = body_mass_g,
           colour = species)) +
  geom_point(alpha = 0.6, size = 2)

# --- Mapping vs setting ---
ggplot(penguins, aes(flipper_length_mm, body_mass_g)) + geom_point(aes(colour = species))
ggplot(penguins, aes(flipper_length_mm, body_mass_g)) + geom_point(colour = "steelblue")

# --- Layers: add a trend line ---
ggplot(penguins,
       aes(x = flipper_length_mm,
           y = body_mass_g,
           colour = species)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",
              se = FALSE)

# --- Aesthetics per layer ---
ggplot(penguins,
       aes(x = flipper_length_mm,
           y = body_mass_g)) +
  geom_point(aes(colour = species),
             alpha = 0.5) +
  geom_smooth(method = "lm",
              colour = "black")

# --- Distributions by group: boxplots ---
penguins |>
  drop_na(sex) |>
  ggplot(aes(x = species,
             y = body_mass_g,
             fill = sex)) +
  geom_boxplot()

# --- Show the raw data too ---
ggplot(penguins,
       aes(x = species,
           y = body_mass_g)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2,
              alpha = 0.3)

# --- Bars: geom_bar() counts rows ---
ggplot(penguins,
       aes(x = island,
           fill = species)) +
  geom_bar()

# --- geom_col() plots values you computed ---
penguins |>
  group_by(species) |>
  summarise(mass = mean(body_mass_g,
                        na.rm = TRUE)) |>
  ggplot(aes(x = species,
             y = mass)) +
  geom_col()

# --- Labels, colour scales and themes ---
myplot <- ggplot(penguins,
    aes(flipper_length_mm, body_mass_g,
        colour = species)) +
  geom_point(alpha = 0.6) +
  scale_colour_viridis_d() +
  labs(title = "Flipper length vs body mass",
       x = "Flipper length (mm)",
       y = "Body mass (g)",
       colour = "Species") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# --- Facets: small multiples ---
ggplot(penguins,
       aes(flipper_length_mm,
           body_mass_g,
           colour = species)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ island)

# --- facet_grid(): rows by columns ---
penguins |>
  drop_na(sex) |>
  ggplot(aes(flipper_length_mm,
             body_mass_g,
             colour = species)) +
  geom_point(alpha = 0.6) +
  facet_grid(sex ~ island)

# --- Save figures with ggsave() ---
dir.create("output", showWarnings = FALSE)
ggsave("output/flipper_mass.png", plot = myplot,
       width = 8, height = 5, dpi = 300)
ggsave("output/flipper_mass.pdf", plot = myplot,
       width = 8, height = 5)        # vector, for journals

# without plot = it saves the last plot displayed

# --- Plotting mtcars ---
ggplot(mtcars, aes(x = factor(cyl),
                   y = mpg)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Fuel economy by number of cylinders",
       x = "Cylinders",
       y = "Miles per gallon")

ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point()

# --- Colour by transmission ---
ggplot(mtcars, aes(x = wt, y = mpg,
                   colour = factor(am))) +
  geom_point(size = 3) +
  scale_colour_manual(
    values = c("#d9580f", "#12796e"),
    labels = c("Automatic", "Manual")) +
  labs(x = "Weight (1000 lbs)",
       y = "Miles per gallon",
       colour = "Transmission")

# --- Reproduce this figure ---
# your code here
# ...
#
#
#

# --- Reproduce this figure ---
penguins |>
  drop_na(sex) |>
  ggplot(aes(x = species,
             y = flipper_length_mm,
             fill = sex)) +
  geom_boxplot() +
  scale_fill_viridis_d(begin = 0.3,
                       end = 0.8) +
  labs(x = NULL,
       y = "Flipper length (mm)",
       fill = NULL) +
  theme_bw(base_size = 12) +
  theme(legend.position = "top")

# ====================================================================
# Final challenge
# ====================================================================

# --- The data: penguins_raw ---
names(penguins_raw)

# --- Solution: tasks 2 and 3 ---
peng <- penguins_raw |>
  select(species = Species, island = Island,
         individual_id = `Individual ID`,
         clutch_completion = `Clutch Completion`,
         date_egg = `Date Egg`,
         bill_length_mm = `Culmen Length (mm)`,
         bill_depth_mm = `Culmen Depth (mm)`,
         flipper_length_mm = `Flipper Length (mm)`,
         body_mass_g = `Body Mass (g)`, sex = Sex,
         d15n = `Delta 15 N (o/oo)`, d13c = `Delta 13 C (o/oo)`) |>
  mutate(species = word(species, 1), sex = str_to_lower(sex))

# --- Solution: tasks 4 and 5 ---
peng |> summarise(across(everything(), ~ sum(is.na(.x))))

peng_clean <- peng |>
  drop_na(body_mass_g, flipper_length_mm, sex)    # 333 rows

summary_tab <- peng_clean |>
  group_by(species, sex) |>
  summarise(n = n(),
            mass_kg_mean = mean(body_mass_g) / 1000,
            mass_kg_sd   = sd(body_mass_g) / 1000,
            flipper_mean = mean(flipper_length_mm),
            .groups = "drop") |>
  arrange(desc(mass_kg_mean))      # male Gentoo: 5.48 kg

# --- Solution: task 6 ---
p1 <- ggplot(peng_clean,
             aes(flipper_length_mm, body_mass_g, colour = species)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_colour_viridis_d(end = 0.85) +
  labs(title = "Bigger flippers, heavier penguins",
       x = "Flipper length (mm)", y = "Body mass (g)",
       colour = "Species") +
  theme_minimal(base_size = 14)

ggsave("output/flipper_vs_mass.png", p1, width = 8, height = 5, dpi = 300)
