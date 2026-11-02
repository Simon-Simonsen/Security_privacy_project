# ---- df3: as df2, plus top/bottom-coded decades -----------------------------
df3 <- read_excel("private_dataP.xlsx")
link_qi <- c("sex", "dob", "zip", "citizenship", "marital_status")
p_keep_sex <- 0.7
p_keep_zip <- 0.7

code_decade <- function(decade) {
  case_when(
    decade <= 1940 ~ "1940 or earlier",
    decade >= 1990 ~ "1990 or later",
    TRUE           ~ as.character(decade)
  )
}

df3_pre_pram <- raw |>
  select(-name) |>
  mutate(
    dob            = code_decade(to_decade(dob)),
    citizenship    = if_else(citizenship == "Denmark", "Denmark", "Non-Danish"),
    marital_status = two_level_marital(marital_status),
    zip            = as.character(zip),
    sex            = as.character(sex)
  )

count(df3_pre_pram, dob)   # check the new age groups

set.seed(123)
df3 <- df3_pre_pram |>
  mutate(
    sex = pram_column(sex, p_keep = p_keep_sex),
    zip = pram_column(zip, p_keep = p_keep_zip)
  )

round(prop.table(table(original = df3_pre_pram$sex, pram = df3$sex), 1), 2)
round(prop.table(table(original = df3_pre_pram$zip, pram = df3$zip), 1), 2)

# ---- Attacker's view --------------------------------------------------------

use_voted_filter <- TRUE

attacker_view3 <- register_raw |>
  mutate(
    dob            = code_decade(to_decade(dob)),
    citizenship    = if_else(citizenship == "Denmark", "Denmark", "Non-Danish"),
    marital_status = two_level_marital(marital_status),
    zip            = as.character(zip),
    sex            = as.character(sex)
  ) |>
  filter(!use_voted_filter | last_voted == 1)

# ---- Sanity check: should be ~0 ---------------------------------------------

df3_pre_pram |> anti_join(attacker_view3, by = link_qi) |> nrow()

# ---- Risk -------------------------------------------------------------------

bind_rows(
  pre_pram = identity_risk(df3_pre_pram, attacker_view3),
  df3      = identity_risk(df3,          attacker_view3),
  .id = "config"
)

bind_rows(
  pre_pram = vote_risk(df3_pre_pram, attacker_view3),
  df3      = vote_risk(df3,          attacker_view3),
  .id = "config"
)

# ---- Looks certain vs actually correct --------------------------------------

pop3 <- attacker_view3 |> count(across(all_of(link_qi)), name = "F")

df3 |>
  mutate(sex_true = df3_pre_pram$sex, zip_true = df3_pre_pram$zip) |>
  group_by(across(all_of(link_qi))) |>
  mutate(n = n(), l = n_distinct(party)) |>
  ungroup() |>
  left_join(pop3, by = link_qi) |>
  summarise(
    looks_certain    = sum(l == 1 & n == F, na.rm = TRUE),
    actually_correct = sum(l == 1 & n == F & sex == sex_true & zip == zip_true,
                           na.rm = TRUE)
  )

# ---- Local suppression of party in df3 --------------------------------------

df3_pre_suppression <- df3

df3 <- df3 |>
  group_by(across(all_of(link_qi))) |>
  mutate(n = n(), l = n_distinct(party)) |>
  ungroup() |>
  left_join(pop3, by = link_qi) |>
  mutate(
    suppressed = l == 1 & n == F & !is.na(F),
    party      = if_else(suppressed, NA_character_, party)
  ) |>
  select(-n, -l, -F)

sum(df3$suppressed)     # expect 14
mean(df3$suppressed)    # share of records with party suppressed

# ---- Final check (uses the updated vote_risk that ignores NA groups) --------

bind_rows(
  pre_pram           = vote_risk(df3_pre_pram,        attacker_view3),
  pram               = vote_risk(df3_pre_suppression, attacker_view3),
  pram_plus_suppress = vote_risk(df3,                 attacker_view3),
  .id = "stage"
)

identity_risk(df3, attacker_view3)

# ---- Release ----------------------------------------------------------------

df3_release <- df3 |> select(-suppressed)    # never publish the flag
write.csv(df3_release, "df3_release.csv", row.names = FALSE)




# -------------- STAT TESTS: original vs released ------------------------------
# Requires from anonymisation.R / df3 block: raw, df3_release, to_decade(), code_decade()

library(dplyr)

# Birth year from whatever format dob is in (year, Excel date or "m/d/Y" text)
birth_year <- function(x) {
  if (inherits(x, c("Date", "POSIXct"))) return(as.numeric(format(as.Date(x), "%Y")))
  if (is.numeric(x) && all(x > 1800 & x < 2100, na.rm = TRUE)) return(x)
  as.numeric(format(as.Date(as.character(x), "%m/%d/%Y"), "%Y"))
}

current_year <- as.numeric(format(Sys.Date(), "%Y"))

original <- raw |>
  select(-name) |>
  mutate(
    zip       = as.character(zip),
    age       = current_year - birth_year(dob),       # exact age, as before
    age_group = code_decade(to_decade(dob))           # same groups as the release
  )

released <- df3_release |>
  mutate(age_group = dob)                             # dob is already the age group

run_tests <- function(d) {
  tables <- list(
    # evote tests
    "A: party x evote"        = table(d$party, d$evote),   # NA (suppressed) rows dropped
    "C: evote x sex"          = table(d$evote, d$sex),
    "C: evote x zip"          = table(d$evote, d$zip),
    "C: evote x age group"    = table(d$evote, d$age_group),
    "C: evote x citizenship"  = table(d$evote, d$citizenship),
    "C: evote x marital"      = table(d$evote, d$marital_status),
    # party tests
    "P: sex x party"          = table(d$sex, d$party),
    "P: age group x party"    = table(d$age_group, d$party),
    "P: education x party"    = table(d$education, d$party),
    "P: citizenship x party"  = table(d$citizenship, d$party)
  )
  tibble(
    test    = names(tables),
    p_value = sapply(tables, function(t) suppressWarnings(chisq.test(t))$p.value)
  )
}

comparison <- left_join(
  run_tests(original),
  run_tests(released),
  by = "test", suffix = c("_original", "_released")
) |>
  mutate(
    across(starts_with("p_value"), ~ signif(.x, 3)),
    same_conclusion = (p_value_original < 0.05) == (p_value_released < 0.05)
  )

print(comparison, width = Inf)

# Exact age (only possible on the original — the release no longer has it)
chisq.test(table(original$evote, original$age))