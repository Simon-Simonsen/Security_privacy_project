# ---- df2: PRAM sex (0.7), decades, PRAM zip (0.7), Danish/non-Danish, ------
# ----      married/non-married ----------------------------------------------
df2 <- read_excel("private_dataP.xlsx")
link_qi <- c("sex", "dob", "zip", "citizenship", "marital_status")
p_keep_sex <- 0.7
p_keep_zip <- 0.7

df2_pre_pram <- raw |>
  select(-name) |>
  mutate(
    dob            = to_decade(dob),
    citizenship    = if_else(citizenship == "Denmark", "Denmark", "Non-Danish"),
    marital_status = two_level_marital(marital_status),
    zip            = as.character(zip),
    sex            = as.character(sex)
  )

set.seed(123)
df2 <- df2_pre_pram |>
  mutate(
    sex = pram_column(sex, p_keep = p_keep_sex),
    zip = pram_column(zip, p_keep = p_keep_zip)
  )

# Transition matrices (report these)
round(prop.table(table(original = df2_pre_pram$sex, pram = df2$sex), 1), 2)
round(prop.table(table(original = df2_pre_pram$zip, pram = df2$zip), 1), 2)

# ---- Attacker's view (register untouched; same recoding as the release) -----

use_voted_filter <- TRUE   # set FALSE if the sanity check below fails

attacker_view2 <- register_raw |>
  mutate(
    dob            = to_decade(dob),
    citizenship    = if_else(citizenship == "Denmark", "Denmark", "Non-Danish"),
    marital_status = two_level_marital(marital_status),
    zip            = as.character(zip)
  ) |>
  filter(!use_voted_filter | last_voted == 1)
# sex and zip stay as they are: the attacker can't undo PRAM

# ---- Sanity check: should be ~0 ---------------------------------------------

df2_pre_pram |> anti_join(attacker_view2, by = link_qi) |> nrow()

# ---- Risk -------------------------------------------------------------------

bind_rows(
  pre_pram = identity_risk(df2_pre_pram, attacker_view2),
  df2      = identity_risk(df2,          attacker_view2),
  .id = "config"
)

bind_rows(
  pre_pram = vote_risk(df2_pre_pram, attacker_view2),
  df2      = vote_risk(df2,          attacker_view2),
  .id = "config"
)

# ---- How many "certain" disclosures are actually correct? -------------------

pop2 <- attacker_view2 |> count(across(all_of(link_qi)), name = "F")

df2 |>
  mutate(sex_true = df2_pre_pram$sex, zip_true = df2_pre_pram$zip) |>
  group_by(across(all_of(link_qi))) |>
  mutate(n = n(), l = n_distinct(party)) |>
  ungroup() |>
  left_join(pop2, by = link_qi) |>
  summarise(
    looks_certain    = sum(l == 1 & n == F, na.rm = TRUE),
    actually_correct = sum(l == 1 & n == F & sex == sex_true & zip == zip_true,
                           na.rm = TRUE)
  )