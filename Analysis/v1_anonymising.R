# ---- df1: sex, decades, Danish/non-Danish, PRAM on marital_status ----------
df1 <- read_excel("private_dataP.xlsx")
link_qi <- c("sex", "dob", "zip", "citizenship", "marital_status")

df1_pre_pram <- raw |>
  select(-name) |>
  mutate(
    dob            = to_decade(dob),
    citizenship    = if_else(citizenship == "Denmark", "Denmark", "Non-Danish"),
    zip            = as.character(zip),
    marital_status = as.character(marital_status)
  )

set.seed(123)
df1 <- df1_pre_pram |> mutate(marital_status = pram_column(marital_status))

table(original = df1_pre_pram$marital_status, pram = df1$marital_status)

# ---- Attacker's view (register untouched; same recoding as the release) -----

use_voted_filter <- TRUE   # set FALSE if the sanity check below fails

attacker_view1 <- register_raw |>
  mutate(
    dob         = to_decade(dob),
    citizenship = if_else(citizenship == "Denmark", "Denmark", "Non-Danish"),
    zip         = as.character(zip)
  ) |>
  filter(!use_voted_filter | last_voted == 1)
# marital_status stays in its original categories: PRAM doesn't change the labels

# ---- Sanity check: should be ~0 ---------------------------------------------
p_keep <- 0.7

set.seed(123)
df1 <- df1_pre_pram |> mutate(marital_status = pram_column(marital_status, p_keep = p_keep))

# ---- Risk -------------------------------------------------------------------

bind_rows(
  pre_pram = identity_risk(df1_pre_pram, attacker_view1),
  df1      = identity_risk(df1,          attacker_view1),
  .id = "config"
)

bind_rows(
  pre_pram = vote_risk(df1_pre_pram, attacker_view1),
  df1      = vote_risk(df1,          attacker_view1),
  .id = "config"
)

# ---- How many "certain" disclosures are actually correct? -------------------

pop1 <- attacker_view1 |> count(across(all_of(link_qi)), name = "F")

df1 |>
  mutate(marital_true = df1_pre_pram$marital_status) |>
  group_by(across(all_of(link_qi))) |>
  mutate(n = n(), l = n_distinct(party)) |>
  ungroup() |>
  left_join(pop1, by = link_qi) |>
  summarise(
    looks_certain    = sum(l == 1 & n == F, na.rm = TRUE),
    actually_correct = sum(l == 1 & n == F & marital_status == marital_true, na.rm = TRUE)
  )