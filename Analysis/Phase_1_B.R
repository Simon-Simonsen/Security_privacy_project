# Phase 1
#B
# Is there a significant difference between political preferences of the voters depending 
# on their demographic attributes recorded in the survey (that is, age, gender, education level…)?

library(dplyr)
library(readxl)
library(lubridate)
df <- read_excel("private_dataP.xlsx")
df
current_date <- as.Date("2026-10-26")
df <- df %>%
  mutate(
    dob = as.Date(dob),
    age = floor(interval(dob, current_date) / years(1)),
    # Bin age into meaningful demographic categories for chi-square testing
    age_group = cut(
      age, 
      breaks = c(-Inf, 29, 44, 64, Inf), 
      labels = c("18-29", "30-44", "45-64", "65+")
    )
  )
df
#p = 0.05 if result is less than p, then its significant that the two variables is related.
#Gender vs Party
tbl_sex <- table(df$sex, df$party)
chisq_sex <- chisq.test(tbl_sex)
cat("\nSex vs. Party\n")
print(chisq_sex)
#p=0.0001021 less 0.05, significant relationship between Gender and party.


#Age Group vs Party
tbl_age <- table(df$age_group, df$party)
chisq_age <- chisq.test(tbl_age)
cat("\nAge Group vs. Party\n")
print(chisq_age)
#p=0.0002891, less than 0.05, there is a significant relationship between age and party.


#Education Level vs Party
tbl_edu <- table(df$education, df$party)
chisq_edu <- chisq.test(tbl_edu)
cat("\nEducation vs. Party\n")
print(chisq_edu)
#p=5.101e-05, so there is a relation and significance between education level and party voted for


#Citizenship vs Party
tbl_cit <- table(df$citizenship, df$party)
chisq_cit <- chisq.test(tbl_cit)
cat("\nCitizenship vs. Party\n")
#p-value = 0.9911, not significant and no relation between citizenship and party
print(chisq_cit)
