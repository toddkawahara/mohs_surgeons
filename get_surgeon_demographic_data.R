library(dplyr)
library(writexl)

# read in data
df <- read.csv('Final_Mohs_Billing.csv')

# Filter for year right after graduation
df1 <- df %>%
  filter(Billing_Year == Graduation.Year + 1)

# Service area data
df_demographic <- df1 %>%
  distinct(NPI, Billing_Year, Rndrng_Prvdr_RUCA_Desc) %>%
  group_by(Billing_Year, Rndrng_Prvdr_RUCA_Desc) %>%
  summarize(surgeons = n()) %>%
  group_by(Billing_Year) %>%
  mutate(total_surgeons_year = sum(surgeons), 
         surgeons_pct = surgeons/total_surgeons_year) %>%
  ungroup()

# state demographic data
df_state <- df1 %>%
  distinct(NPI, Billing_Year, Rndrng_Prvdr_State_Abrvtn) %>%
  group_by(Billing_Year, Rndrng_Prvdr_State_Abrvtn) %>%
  summarize(surgeons = n()) %>%
  group_by(Billing_Year) %>%
  mutate(total_surgeons_year = sum(surgeons), 
         surgeons_pct = surgeons/total_surgeons_year) %>%
  ungroup()

# office demographic data
df_office <- df1 %>%
  distinct(NPI, Billing_Year, Place_Of_Srvc) %>%
  group_by(Billing_Year, Place_Of_Srvc) %>%
  summarize(surgeons = n()) %>%
  group_by(Billing_Year) %>%
  mutate(total_surgeons_year = sum(surgeons), 
         surgeons_pct = surgeons/total_surgeons_year) %>%
  ungroup()

# save new csvs
write.csv(df_demographic, "df_demographic.csv")
write.csv(df_state, "df_state.csv")
write.csv(df_office, "df_office.csv")








