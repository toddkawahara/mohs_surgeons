library(dplyr)

# read in data
df <- read.csv('Final_Mohs_Billing.csv')

# Filter for year right after graduation
df1 <- df %>%
  filter(Billing_Year == Graduation.Year + 1)

# Billing for each code averaged
df_total_billed <- df1 %>%
  group_by(Billing_Year, HCPCS_Cd) %>%
  summarize(total_billed = sum(Tot_Srvcs))

df_yearly_surgeons <- df1 %>%
  distinct(NPI, Billing_Year, HCPCS_Cd) %>%
  group_by(Billing_Year, HCPCS_Cd) %>%
  summarize(total_surgeons = n())

df_yearly_avg_bill <- merge(df_total_billed, df_yearly_surgeons, by = c('Billing_Year', 'HCPCS_Cd')) %>%
  mutate(yearly_avg = total_billed/total_surgeons)

# Billing for all codes averaged
df_total_billed1 <- df1 %>%
  group_by(Billing_Year) %>%
  summarize(total_billed = sum(Tot_Srvcs))

df_yearly_surgeons1 <- df1 %>%
  distinct(NPI, Billing_Year) %>%
  group_by(Billing_Year) %>%
  summarize(total_surgeons = n())

df_yearly_avg_bill_all <- merge(df_total_billed1, df_yearly_surgeons1, by = c('Billing_Year')) %>%
  mutate(yearly_avg = total_billed/total_surgeons)

# Charged for each code averaged
df_total_charged <- df1 %>%
  group_by(Billing_Year, HCPCS_Cd) %>%
  summarize(total_charged = sum(Avg_Sbmtd_Chrg))

df_yearly_avg_charge <- merge(df_total_charged, df_yearly_surgeons, by = c('Billing_Year', 'HCPCS_Cd')) %>%
  mutate(yearly_avg = total_charged/total_surgeons)

# Charged for all codes averaged
df_total_charged1 <- df1 %>%
  group_by(Billing_Year) %>%
  summarize(total_charged = sum(Avg_Sbmtd_Chrg))

df_yearly_avg_charge_all <- merge(df_total_charged1, df_yearly_surgeons1, by = c('Billing_Year')) %>%
  mutate(yearly_avg = total_charged/total_surgeons)

# Grouping by demographics
df_demographic <- df1 %>%
  distinct(NPI, Billing_Year, Rndrng_Prvdr_RUCA_Desc) %>%
  group_by(Billing_Year, Rndrng_Prvdr_RUCA_Desc) %>%
  summarize(surgeons = n()) %>%
  group_by(Billing_Year) %>%
  mutate(total_surgeons_year = sum(surgeons), 
         surgeons_pct = surgeons/total_surgeons_year) %>%
  ungroup()

# Grouping by state
df_state <- df1 %>%
  distinct(NPI, Billing_Year, Rndrng_Prvdr_State_Abrvtn) %>%
  group_by(Billing_Year, Rndrng_Prvdr_State_Abrvtn) %>%
  summarize(surgeons = n()) %>%
  group_by(Billing_Year) %>%
  mutate(total_surgeons_year = sum(surgeons), 
         surgeons_pct = surgeons/total_surgeons_year) %>%
  ungroup()

# Grouping by office/facility
df_office <- df1 %>%
  distinct(NPI, Billing_Year, Place_Of_Srvc) %>%
  group_by(Billing_Year, Place_Of_Srvc) %>%
  summarize(surgeons = n()) %>%
  group_by(Billing_Year) %>%
  mutate(total_surgeons_year = sum(surgeons), 
         surgeons_pct = surgeons/total_surgeons_year) %>%
  ungroup()

# save new csvs
write.csv(df_yearly_avg_bill, "df_yearly_avg_bill.csv")
write.csv(df_yearly_avg_bill_all, "df_yearly_avg_bill_all.csv")
write.csv(df_yearly_avg_charge, "df_yearly_avg_charge.csv")
write.csv(df_yearly_avg_charge_all, "df_yearly_avg_charge_all")
write.csv(df_demographic, "df_demographic.csv")
write.csv(df_state, "df_state.csv")
write.csv(df_office, "df_office.csv")









