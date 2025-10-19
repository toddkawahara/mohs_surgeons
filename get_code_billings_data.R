library(dplyr)
library(writexl)

# read in data
df <- read.csv('Final_Mohs_Billing.csv')

# Filter for year right after graduation
df1 <- df %>%
  filter(Billing_Year == Graduation.Year + 1)

# yearly avg codes per surgeon
df_total_billed <- df1 %>%
  group_by(Billing_Year, HCPCS_Cd) %>%
  summarize(total_billed = sum(Tot_Srvcs))

df_yearly_surgeons <- df1 %>%
  distinct(NPI, Billing_Year, HCPCS_Cd) %>%
  group_by(Billing_Year, HCPCS_Cd) %>%
  summarize(total_surgeons = n())

df_yearly_avg_bill <- merge(df_total_billed, df_yearly_surgeons, by = c('Billing_Year', 'HCPCS_Cd')) %>%
  mutate(yearly_avg = total_billed/total_surgeons)

df_codes_per_surgeon_yearly_table <- df_yearly_avg_bill %>%
  select(Billing_Year, yearly_avg, HCPCS_Cd) %>%
  pivot_wider(
    names_from = Billing_Year, 
    values_from = yearly_avg
  )

# yearly avg total codes per surgeon
df_total_billed1 <- df1 %>%
  group_by(Billing_Year) %>%
  summarize(total_billed = sum(Tot_Srvcs))

df_yearly_surgeons1 <- df1 %>%
  distinct(NPI, Billing_Year) %>%
  group_by(Billing_Year) %>%
  summarize(total_surgeons = n())

df_yearly_avg_bill_all <- merge(df_total_billed1, df_yearly_surgeons1, by = c('Billing_Year')) %>%
  mutate(yearly_avg = total_billed/total_surgeons)

# yearly avg code charges per surgeon
df_total_charged <- df1 %>%
  group_by(Billing_Year, HCPCS_Cd) %>%
  summarize(total_charged = sum(Avg_Sbmtd_Chrg))

df_yearly_avg_charge <- merge(df_total_charged, df_yearly_surgeons, by = c('Billing_Year', 'HCPCS_Cd')) %>%
  mutate(yearly_avg = total_charged/total_surgeons)

# yearly avg total code charges per surgeon
df_total_charged1 <- df1 %>%
  group_by(Billing_Year) %>%
  summarize(total_charged = sum(Avg_Sbmtd_Chrg))

df_yearly_avg_charge_all <- merge(df_total_charged1, df_yearly_surgeons1, by = c('Billing_Year')) %>%
  mutate(yearly_avg = total_charged/total_surgeons)


# save new csvs
write.csv(df_yearly_avg_bill, "df_yearly_avg_bill.csv")
write.csv(df_yearly_avg_bill_all, "df_yearly_avg_bill_all.csv")
write.csv(df_yearly_avg_charge, "df_yearly_avg_charge.csv")
write.csv(df_yearly_avg_charge_all, "df_yearly_avg_charge_all")

# save excel files
write_xlsx(df_codes_per_surgeon_yearly_table, 'df_codes_per_surgeon_yearly_table.xlsx')