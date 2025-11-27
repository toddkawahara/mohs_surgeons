library(dplyr)
library(tidyverse)
library(writexl)

# read in data
df_grads <- read.csv('Final_Mohs_Billing.csv') 
df_all <- read.csv('mohs_billing_all.csv')

# ---- avg codes per year ----
# yearly avg codes per surgeon
df_total_billed <- df_grads %>%
  group_by(Billing_Year, HCPCS_Cd) %>%
  summarize(total_new_grad_billed = sum(Tot_Srvcs))

df_yearly_surgeons <- df_grads %>%
  distinct(NPI, Billing_Year) %>%
  group_by(Billing_Year) %>%
  summarize(total_surgeons = n())

df_yearly_avg_bill <- merge(df_total_billed, df_yearly_surgeons, by = c('Billing_Year')) %>%
  mutate(yearly_avg = round(total_new_grad_billed/total_surgeons, 2))

df_codes_per_surgeon_yearly_table <- df_yearly_avg_bill %>%
  select(Billing_Year, yearly_avg, HCPCS_Cd) %>%
  pivot_wider(
    names_from = Billing_Year, 
    values_from = yearly_avg
  )

# yearly avg total cases per surgeon
df_total_billed1 <- df_grads %>%
  filter(HCPCS_Cd %in% c(17311, 17313)) %>%
  group_by(Billing_Year) %>%
  summarize(total_billed = sum(Tot_Srvcs))

df_yearly_avg_bill_all <- merge(df_total_billed1, df_yearly_surgeons, by = c('Billing_Year')) %>%
  mutate(yearly_avg =  round(total_billed/total_surgeons, 2), 
         HCPCS_Cd = 'Total_Cases')

df_codes_per_surgeon_bill_all_yearly_table <- df_yearly_avg_bill_all %>%
  select(Billing_Year, yearly_avg, HCPCS_Cd) %>%
  pivot_wider(
    names_from = Billing_Year, 
    values_from = yearly_avg
  )

# yearly avg total services per surgeon
df_total_billed2 <- df_grads %>%
  group_by(Billing_Year) %>%
  summarize(total_billed = sum(Tot_Srvcs))

df_yearly_avg_services_all <- merge(df_total_billed2, df_yearly_surgeons, by = c('Billing_Year')) %>%
  mutate(yearly_avg = round(total_billed/total_surgeons, 2), 
         HCPCS_Cd = 'Total_Services')

df_codes_per_surgeon_services_all_yearly_table <- df_yearly_avg_services_all %>%
  select(Billing_Year, yearly_avg, HCPCS_Cd) %>%
  pivot_wider(
    names_from = Billing_Year, 
    values_from = yearly_avg
  )

df_codes_per_surgeon_yearly_table <- rbind(df_codes_per_surgeon_yearly_table, 
                                           df_codes_per_surgeon_bill_all_yearly_table, 
                                           df_codes_per_surgeon_services_all_yearly_table)

########################################
# percent of cases done by new grads
df_total_billed_filtered_cases <- df_grads %>%
  filter(HCPCS_Cd %in% c(17311, 17313)) %>%
  group_by(Billing_Year, HCPCS_Cd) %>%
  summarize(total_new_grad_billed = sum(Tot_Srvcs))

df_all_surgeons_codes <- df_all %>%
  filter(HCPCS_Cd %in% c(17311, 17313)) %>%
  group_by(Billing_Year, HCPCS_Cd) %>%
  summarize(total_surgeons_billed = sum(Tot_Srvcs))

df_all_surgeons_codes <- merge(df_all_surgeons_codes, df_total_billed_filtered_cases, by = c('Billing_Year', 'HCPCS_Cd')) %>%
  mutate(new_grad_bill_pct = total_new_grad_billed/total_surgeons_billed)

df_new_grad_bill_pct <- df_all_surgeons_codes %>%
  group_by(Billing_Year) %>%
  summarize(total_new_grad_billed = sum(total_new_grad_billed), 
            total_surgeons_billed = sum(total_surgeons_billed)) %>%
  ungroup() %>%
  mutate(new_grad_bill_pct = round(total_new_grad_billed/total_surgeons_billed, 4))
         
# year to year new grad percent changes
df_total_billed_changes <- df_total_billed %>%
  group_by(HCPCS_Cd) %>%
  mutate(total_new_grad_billed_lagged = lag(total_new_grad_billed, 1)) %>%
  ungroup() %>%
  mutate(pct_change = round((total_new_grad_billed - total_new_grad_billed_lagged)/total_new_grad_billed_lagged, 4))

df_total_billed_changes_total <- df_total_billed_changes %>%
  filter(HCPCS_Cd %in% c(17311, 17313)) %>%
  group_by(Billing_Year) %>%
  summarize(total_new_grad_billed = sum(total_new_grad_billed), 
            total_new_grad_billed_lagged = sum(total_new_grad_billed_lagged)) %>%
  ungroup() %>%
  mutate(pct_change = round((total_new_grad_billed - total_new_grad_billed_lagged)/total_new_grad_billed_lagged, 4), 
         HCPCS_Cd = 'Total_Cases')

df_total_billed_changes_total <- rbind(df_total_billed_changes, df_total_billed_changes_total)

ggplot(df_total_billed_changes_total, 
       aes(x = Billing_Year, 
           y = pct_change * 100, 
           color = factor(HCPCS_Cd)
       )
) +
  geom_line(linewidth = 0.8) + 
  geom_point() +
  theme_classic() +
  labs(
    y = "Total New Grad Cases Change (Percent)",
    x = "Year",
    color = "Code"
  ) +
  scale_x_continuous(breaks = unique(df_total_billed_changes_total$Billing_Year)) +
  geom_hline(yintercept = 0, color = 'black') +
  scale_color_manual(
    values = c(
      "Total_Cases" = 'gray', 
      "17311" = 'red', 
      "17312" = 'blue', 
      "17313" = 'orange', 
      "17314" = 'yellow'
    )
  )

# avg cases billed post grad
df_bill_trend_surgeons <- df_all %>%
  mutate(year_post_grad = Billing_Year - Graduation.Year) %>%
  distinct(NPI, year_post_grad) %>%
  group_by(year_post_grad) %>%
  summarize(surgeons = n()) %>%
  ungroup %>%
  filter(year_post_grad > 0, 
         !is.na(year_post_grad))

df_bill_trend_billed <- df_all %>%
  mutate(year_post_grad = Billing_Year - Graduation.Year) %>%
  group_by(year_post_grad, HCPCS_Cd) %>%
  summarize(total_billed = sum(Tot_Srvcs)) %>%
  ungroup %>%
  filter(year_post_grad > 0, 
         !is.na(year_post_grad))

df_bill_trend <- merge(df_bill_trend_surgeons, df_bill_trend_billed, by = c('year_post_grad')) %>%
  mutate(avg_billed = round(total_billed/surgeons, 4))

df_bill_trend_total <- df_bill_trend %>%
  filter(HCPCS_Cd %in% c(17311, 17313)) %>%
  group_by(year_post_grad, surgeons) %>%
  summarize(total_billed = sum(total_billed)) %>%
  ungroup() %>%
  mutate(avg_billed = round(total_billed/surgeons, 4), 
         HCPCS_Cd = 'Total_Cases')

df_bill_trend_total <- rbind(df_bill_trend, df_bill_trend_total)

ggplot(df_bill_trend_total, 
       aes(x = year_post_grad, 
           y = avg_billed, 
           color = factor(HCPCS_Cd)
       )
) +
  geom_line(linewidth = 0.8) + 
  geom_point(aes(size = surgeons)) +
  scale_x_continuous(breaks = unique(df_bill_trend_total$year_post_grad)) + 
  theme_classic() +
  labs(
    y = "Avg Billings",
    x = "Years Post Grad",
    color = "Code"
  ) +
  scale_color_manual(
    values = c(
      "Total_Cases" = 'gray', 
      "17311" = 'red', 
      "17312" = 'blue', 
      "17313" = 'orange', 
      "17314" = 'yellow'
    )
  )


# save new csvs
write.csv(df_yearly_avg_bill, "df_yearly_avg_bill.csv")
write.csv(df_yearly_avg_bill_all, "df_yearly_avg_bill_all.csv")
write.csv(df_yearly_avg_charge, "df_yearly_avg_charge.csv")
write.csv(df_yearly_avg_charge_all, "df_yearly_avg_charge_all.csv")

write.csv(df_new_grad_bill_pct, "df_new_grad_bill_pct.csv")

# save excel files
write_xlsx(df_codes_per_surgeon_yearly_table, 'df_codes_per_surgeon_yearly_table.xlsx')
