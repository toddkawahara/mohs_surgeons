library(tidyr)
library(dplyr)
library(ggplot2)
library(gt)
library(writexl)

# loading data
df_billing <- read.csv('Final_Mohs_Billing.csv', row.names = 1) %>%
  # filter to first year out of practice
  filter(Billing_Year == Graduation.Year + 1) %>%
  # group areas together
  mutate(
    Rndrng_Prvdr_RUCA_Desc = case_when(
      Rndrng_Prvdr_RUCA_Desc == "Metropolitan area core: primary flow within an urbanized area of 50,000 and greater" ~ 'Metropolitan',
      Rndrng_Prvdr_RUCA_Desc == "Micropolitan area core: primary flow within an urban cluster of 10,000 to 49,999" ~ 'Micropolitan', 
      Rndrng_Prvdr_RUCA_Desc == "Secondary flow 30% to <50% to a larger urbanized area of 50,000 and greater" ~ 'Metropolitan', 
      Rndrng_Prvdr_RUCA_Desc == "Metropolitan area high commuting: primary flow 30% or more to a urbanized area of 50,000 and greater" ~ 'Metropolitan', 
      Rndrng_Prvdr_RUCA_Desc == "Micropolitan high commuting: primary flow 30% or more to a urban cluster of 10,000 to 49,999" ~ 'Micropolitan', 
      Rndrng_Prvdr_RUCA_Desc == "Rural areas: primary flow to a tract outside a urbanized area of 50,000 and greater or UC" ~ 'Rural', 
      Rndrng_Prvdr_RUCA_Desc == "Secondary flow 30% to <50% to a urbanized area of 50,000 and greater" ~ 'Micropolitan', 
      Rndrng_Prvdr_RUCA_Desc == "Unknown" ~ 'Unknown'
    )
  )

# getting yearly surgeons in each office
df_office_codes_surgeons <- df_billing %>%
  distinct(NPI, Billing_Year, Place_Of_Srvc) %>%
  group_by(Billing_Year, Place_Of_Srvc) %>%
  mutate(total_surgeons = n()) %>%
  ungroup() %>%
  distinct(Billing_Year, total_surgeons, Place_Of_Srvc)

# getting yearly services in each office
df_office_codes_services <- df_billing %>%
  group_by(Billing_Year, Place_Of_Srvc) %>%
  summarize(total_billings = sum(Tot_Srvcs)) %>%
  ungroup()

# getting yearly average services in each office
df_office_codes_per_surgeon_yearly <- inner_join(df_office_codes_surgeons, df_office_codes_services, by = c('Billing_Year', 'Place_Of_Srvc')) %>%
  mutate(codes_per_surgeon = total_billings/total_surgeons)

# getting total average services in each office
df_office_codes_per_surgeon_total <- df_office_codes_per_surgeon_yearly %>%
  group_by(Billing_Year) %>%
  summarize(total_surgeons = sum(total_surgeons), 
            total_billings = sum(total_billings)) %>%
  ungroup() %>%
  mutate(codes_per_surgeon = total_billings/total_surgeons)

# get yearly total row for offices
df_office_total <- df_office_codes_per_surgeon_yearly %>%
  group_by(Billing_Year) %>%
  summarize(
    Place_Of_Srvc = 'Total',
    total_surgeons = sum(total_surgeons), 
    total_billings = sum(total_billings), 
    codes_per_surgeon = total_billings/total_surgeons, 
    .groups = 'drop'
  )
# transform to pivot table and concat
df_office_codes_full <- rbind(df_office_codes_per_surgeon_yearly, df_office_total) %>%
  arrange(Billing_Year) %>%
  select(-c('total_surgeons', 'total_billings')) %>%
  pivot_wider(names_from = Billing_Year, values_from = codes_per_surgeon) %>%
  mutate(Place_Of_Srvc = recode(
    Place_Of_Srvc, 
    'F' = 'Inpatient', 
    'O' = 'Outpatient'
  )) %>%
  rename('Inpatient/Outpatient' = Place_Of_Srvc)

# get office codes table
df_office_codes_full %>%
  gt() %>%
  tab_header(
    title = "Office Billing Codes per Surgeon"
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(3)   # make it bold/thick
    ),
    locations = cells_body(
      rows = nrow(df_office_codes_full) - 1       # applies to the last row
    )
  )

# saving csvs
write.csv(df_office_codes_per_surgeon_yearly, 'df_office_codes_per_surgeon_yearly.csv')
write.csv(df_office_codes_per_surgeon_total, 'df_office_codes_per_surgeon_total.csv')
# save excel file
write_xlsx(df_office_codes_full, 'df_office_codes_table.xlsx')


# office yearly billings per surgeon plot
data_office_plot <- df_office_codes_full %>%
  pivot_longer(
    cols = -`Inpatient/Outpatient`, 
    names_to = 'year', 
    values_to = 'value'
  )

ggplot(data_office_plot %>%
         mutate(`Inpatient/Outpatient` = factor(`Inpatient/Outpatient`, levels = c('Outpatient', 'Total', 'Inpatient'))), 
       aes(x=year, y = value, color = `Inpatient/Outpatient`, group = `Inpatient/Outpatient`)) +
  geom_line(size = 1) +
  geom_point(size = 2) + 
  scale_color_brewer(palette = 'Set1') +
  labs(
    title = 'Yearly Practice Setting Billings Per Surgeon', 
    x = 'Year', 
    y = 'Billings per Surgeon', 
    color = 'Group'
  ) +
  theme_classic() + 
  theme(
    panel.grid = element_blank()
  )
