library(tidyr)
library(dplyr)
library(ggplot2)

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

# getting avg billings per surgeon
df_billing_services <- df_billing %>%
  group_by(Billing_Year) %>%
  mutate(total_services = sum(Tot_Srvcs)) %>%
  ungroup() %>%
  group_by(Billing_Year, HCPCS_Cd) %>%
  mutate(code_services = sum(Tot_Srvcs)) %>%
  ungroup() %>%
  group_by(Billing_Year) %>%
  mutate(total_surgeons = n_distinct(NPI)) %>%
  ungroup() %>%
  distinct(Billing_Year, HCPCS_Cd, code_services, total_services, total_surgeons) %>%
  mutate(code_avg_services = code_services/total_surgeons, 
         total_avg_services = total_services/total_surgeons)

# filtering to individual codes
df_billing_services_17311 <- df_billing_services %>% filter(HCPCS_Cd == 17311)
df_billing_services_17312 <- df_billing_services %>% filter(HCPCS_Cd == 17312)
df_billing_services_17313 <- df_billing_services %>% filter(HCPCS_Cd == 17313)
df_billing_services_17314 <- df_billing_services %>% filter(HCPCS_Cd == 17314)
df_billing_services_all <- df_billing_services %>% distinct(Billing_Year, total_services, total_surgeons, total_avg_services)

# running regressions for codes
model_17311 <- lm(code_avg_services ~ Billing_Year, data = df_billing_services_17311)
model_17312 <- lm(code_avg_services ~ Billing_Year, data = df_billing_services_17312)
model_17313 <- lm(code_avg_services ~ Billing_Year, data = df_billing_services_17313)
model_17314 <- lm(code_avg_services ~ Billing_Year, data = df_billing_services_17314)
model_all <- lm(total_avg_services ~ Billing_Year, data = df_billing_services_all)

# model summaries
summary(model_17311)
summary(model_17312)
summary(model_17313)
summary(model_17314)
summary(model_all)

# individual trend plot labels
p_values = c(0.951, 0.011, 0.007, 0.835)

label_positions <- df_billing_services %>%
  filter(Billing_Year == 2023) %>%
  mutate(p_values = p_values) %>%
  select(HCPCS_Cd, Billing_Year, code_avg_services, p_values)

# individual trend plot
ggplot(df_billing_services, 
       aes(x = Billing_Year, 
           y = code_avg_services, 
           color = factor(HCPCS_Cd)
       )
  ) +
  geom_line(linewidth = 0.8) + 
  geom_smooth(method = 'lm', 
              se = FALSE, 
              linetype = 'longdash', 
              linewidth = 0.825
  ) +
  geom_point() +
  geom_text(data = label_positions, 
            aes(label = paste0('trend p-value: ', p_values)), 
            hjust = 1, vjust = -2, size = 3.2
  ) +
  theme_classic() +
  labs(
    title = "Average Code Services per Year",
    y = "Services",
    x = "Year",
    color = "Code"
  )

# total trend plot labels
label_positions_total <- df_billing_services %>%
  slice(n()) %>%
  select(Billing_Year, total_avg_services) %>%
  mutate(p_value = 0.391)

# total trend plot
ggplot(df_billing_services %>% distinct(Billing_Year, total_avg_services), 
       aes(x = Billing_Year, 
           y = total_avg_services
       )
  ) + 
  geom_line(linewidth = 0.9) + 
  geom_smooth(method = 'lm', 
              color = 'black',
              se = FALSE, 
              linetype = 'longdash', 
              linewidth = 0.825
  ) +
  geom_point() +
  geom_text(data = label_positions_total, 
            aes(label = paste0('trend p-value: ', p_value)), 
            hjust = 1, vjust = 10, size = 3.2
  ) +
  theme_classic() +
  labs(
    title = "Average Total Services per Year",
    y = "Services",
    x = "Year",
    color = "Code"
  )