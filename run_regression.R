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
df_billing_services_17311_17313 <- df_billing_services %>% filter(HCPCS_Cd == 17311 | HCPCS_Cd == 17313) %>%
  select(-c(total_services, code_avg_services, total_avg_services)) %>%
  group_by(Billing_Year, total_surgeons) %>%
  summarize(code_services = sum(code_services)) %>%
  ungroup() %>%
  mutate(code_avg_services = code_services/total_surgeons, 
         HCPCS_Cd = '17311_17313') %>%
  select(HCPCS_Cd, everything())

# save 17311+17313 df
write.csv(df_billing_services_17311_17313, 'df_billing_services_17311_17313.csv', row.names = FALSE)

# running regressions for codes
model_17311 <- lm(code_avg_services ~ Billing_Year, data = df_billing_services_17311)
model_17312 <- lm(code_avg_services ~ Billing_Year, data = df_billing_services_17312)
model_17313 <- lm(code_avg_services ~ Billing_Year, data = df_billing_services_17313)
model_17314 <- lm(code_avg_services ~ Billing_Year, data = df_billing_services_17314)
model_all <- lm(total_avg_services ~ Billing_Year, data = df_billing_services_all)
model_17311_17313 <- lm(code_avg_services ~ Billing_Year, data = df_billing_services_17311_17313)

# model summaries
summary(model_17311)
summary(model_17312)
summary(model_17313)
summary(model_17314)
summary(model_all)
summary(model_17311_17313)

# ---- plots ----


# individual trend plot labels
p_values = c(0.951, 0.011, 0.007, 0.835)

label_positions <- df_billing_services %>%
  filter(Billing_Year == 2023) %>%
  mutate(p_values = p_values) %>%
  select(HCPCS_Cd, Billing_Year, code_avg_services, p_values)

# total trend plot labels
label_positions_total <- df_billing_services %>%
  slice(n()) %>%
  select(Billing_Year, total_avg_services) %>%
  mutate(p_values = 0.391, 
         HCPCS_Cd = 'Total') %>%
  rename(code_avg_services = total_avg_services)

# combine labels
label_positions_combined = rbind(label_positions, label_positions_total)

# combine total and individual code data
df_billing_services_all <- df_billing_services_all %>%
  mutate(HCPCS_Cd = 'Total', 
         code_services = total_services, 
         code_avg_services = total_avg_services)
df_billing_services <- rbind(df_billing_services, df_billing_services_all)

#  trend plot
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
  geom_text(data = label_positions_combined,
            aes(label = paste0('trend p-value: ', p_values)),
            hjust = 1, vjust = -1, size = 3.2,
            color = 'black'
  ) +
  theme_classic() +
  labs(
    # title = "Average Code Services per Year",
    y = "Services",
    x = "Year",
    color = "Code"
  ) +
  scale_color_manual(
    values = c(
      "Total" = 'black', 
      "17311" = 'red', 
      "17312" = 'blue', 
      "17313" = 'orange', 
      "17314" = 'darkgreen'
    )
  ) +
  scale_y_continuous(limits = c(0, 600))

# 17311 17313 trend plot labels
label_positions_17311_17313 <- df_billing_services_17311_17313 %>%
  slice(n()) %>%
  select(Billing_Year, code_avg_services) %>%
  mutate(p_value = 0.455)

# 17311 17313 trend plot
ggplot(df_billing_services_17311_17313, 
       aes(x = Billing_Year, 
           y = code_avg_services
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
  geom_text(data = label_positions_17311_17313, 
            aes(label = paste0('trend p-value: ', p_value)), 
            hjust = 1, vjust = -1, size = 3.2
  ) +
  theme_classic() +
  labs(
    # title = "17311 and 17313 Code Avg Services per Year",
    y = "Cases",
    x = "Year",
    color = "Code"
  ) +
  scale_y_continuous(limits = c(0, 600))
