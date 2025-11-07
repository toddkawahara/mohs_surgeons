library(tidyr)
library(dplyr)
library(ggplot2)
library(usmap)

# load in data
df_state <- read.csv('df_state.csv', row.names = 1)

# clean data, fill in missing states with 0
df_state_table <- df_state %>% 
  mutate(state = Rndrng_Prvdr_State_Abrvtn) %>%
  select(c(Billing_Year, state, surgeons, total_surgeons_year, surgeons_pct)) %>%
  mutate(surgeons_pct = round(surgeons_pct, 4)) %>%
  complete(Billing_Year, state, fill = list(surgeons_pct = 0, 
                                            surgeons = 0)) %>%
  group_by(Billing_Year)

# manually adding delaware since it wasnt in any year
df_state_table <- df_state_table %>%
  bind_rows(
    tibble(
      Billing_Year = unique(df_state_table$Billing_Year),
      state = "DE",
      surgeons = 0,
      surgeons_pct = 0,
      total_surgeons_year = NA
    )) %>%
  fill(total_surgeons_year, .direction = 'downup') %>%
  ungroup() %>%
  arrange(Billing_Year, state)

# get data for all years
df_state_all <- df_state_table %>%
  select(c(state, surgeons, total_surgeons_year)) %>%
  group_by(state) %>%
  summarize(surgeons = sum(surgeons), 
            total_surgeons = sum(total_surgeons_year)) %>%
  ungroup()

# state centroid coordinates for labels
centroid_labels <- usmapdata::centroid_labels("states")
data_labels <- inner_join(centroid_labels, df_state_all, by = c('abbr' = 'state'))

# state map for all years
plot_usmap(data = df_state_all, values = 'surgeons', regions = 'states') + 
  scale_fill_continuous(
    low = 'white', 
    high = 'red', 
    name = 'Surgeons', 
    label = scales::comma
  ) + 
  geom_sf_text(
    data = data_labels, 
    ggplot2::aes(
      label = scales::number(
        surgeons, 
        accuracy = 1
      )
    ), 
    color = "black"
  ) + 
  # labs(
  #   title = '2014-2023 State Surgeons'
  # ) + 
  theme(legend.position = 'right', 
        plot.title = element_text(
          hjust = 0.5, 
          size = 20, 
          face = "bold"
        )
  )

# state maps for individual years
for (year in 2014:2023){
  data_labels <- inner_join(centroid_labels, df_state_table %>% filter(Billing_Year == year), by = c('abbr' = 'state'))
  plot <- plot_usmap(data = filter(df_state_table, Billing_Year == year), values = 'surgeons', regions = 'states') + 
    scale_fill_continuous(
      low = 'white', 
      high = 'red', 
      name = 'Surgeons', 
      label = scales::comma
    ) + 
    geom_sf_text(
      data = data_labels, 
      ggplot2::aes(
        label = scales::number(
          surgeons, 
          accuracy = 1
        )
      ), 
      color = "black"
    ) + 
    labs(
      title = paste0(year, ' State Surgeons')
    ) + 
    theme(legend.position = 'right', 
          plot.title = element_text(
            hjust = 0.5, 
            size = 20, 
            face = "bold"
          )
    )
  print(plot)
}