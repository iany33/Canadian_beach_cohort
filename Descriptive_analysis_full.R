
pacman::p_load(
  rio,
  here,
  tidyverse,
  gtsummary,
  rstatix, 
  janitor, 
  flextable
)

# Examine E. coli results

data |>
  ggplot(aes(x = date, y = e_coli)) +
  geom_point(colour = "black") + 
  geom_line(colour = "steelblue") + 
  theme_minimal() +
  labs(y = "E. coli (CFU / 100 mL)", x = "Recruitment date")

# Examine MST results

data |> 
  select(mst_human2, mst_gull2, date) |> 
  distinct(date, .keep_all=TRUE) |> 
  tbl_summary(digits = list(all_categorical() ~ c(0, 1)),
              type = all_categorical() ~ "categorical")

# Examine dose-response for water contact and AGI outcome

data_follow |> 
  select(agi3, water_contact2) |> 
  tbl_summary(by = water_contact2, digits = list(all_categorical() ~ c(0, 1)),
              type = all_categorical() ~ "categorical")

data_follow |> 
  ggplot(aes(x = agi3, y = e_coli, fill = agi3)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "cividis") +
  labs(x = "Acute gastrointestinal illness (AGI)",
       y = "E. coli geometric mean") +
  facet_wrap(~water_contact2)

data_follow |> 
  ggplot(aes(x = agi3, y = mst_human, fill = agi3)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "cividis") +
  labs(x = "Acute gastrointestinal illness (AGI)",
       y = "HF183 human sewage marker (DNA / 100mL)") +
  facet_wrap(~water_contact2)

data_follow |> 
  filter(water_contact == "Yes") |> 
  ggplot(aes(x = agi3, y = mst_human, fill = agi3)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "cividis") +
  labs(x = "Acute gastrointestinal illness (AGI)",
       y = "HF183 human sewage marker (DNA / 100mL)")

data_follow |> 
  ggplot(aes(x = agi3, y = mst_gull, fill = agi3)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "cividis") +
  facet_wrap(~water_contact2) +
  labs(x = "Acute gastrointestinal illness (AGI)",
       y = "Seagull biomarker (DNA / 100mL)")

data_follow |> 
  filter(water_contact == "Yes") |> 
  ggplot(aes(x = agi3, y = turbidity, fill = agi3)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "cividis") +
  labs(x = "Acute gastrointestinal illness (AGI)",
       y = "Water Turbidity" )

data_follow |> 
  filter(water_contact == "Yes") |> 
  ggplot(aes(x = agi3, y = air_temp_mean, fill = agi3)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "cividis") +
  labs(x = "Acute gastrointestinal illness (AGI)",
       y = "Daily Mean Air Temperature" )

data_follow |> 
  filter(water_contact == "Yes") |> 
  ggplot(aes(x = agi3, y = rainfall_48hr, fill = agi3)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "cividis") +
  labs(x = "Acute gastrointestinal illness (AGI)",
       y = "48-hr Rainfall" )

data_follow |> 
  ggplot(aes(x = agi3, y = prev_day_ecoli, fill = agi3)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "cividis") +
  labs(x = "Acute gastrointestinal illness (AGI)",
       y = "Previous Day E. coli Geometric Mean") +
  facet_wrap(~water_contact2)


# Descriptive summaries of water quality indicators by outcomes

data_follow |> 
  select(agi3, mst_human2) |> 
  tbl_summary(by = mst_human2, digits = list(all_categorical() ~ c(0, 1)),
              type = all_categorical() ~ "categorical")

data_follow |> 
  filter(water_contact == "Yes") |> 
  select(agi3, mst_human2) |> 
  tbl_summary(by = mst_human2, digits = list(all_categorical() ~ c(0, 1)),
              type = all_categorical() ~ "categorical")

data_follow |> 
  filter(water_contact == "Yes") |> 
  select(e_coli, mst_human, mst_gull, rainfall_48hr, air_temp_mean, turbidity, agi3) |> 
  tbl_summary(by = agi3, statistic = list(all_continuous() ~ "{mean} ({sd})"),
                                  digits = all_continuous() ~ 1, missing_text = "Missing")

data_follow |> 
  filter(water_exp_body == "Yes") |> 
  select(e_coli, mst_human, mst_gull, rainfall_48hr, air_temp_mean, turbidity, agi3) |> 
  tbl_summary(by = agi3, statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = all_continuous() ~ 1, missing_text = "Missing")

data_follow |> 
  filter(water_contact == "Yes") |> 
  select(e_coli, mst_human, mst_gull, rainfall_48hr, air_temp_mean, turbidity, illness_any) |> 
  tbl_summary(by = illness_any, statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = all_continuous() ~ 1, missing_text = "Missing")

data_follow |> 
  filter(water_contact == "Yes") |> 
  select(e_coli, mst_human, mst_gull, rainfall_48hr, air_temp_mean, turbidity, skin_infection3) |> 
  tbl_summary(by = skin_infection3, statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = all_continuous() ~ 1, missing_text = "Missing")

data_follow |> 
  filter(water_contact == "Yes") |> 
  select(e_coli, mst_human, mst_gull, rainfall_48hr, air_temp_mean, turbidity, ear_infection3) |> 
  tbl_summary(by = ear_infection3, statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = all_continuous() ~ 1, missing_text = "Missing")

data_follow |> 
  filter(water_contact == "Yes") |> 
  select(e_coli, mst_human, mst_gull, rainfall_48hr, air_temp_mean, turbidity, respiratory3) |> 
  tbl_summary(by = respiratory3, statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = all_continuous() ~ 1, missing_text = "Missing")


# Examine water activities by E. coli levels

data |> select(water_contact, water_act_swim, water_act_wade, water_contact2, ecoli_100) |> 
  tbl_summary(by = ecoli_100, digits = list(all_categorical() ~ c(0, 1)),
              type = all_categorical() ~ "categorical")

data |> select(water_contact, water_act_swim, water_act_wade, water_contact2, ecoli_200) |> 
  tbl_summary(by = ecoli_200, digits = list(all_categorical() ~ c(0, 1)),
              type = all_categorical() ~ "categorical")

# Examine any illness outcomes

data_follow |> 
  select(illness_any, water_contact2) |> 
  tbl_summary(by = water_contact2, digits = list(all_categorical() ~ c(0, 1)),
              type = all_categorical() ~ "categorical")

data_follow |> 
  ggplot(aes(x = illness_any, y = e_coli, fill = illness_any)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "cividis") +
  labs(x = "Any Acute Illness",
       y = "E. coli geometric mean" ) +
  facet_wrap(~water_contact2)

data_follow |> 
  ggplot(aes(x = illness_any, y = mst_human, fill = illness_any)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "cividis") +
  labs(x = "Any Acute Illness",
       y = "HF183 human sewage marker (DNA / 100mL)" ) +
  facet_wrap(~water_contact2)

data_follow |> 
  ggplot(aes(x = illness_any, y = mst_gull, fill = illness_any)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "cividis") +
  labs(x = "Any Acute Illness",
       y = "Seagull biomarker (DNA / 100mL)" ) +
  facet_wrap(~water_contact2)

data_follow |> 
  filter(water_contact == "Yes") |> 
  ggplot(aes(x = illness_any, y = turbidity, fill = illness_any)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "cividis") +
  labs(x = "Any Acute Illness",
       y = "Water Turbidity" ) 

data_follow |> 
  filter(water_contact == "Yes") |> 
  ggplot(aes(x = illness_any, y = air_temp_mean, fill = illness_any)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "cividis") +
  labs(x = "Any Acute Illness",
       y = "Daily Mean Air Temperature" )

data_follow |> 
  filter(water_contact == "Yes") |> 
  ggplot(aes(x = illness_any, y = rainfall_48hr, fill = illness_any)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "cividis") +
  labs(x = "Any Acute Illness",
       y = "48-hr Rainfall" )

# E. coli and human biomarker relationship

data |> 
  distinct(date, .keep_all = TRUE) |> 
  ggplot(aes(x = mst_human, y = e_coli)) +
  geom_point() +
  geom_smooth(method = "lm") 

data |> 
  distinct(date, .keep_all = TRUE) |> 
  ggplot(aes(x = mst_gull, y = e_coli)) +
  geom_point() +
  geom_smooth(method = "lm") 

data |> distinct(date, mst_human, mst_gull, e_coli, rainfall_48hr, air_temp_mean, turbidity) |> 
  select(mst_human, mst_gull, e_coli, rainfall_48hr, air_temp_mean, turbidity) |> 
  corrr::correlate()





