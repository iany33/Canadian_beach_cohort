
pacman::p_load(
  skimr,     
  Matrix,
  tidyverse,   
  gtsummary,  
  rstatix,    
  janitor, 
  flextable, 
  lubridate, 
  forcats    
)

# Descriptive stats for socio-demographic variables and baseline variables

data |> tabyl(age2)
data |> tabyl(sex1)
data |> tabyl(gender)
data |> tabyl(education2)
data |> tabyl(residence)
data |> tabyl(eth_arab)
data |> tabyl(eth_black)
data |> tabyl(eth_east_asian)
data |> tabyl(eth_indigenous)
data |> tabyl(eth_latin)
data |> tabyl(eth_south_asian)
data |> tabyl(eth_se_asian)
data |> tabyl(eth_white)
data |> tabyl(eth_other)
data |> tabyl(eth_other_s)
data |> tabyl(ethnicity)

data |> 
  select(age2, gender, education2, residence, ethnicity) |> 
  tbl_summary(digits = list(all_categorical() ~ c(0, 1))) |> as_flex_table() 

data |> 
  select(age2, gender, education2, residence, ethnicity, month) |> 
  tbl_summary(by = month, digits = list(all_categorical() ~ c(0, 1))) |> as_flex_table() 

data |> group_by(house_id) |>
  summarize(number = n()) |> summarize(mean_house_size = mean(number), count = n())

data |> 
  select(base_agi, base_resp, base_ear, base_eye, base_skin, cond_GI, cond_resp, cond_skin, 
         cond_allergy, cond_immune, cond_none, prev_act1, beach_exp_algae, beach_exp_sunscreen, 
         beach_exp_repellent, beach_exp_food) |> 
  tbl_summary(digits = list(all_categorical() ~ c(0, 1))) 

data |> 
  select(base_agi, base_resp, base_ear, base_eye, base_skin, cond_GI, cond_resp, cond_skin, 
         cond_allergy, cond_immune, cond_none, prev_act1, beach_exp_algae, beach_exp_sunscreen, 
         beach_exp_repellent, beach_exp_food, month) |> 
  tbl_summary(by = month, digits = list(all_categorical() ~ c(0, 1))) |> as_flex_table() 

data |> 
  select(water_contact, water_act_swim, water_act_surf, water_act_kite, water_act_wind,
         water_act_wake, water_act_ski, water_act_paddle, water_act_snorkel, water_act_dive,
         water_act_wade, water_act_sail, water_act_boat, water_act_fish, water_act_canoe,
         water_act_kayak, water_act_other, water_exp_body, water_exp_head, water_exp_mouth, 
         sand_contact, sand_mouth1, sand_act_dig, sand_act_bury) |> 
  tbl_summary(digits = list(all_categorical() ~ c(0, 1)))

data |> 
  select(water_contact, water_act_swim, water_act_surf, water_act_kite, water_act_wind,
         water_act_wake, water_act_ski, water_act_paddle, water_act_snorkel, water_act_dive,
         water_act_wade, water_act_sail, water_act_boat, water_act_fish, water_act_canoe,
         water_act_kayak, water_act_other, water_exp_body, water_exp_head, water_exp_mouth, 
         sand_contact, sand_act_dig, sand_act_bury, sand_mouth1, month) |> 
  tbl_summary(by = month, digits = list(all_categorical() ~ c(0, 1))) |> 
  as_flex_table() 

## E. coli levels

data |>
  ggplot(aes(x = e_coli)) +
  geom_histogram(bins = 20, fill = "#3b528b") +                    
  theme_minimal() +
  labs(x = "E. coli geometric mean (CFU / 100 mL)", y = "Number of participants")

data |>
  ggplot(aes(x = e_coli)) +
  geom_bar(fill = "#3b528b") +                    
  theme_minimal() +
  labs(x = "E. coli geometric mean (CFU / 100 mL)", y = "Number of participants")

data |> rstatix::get_summary_stats(
  e_coli, log_ecoli, type = "common")

data |>
  ggplot(aes(x = date, y = e_coli)) +
  geom_point(colour = "black") + 
  geom_line(colour = "#3b528b") + 
  theme_minimal() +
  labs(y = "E. coli geometric mean (CFU / 100 mL)", x = "Recruitment date")

data |> group_by(date) |> mutate(count = n()) |> 
  ggplot(aes(x = date, y = count)) +
  geom_point(colour = "black") + 
  geom_line(colour = "#3b528b") + 
  theme_minimal() +
  labs(y = "Number of participants", ye = "Recruitment date")


## Summarize follow-up survey results


data_grouped <- data |> group_by(house_id) |> 
  summarize(n_cases  = n(), n_follow = sum(follow==1, na.rm=T),
            follow_percent = sum((n_follow/n_cases)*100))

data_follow <- data[ which(data$follow == "Yes"), ]

data |> tabyl(follow)
data |> tabyl(preference, follow) |> adorn_totals("row") |> adorn_totals("col") 
data |> tabyl(preference, follow) |> adorn_percentages("row") 

data_follow |> 
  select(agi, diarrhea, respiratory, ear_infection, eye_infection, skin_infection, misswork, 
         med_antibiotics, med_otc, med_none, healthcare1, blood_stool1, stool_test1, emergency, hospital) |> 
  tbl_summary(digits = list(all_categorical() ~ c(0, 1)))

## Exclude those with baseline symptoms

data_follow |> 
  select(agi3, diarrhea3, respiratory3, ear_infection3, eye_infection3, skin_infection3, misswork, 
         med_antibiotics, med_otc, med_none, healthcare1, blood_stool1, stool_test1, emergency, hospital) |> 
  tbl_summary(digits = list(all_categorical() ~ c(0, 1)))

## Outcomes by water and sand exposure status - raw data

data_follow |> 
  select(agi, diarrhea, respiratory, ear_infection, eye_infection, skin_infection, water_contact) |> 
  tbl_summary(by = water_contact) 

data_follow |> 
  select(agi, diarrhea, respiratory, ear_infection, eye_infection, skin_infection, sand_contact) |> 
  tbl_summary(by = sand_contact)

## Outcomes by water and sand exposure status - excluding those with same outcomes at baseline

data_follow |> 
  select(agi3, diarrhea3, respiratory3, ear_infection3, eye_infection3, skin_infection3, water_contact) |> 
  tbl_summary(by = water_contact, digits = list(all_categorical() ~ c(0, 1))) 

data_follow |> 
  select(agi3, diarrhea3, respiratory3, ear_infection3, eye_infection3, skin_infection3, sand_contact) |> 
  tbl_summary(by = sand_contact, digits = list(all_categorical() ~ c(0, 1)))


## Compare sociodemographics of those who completed follow-up survey vs. not

data |> 
  select(age2, gender, education2, residence, ethnicity, base_agi, base_resp, base_ear, base_eye, base_skin, 
         cond_GI, cond_resp, cond_skin, cond_allergy, cond_immune, cond_none, prev_act1, water_contact, water_act_swim, 
         water_act_wade, water_exp_body, water_exp_head, water_exp_mouth, sand_contact, sand_act_dig, sand_act_bury, 
         sand_mouth1, beach_exp_algae, beach_exp_sunscreen, beach_exp_repellent, beach_exp_food, follow) |> 
  tbl_summary(by = follow, digits = list(all_categorical() ~ c(0, 1))) 


## Cross-tab water & sand activities by sociodemographics

data |> 
  select(water_contact, water_act_swim, water_act_wade, water_exp_face, water_exp_mouth, sand_contact, sand_act_dig, 
         sand_act_bury, sand_mouth1, age2) |> 
  tbl_summary(by = age2, digits = list(all_categorical() ~ c(0, 1))) 

data |> 
  select(water_contact, water_act_swim, water_act_wade, water_exp_face, water_exp_mouth, sand_contact, sand_act_dig, 
         sand_act_bury, sand_mouth1, gender) |> 
  tbl_summary(by = gender, digits = list(all_categorical() ~ c(0, 1)))

data |> 
  select(water_contact, water_act_swim, water_act_wade, water_exp_face, water_exp_mouth, sand_contact, sand_act_dig, 
         sand_act_bury, sand_mouth1, education2) |> 
  tbl_summary(by = education2, digits = list(all_categorical() ~ c(0, 1))) 
data |> 
  select(water_contact, water_act_swim, water_act_wade, water_exp_face, water_exp_mouth, sand_contact, sand_act_dig, 
         sand_act_bury, sand_mouth1, ethnicity) |> 
  tbl_summary(by = ethnicity, digits = list(all_categorical() ~ c(0, 1)))





