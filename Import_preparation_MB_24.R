
pacman::p_load(
  rio,    
  here,  
  skimr,
  Matrix,
  tidyverse, 
  lubridate,
  janitor,
  data.table,
  uuid,
  psych
)

beach <- import(here("Datasets", "Manitoba", "2024-beach-Manitoba.xlsx"))
follow <- import(here("Datasets", "Manitoba", "2024-follow-Manitoba.xlsx"))

e_coli <- import(here("Datasets", "Manitoba", "2024_e_coli_Manitoba.xlsx"))

mst <- import(here("Datasets", "Manitoba", "2024_molecular_Manitoba.xlsx"))

# Clean variable names

beach <- beach  |> clean_names()
follow <- follow |> clean_names()

# Remove dog and algal bloom survey symptoms

beach <- beach |> 
  select(-pets, -pets1, -dog_name:-dog_sand5) |> 
  select(-matches("^symptoms.*breathing$")) |> 
  select(-matches("^symptoms.*fatigue$")) |> 
  select(-matches("^symptoms.*dizzy$")) |> 
  select(-matches("^symptoms.*appetite$")) |> 
  select(-matches("^symptoms.*chills$")) |> 
  select(-matches("^symptoms.*muscles$")) |> 
  select(-matches("^symptoms.*pain$")) |> 
  select(-matches("^symptoms.*head$"))

follow <- follow |> 
  select(-pet_participation, -dog_name:-dog_hospital5) |> 
  select(-matches("^symptoms.*breathing$")) |> 
  select(-matches("^symptoms.*fatigue$")) |> 
  select(-matches("^symptoms.*dizzy$")) |> 
  select(-matches("^symptoms.*appetite$")) |> 
  select(-matches("^symptoms.*chills$")) |> 
  select(-matches("^symptoms.*muscles$")) |> 
  select(-matches("^symptoms.*pain$")) |> 
  select(-matches("^symptoms.*head$"))

# Combine household member answers together then split into multiple rows per respondent

beach <- beach |> mutate(others10 = NA)

beach <- beach |> 
  unite(name, c("firstname", "lastname"), sep=" ") |> 
  unite(name1, starts_with("name"), sep=",") |> 
  unite(age1, starts_with("age"), sep=",") |> 
  unite(sex1, c("sex", num_range("sex", 2:10)), sep=",") |>
  unite(sex_other, ends_with("other_sex"), sep=",")  |>   
  unite(gender1, c("gender", num_range("gender", 2:10)), sep=",") |>
  unite(gender_other, ends_with("other_gender"), sep=",") |>  
  unite(ethnicity_arab, ends_with("arab"), sep=",") |> 
  unite(ethnicity_black, ends_with("black"), sep=",") |> 
  unite(ethnicity_se_asian, ends_with("southeast_asian"), sep=",") |> 
  unite(ethnicity_east_asian, ends_with("east_asian"), sep=",") |> 
  unite(ethnicity_indigenous, starts_with("indigenous"), sep=",") |> 
  unite(ethnicity_latin, ends_with("latin"), sep=",") |> 
  unite(ethnicity_south_asian, ends_with("south_asian"), sep=",") |> 
  unite(ethnicity_white, ends_with("white"), sep=",") |> 
  unite(ethnicity_other, c("ethnicityother_eth_37", "ethnicity2other_eth_116", "ethnicity3other_eth_195", "ethnicity4other_eth_274", "ethnicity5other_eth_353", "ethnicity6other_eth_432", "ethnicity7other_eth_511", "ethnicity8other_eth_590", "ethnicity9other_eth_669", "ethnicity10other_eth_748"), sep=",") |> 
  unite(ethnicity_other_s, c("ethnicityother_eth_38", "ethnicity2other_eth_117", "ethnicity3other_eth_196", "ethnicity4other_eth_275", "ethnicity5other_eth_354", "ethnicity6other_eth_433","ethnicity7other_eth_512", "ethnicity8other_eth_591", "ethnicity9other_eth_670", "ethnicity10other_eth_749"), sep=",") |>
  unite(ethnicity_na, matches("^ethnicity.*na$"), sep=",") |> 
  unite(base_symp_diar, ends_with("diarrhea"), sep=",") |> 
  unite(base_symp_vomit, ends_with("vomiting"), sep=",") |> 
  unite(base_symp_cramps, ends_with("cramps"), sep=",") |> 
  unite(base_symp_naus, ends_with("nausea"), sep=",") |> 
  unite(base_symp_fever, ends_with("fever"), sep=",") |> 
  unite(base_symp_throat, ends_with("throat"), sep=",") |> 
  unite(base_symp_nose, ends_with("nose"), sep=",") |> 
  unite(base_symp_cough, ends_with("cough"), sep=",") |> 
  unite(base_symp_ear, ends_with("ear"), sep=",") |> 
  unite(base_symp_eye, ends_with("eye"), sep=",") |> 
  unite(base_symp_rash, ends_with("rash"), sep=",") |> 
  unite(base_symp_none, matches("^symptoms.*none$"), sep=",") |> 
  unite(cond_GI, ends_with("gi"), sep=",") |> 
  unite(cond_resp, ends_with("respiratory"), sep=",") |> 
  unite(cond_skin, ends_with("skin"), sep=",") |> 
  unite(cond_allergy, ends_with("allergies"), sep=",")  |> 
  unite(cond_immune, ends_with("immune"), sep=",")  |>
  unite(cond_none, matches("^conditions.*none$"), sep=",")  |>
  unite(cond_na, matches("^conditions.*na$"), sep=",")  |>
  unite(prev_act1, starts_with("prev_act"), sep=",") |>
  unite(water_contact, starts_with("swam"), sep=",") |> 
  unite(water_act_swim, ends_with("swim"), sep=",") |> 
  unite(water_act_surf, ends_with("surf"), sep=",") |> 
  unite(water_act_kite, ends_with("kite"), sep=",") |> 
  unite(water_act_wind, ends_with("wind"), sep=",") |> 
  unite(water_act_wake, ends_with("wake"), sep=",") |> 
  unite(water_act_ski, ends_with("ski"), sep=",") |> 
  unite(water_act_paddle, ends_with("paddle"), sep=",") |> 
  unite(water_act_snorkel, ends_with("snorkel"), sep=",") |> 
  unite(water_act_dive, ends_with("dive"), sep=",") |> 
  unite(water_act_wade, ends_with("wade"), sep=",") |> 
  unite(water_act_sail, ends_with("sail"), sep=",") |> 
  unite(water_act_boat, ends_with("boat"), sep=",") |> 
  unite(water_act_fish, ends_with("fish"), sep=",") |> 
  unite(water_act_canoe, ends_with("canoe"), sep=",") |> 
  unite(water_act_kayak, ends_with("kayak"), sep=",") |> 
  unite(water_act_other, c("water_actother_84", "water_act2other_163", "water_act3other_242", "water_act4other_321", "water_act5other_400", "water_act6other_479", "water_act7other_558", "water_act8other_637", "water_act9other_716", "water_act10other_795"), sep=",") |>   
  unite(water_act_other_s, c("water_actother_85", "water_act2other_164", "water_act3other_243", "water_act4other_322", "water_act5other_401", "water_act6other_480", "water_act7other_559", "water_act8other_638", "water_act9other_717", "water_act10other_796"), sep=",") |>  
  unite(water_exp_body, ends_with("face"), sep=",") |> 
  unite(water_exp_head, ends_with("head"), sep=",") |> 
  unite(water_exp_mouth, matches("^water_exp.*mouth$"), sep=",") |> 
  unite(water_exp_neither, ends_with("neither"), sep=",") |> 
  unite(water_time, starts_with("water_time"), sep=",") |> 
  unite(beach_exp_algae, ends_with("algae"), sep=",") |> 
  unite(beach_exp_sun, ends_with("sun"), sep=",") |> 
  unite(beach_exp_rep, ends_with("repellent"), sep=",") |> 
  unite(beach_exp_food, ends_with("food"), sep=",") |> 
  unite(sand1, c("sand", num_range("sand", 2:10)), sep=",") |> 
  unite(sand_act_dig, ends_with("dig"), sep=",") |> 
  unite(sand_act_bury, ends_with("bury"), sep=",") |> 
  unite(sand_act_other, c("sand_actother_99", "sand_act2other_178", "sand_act3other_257", "sand_act4other_336", "sand_act5other_415", "sand_act6other_494", "sand_act7other_573", "sand_act8other_652", "sand_act9other_731", "sand_act10other_810"), sep=",") |>   
  unite(sand_act_other_s, c("sand_actother_100", "sand_act2other_179", "sand_act3other_258", "sand_act4other_337", "sand_act5other_416", "sand_act6other_495", "sand_act7other_574", "sand_act8other_653", "sand_act9other_732", "sand_act10other_811"), sep=",") |> 
  unite(sand_mouth1, starts_with("sand_mouth"), sep=",")  |> 
  unite(others, starts_with("others"), sep=",")

beach <- beach |> 
  separate_rows(name1, age1, sex1, sex_other, gender1, gender_other, ethnicity_arab, ethnicity_black, 
                ethnicity_east_asian, ethnicity_indigenous, ethnicity_latin, ethnicity_south_asian, 
                ethnicity_se_asian, ethnicity_white, ethnicity_other, ethnicity_other_s, ethnicity_na, 
                base_symp_diar, base_symp_vomit, base_symp_cramps, base_symp_naus, base_symp_fever, 
                base_symp_throat, base_symp_nose, base_symp_cough, base_symp_ear, base_symp_eye, 
                base_symp_rash, base_symp_none, cond_GI, cond_resp, cond_skin, cond_allergy, 
                cond_immune, cond_none, cond_na, prev_act1, water_contact, water_act_swim, water_act_surf, 
                water_act_kite, water_act_wind, water_act_wake, water_act_ski, water_act_paddle, 
                water_act_snorkel, water_act_dive, water_act_wade, water_act_sail, water_act_boat, 
                water_act_fish, water_act_canoe, water_act_kayak, water_act_other, water_act_other_s, 
                water_time, water_exp_body, water_exp_head, water_exp_mouth, water_exp_neither, 
                beach_exp_algae, beach_exp_sun, beach_exp_rep, beach_exp_food, sand1, sand_act_dig, 
                sand_act_bury, sand_act_other, sand_act_other_s, sand_mouth1, others, sep=",") 

beach <- beach[!(beach$name1=="NA"),]

follow <- follow |> mutate(others10 = NA)

follow <- follow |> 
  unite(household_name, c("first_name", "last_name"), sep=" ") |> 
  unite(name, c("fname", "lname"), sep=" ") |> 
  unite(name1, starts_with("name"), sep=",") |> 
  unite(rec_act1, starts_with("rec_act"), sep=",") |> 
  unite(symptoms_diar, ends_with("diarrhea"), sep=",") |>  
  unite(symptoms_vomit, ends_with("vomiting"), sep=",") |>  
  unite(symptoms_cramps, ends_with("cramps"), sep=",") |> 
  unite(symptoms_naus, ends_with("nausea"), sep=",") |>  
  unite(symptoms_fever, ends_with("fever"), sep=",") |> 
  unite(symptoms_throat, ends_with("throat"), sep=",") |> 
  unite(symptoms_nose, ends_with("nose"), sep=",") |> 
  unite(symptoms_cough, ends_with("cough"), sep=",") |> 
  unite(symptoms_ear, ends_with("ear"), sep=",") |> 
  unite(symptoms_eye, ends_with("eye"), sep=",") |>  
  unite(symptoms_rash, ends_with("rash"), sep=",") |> 
  unite(symptoms_none, matches("^symptoms.*none$"), sep=",") |>  
  unite(symp_date, starts_with("symp_start"), sep=",") |> 
  unite(misswork, c("misswork", num_range("misswork", 2:10)), sep=",") |> 
  unite(misswork_days, matches("^misswork.*1$"), sep=",") |> 
  unite(med_antibiotics, ends_with("antibiotics"), sep=",") |> 
  unite(med_otc, ends_with("drugs"), sep=",") |> 
  unite(med_none, matches("^medication.*none$"), sep=",")  |> 
  unite(healthcare1, starts_with("healthcare"), sep=",")  |>
  unite(blood_stool1, starts_with("blood_stool"), sep=",")  |>
  unite(stool_test1, starts_with("stool_test"), sep=",")  |>
  unite(emergency, starts_with("ed_visit"), sep=",")  |>
  unite(hospital, starts_with("hospitalized"), sep=",") |>
  unite(others_follow, starts_with("others"), sep=",")

follow <- follow |> 
  separate_rows(name1, rec_act1, symptoms_diar, symptoms_vomit, symptoms_cramps, 
                symptoms_naus, symptoms_fever, symptoms_throat, symptoms_nose, 
                symptoms_cough, symptoms_ear, symptoms_eye, symptoms_rash,
                symptoms_none, symp_date, misswork, misswork_days, med_antibiotics, 
                med_otc, med_none, healthcare1, blood_stool1, stool_test1,
                emergency, hospital, others_follow, sep=",") 

follow <- follow[!(follow$name1=="NA"),]

# Merge survey datasets

beach <- beach |> 
  rename(house_id = internal_id) |> 
  rename(date = accessed_date) 

survey_data <- left_join(beach, follow, by = "name1")

survey_data <- survey_data |> 
  mutate(date = as.Date(date))  |> 
  mutate(month = as.factor(month(date))) |> 
  mutate(dow = as.factor(wday(date))) # Sunday is 1, Sat. is 7

# Check for duplicate names 

survey_data |> group_by(name1) |> filter(n()>1) 

## Check for any follow-up participants that did not match to beach participants

follow |> anti_join(beach, by = "name1")
investigate <- follow |> anti_join(beach, by = "name1")

## Load, Format and Merge E. coli data

e_coli <- e_coli |> 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) 

MC_data <- import(here("Datasets", "2023-MC.xlsx"))
SS_data <- import(here("Datasets", "2023-SS.xlsx"))

MC_data <- MC_data |> 
  group_by(date) |> 
  mutate(e_coli = geometric.mean(as.numeric(e_coli), na.rm = TRUE)) |> 
  distinct(date, e_coli) |> 
  ungroup()

MC_data <- MC_data |> 
  mutate(prev_day_ecoli = lag(as.numeric(e_coli))) |> 
  mutate(date = as.Date(date, format = "%m/%d/%Y")) 

SS_data <- SS_data |> 
  group_by(date) |> 
  mutate(e_coli = geometric.mean(as.numeric(e_coli), na.rm = TRUE)) |> 
  distinct(date, e_coli) |> 
  ungroup()

SS_data <- SS_data |> 
  mutate(prev_day_ecoli = lag(as.numeric(e_coli))) |> 
  mutate(date = as.Date(date, format = "%m/%d/%Y")) 

e_coli1 <- e_coli |> filter(beach == "Marie Curtis") |> 
  left_join(MC_data, by = "date")

e_coli2 <- e_coli |> filter(beach == "Sunnyside") |> 
  left_join(SS_data, by = "date") 

e_coli <- rbind(e_coli1, e_coli2)

e_coli <- e_coli |> arrange(date)

remove(MC_data, SS_data, e_coli1, e_coli2)

survey_data <- left_join(survey_data, e_coli, by = "date")

# Format MST data

mst <- mst |> 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) |> 
  mutate_all(function(x) gsub("BD", 0, x)) |> 
  mutate(HF183_human = as.numeric(HF183_human)) |> 
  mutate(Gull4_marker = as.numeric(Gull4_marker)) 
    
mst <- mst |> 
  group_by(date) |> 
  summarize(mst_human = mean(HF183_human),
            mst_gull = mean(Gull4_marker)) |> 
  ungroup()

mst <- mst |> 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) 

survey_data <- left_join(survey_data, mst, by = "date")

## Reformat participants in survey that participated more than once 

survey_data <- survey_data |> 
  mutate(name1 = str_remove_all(survey_data$name1, "[0-9]$"))

investigate <- survey_data |> 
  add_count(name1) |> 
  filter(n!=1) |> 
  select (-n)

survey_data <- survey_data |>
  group_by(name1) |>
  mutate(house_id = first(house_id)) |>
  ungroup() 

## Replace name column with unique/random ID - drop email, phone

survey_data$row_id <- 1:nrow(survey_data)

survey_data <- survey_data |> 
  group_by(name1) |> 
  mutate(participant_id = cur_group_id()) |> 
  ungroup() |> 
  select(-name1, -household_name, participant_id) |> 
  relocate(participant_id, .after = house_id)

survey_data$participant_id <- paste("MB_2024", survey_data$participant_id, sep = "_")

survey_data <- survey_data |> 
  relocate(row_id, .after = participant_id)

data_TO <- subset(survey_data, select = -c(email.x, email.y, phone))

remove(beach, follow, investigate, survey_data)


