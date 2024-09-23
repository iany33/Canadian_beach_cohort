
pacman::p_load(
  rio, 
  here,
  skimr,        # get overview of data
  Matrix,
  tidyverse,    # data management + ggplot2 graphics, 
  gtsummary,    # summary statistics and tests
  rstatix,      # statistics
  corrr,        # correlation analysis for numeric variables
  janitor,      # adding totals and percents to tables
  flextable,     # converting tables to HTML
  lubridate,     # working with dates
  forcats       # factors
)

# Combine data from across sites/regions

data_VAN <- import(here("Datasets", "Vancouver", "data_VAN.csv"))
data_MB <- import(here("Datasets", "Manitoba", "data_MB.csv"))
data_TO <- import(here("Datasets", "Toronto", "data_TO.csv"))

data_MB <- data_MB |> mutate(ethnicity_indigenous = as.factor(ethnicity_indigenous))

data <- bind_rows(data_MB, data_VAN, data_TO)

# Create variable to determine if follow-up was complete or not

data  <- data  |> 
  mutate(follow = case_when(
    (rec_act1 == 1 | rec_act1 == 0) ~ "Yes", 
    TRUE ~ "No")) |> 
  mutate(follow = as.factor(follow)) 

# Format symptom start date variable

data <- data |> 
  mutate(symp_date = as.Date(symp_date, format = "%Y-%m-%d"))

# Create exposure and outcome variables of interest

data <- data |> 
  mutate(base_agi = case_when(
    base_symp_diar == "diarrhea" ~ 1,
    base_symp_vomit == "vomiting" ~ 1,
    (base_symp_cramps == "cramps" & base_symp_naus == "nausea") ~ 1,
    TRUE ~ 0)) |>
  mutate(base_agi = as.numeric(base_agi))

data <- data |> 
  mutate(base_resp = case_when(
    (base_symp_fever == "fever" & base_symp_throat == "throat") ~ 1,
    (base_symp_fever == "fever" & base_symp_nose == "nose") ~ 1,
    base_symp_cough == "cough" ~ 1,
    TRUE ~ 0)) |>
  mutate(base_resp = as.numeric(base_resp))

data <- data |> 
  mutate(base_ear = case_when(
    base_symp_ear == "ear" ~ 1,
    TRUE ~ 0)) |>
  mutate(base_ear = as.numeric(base_ear))

data <- data |> 
  mutate(base_eye = case_when(
    base_symp_eye == "eye" ~ 1,
    TRUE ~ 0)) |>
  mutate(base_eye = as.numeric(base_eye))

data <- data |> 
  mutate(base_skin = case_when(
    base_symp_rash == "rash" ~ 1,
    TRUE ~ 0)) |>
  mutate(base_skin = as.numeric(base_skin))


# Create 7-day outcome/illness variables 

data <- data |> 
  mutate(symp_inc = as.numeric(symp_date-date)) 

data <- data |> 
  mutate(agi = case_when(
    (symptoms_diar == "diarrhea" & symp_inc <=7 & symp_inc >=0) ~ 1,
    (symptoms_vomit == "vomiting" & symp_inc <=7 & symp_inc >=0) ~ 1,
    (symptoms_cramps == "cramps" & symptoms_naus == "nausea" & symp_inc <=7 & symp_inc >=0) ~ 1,
    (symptoms_cramps == "cramps" & symp_inc <=7 & symp_inc >=0 & misswork == 1) ~ 1,
    (symptoms_naus == "nausea" & symp_inc <=7 & symp_inc >=0 & misswork == 1) ~ 1,
    TRUE ~ 0)) |>
  mutate(agi = as.numeric(agi))

data <- data |> 
  mutate(respiratory = case_when(
    (symptoms_cough == "cough" & symp_inc <=7 & symp_inc >=0) ~ 1,
    (symptoms_fever == "fever" & symptoms_throat == "throat" & symp_inc <=7 & symp_inc >=0) ~ 1,
    (symptoms_fever == "fever" & symptoms_nose == "nose" & symp_inc <=7 & symp_inc >=0) ~ 1,
    TRUE ~ 0)) |>
  mutate(respiratory = as.numeric(respiratory))

data <- data |> 
  mutate(ear_infection = case_when(
    (symptoms_ear == "ear" & symp_inc <=7 & symp_inc >=0) ~ 1,
    TRUE ~ 0)) |>
  mutate(ear_infection = as.numeric(ear_infection))

data <- data |> 
  mutate(eye_infection = case_when(
    (symptoms_eye == "eye" & symp_inc <=7 & symp_inc >=0) ~ 1,
    TRUE ~ 0)) |>
  mutate(eye_infection = as.numeric(eye_infection))

data <- data |> 
  mutate(skin_infection = case_when(
    (symptoms_rash == "rash" & symp_inc <=7 & symp_inc >=0) ~ 1,
    TRUE ~ 0)) |>
  mutate(skin_infection = as.numeric(skin_infection))

data <- data |> 
  mutate(diarrhea = case_when(
    (symptoms_diar == "diarrhea" & symp_inc <=7 & symp_inc >=0) ~ 1,
    TRUE ~ 0)) |>
  mutate(diarrhea = as.numeric(diarrhea))

# Create Yes/No versions of outcome variables

data <- data |> mutate(agi2 = case_when(agi == 1 ~ "Yes", TRUE ~ "No")) |> 
  mutate(agi2 = as.factor(agi2))

data <- data |> mutate(respiratory2 = case_when(respiratory == 1 ~ "Yes", TRUE ~ "No")) |> 
  mutate(respiratory2 = as.factor(respiratory2))

data <- data |> mutate(eye_infection2 = case_when(eye_infection == 1 ~ "Yes", TRUE ~ "No")) |> 
  mutate(eye_infection2 = as.factor(eye_infection2))

data <- data |> mutate(ear_infection2 = case_when(ear_infection == 1 ~ "Yes", TRUE ~ "No")) |> 
  mutate(ear_infection2 = as.factor(ear_infection2))

data <- data |> mutate(skin_infection2 = case_when(skin_infection == 1 ~ "Yes", TRUE ~ "No")) |> 
  mutate(skin_infection2 = as.factor(skin_infection2))

data <- data |> mutate(diarrhea2 = case_when(diarrhea == 1 ~ "Yes", TRUE ~ "No")) |> 
  mutate(diarrhea2 = as.factor(diarrhea2))

# Create outcome versions that exclude those with same baseline symptoms

data <- data |> 
  mutate(agi3 = case_when(
    (agi == 1 & base_agi == 0) ~ "Yes",
    TRUE ~ "No")) |>
  mutate(agi3 = as.factor(agi3))

data <- data |> 
  mutate(respiratory3 = case_when(
    (respiratory == 1 & base_resp == 0) ~ "Yes",
    TRUE ~ "No")) |>
  mutate(respiratory3 = as.factor(respiratory3))

data <- data |> 
  mutate(skin_infection3 = case_when(
    (skin_infection == 1 & base_skin == 0) ~ "Yes",
    TRUE ~ "No")) |>
  mutate(skin_infection3 = as.factor(skin_infection3))

data <- data |> 
  mutate(ear_infection3 = case_when(
    (ear_infection == 1 & base_ear == 0) ~ "Yes",
    TRUE ~ "No")) |>
  mutate(ear_infection3 = as.factor(ear_infection3))

data <- data |> 
  mutate(eye_infection3 = case_when(
    (eye_infection == 1 & base_eye == 0) ~ "Yes",
    TRUE ~ "No")) |>
  mutate(eye_infection3 = as.factor(eye_infection3))

data <- data |> 
  mutate(diarrhea3 = case_when(
    (diarrhea == 1 & base_symp_diar == 0) ~ "Yes",
    TRUE ~ "No")) |>
  mutate(diarrhea3 = as.factor(diarrhea3))

# Create overall outcome variable

data <- data |> 
  mutate(illness_any = case_when(
    (agi3 == "Yes" | respiratory3 == "Yes" | skin_infection3 == "Yes" | 
       ear_infection3 == "Yes" | eye_infection3 == "Yes") ~ "Yes",
    TRUE ~ "No")) |>
  mutate(illness_any = as.factor(illness_any))

# Convert various variables from text and NA to 1/0 entries

data <- data |> mutate(cond_GI = case_when(cond_GI == "GI" ~ "Yes", TRUE ~ "No")) |>
  mutate(cond_GI = as.factor(cond_GI))

data <- data |> mutate(cond_resp = case_when(cond_resp == "respiratory" ~ "Yes", TRUE ~ "No")) |>
  mutate(cond_resp = as.factor(cond_resp))

data <- data |> mutate(cond_skin = case_when(cond_skin == "cond_skin" ~ "Yes", TRUE ~ "No")) |>
  mutate(cond_skin = as.factor(cond_skin))

data <- data |> mutate(cond_allergy = case_when(cond_allergy == "allergies" ~ "Yes", TRUE ~ "No")) |>
  mutate(cond_allergy = as.factor(cond_allergy))

data <- data |> mutate(cond_immune = case_when(cond_immune == "immune" ~ "Yes", TRUE ~ "No")) |>
  mutate(cond_immune = as.factor(cond_immune))

data <- data |> mutate(cond_none = case_when(cond_none == "none" ~ "Yes", TRUE ~ "No")) |>
  mutate(cond_none = as.factor(cond_none))

data <- data |> mutate(eth_arab = case_when(ethnicity_arab == "arab" ~ "Yes", TRUE ~ "No")) |>
  mutate(eth_arab = as.factor(eth_arab))

data <- data |> mutate(eth_black = case_when(ethnicity_black == "black" ~ "Yes", TRUE ~ "No")) |>
  mutate(eth_black = as.factor(eth_black))

data <- data |> mutate(eth_east_asian = case_when(ethnicity_east_asian == "east_asian" ~ "Yes", TRUE ~ "No")) |>
  mutate(eth_east_asian = as.factor(eth_east_asian))

data <- data |> mutate(eth_indigenous = case_when(
  (ethnicity_indigenous == "indigenous" | ethnicity_indigenous == 1) ~ "Yes", 
  TRUE ~ "No")) |>
  mutate(eth_indigenous = as.factor(eth_indigenous))

data <- data |> mutate(eth_latin = case_when(ethnicity_latin == "latin" ~ "Yes", TRUE ~ "No")) |>
  mutate(eth_latin = as.factor(eth_latin))

data <- data |> mutate(eth_south_asian = case_when(ethnicity_south_asian == "south_asian" ~ "Yes", TRUE ~ "No")) |>
  mutate(eth_south_asian = as.factor(eth_south_asian))

data <- data |> mutate(eth_se_asian = case_when(ethnicity_se_asian == "southeast_asian" ~ "Yes", TRUE ~ "No")) |>
  mutate(eth_se_asian = as.factor(eth_se_asian))

data <- data |> mutate(eth_white = case_when(ethnicity_white == "white" ~ "Yes", TRUE ~ "No")) |>
  mutate(eth_white = as.factor(eth_white))

data <- data |> mutate(eth_other = case_when(ethnicity_other == "other_eth" ~ "Yes", TRUE ~ "No")) |>
  mutate(eth_other = as.factor(eth_other))

data <- data |> mutate(other_rec_act = case_when((prev_act1 == 1 | rec_act1 == 1) ~ "Yes", 
                                             TRUE ~ "No")) |>
  mutate(prev_act1 = as.factor(other_rec_act))

data <- data |> mutate(water_contact = case_when(water_contact == 1 ~ "Yes", 
                                                 water_contact == 0 ~ "No")) |>
  mutate(water_contact = as.factor(water_contact))

data <- data |> mutate(sand1 = case_when(sand1 == 1 ~ "Yes", sand1 == "NA" ~ NA_character_, TRUE ~ "No")) |>
  mutate(sand1 = as.factor(sand1))

data <- data |> mutate(sand_mouth1 = case_when(sand_mouth1 == 1 ~ "Yes", sand_mouth1 == "NA" ~ NA_character_, TRUE ~ "No")) |>
  mutate(sand_mouth1 = as.factor(sand_mouth1))

data <- data |> mutate(water_act_swim = case_when(water_act_swim == "swim" ~ "Yes", TRUE ~ "No")) |>
  mutate(water_act_swim = as.factor(water_act_swim))

data <- data |> mutate(water_act_surf = case_when(water_act_surf == "surf" ~ "Yes", TRUE ~ "No")) |>
  mutate(water_act_surf = as.factor(water_act_surf))

data <- data |> mutate(water_act_kite = case_when(water_act_kite == "kite" ~ "Yes", TRUE ~ "No")) |>
  mutate(water_act_kite = as.factor(water_act_kite))

data <- data |> mutate(water_act_wind = case_when(water_act_wind == "wind" ~ "Yes", TRUE ~ "No")) |>
  mutate(water_act_wind = as.factor(water_act_wind))

data <- data |> mutate(water_act_wake = case_when(water_act_wake == "wake" ~ "Yes", TRUE ~ "No")) |>
  mutate(water_act_wake = as.factor(water_act_wake))

data <- data |> mutate(water_act_ski = case_when(water_act_ski == "ski" ~ "Yes", TRUE ~ "No")) |>
  mutate(water_act_ski = as.factor(water_act_ski))

data <- data |> mutate(water_act_paddle = case_when(water_act_paddle == "paddle" ~ "Yes", TRUE ~ "No")) |>
  mutate(water_act_paddle = as.factor(water_act_paddle))

data <- data |> mutate(water_act_snorkel = case_when(water_act_snorkel == "snorkel" ~ "Yes", TRUE ~ "No")) |>
  mutate(water_act_snorkel = as.factor(water_act_snorkel))

data <- data |> mutate(water_act_dive = case_when(water_act_dive == "dive" ~ "Yes", TRUE ~ "No")) |>
  mutate(water_act_dive = as.factor(water_act_dive))

data <- data |> mutate(water_act_wade = case_when(water_act_wade == "wade" ~ "Yes", TRUE ~ "No")) |>
  mutate(water_act_wade = as.factor(water_act_wade))

data <- data |> mutate(water_act_sail = case_when(water_act_sail == "sail" ~ "Yes", TRUE ~ "No")) |>
  mutate(water_act_sail = as.factor(water_act_sail))

data <- data |> mutate(water_act_boat = case_when(water_act_boat == "boat" ~ "Yes", TRUE ~ "No")) |>
  mutate(water_act_boat = as.factor(water_act_boat))

data <- data |> mutate(water_act_fish = case_when(water_act_fish == "fish" ~ "Yes", TRUE ~ "No")) |>
  mutate(water_act_fish = as.factor(water_act_fish))

data <- data |> mutate(water_act_canoe = case_when(water_act_canoe == "canoe" ~ "Yes", TRUE ~ "No")) |>
  mutate(water_act_canoe = as.factor(water_act_canoe))

data <- data |> mutate(water_act_kayak = case_when(water_act_kayak == "kayak" ~ "Yes", TRUE ~ "No")) |>
  mutate(water_act_kayak = as.factor(water_act_kayak))

data <- data |> mutate(water_act_other = case_when(water_act_other == "other" ~ "Yes", TRUE ~ "No")) |>
  mutate(water_act_other = as.factor(water_act_other))

data <- data |> mutate(water_exp_body = case_when(water_exp_body == "face" ~ "Yes", TRUE ~ "No")) |>
  mutate(water_exp_body = as.factor(water_exp_body))

data <- data |> mutate(water_exp_head = case_when(water_exp_head == "head" ~ "Yes", TRUE ~ "No")) |>
  mutate(water_exp_head = as.factor(water_exp_head))

data <- data |> mutate(water_exp_mouth = case_when(water_exp_mouth == "mouth" ~ "Yes", TRUE ~ "No")) |>
  mutate(water_exp_mouth = as.factor(water_exp_mouth))

data <- data |> mutate(water_exp_neither = case_when(water_exp_neither == "neither" ~ "Yes", TRUE ~ "No")) |>
  mutate(water_exp_neither = as.factor(water_exp_neither))

data <- data |> mutate(water_time = as.numeric(water_time))

data <- data |> mutate(beach_exp_algae = case_when(beach_exp_algae == "algae" ~ "Yes", TRUE ~ "No")) |>
  mutate(beach_exp_algae = as.factor(beach_exp_algae))

data <- data |> mutate(beach_exp_sunscreen = case_when(beach_exp_sun == "sun" ~ "Yes", TRUE ~ "No")) |>
  mutate(beach_exp_sunscreen = as.factor(beach_exp_sunscreen))

data <- data |> mutate(beach_exp_repellent = case_when(beach_exp_rep == "repellent" ~ "Yes", TRUE ~ "No")) |>
  mutate(beach_exp_repellent = as.factor(beach_exp_repellent))

data <- data |> mutate(beach_exp_food = case_when(beach_exp_food == "food" ~ "Yes", TRUE ~ "No")) |>
  mutate(beach_exp_food = as.factor(beach_exp_food))

data <- data |> mutate(sand_act_dig = case_when(sand_act_dig == "dig" ~ "Yes", TRUE ~ "No")) |>
  mutate(sand_act_dig = as.factor(sand_act_dig))

data <- data |> mutate(sand_act_bury = case_when(sand_act_bury == "bury" ~ "Yes", TRUE ~ "No")) |>
  mutate(sand_act_bury = as.factor(sand_act_bury))

data <- data |> mutate(sand_act_other = case_when(sand_act_other == "other" ~ "Yes", TRUE ~ "No")) |>
  mutate(sand_act_other = as.factor(sand_act_other))

# Recode illness severity outcomes only for those with AGI symptoms

data <- data |> 
  mutate(misswork = case_when(
    (agi3 == "Yes" & misswork == 1) ~ "Yes",
    agi3 == "No" ~ NA,
    TRUE ~ "No")) |>
  mutate(misswork = as.factor(misswork))

data <- data |> 
  mutate(misswork_days = case_when(
    agi3 == "No" ~ NA,
    misswork_days == "NA" ~ NA,
    TRUE ~ misswork_days)) |>
  mutate(misswork_days = as.numeric(misswork_days))

data <- data |> 
  mutate(med_antibiotics = case_when(
    (agi3 == "Yes" & med_antibiotics == 1) ~ "Yes",
    agi3 == "No" ~ NA,
    TRUE ~ "No")) |>
  mutate(med_antibiotics = as.factor(med_antibiotics))

data <- data |> 
  mutate(med_otc = case_when(
    (agi3 == "Yes" & med_otc == 1) ~ "Yes",
    agi3 == "No" ~ NA,
    TRUE ~ "No")) |>
  mutate(med_otc = as.factor(med_otc))

data <- data |> 
  mutate(med_none = case_when(
    (agi3 == "Yes" & med_none == 1) ~ "Yes",
    agi3 == "No" ~ NA,
    TRUE ~ "No")) |>
  mutate(med_none = as.factor(med_none))

data <- data |> 
  mutate(healthcare1 = case_when(
    (agi3 == "Yes" & healthcare1 == 1) ~ "Yes",
    agi3 == "No" ~ NA,
    TRUE ~ "No")) |>
  mutate(healthcare1 = as.factor(healthcare1))

data <- data |> 
  mutate(blood_stool1 = case_when(
    (agi3 == "Yes" & blood_stool1 == 1) ~ "Yes",
    agi3 == "No" ~ NA,
    TRUE ~ "No")) |>
  mutate(blood_stool1 = as.factor(blood_stool1))

data <- data |> 
  mutate(stool_test1 = case_when(
    (agi3 == "Yes" & stool_test1 == 1) ~ "Yes",
    agi3 == "No" ~ NA,
    TRUE ~ "No")) |>
  mutate(stool_test1 = as.factor(stool_test1))

data <- data |> 
  mutate(emergency = case_when(
    (agi3 == "Yes" & emergency == 1) ~ "Yes",
    agi3 == "No" ~ NA,
    TRUE ~ "No")) |>
  mutate(emergency = as.factor(emergency))

data <- data |> 
  mutate(hospital = case_when(
    (agi3 == "Yes" & hospital == 1) ~ "Yes",
    agi3 == "No" ~ NA,
    TRUE ~ "No")) |>
  mutate(hospital = as.factor(hospital))

# Recode some socio-demographic variables

data <- data |> mutate(age1 = if_else(age1 == "", NA, age1))

data <- data |> 
  mutate(age1 = fct_relevel(age1, "5-9", after = 1)) |> 
  mutate(education = fct_relevel(education, "none", "secondary", "apprenticeship", "college", "bachelors", "graduate"))

levels(data$age1)
levels(data$education)

data <- data |> 
  mutate(education2 = case_when(
    (education == "none" | education == "secondary") ~ "high school or less",
    (education == "apprenticeship" | education == "college") ~ "college/trades",
    (education == "bachelors" | education == "bachelors") ~ "bachelors",
    education == "graduate" ~ "post-graduate",
        TRUE ~ NA_character_))  |>
  mutate(education2 = fct_relevel(education2, "high school or less", "college/trades", "bachelors", "post-graduate"))

data <- data |> 
  mutate(gender = case_when(
    ((gender1 == "woman" | gender1 == "girl") & sex1 == "female") ~ "woman/girl",
    ((gender1 == "man" | gender1 == "boy") & sex1 == "male") ~ "man/boy",
    (gender1 == "nonbinary" | gender1 == "other_gender") ~ "fluid/trans",
    ((gender1 == "woman" | gender1 == "girl") & sex1 == "male") ~ "fluid/trans",
    ((gender1 == "man" | gender1 == "boy") & sex1 == "female") ~ "fluid/trans",   
    TRUE ~ NA_character_))  |>
  mutate(gender = fct_relevel(gender, "woman/girl", "man/boy", "fluid/trans"))

data <- data |> mutate(age1 = as.factor(age1))
data$age1 <- factor(data$age1, exclude = "NA")

data <- data |> 
  mutate(age2 = case_when(
    (age1 == "0-4" | age1 == "5-9" | age1 == "10-14") ~ "0-14",
    age1 == "15-19" ~ "15-19",
    age1 == "20-39" ~ "20-39",
    (age1 == "40-59" | age1 == "60+") ~ "40+",
    TRUE ~ NA_character_))  |>
  mutate(age2 = fct_relevel(age2, "0-14", "15-19", "20-39", "40+"))

data <- data |> 
  mutate(age3 = case_when(
    (age1 == "0-4" | age1 == "5-9" | age1 == "10-14") ~ "0-14",
    (age1 == "15-19" | age1 == "20-39" | age1 == "40-59" | age1 == "60+") ~ "15+",   
    TRUE ~ NA_character_))  |>
  mutate(age3 = as.factor(age3)) |>
  mutate(age3 = fct_relevel(age3, "15+", "0-14"))

data <- data |> 
  mutate(ethnicity = case_when(
    ethnicity_other == "other_eth" ~ "Multiple ethnicities",
    (eth_white == "Yes" & (eth_south_asian == "Yes" | eth_arab == "Yes" | eth_se_asian == "Yes" | 
                             eth_east_asian == "Yes" | eth_black == "Yes" | 
                             eth_latin == "Yes" | eth_indigenous == "Yes")) ~ "Multiple ethnicities",
    (eth_arab == "Yes" & (eth_south_asian == "Yes" | eth_se_asian == "Yes" | 
                             eth_east_asian == "Yes" | eth_black == "Yes" | 
                             eth_latin == "Yes" | eth_indigenous == "Yes")) ~ "Multiple ethnicities",
    (eth_south_asian == "Yes" & (eth_se_asian == "Yes" | eth_east_asian == "Yes" | 
                                   eth_black == "Yes" | eth_latin == "Yes" | 
                                   eth_indigenous == "Yes")) ~ "Multiple ethnicities",    
    eth_white == "Yes" ~ "White",
    eth_arab == "Yes" ~ "Arab",
    eth_south_asian == "Yes" ~ "South Asian",
    eth_se_asian == "Yes" ~ "Southeast Asian",
    eth_east_asian == "Yes" ~ "East Asian",
    eth_black == "Yes" ~ "Black",
    eth_latin == "Yes" ~ "Latin",
    eth_indigenous == "Yes" ~ "Indigenous",
    TRUE ~ NA_character_)) 

data |> tabyl(ethnicity)  
    
# Create exposure variables

data <- data |> 
  mutate(water_contact2 = case_when(
    water_contact == "No" ~ "No contact",
    water_exp_mouth == "Yes" ~ "Swallowed water",
    water_exp_body == "Yes" ~ "Body immersion",
    TRUE ~ "Minimal contact")) |>
  mutate(water_contact2 = as.factor(water_contact2)) |> 
  mutate(water_contact2 = fct_relevel(water_contact2, "No contact", "Minimal contact")) 

data |> tabyl(water_contact)
data |> tabyl(water_contact2)

data <- data |> 
  mutate(water_sports = case_when(
    (water_act_surf == "Yes" | water_act_kite == "Yes" | water_act_wind == "Yes" | water_act_wake == "Yes" | 
       water_act_ski == "Yes" | water_act_snorkel == "Yes" | water_act_dive == "Yes") ~ "Yes",
    TRUE ~ "No")) |>
  mutate(prim_contact = as.factor(water_sports))

data <- data |> 
  mutate(minimal_contact = case_when(
    (water_act_sail == "Yes" | water_act_boat == "Yes" | water_act_fish == "Yes" | 
       water_act_canoe == "Yes" | water_act_kayak == "Yes") ~ "Yes",
    TRUE ~ "No")) |>
  mutate(minimal_contact = as.factor(minimal_contact))

# Define sand contact as only digging or burying - excluding others like tanning, laying in sand

data <- data |> 
  mutate(sand_contact = case_when(
    (sand_act_dig == "Yes" | sand_act_bury == "Yes") ~ "Yes",
    TRUE ~ "No")) |>
  mutate(sand_contact = as.factor(sand_contact))

# Create weekend-holiday recruitment day variable

data <- data |> 
  mutate(weekend_holiday = case_when(
    (dow == 1 | dow == 7) ~ "Yes",
    date == "2023-07-03" ~ "Yes",
    TRUE ~ "No")) 
    
# Create mean centered and standardized E. coli and other water variables

data <- data |> 
  mutate(log_e_coli = log(e_coli + 1))

data <- data |> 
  mutate(log_e_coli_s = (log_e_coli - mean(log_e_coli, na.rm = TRUE)) / sd(log_e_coli, na.rm = TRUE))

data <- data |> 
  mutate(e_coli_s = (e_coli - mean(e_coli, na.rm = TRUE)) / sd(e_coli, na.rm = TRUE))

data <- data |> 
  mutate(e_coli_max_s = (e_coli_max - mean(e_coli_max, na.rm = TRUE)) / sd(e_coli_max, na.rm = TRUE))

data <- data |> 
  mutate(turbidity_s = (turbidity - mean(turbidity, na.rm = TRUE)) / sd(turbidity, na.rm = TRUE))

# Create E. coli threshold variables

data <- data |> 
  mutate(ecoli_100 = if_else(e_coli >=100, "Yes", "No"),
         ecoli_200 = if_else(e_coli >=200, "Yes", "No"),
         ecoli_bav = if_else(e_coli_max >=235, "Yes", "No"),
         ecoli_500 = if_else(e_coli >=500, "Yes", "No"),
         ecoli_1000 = if_else(e_coli >=1000, "Yes", "No"))

data <- data |> mutate(ecoli_thresholds = case_when(
  e_coli <= 10 ~ "<=10",
  (e_coli >10 & e_coli <=100) ~ "11-100",
  (e_coli >100 & e_coli <=200) ~ "101-200",
  (e_coli >200 & e_coli <=300) ~ "201-300",
  (e_coli >300 & e_coli <=400) ~ "301-400",
  (e_coli >400 & e_coli <=500) ~ "401-500",
  (e_coli >501 & e_coli <=1000) ~ "501-1000",
  e_coli >1000 ~ ">1000"))

data <- data |> mutate(ecoli_thresholds = factor(ecoli_thresholds, ordered = T, 
                     levels = c("<=10", "11-100", "101-200", "201-300", "301-400", 
                                "401-500", "501-1000", ">1000")))
                           
# Create standardized and binary MST variables

data <- data |> 
  mutate(mst_human_s = (mst_human - mean(mst_human, na.rm=TRUE)) / sd(mst_human, na.rm=TRUE))

data <- data |> 
  mutate(mst_gull_s = (mst_gull - mean(mst_gull, na.rm=TRUE)) / sd(mst_gull, na.rm=TRUE))

data <- data |> 
  mutate(mst_human2 = case_when(
    mst_human > 0 ~ "Yes", TRUE ~ "No"))

data <- data |> 
  mutate(mst_gull2 = case_when(
  mst_gull > 0 ~ "Yes", TRUE ~ "No"))

# Extract year as factor

data <- data |> 
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         year = year(date))

# Create dataset of those who completed follow-up survey

data_follow <- data |> filter(follow == "Yes") 

# Export data

data |> export(here("Datasets", "data.xlsx"))

