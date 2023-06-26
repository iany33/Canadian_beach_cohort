
pacman::p_load(
  rio,    
  here,  
  skimr,
  Matrix,
  tidyverse, 
  lubridate,
  janitor,
  data.table,
  uuid
)

beach <- import(here("Datasets", "2023-beach.xlsx"))
follow <- import(here("Datasets", "2023-follow.xlsx"))

e_coli <- import(here("Datasets", "2023_e_coli.xlsx"))


# Clean variable names

beach <- beach  |> clean_names()
follow <- follow |> clean_names()

# Combine household member answers together then split into multiple rows per respondent

beach <- beach |> 
  unite(name, c("firstname", "lastname"), sep=" ") |> 
  unite(name1, c("name", "name2", "name3", "name4", "name5", "name6"), sep=",") |> 
  unite(age1, c("age", "age2", "age3", "age4", "age5", "age6"), sep=",") |> 
  unite(sex1, c("sex", "sex2", "sex3", "sex4", "sex5", "sex6"), sep=",") |>
  unite(sex_other, c("sexother_sex", "sex2other_sex", "sex3other_sex", "sex4other_sex", "sex5other_sex", "sex6other_sex"), sep=",") |>  
  unite(gender1, c("gender", "gender2", "gender3", "gender4", "gender5", "gender6"), sep=",") |>
  unite(gender_other, c("genderother_gender", "gender2other_gender", "gender3other_gender", "gender4other_gender", "gender5other_gender", "gender6other_gender"), sep=",") |>  
  unite(ethnicity_arab, c("ethnicityarab", "ethnicity2arab", "ethnicity3arab", "ethnicity4arab", "ethnicity5arab", "ethnicity6arab"), sep=",") |> 
  unite(ethnicity_black, c("ethnicityblack", "ethnicity2black", "ethnicity3black", "ethnicity4black", "ethnicity5black", "ethnicity6black"), sep=",") |> 
  unite(ethnicity_east_asian, c("ethnicityeast_asian", "ethnicity2east_asian", "ethnicity3east_asian", "ethnicity4east_asian", "ethnicity5east_asian", "ethnicity6east_asian"), sep=",") |> 
  unite(ethnicity_indigenous, c("ethnicityindigenous", "ethnicity2indigenous", "ethnicity3indigenous", "ethnicity4indigenous", "ethnicity5indigenous", "ethnicity6indigenous"), sep=",") |> 
  unite(ethnicity_latin, c("ethnicitylatin", "ethnicity2latin", "ethnicity3latin", "ethnicity4latin", "ethnicity5latin", "ethnicity6latin"), sep=",") |> 
  unite(ethnicity_south_asian, c("ethnicitysouth_asian", "ethnicity2south_asian", "ethnicity3south_asian", "ethnicity4south_asian", "ethnicity5south_asian", "ethnicity6south_asian"), sep=",") |> 
  unite(ethnicity_se_asian, c("ethnicitysoutheast_asian", "ethnicity2southeast_asian", "ethnicity3southeast_asian", "ethnicity4southeast_asian", "ethnicity5southeast_asian", "ethnicity6southeast_asian"), sep=",") |> 
  unite(ethnicity_white, c("ethnicitywhite", "ethnicity2white", "ethnicity3white", "ethnicity4white", "ethnicity5white", "ethnicity6white"), sep=",") |> 
  unite(ethnicity_other, c("ethnicityother_eth_34", "ethnicity2other_eth_105", "ethnicity3other_eth_176", "ethnicity4other_eth_247", "ethnicity5other_eth_318", "ethnicity6other_eth_389"), sep=",") |> 
  unite(ethnicity_other_s, c("ethnicityother_eth_35", "ethnicity2other_eth_106", "ethnicity3other_eth_177", "ethnicity4other_eth_248", "ethnicity5other_eth_319", "ethnicity6other_eth_390"), sep=",") |> 
  unite(ethnicity_na, c("ethnicity_na", "ethnicity2na", "ethnicity3na", "ethnicity4na", "ethnicity5na", "ethnicity6na"), sep=",") |> 
  unite(base_symp_diar, c("symptomsdiarrhea", "symptoms2diarrhea", "symptoms3diarrhea", "symptoms4diarrhea", "symptoms5diarrhea", "symptoms6diarrhea"), sep=",") |> 
  unite(base_symp_vomit, c("symptomsvomiting", "symptoms2vomiting", "symptoms3vomiting", "symptoms4vomiting", "symptoms5vomiting", "symptoms6vomiting"), sep=",") |> 
  unite(base_symp_cramps, c("symptomscramps", "symptoms2cramps", "symptoms3cramps", "symptoms4cramps", "symptoms5cramps", "symptoms6cramps"), sep=",") |> 
  unite(base_symp_naus, c("symptomsnausea", "symptoms2nausea", "symptoms3nausea", "symptoms4nausea", "symptoms5nausea", "symptoms6nausea"), sep=",") |> 
  unite(base_symp_fever, c("symptomsfever", "symptoms2fever", "symptoms3fever", "symptoms4fever", "symptoms5fever", "symptoms6fever"), sep=",") |> 
  unite(base_symp_throat, c("symptomsthroat", "symptoms2throat", "symptoms3throat", "symptoms4throat", "symptoms5throat", "symptoms6throat"), sep=",") |> 
  unite(base_symp_nose, c("symptomsnose", "symptoms2nose", "symptoms3nose", "symptoms4nose", "symptoms5nose", "symptoms6nose"), sep=",") |> 
  unite(base_symp_cough, c("symptomscough", "symptoms2cough", "symptoms3cough", "symptoms4cough",  "symptoms5cough", "symptoms6cough"), sep=",") |> 
  unite(base_symp_ear, c("symptomsear", "symptoms2ear", "symptoms3ear", "symptoms4ear", "symptoms5ear", "symptoms6ear"), sep=",") |> 
  unite(base_symp_eye, c("symptomseye", "symptoms2eye", "symptoms3eye", "symptoms4eye", "symptoms5eye", "symptoms6eye"), sep=",") |> 
  unite(base_symp_rash, c("symptomsrash", "symptoms2rash", "symptoms3rash", "symptoms4rash", "symptoms5rash", "symptoms6rash"), sep=",") |> 
  unite(base_symp_none, c("symptomsnone", "symptoms2none", "symptoms3none", "symptoms4none", "symptoms5none", "symptoms6none"), sep=",") |> 
  unite(cond_GI, c("conditions_gi", "conditions2gi", "conditions3gi", "conditions4gi", "conditions5gi", "conditions6gi"), sep=",") |> 
  unite(cond_resp, c("conditionsrespiratory", "conditions2respiratory", "conditions3respiratory", "conditions4respiratory", "conditions5respiratory", "conditions6respiratory"), sep=",") |> 
  unite(cond_skin, c("conditionsskin", "conditions2skin", "conditions3skin", "conditions4skin", "conditions5skin", "conditions6skin"), sep=",") |> 
  unite(cond_allergy, c("conditionsallergies", "conditions2allergies", "conditions3allergies", "conditions4allergies", "conditions5allergies", "conditions6allergies"), sep=",")  |> 
  unite(cond_immune, c("conditionsimmune", "conditions2immune", "conditions3immune", "conditions4immune", "conditions5immune", "conditions6immune"), sep=",")  |>
  unite(cond_none, c("conditionsnone", "conditions2none", "conditions3none", "conditions4none", "conditions5none", "conditions6none"), sep=",")  |>
  unite(cond_na, c("conditions_na", "conditions2na", "conditions3na", "conditions4na", "conditions5na", "conditions6na"), sep=",")  |>
  unite(prev_act1, c("prev_act", "prev_act2", "prev_act3", "prev_act4", "prev_act5", "prev_act6"), sep=",") |>
  unite(water_contact, c("swam", "swam2", "swam3", "swam4", "swam5", "swam6"), sep=",") |> 
  unite(water_act_swim, c("water_actswim", "water_act2swim", "water_act3swim", "water_act4swim", "water_act5swim", "water_act6swim"), sep=",") |> 
  unite(water_act_surf, c("water_actsurf", "water_act2surf", "water_act3surf", "water_act4surf", "water_act5surf", "water_act6surf"), sep=",") |> 
  unite(water_act_kite, c("water_actkite", "water_act2kite", "water_act3kite", "water_act4kite", "water_act5kite", "water_act6kite"), sep=",") |> 
  unite(water_act_wind, c("water_actwind", "water_act2wind", "water_act3wind", "water_act4wind", "water_act5wind", "water_act6wind"), sep=",") |> 
  unite(water_act_wake, c("water_actwake", "water_act2wake", "water_act3wake", "water_act4wake", "water_act5wake", "water_act6wake"), sep=",") |> 
  unite(water_act_ski, c("water_actski", "water_act2ski", "water_act3ski", "water_act4ski", "water_act5ski", "water_act6ski"), sep=",") |> 
  unite(water_act_paddle, c("water_actpaddle", "water_act2paddle", "water_act3paddle", "water_act4paddle", "water_act5paddle", "water_act6paddle"), sep=",") |> 
  unite(water_act_snorkel, c("water_actsnorkel", "water_act2snorkel", "water_act3snorkel", "water_act4snorkel", "water_act5snorkel", "water_act6snorkel"), sep=",") |> 
  unite(water_act_dive, c("water_actdive", "water_act2dive", "water_act3dive", "water_act4dive", "water_act5dive", "water_act6dive"), sep=",") |> 
  unite(water_act_wade, c("water_actwade", "water_act2wade", "water_act3wade", "water_act4wade", "water_act5wade", "water_act6wade"), sep=",") |> 
  unite(water_act_sail, c("water_actsail", "water_act2sail", "water_act3sail", "water_act4sail", "water_act5sail", "water_act6sail"), sep=",") |> 
  unite(water_act_boat, c("water_actboat", "water_act2boat", "water_act3boat", "water_act4boat", "water_act5boat", "water_act6boat"), sep=",") |> 
  unite(water_act_fish, c("water_actfish", "water_act2fish", "water_act3fish", "water_act4fish", "water_act5fish", "water_act6fish"), sep=",") |> 
  unite(water_act_canoe, c("water_actcanoe", "water_act2canoe", "water_act3canoe", "water_act4canoe", "water_act5canoe", "water_act6canoe"), sep=",") |> 
  unite(water_act_kayak, c("water_actkayak", "water_act2kayak", "water_act3kayak", "water_act4kayak", "water_act5kayak", "water_act6kayak"), sep=",") |> 
  unite(water_act_other, c("water_actother_73", "water_act2other_144", "water_act3other_215", "water_act4other_286", "water_act5other_357", "water_act6other_428"), sep=",") |>   
  unite(water_act_other_s, c("water_actother_74", "water_act2other_145", "water_act3other_216", "water_act4other_287", "water_act5other_358", "water_act6other_429"), sep=",") |> 
  unite(water_exp_body, c("water_expface", "water_exp2face", "water_exp3face", "water_exp4face", "water_exp5face", "water_exp6face"), sep=",") |> 
  unite(water_exp_head, c("water_exphead", "water_exp2head", "water_exp3head", "water_exp4head", "water_exp5head", "water_exp6head"), sep=",") |> 
  unite(water_exp_mouth, c("water_expmouth", "water_exp2mouth", "water_exp3mouth", "water_exp4mouth", "water_exp5mouth", "water_exp6mouth"), sep=",") |> 
  unite(water_exp_neither, c("water_expneither", "water_exp2neither", "water_exp3neither", "water_exp4neither", "water_exp5neither", "water_exp6neither"), sep=",") |> 
  unite(water_time, c("water_time", "water_time2", "water_time3", "water_time4", "water_time5", "water_time6"), sep=",") |> 
  unite(beach_exp_algae, c("beach_expalgae", "beach_exp2algae", "beach_exp3algae", "beach_exp4algae", "beach_exp5algae", "beach_exp6algae"), sep=",") |> 
  unite(beach_exp_sun, c("beach_expsun", "beach_exp2sun", "beach_exp3sun", "beach_exp4sun", "beach_exp5sun", "beach_exp6sun"), sep=",") |> 
  unite(beach_exp_rep, c("beach_exprepellent", "beach_exp2repellent", "beach_exp3repellent", "beach_exp4repellent", "beach_exp5repellent", "beach_exp6repellent"), sep=",") |> 
  unite(beach_exp_food, c("beach_expfood", "beach_exp2food", "beach_exp3food", "beach_exp4food", "beach_exp5food", "beach_exp6food"), sep=",") |> 
  unite(sand1, c("sand", "sand2", "sand3", "sand4", "sand5", "sand6"), sep=",") |> 
  unite(sand_act_dig, c("sand_actdig", "sand_act2dig", "sand_act3dig", "sand_act4dig", "sand_act5dig", "sand_act6dig"), sep=",") |> 
  unite(sand_act_bury, c("sand_actbury", "sand_act2bury", "sand_act3bury", "sand_act4bury", "sand_act5bury", "sand_act6bury"), sep=",") |> 
  unite(sand_act_other, c("sand_actother_88", "sand_act2other_159", "sand_act3other_230", "sand_act4other_301", "sand_act5other_372", "sand_act6other_443"), sep=",") |>   
  unite(sand_act_other_s, c("sand_actother_89", "sand_act2other_160", "sand_act3other_231", "sand_act4other_302", "sand_act5other_373", "sand_act6other_444"), sep=",") |> 
  unite(sand_mouth1, c("sand_mouth", "sand_mouth2", "sand_mouth3", "sand_mouth4", "sand_mouth5", "sand_mouth6"), sep=",")  |> 
  unite(others, c("others", "others2", "others3", "others4", "others5", "others6"), sep=",")

beach <- beach |> 
  separate_rows(name1, age1, sex1, sex_other, gender1, gender_other, ethnicity_arab, ethnicity_black, 
                ethnicity_east_asian, ethnicity_indigenous, ethnicity_latin, ethnicity_south_asian, 
                ethnicity_se_asian, ethnicity_white, ethnicity_other, ethnicity_other_s, ethnicity_na, 
                base_symp_diar, base_symp_vomit, base_symp_cramps, base_symp_naus, base_symp_fever, 
                base_symp_throat, base_symp_nose, base_symp_cough, base_symp_ear, base_symp_eye, 
                base_symp_rash, base_symp_none, cond_GI, cond_resp, cond_skin, cond_allergy, 
                cond_immune, cond_none, prev_act1, water_contact, water_act_swim, water_act_surf, 
                water_act_kite, water_act_wind, water_act_wake, water_act_ski, water_act_paddle, 
                water_act_snorkel, water_act_dive, water_act_wade, water_act_sail, water_act_boat, 
                water_act_fish, water_act_canoe, water_act_kayak, water_act_other, water_act_other_s, 
                water_time, water_exp_body, water_exp_head, water_exp_mouth, water_exp_neither, 
                beach_exp_algae, beach_exp_sun, beach_exp_rep, beach_exp_food, sand1, sand_act_dig, 
                sand_act_bury, sand_act_other, sand_act_other_s, sand_mouth1,
                others, sep=",") 

beach <- beach[!(beach$name1=="NA"),]
beach <- select(beach, -name7:-sand_mouth10)

follow <- follow |> 
  unite(household_name, c("first_name", "last_name"), sep=" ") |> 
  unite(name, c("fname", "lname"), sep=" ") |> 
  unite(name1, c("name", "name2", "name3", "name4", "name5", "name6"), sep=",") |> 
  unite(rec_act1, c("rec_act", "rec_act2", "rec_act3", "rec_act4", "rec_act5", "rec_act6"), sep=",") |> 
  unite(symptoms_diar, c("symptomsdiarrhea", "symptoms2diarrhea", "symptoms3diarrhea", "symptoms4diarrhea", "symptoms5diarrhea", "symptoms6diarrhea"), sep=",") |> 
  unite(symptoms_vomit, c("symptomsvomiting", "symptoms2vomiting", "symptoms3vomiting", "symptoms4vomiting", "symptoms5vomiting", "symptoms6vomiting"), sep=",") |> 
  unite(symptoms_cramps, c("symptomscramps", "symptoms2cramps", "symptoms3cramps", "symptoms4cramps", "symptoms5cramps", "symptoms6cramps"), sep=",") |> 
  unite(symptoms_naus, c("symptomsnausea", "symptoms2nausea", "symptoms3nausea", "symptoms4nausea", "symptoms5nausea", "symptoms6nausea"), sep=",") |> 
  unite(symptoms_fever, c("symptomsfever", "symptoms2fever", "symptoms3fever", "symptoms4fever", "symptoms5fever", "symptoms6fever"), sep=",") |> 
  unite(symptoms_throat, c("symptomsthroat", "symptoms2throat", "symptoms3throat", "symptoms4throat", "symptoms5throat", "symptoms6throat"), sep=",") |> 
  unite(symptoms_nose, c("symptomsnose", "symptoms2nose", "symptoms3nose", "symptoms4nose", "symptoms5nose", "symptoms6nose"), sep=",") |> 
  unite(symptoms_cough, c("symptomscough", "symptoms2cough", "symptoms3cough", "symptoms4cough", "symptoms5cough","symptoms6cough"), sep=",") |> 
  unite(symptoms_ear, c("symptomsear", "symptoms2ear", "symptoms3ear", "symptoms4ear", "symptoms5ear", "symptoms6ear"), sep=",") |> 
  unite(symptoms_eye, c("symptomseye", "symptoms2eye", "symptoms3eye", "symptoms4eye", "symptoms5eye", "symptoms6eye"), sep=",") |> 
  unite(symptoms_rash, c("symptomsrash", "symptoms2rash", "symptoms3rash", "symptoms4rash", "symptoms5rash", "symptoms6rash"), sep=",") |> 
  unite(symptoms_none, c("symptomsnone", "symptoms2none", "symptoms3none", "symptoms4none", "symptoms5none", "symptoms6none"), sep=",") |> 
  unite(symp_date, c("symp_start", "symp_start2", "symp_start3", "symp_start4", "symp_start5","symp_start6"), sep=",") |> 
  unite(misswork, c("misswork", "misswork2", "misswork3", "misswork4", "misswork5", "misswork6"), sep=",") |> 
  unite(misswork_days, c("misswork1", "misswork21", "misswork31", "misswork41", "misswork51", "misswork61"), sep=",") |> 
  unite(med_antibiotics, c("medicationantibiotics", "medication2antibiotics", "medication3antibiotics", "medication4antibiotics", "medications5antibiotics", "medication6antibiotics"), sep=",") |> 
  unite(med_otc, c("medicationotc_drugs", "medication2otc_drugs", "medication3otc_drugs", "medication4otc_drugs", "medications5otc_drugs", "medication6otc_drugs"), sep=",") |> 
  unite(med_none, c("medicationnone", "medication2none", "medication3none", "medication4none", "medications5none", "medication6none"), sep=",")  |> 
  unite(healthcare1, c("healthcare", "healthcare2", "healthcare3", "healthcare4", "healthcare5", "healthcare6"), sep=",")  |>
  unite(blood_stool1, c("blood_stool", "blood_stool2", "blood_stool3", "blood_stool4", "blood_stool5", "blood_stool6"), sep=",")  |>
  unite(stool_test1, c("stool_test", "stool_test2", "stool_test3", "stool_test4", "stool_test5", "stool_test6"), sep=",")  |>
  unite(emergency, c("ed_visit", "ed_visit2", "ed_visit3", "ed_visit4", "ed_visit5", "ed_visit6"), sep=",")  |>
  unite(hospital, c("hospitalized", "hospitalized2", "hospitalized3", "hospitalized4", "hospitalized5", "hospitalized6"), sep=",") |>
  unite(others_follow, c("others", "others2", "others3", "others4", "others5", "others6"), sep=",")

follow <- follow |> 
  separate_rows(name1, rec_act1, symptoms_diar, symptoms_vomit, symptoms_cramps, 
                symptoms_naus, symptoms_fever, symptoms_throat, symptoms_nose, 
                symptoms_cough, symptoms_ear, symptoms_eye, symptoms_rash,
                symptoms_none, symp_date, misswork, misswork_days, med_antibiotics, 
                med_otc, med_none, healthcare1, blood_stool1, stool_test1,
                emergency, hospital, others_follow, sep=",") 

follow <- follow[!(follow$name1=="NA"),]
follow <- select(follow, -name7:-hospitalized10)

# Merge survey datasets

beach <- beach |> 
  rename(house_id = internal_id) |> 
  rename(date = submitted_date) 

survey_data <- left_join(beach, follow, by = "name1")


# Check for duplicate names 
survey_data |> group_by(name1) |> filter(n()>1) 


## Check for any follow-up participants that did not match to beach participants

follow |> anti_join(beach, by = "name1")
investigate <- follow |> anti_join(beach, by = "name1")


## Merge E. coli data

e_coli <- e_coli |> 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) 

survey_data <- survey_data |> 
  mutate(date = as.Date(date))  |> 
  mutate(month = as.factor(month(date))) |> 
  mutate(dow = as.factor(wday(date))) # Sunday is 1, Sat. is 7

survey_data <- left_join(survey_data, e_coli, by = "date")


## Replace name column with unique/random ID - drop email, phone

survey_data$row_id <- 1:nrow(survey_data)

survey_data <- survey_data |> 
  group_by(name1) |> 
  mutate(
    id = uuid::UUIDgenerate(use.time = FALSE)
  ) |> 
  ungroup() |> 
  select(-name1, participant_id = id) |> 
  relocate(participant_id)

survey_data <- survey_data |> 
  relocate(participant_id, .after = house_id)  |> 
  relocate(row_id, .after = participant_id)

data <- subset(survey_data, select = -c(email.x, email.y, phone))

remove(beach, beach_surveys, follow, investigate, survey_data)


