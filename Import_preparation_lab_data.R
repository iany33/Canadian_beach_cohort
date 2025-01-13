
pacman::p_load(
  rio,    
  here,  
  Matrix,
  tidyverse, 
  lubridate,
  janitor,
  data.table
)

# Import datasets

data_TO <- import(here("Datasets", "Toronto", "data_TO.csv"))
data_VAN <- import(here("Datasets", "Vancouver", "data_VAN.csv"))
data_MB <- import(here("Datasets", "Manitoba", "data_MB.csv"))

data_NS <- import(here("Datasets", "Halifax", "data_NS.csv"))
data_NR <- import(here("Datasets", "Niagara", "data_NR.csv"))

e_coli_TO <- import(here("Datasets", "Toronto", "2023_e_coli.xlsx"))
mst_TO <- import(here("Datasets", "Toronto", "2023_mst_data.xlsx"))

e_coli_VAN <- import(here("Datasets", "Vancouver", "2024-FIB_Vancouver.xlsx"))
e_coli_MB <- import(here("Datasets", "Manitoba", "2024-FIB_Manitoba.xlsx"))

e_coli_NS <- import(here("Datasets", "Halifax", "2025-FIB_Halifax.xlsx"))
e_coli_NR <- import(here("Datasets", "Niagara", "2025-FIB_Niagara.xlsx"))

## TORONTO = Load, Format and Merge E. coli data
# For now, reformat <10 E. coli to 5 counts to allow averaging and merging with other sites
# Similarly, there are some >1000 counts on a few days that will need to be reported as 1001 for now

e_coli_TO <- e_coli_TO |> 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) 

e_coli_TO <- e_coli_TO |> mutate(e_coli1 = case_when(
  e_coli1 == "<10" ~ 5, 
  e_coli1 == ">1000" ~ 1001,
  TRUE ~ as.numeric(e_coli1)))

e_coli_TO <- e_coli_TO |> mutate(e_coli2 = case_when(
  e_coli2 == "<10" ~ 5, 
  e_coli2 == ">1000" ~ 1001,
  TRUE ~ as.numeric(e_coli2)))

e_coli_TO <- e_coli_TO |> mutate(e_coli3 = case_when(
  e_coli3 == "<10" ~ 5, 
  e_coli3 == ">1000" ~ 1001,
  TRUE ~ as.numeric(e_coli3)))

e_coli_TO <- e_coli_TO |> mutate(e_coli4 = case_when(
  e_coli4 == "<10" ~ 5, 
  e_coli4 == ">1000" ~ 1001,
  TRUE ~ as.numeric(e_coli4)))

e_coli_TO <- e_coli_TO |> mutate(e_coli5 = case_when(
  e_coli5 == "<10" ~ 5, 
  e_coli5 == ">1000" ~ 1001,
  TRUE ~ as.numeric(e_coli5)))

e_coli_TO <- e_coli_TO |> mutate(e_coli6 = case_when(
  e_coli6 == "<10" ~ 5, 
  e_coli6 == ">1000" ~ 1001,
  TRUE ~ as.numeric(e_coli6)))

# Calculate geometric mean of individual samples and max value single sample

e_coli_TO <- e_coli_TO |> rowwise() |>
  mutate(e_coli = exp(mean(log(c(e_coli1, e_coli2, e_coli3, e_coli4, e_coli5, e_coli6)), na.rm = TRUE)))

e_coli_TO <- e_coli_TO |> rowwise() |>
  mutate(e_coli_max = max(c(e_coli1, e_coli2, e_coli3, e_coli4, e_coli5, e_coli6), na.rm = TRUE))

e_coli_TO <- e_coli_TO |> rowwise() |>
  mutate(e_coli_min = min(c(e_coli1, e_coli2, e_coli3, e_coli4, e_coli5, e_coli6), na.rm = TRUE))

data_TO <- data_TO |> 
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

data_TO <- left_join(data_TO, e_coli_TO, by = "date")

# Format MST data

mst_TO <- mst_TO |> 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) |> 
  mutate_all(function(x) gsub("BD", 0, x)) |> 
  mutate(HF183_human = as.numeric(HF183_human)) |> 
  mutate(Gull4_marker = as.numeric(Gull4_marker))

mst_TO <- mst_TO |> 
  group_by(date) |> 
  summarize(mst_human = mean(HF183_human),
            mst_gull = mean(Gull4_marker)) |> 
  ungroup()

mst_TO <- mst_TO |> 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) 

data_TO <- left_join(data_TO, mst_TO, by = "date")

remove(e_coli_TO, mst_TO)

data_TO |> export(here("Datasets", "Toronto", "data_TO_FIB.csv"))


## MANITOBA = Load, Format and Merge Lab Results
# For now, reformat <10 E. coli to 5 counts to allow averaging and merging with other sites

e_coli_MB <- e_coli_MB |> 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) 

e_coli_MB <- e_coli_MB |> 
  mutate(e_coli1 = ifelse(e_coli1 == "<10", 5, as.numeric(e_coli1)),
         e_coli2 = ifelse(e_coli2 == "<10", 5, as.numeric(e_coli2)))

e_coli_MB <- e_coli_MB |> rowwise() |>
  mutate(e_coli = exp(mean(log(c(e_coli1, e_coli2)), na.rm = TRUE)))

e_coli_MB <- e_coli_MB |> rowwise() |>
  mutate(e_coli_max = max(c(e_coli1, e_coli2)))

e_coli_MB <- e_coli_MB |> rowwise() |>
  mutate(e_coli_min = min(c(e_coli1, e_coli2)))

e_coli_MB <- e_coli_MB |> 
  mutate(entero_cce1 = ifelse(entero_cce1 == 0, 1, as.numeric(entero_cce1)),
         entero_cce2 = ifelse(entero_cce2 == 0, 1, as.numeric(entero_cce2)))

e_coli_MB <- e_coli_MB |> rowwise() |>
  mutate(entero_cce = exp(mean(log(c(entero_cce1, entero_cce2)), na.rm = TRUE))) 

e_coli_MB <- e_coli_MB |> rowwise() |>
  mutate(entero_cce_max = max(c(entero_cce1, entero_cce2), na.rm = TRUE))

data_MB <- data_MB |> 
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

data_MB <- left_join(data_MB, e_coli_MB, by = "date")

remove(e_coli_MB)

data_MB |> export(here("Datasets", "Manitoba", "data_MB_FIB.csv"))


## VANCOUVER = Load, Format and Merge Lab Results

e_coli_VAN <- e_coli_VAN |> 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) 

e_coli_VAN <- e_coli_VAN |> 
  mutate(e_coli1 = ifelse(e_coli1 == 0, 1, as.numeric(e_coli1)),
         e_coli2 = ifelse(e_coli2 == 0, 1, as.numeric(e_coli2)))

e_coli_VAN <- e_coli_VAN |> rowwise() |>
  mutate(e_coli = exp(mean(log(c(e_coli1, e_coli2)), na.rm = TRUE)))

e_coli_VAN <- e_coli_VAN |> rowwise() |>
  mutate(e_coli_max = max(c(e_coli1, e_coli2)))

e_coli_VAN <- e_coli_VAN |> rowwise() |>
  mutate(e_coli_min = min(c(e_coli1, e_coli2)))

e_coli_VAN <- e_coli_VAN |> 
  mutate(entero_cce1 = ifelse(entero_cce1 == 0, 1, as.numeric(entero_cce1)),
         entero_cce2 = ifelse(entero_cce2 == 0, 1, as.numeric(entero_cce2)))

e_coli_VAN <- e_coli_VAN |> rowwise() |>
  mutate(entero_cce = exp(mean(log(c(entero_cce1, entero_cce2)), na.rm = TRUE))) 

e_coli_VAN <- e_coli_VAN |> rowwise() |>
  mutate(entero_cce_max = max(c(entero_cce1, entero_cce2), na.rm = TRUE))

data_VAN <- data_VAN |> 
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

data_VAN <- left_join(data_VAN, e_coli_VAN, by = "date")

remove(e_coli_VAN)

data_VAN |> export(here("Datasets", "Vancouver", "data_VAN_FIB.csv"))

