rm(list = ls())

library(tidyverse)
library(lubridate)
library(RDS)

diary1 <- read_csv("data/diary/amethist1_data 20230530.csv")
diary1_demog <- read_csv("data/diary/amethist1_demographics.csv")
#diary1_diary <- read_csv("data/diary/amethist1_diary.csv")
diary2 <- read_csv("data/diary/AMETHIST2-Amethist2_DATA_2023-05-30_0922.csv")
diary3 <- read_csv("data/diary/AMETHIST3_DATA_2023-05-30_0923.csv")

# entire RDS data
rds_data <- read_csv("data/amethist_rds_survey_raw.csv")

# additional data from Wezzie Nyapigoti

participant_data <- read_csv("data/diary/participants_log_071222.csv")

# ----------------------------------------------------------------------------
# data management
# ----------------------------------------------------------------------------

diary1_data <- diary1 |>
  rename(pid = pid1) |>
  mutate(pid = gsub("B", "D", pid)) |>
  filter(redcap_repeat_instrument != "Diary Demo") |>
  mutate(oral = ifelse(oral == "Yes", 1, 0)) |>
  mutate(anal = ifelse(anal == "Yes", 1, 0)) |>
  mutate(vaginal = ifelse(vaginal == "Yes", 1, 0)) |>
  mutate(condomuse = case_when(
    condomuse == "Kugwiritsa ntchito kondomu nthawi zonse" ~ 1,
    condomuse == "Kugwiritsa ntchito kondomu nthawi zina apena kondomu kubooka " ~ 2,
    condomuse == "Osagwiritsa ntchito kondomu" ~ 3)) |>
  mutate(client = case_when(
    client1 == "Chibwenzi chokhazikika kapena mwamuna wanu" ~ 1,
    client1 == "Mwamuna obwera bwera" ~ 2,
    client1 == "Mwamuna watsopano" ~ 3)) |>
  mutate(condoms = ifelse(condoms == "Yes", 1, 0)) |>
  mutate(verbalabuse = ifelse(verbalabuse == "Yes", 1, 0)) |>
  mutate(physical = ifelse(physical == "Yes", 1, 0)) |>
  mutate(sex_abuse = ifelse(sex_abuse == "Yes", 1, 0)) |>
  select(-client1, -diary_demo_complete) |>
  relocate(client, .after = clients)

# filter participants with zero clients but repeat instance

noclient <- filter(diary1, clients == 0 & !is.na(redcap_repeat_instance))
noclient2 <- filter(diary1_data, clients == 0 & redcap_repeat_instance > 0)


# change to wide format
diary1_data_wide <- diary1_data |>
  #mutate(row_num = row_number()) |>
  pivot_wider(names_from = redcap_repeat_instance, values_from = c(client, clientage, oral, vaginal,
                                                    anal, condomuse, verbalabuse, physical,
                                                    sex_abuse),
              names_sep = "") 

# replace NULL values from the data



diary1_demog <- diary1_demog |>
  rename(pid = pid1)

diary1_diary <- diary1_diary |>
  rename(pid = pid1)

#diary1_data <- diary1 |>
#  mutate(diary_date)


diary2_data <- diary2 |>
  mutate(diary_date = dmy(diary_date)) |>
  mutate(redcap_event_name = gsub("_arm_1","", redcap_event_name)) |>
  mutate(redcap_event_name = gsub("_"," ", redcap_event_name)) |>
  mutate(redcap_event_name = str_to_sentence(redcap_event_name))

diary3_data <- diary3 |>
  mutate(diary_date = dmy(diary_date)) |>
  mutate(redcap_event_name = gsub("_arm_1","", redcap_event_name)) |>
  mutate(redcap_event_name = gsub("_"," ", redcap_event_name)) |>
  mutate(redcap_event_name = str_to_sentence(redcap_event_name))

# process additional participant data
pid_data <- participant_data |>
  select(-`Geographical location`,-`Sex work venue`,-`Date of diary 5`,-`Date of diary 6`,-Comment) |>
  select(-Syphilis, -Chlamydia,-`Type of sex work`) |>
  mutate(date_diary1 = `Date of diary 1`, date_diary2 = `Date of diary 2`) |>
  mutate(date_diary3 = `Date of diary 3`, date_diary4 = `Date of diary 1`) |>
  mutate(date_diary4 = `Date of diary 4`) |>
  mutate(date_acasi1 = `Date of ACASI 1`, date_acasi2 = `Date of ACASI 2`,
         date_acasi3 = `Date of ACASI 3`) |>
  select(-`Date of diary 1`,-`Date of diary 2`,-`Date of diary 3`,-`Date of diary 4`) |>
  select(-`Date of ACASI 1`,-`Date of ACASI 2`,-`Date of ACASI 3`,-HPV, -`Staff ID No`) |>
  select(-`PK UIC`, -`Seed UIC`) |>
  mutate(rds_date = dmy(`RDS date`)) |>
  mutate(date_diary1 = dmy(date_diary1)) |>
  mutate(date_diary2 = dmy(date_diary2)) |>
  mutate(date_diary3 = dmy(date_diary3)) |>
  mutate(date_acasi1 = dmy(date_acasi1)) |>
  mutate(date_acasi2 = dmy(date_acasi2)) |>
  mutate(date_acasi3 = dmy(date_acasi3))

#----------------------------------------------------------------------
# participation 
# ---------------------------------------------------------------------
  