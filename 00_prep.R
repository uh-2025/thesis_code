age_df <- read.xlsx(paste0(source_path, ""))
sex_df <- read.xlsx(paste0(source_path, ""))

age_df$indiv_ID_c <- str_remove_all(age_df$indiv_ID_c, "[^A-z|0-9]")
sex_df$indiv_ID_c <- str_remove_all(sex_df$indiv_ID_c, "[^A-z|0-9]")

age_sex <- age_df %>%
  left_join(sex_df, by = c("indiv_ID_c"))

colnames(age_sex) <- c("individual_id", "age", "sex")

age_sex[age_sex$sex == "Unknown",]$sex <- NA
age_sex$sex <- as.factor(age_sex$sex)


Intercodeshouses <- read.xlsx(paste0(source_path, ""))


Intercodeshouses$Cour_ID <- str_remove_all(Intercodeshouses$Cour_ID, "[^A-z|0-9]")
Intercodeshouses$HH_ID <- str_remove_all(Intercodeshouses$HH_ID, "[^A-z|0-9]")

compound_household_relation <- Intercodeshouses %>% 
  select(Cour_ID, HH_ID) %>% 
  unique()

colnames(compound_household_relation) <- c("compound_id", "household_id")


Intercodeshouses <- read.xlsx(paste0(source_path, ""))


Intercodeshouses$HH_ID <- str_remove_all(Intercodeshouses$HH_ID, "[^A-z|0-9]")

house_size <- Intercodeshouses %>%
  select(HH_ID, m2) %>%
  filter(!is.na(HH_ID)) %>%
  unique()

colnames(house_size) <-c("household_id", "house_size_m2")


HHegodata <- read.xlsx(paste0(source_path, ""))

HHegodata$HH_ID <- str_remove_all(HHegodata$HH_ID, "[^A-z|0-9]")

hh_ego_data <- HHegodata %>%
  select(HH_ID,
         neighbourhood_coran_sch_1, neighbourhood_coran_sch_2,
         neighbourhood_coran_sch_3, neighbourhood_coran_sch_4,
         neighbourhood_coran_sch_5, neighbourhood_coran_sch_6,
         neighbourhood_coran_sch_7, neighbourhood_coran_sch_8) %>%
  unique()

colnames(hh_ego_data)[1] <- "household_id"



HHattrdata <- read.xlsx(paste0(source_path, ""))

HHattrdata$HH_ID <- str_remove_all(HHattrdata$HH_ID, "[^A-z|0-9]")
HHattrdata$indiv_ID_c <- str_remove_all(HHattrdata$indiv_ID_c, "[^A-z|0-9]")

hh_attribute_data <- HHattrdata %>%
  left_join(age_sex, by = c("indiv_ID_c" = "individual_id")) %>%
  select(HH_ID, indiv_ID_c, 
         age, sex, HH_members, 
         place,
         sleep, sleep_mobile,
         sleeping_x, sleeping_y,
         eat, eat_mobile,
         eating_x, eating_y,
         Child_caretak,
         child_coranic_attend,
         child_french_school_attend,
         FrenchSchool_place_1, FrenchSchool_place_2,
         FrenchSchool_place_3, FrenchSchool_place_4,
         FrenchSchool_place_5, FrenchSchool_place_6,
         FrenchSchool_place_7, FrenchSchool_place_8)

colnames(hh_attribute_data)[c(1,2)] <- c("household_id", "individual_id")



Indivegodata <- read.xlsx(paste0(source_path, ""))

Indivegodata$indiv_ID <- str_remove_all(Indivegodata$indiv_ID, "[^A-z|0-9]")

ind_ego_data <- Indivegodata %>%
  left_join(age_sex, by = c("indiv_ID" = "individual_id")) %>%
  select(indiv_ID, age, sex,
         kids, kids_adopted,
         HH_ID_extra,
         coranic_sch,
         neighbourhood_coranicSchool_1,
         neighbourhood_coranicSchool_2,
         neighbourhood_coranicSchool_3,
         neighbourhood_coranicSchool_4,
         neighbourhood_coranicSchool_5,
         neighbourhood_coranicSchool_6,
         neighbourhood_coranicSchool_7,
         schools, univ,
         mosque_1, mosque_2, mosque_3,
         mosque_4, mosque_5, mosque_6,
         mosque_7, mosque_8,
         neighbourhood_madrass_1, neighbourhood_madrass_2,
         neighbourhood_madrass_3, neighbourhood_madrass_4,
         neighbourhood_madrass_5, neighbourhood_madrass_6,
         neighbourhood_madrass_7,
         work_1, work_2, work_3, work_4, work_5, work_6,
         work_7, work_8, work_9, work_10, work_11, work_12,
         work_13, work_14, work_15, work_o,
         studies_level)

colnames(ind_ego_data)[1] <- "individual_id"



Indivattrdata <- read.xlsx(paste0(source_path, ""))

ind_attribute_data <- Indivattrdata %>%
  mutate(case_ID = str_remove_all(case_ID, "[^A-Za-z0-9]"),
         individual_1_id = substr(case_ID, 7, 11),
         individual_1_id = str_remove_all(individual_1_id, "[^A-z|0-9]"),
         individual_2_id = str_remove_all(indiv_ID_c, "[^A-z|0-9]"))
