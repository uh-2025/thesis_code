
quartier_df <- read.xlsx(paste0(source_path, "")) %>%
  select(HH_ID_23, Admin, m2)

hh_members <- hh_attribute_data %>% 
  filter(HH_members) %>% 
  select(household_id, individual_id) %>%
  filter(individual_id %in% ego_individuals$individual_id) %>%
  unique() %>%
  left_join(quartier_df, by = c("household_id" = "HH_ID_23"))

quartier_m2_df <- hh_members %>%
  group_by(individual_id) %>%
  slice_max(order_by = m2, n = 1)

ego_individuals <- ego_individuals %>%
  left_join(quartier_m2_df, by=c("individual_id")) %>%
  select(individual_id, age, sex, survey_group, Admin, m2) %>%
  set_colnames(c("individual_id", "age", "sex", "survey_group", "quartier", "house_size"))


