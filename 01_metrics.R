length(unique(hh_ego_data$household_id))
length(unique(hh_attribute_data$household_id))
length(unique(ind_ego_data$individual_id))

household_unique <- hh_attribute_data %>%
  select(individual_id, age, sex) %>%
  unique()

children <- household_unique %>%
  filter(age < 10)

summary(children)
children$survey_group <- "child"

ego_teens <- household_unique %>%
  filter(age >= 10 & age < 15) %>%
  filter(individual_id %in% ind_ego_data$individual_id)

summary(ego_teens)
ego_teens$survey_group <- "teen"

ego_adults <- household_unique %>%
  filter(age >= 15) %>%
  filter(individual_id %in% ind_ego_data$individual_id)

summary(ego_adults)
ego_adults$survey_group <- "adult"

ego_individuals <- rbind(children, ego_teens, ego_adults)


alter_individuals <- age_sex %>%
  filter(!(individual_id %in% ego_individuals$individual_id))

summary(alter_individuals)

alter_children <- alter_individuals %>%
  filter(age < 10)

summary(alter_children)

alter_teens <- alter_individuals %>%
  filter(age >= 10 & age < 15)

summary(alter_teens)

alter_adults <- alter_individuals %>%
  filter(age >= 15)

summary(alter_adults)

alter_na <- alter_individuals %>%
  filter(is.na(age))

summary(alter_na)

ego_vec <- hh_attribute_data %>% filter(HH_members) %>% select(individual_id) %>% unique() %>% pull()
ego_alter_vec <- ego_vec[!(ego_vec %in% ego_individuals$individual_id)]
