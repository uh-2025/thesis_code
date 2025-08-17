# distance

location_polygon <- st_read(paste0(source_path, ""))
location_point <- st_point_on_surface(x = location_polygon)
location_projection <- st_transform(location_point, crs = 32738)
distance_mat <- st_distance(location_projection)

rownames(distance_mat) <- location_projection$HH_ID_23
colnames(distance_mat) <- location_projection$HH_ID_23

hh_members <- hh_attribute_data %>% 
  filter(HH_members) %>% 
  select(household_id, individual_id) %>%
  filter(individual_id %in% ego_individuals$individual_id) %>%
  unique()

unique_individuals <- unique(hh_members$individual_id)


individual_distance <- matrix(ncol = 2548, nrow = 2548)
rownames(individual_distance) <- ego_individuals$individual_id
colnames(individual_distance) <- ego_individuals$individual_id
individual_distance[1,1] <- 0


for (i in 1:length(unique_individuals)){
  selection_df <- hh_members %>% filter(individual_id == unique_individuals[i])
  distance_df_total <- data.frame(hh_id = NA, distance = c(0))
  for (j in 1:nrow(selection_df)){
    distance_vec <- distance_mat[,selection_df[j,1]]
    distance_df_loop <- data.frame(hh_id = names(distance_vec), distance = unclass(distance_vec))
    distance_df_total <- rbind(distance_df_total,distance_df_loop)
  }
  distance_df_summary <- distance_df_total[complete.cases(distance_df_total),] %>%
    group_by(hh_id) %>%
    summarise(min_distance = min(distance)) %>% ungroup()
  
  ind_distance_df <- hh_members %>%
    left_join(distance_df_summary, by = c("household_id" = "hh_id")) %>%
    group_by(individual_id) %>%
    summarise(ind_distance = min(min_distance)) %>%
    ungroup()
  
  ind_distance_df_ordered <- ind_distance_df[match(rownames(individual_distance), ind_distance_df$individual_id), ]
  
  individual_distance[,unique_individuals[i]] <- ind_distance_df_ordered$ind_distance
  
}

if(FALSE){
  ggplot() + 
    geom_sf(data = location_polygon, size = 1.5, color = "black", fill = "cyan1") + 
    ggtitle("village") + 
    coord_sf()
}

# household
hh_members <- hh_attribute_data %>% 
  filter(HH_members) %>% 
  select(household_id, individual_id) %>%
  unique()

edge_same_household <- hh_members %>%
  left_join(hh_members, by = c("household_id"), relationship = "many-to-many") %>%
  set_colnames(c("household_id", "individual_1", "individual_2")) %>%
  filter(individual_1 < individual_2) %>%
  select(individual_1, individual_2) %>%
  mutate(ego_1 = individual_1 %in% ego_individuals$individual_id,
         ego_2 = individual_2 %in% ego_individuals$individual_id) %>%
  filter(ego_1 | ego_2) %>%
  mutate(connection_string = paste0(individual_1, "_", individual_2)) %>%
  select(individual_1, individual_2, connection_string, ego_1, ego_2) %>%
  unique()

nrow(edge_same_household)
nrow(edge_same_household %>% filter(ego_1 & ego_2))
nrow(edge_same_household %>% rowwise() %>% filter(sum(ego_1, ego_2) ==1 ))

included_individuals <- unique(c(edge_same_household$individual_1, edge_same_household$individual_2))
length(included_individuals)
sum(included_individuals %in% ego_individuals$individual_id)
sum(included_individuals %in% alter_individuals$individual_id)

# eating
house_eating <- readPNG("")

hh_eating_data <- hh_attribute_data %>%
  filter(!is.na(eating_x)) %>%
  select(household_id, individual_id,
         eating_x, eating_y) %>%
  mutate(eating_y_inv = 1 - eating_y,
         ## down left
         room = ifelse(eating_x < 0.38 &
                         eating_y_inv < 0.35, "room1", "outside"),
         ## middle left
         room = ifelse(eating_x < 0.38 &
                         eating_y_inv > 0.35 &
                         eating_y_inv < 0.5, "room2", room),
         ## top left
         room = ifelse(eating_x < 0.38 &
                         eating_y_inv > 0.7, "room3", room),
         ## down middle
         room = ifelse(eating_x > 0.38 &
                         eating_x < 0.58 &
                         eating_y_inv < 0.35, "room4", room),
         ## middle middle
         room = ifelse(eating_x > 0.38 &
                         eating_x < 0.58 &
                         eating_y_inv > 0.35 &
                         eating_y_inv < 0.7, "room5", room),
         ## top middle
         room = ifelse(eating_x > 0.38 &
                         eating_x < 0.58 &
                         eating_y_inv > 0.7, "room6", room),
         ## down right
         room = ifelse(eating_x > 0.58 &
                         eating_x < 0.71 &
                         eating_y_inv < 0.35, "room7", room),
         ## middle right
         room = ifelse(eating_x > 0.58 &
                         eating_x < 0.71 &
                         eating_y_inv > 0.35 &
                         eating_y_inv < 0.7, "room8", room),
         house_room_str = paste0(household_id, "_", room))


eating_inside <- hh_eating_data %>%
  filter(!(room == "outside")) %>%
  select(house_room_str, individual_id)

edge_eating_together <- eating_inside %>%
  left_join(eating_inside, by = c("house_room_str"), relationship = "many-to-many") %>%
  set_colnames(c("house_room_str", "individual_1", "individual_2")) %>%
  filter(individual_1 < individual_2) %>%
  select(individual_1, individual_2) %>%
  mutate(ego_1 = individual_1 %in% ego_individuals$individual_id,
         ego_2 = individual_2 %in% ego_individuals$individual_id) %>%
  filter(ego_1 | ego_2) %>%
  mutate(connection_string = paste0(individual_1, "_", individual_2)) %>%
  select(individual_1, individual_2, connection_string, ego_1, ego_2) %>%
  unique()


nrow(edge_eating_together)
nrow(edge_eating_together %>% filter(ego_1 & ego_2))
nrow(edge_eating_together %>% rowwise() %>% filter(sum(ego_1, ego_2) ==1 ))


included_individuals <- unique(c(edge_eating_together$individual_1, edge_eating_together$individual_2))
length(included_individuals)
sum(included_individuals %in% ego_individuals$individual_id)
sum(included_individuals %in% alter_individuals$individual_id)

# sleeping
hh_sleeping_data <- hh_attribute_data %>%
  filter(!is.na(sleeping_x)) %>%
  select(household_id, individual_id,
         sleeping_x, sleeping_y) %>%
  mutate(sleeping_y_inv = 1 - sleeping_y,
         ## down left
         room = ifelse(sleeping_x < 0.38 &
                         sleeping_y_inv < 0.35, "room1", "outside"),
         ## middle left
         room = ifelse(sleeping_x < 0.38 &
                         sleeping_y_inv > 0.35 &
                         sleeping_y_inv < 0.7, "room2", room),
         ## top left
         room = ifelse(sleeping_x < 0.38 &
                         sleeping_y_inv > 0.7, "room3", room),
         ## down middle
         room = ifelse(sleeping_x > 0.38 &
                         sleeping_x < 0.58 &
                         sleeping_y_inv < 0.35, "room4", room),
         ## middle middle
         room = ifelse(sleeping_x > 0.38 &
                         sleeping_x < 0.58 &
                         sleeping_y_inv > 0.35 &
                         sleeping_y_inv < 0.7, "room5", room),
         house_room_str = paste0(household_id, "_", room))


sleeping_inside <- hh_sleeping_data %>%
  filter(!(room == "outside")) %>%
  select(house_room_str, individual_id)

sleeping_together <- sleeping_inside %>%
  left_join(sleeping_inside, by = c("house_room_str"), relationship = "many-to-many") %>%
  set_colnames(c("house_room_str", "individual_1", "individual_2")) %>%
  filter(individual_1 < individual_2) %>%
  select(individual_1, individual_2) %>%
  mutate(ego_1 = individual_1 %in% ego_individuals$individual_id,
         ego_2 = individual_2 %in% ego_individuals$individual_id) %>%
  filter(ego_1 | ego_2) %>%
  mutate(connection_string = paste0(individual_1, "_", individual_2)) %>%
  select(individual_1, individual_2, connection_string) %>%
  unique()

sleeping_friends <- ind_attribute_data %>%
  filter(sleep_friends_n) %>%
  select(individual_1_id, individual_2_id) %>%
  mutate(individual_1 = ifelse(individual_1_id < individual_2_id, individual_1_id, individual_2_id),
         individual_2 = ifelse(individual_1_id < individual_2_id, individual_2_id, individual_1_id)) %>%
  mutate(connection_string = paste0(individual_1, "_", individual_2)) %>%
  select(individual_1, individual_2, connection_string) %>%
  unique()

edge_shared_bedroom <- rbind(sleeping_together, sleeping_friends) %>%
  mutate(ego_1 = individual_1 %in% ego_individuals$individual_id,
         ego_2 = individual_2 %in% ego_individuals$individual_id) %>%
  filter(ego_1 | ego_2) %>%
  unique()


nrow(edge_shared_bedroom)
nrow(edge_shared_bedroom %>% filter(ego_1 & ego_2))
nrow(edge_shared_bedroom %>% rowwise() %>% filter(sum(ego_1, ego_2) ==1 ))

included_individuals <- unique(c(edge_shared_bedroom$individual_1, edge_shared_bedroom$individual_2))
length(included_individuals)
sum(included_individuals %in% ego_individuals$individual_id)
sum(included_individuals %in% alter_individuals$individual_id)


# caretaker

caretaker_list <- hh_attribute_data %>%
  filter(Child_caretak) %>%
  select(household_id, individual_id)


house_list <- caretaker_list %>%
  pull(household_id) %>% 
  unique()

child_list <- hh_attribute_data %>%
  filter(household_id %in% house_list) %>%
  filter(age < 15) %>%
  select(household_id, individual_id)

edge_caretaker_link <- child_list %>%
  left_join(caretaker_list, by = c("household_id"), relationship = "many-to-many") %>%
  set_colnames(c("household_id", "individual_1_id", "individual_2_id")) %>%
  mutate(individual_1 = ifelse(individual_1_id < individual_2_id, individual_1_id, individual_2_id),
         individual_2 = ifelse(individual_1_id < individual_2_id, individual_2_id, individual_1_id)) %>%
  mutate(connection_string = paste0(individual_1, "_", individual_2)) %>%
  mutate(ego_1 = individual_1 %in% ego_individuals$individual_id,
         ego_2 = individual_2 %in% ego_individuals$individual_id) %>%
  filter(ego_1 | ego_2) %>%
  select(individual_1, individual_2, connection_string, ego_1, ego_2) %>%
  unique()

nrow(edge_caretaker_link)
nrow(edge_caretaker_link %>% filter(ego_1 & ego_2))
nrow(edge_caretaker_link %>% rowwise() %>% filter(sum(ego_1, ego_2) ==1 ))

included_individuals <- unique(c(edge_caretaker_link$individual_1, edge_caretaker_link$individual_2))
length(included_individuals)
sum(included_individuals %in% ego_individuals$individual_id)
sum(included_individuals %in% alter_individuals$individual_id)


# marriage

edge_marriage_partners <- ind_attribute_data %>%
  filter(partners) %>%
  select(individual_1_id, individual_2_id) %>%
  mutate(individual_1 = ifelse(individual_1_id < individual_2_id, individual_1_id, individual_2_id),
         individual_2 = ifelse(individual_1_id < individual_2_id, individual_2_id, individual_1_id)) %>%
  mutate(connection_string = paste0(individual_1, "_", individual_2)) %>%
  mutate(ego_1 = individual_1 %in% ego_individuals$individual_id,
         ego_2 = individual_2 %in% ego_individuals$individual_id) %>%
  filter(ego_1 | ego_2) %>%
  select(individual_1, individual_2, connection_string, ego_1, ego_2) %>%
  unique()

nrow(edge_marriage_partners)
nrow(edge_marriage_partners %>% filter(ego_1 & ego_2))
nrow(edge_marriage_partners %>% rowwise() %>% filter(sum(ego_1, ego_2) ==1 ))

included_individuals <- unique(c(edge_marriage_partners$individual_1, edge_marriage_partners$individual_2))
length(included_individuals)
sum(included_individuals %in% ego_individuals$individual_id)
sum(included_individuals %in% alter_individuals$individual_id)


# child parent


child_parent_list <- ind_attribute_data %>%
  select(individual_1_id, individual_2_id,
         kids_clusters_1, kids_clusters_2,
         kids_clusters_3, kids_clusters_4,
         kids_clusters_5, kids_clusters_6, kids_clusters_7) %>%
  filter(kids_clusters_1 | kids_clusters_2 | kids_clusters_3 |
           kids_clusters_4 | kids_clusters_5 | kids_clusters_6 |
           kids_clusters_7)

child_parent_list[is.na(child_parent_list$kids_clusters_6),]$kids_clusters_6 <- FALSE
child_parent_list[is.na(child_parent_list$kids_clusters_7),]$kids_clusters_7 <- FALSE

edge_childparent_link <- child_parent_list %>%
  select(individual_1_id, individual_2_id) %>%
  mutate(individual_1 = ifelse(individual_1_id < individual_2_id, individual_1_id, individual_2_id),
         individual_2 = ifelse(individual_1_id < individual_2_id, individual_2_id, individual_1_id)) %>%
  mutate(connection_string = paste0(individual_1, "_", individual_2)) %>%
  mutate(ego_1 = individual_1 %in% ego_individuals$individual_id,
         ego_2 = individual_2 %in% ego_individuals$individual_id) %>%
  filter(ego_1 | ego_2) %>%
  select(individual_1, individual_2, connection_string, ego_1, ego_2) %>%
  unique()

edge_coparents <- child_parent_list %>%
  left_join(child_parent_list, by = c("individual_2_id"), relationship = "many-to-many") %>%
  select(individual_1_id.x, individual_2_id, individual_1_id.y) %>%
  set_colnames(c("individual_1_id", "child", "individual_2_id")) %>%
  filter(individual_1_id < individual_2_id) %>%
  select(individual_1_id, individual_2_id) %>%
  mutate(individual_1 = ifelse(individual_1_id < individual_2_id, individual_1_id, individual_2_id),
         individual_2 = ifelse(individual_1_id < individual_2_id, individual_2_id, individual_1_id)) %>%
  mutate(connection_string = paste0(individual_1, "_", individual_2)) %>%
  mutate(ego_1 = individual_1 %in% ego_individuals$individual_id,
         ego_2 = individual_2 %in% ego_individuals$individual_id) %>%
  filter(ego_1 | ego_2) %>%
  select(individual_1, individual_2, connection_string, ego_1, ego_2) %>%
  unique()

child_parent_cluster <- child_parent_list %>%
  mutate(child_cluster = ifelse(kids_clusters_1, "cluster_1", "no_cluster"),
         child_cluster = ifelse(kids_clusters_2, "cluster_2", child_cluster),
         child_cluster = ifelse(kids_clusters_3, "cluster_3", child_cluster),
         child_cluster = ifelse(kids_clusters_4, "cluster_4", child_cluster),
         child_cluster = ifelse(kids_clusters_5, "cluster_5", child_cluster),
         child_cluster = ifelse(kids_clusters_6, "cluster_6", child_cluster),
         child_cluster = ifelse(kids_clusters_7, "cluster_7", child_cluster)) %>%
  select(individual_1_id, individual_2_id, child_cluster) %>%
  set_colnames(c("parent_id", "child_id", "cluster")) %>%
  mutate(parent_cluster = paste0(parent_id, "_", cluster)) %>%
  select(parent_cluster, child_id)


edge_siblings <- child_parent_cluster %>%
  left_join(child_parent_cluster, by = c("parent_cluster"), relationship = "many-to-many") %>%
  filter(child_id.x < child_id.y) %>%
  select(child_id.x, child_id.y) %>%
  set_colnames(c("individual_1_id", "individual_2_id")) %>%
  mutate(individual_1 = ifelse(individual_1_id < individual_2_id, individual_1_id, individual_2_id),
         individual_2 = ifelse(individual_1_id < individual_2_id, individual_2_id, individual_1_id)) %>%
  mutate(connection_string = paste0(individual_1, "_", individual_2)) %>%
  mutate(ego_1 = individual_1 %in% ego_individuals$individual_id,
         ego_2 = individual_2 %in% ego_individuals$individual_id) %>%
  filter(ego_1 | ego_2) %>%
  select(individual_1, individual_2, connection_string, ego_1, ego_2) %>%
  unique()



nrow(edge_childparent_link)
nrow(edge_childparent_link %>% filter(ego_1 & ego_2))
nrow(edge_childparent_link %>% rowwise() %>% filter(sum(ego_1, ego_2) ==1 ))

included_individuals <- unique(c(edge_childparent_link$individual_1, edge_childparent_link$individual_2))
length(included_individuals)
sum(included_individuals %in% ego_individuals$individual_id)
sum(included_individuals %in% alter_individuals$individual_id)


nrow(edge_coparents)
nrow(edge_coparents %>% filter(ego_1 & ego_2))
nrow(edge_coparents %>% rowwise() %>% filter(sum(ego_1, ego_2) ==1 ))

included_individuals <- unique(c(edge_coparents$individual_1, edge_coparents$individual_2))
length(included_individuals)
sum(included_individuals %in% ego_individuals$individual_id)
sum(included_individuals %in% alter_individuals$individual_id)


nrow(edge_siblings)
nrow(edge_siblings %>% filter(ego_1 & ego_2))
nrow(edge_siblings %>% rowwise() %>% filter(sum(ego_1, ego_2) ==1 ))

included_individuals <- unique(c(edge_siblings$individual_1, edge_siblings$individual_2))
length(included_individuals)
sum(included_individuals %in% ego_individuals$individual_id)
sum(included_individuals %in% alter_individuals$individual_id)

# compound

compound_members <- hh_attribute_data %>%
  left_join(compound_household_relation, by = c("household_id")) %>%
  filter(HH_members) %>%
  select(compound_id, individual_id) %>%
  unique()

edge_same_compound <- compound_members %>%
  left_join(compound_members, by = c("compound_id"), relationship = "many-to-many") %>%
  set_colnames(c("compound_id", "individual_1", "individual_2")) %>%
  filter(individual_1 < individual_2) %>%
  select(individual_1, individual_2) %>%
  mutate(ego_1 = individual_1 %in% ego_individuals$individual_id,
         ego_2 = individual_2 %in% ego_individuals$individual_id) %>%
  filter(ego_1 | ego_2) %>%
  mutate(connection_string = paste0(individual_1, "_", individual_2)) %>%
  select(individual_1, individual_2, connection_string, ego_1, ego_2) %>%
  unique()


ee_edges <- nrow(edge_same_compound %>% filter(ego_1 & ego_2))
ae_edges <- nrow(edge_same_compound %>% rowwise() %>% filter(sum(ego_1, ego_2) ==1 ))
all_edges <- nrow(edge_same_compound)

included_individuals <- unique(c(edge_same_compound$individual_1, edge_same_compound$individual_2))
ego_ind <- sum(included_individuals %in% ego_individuals$individual_id)
paste(ee_edges, "&", ae_edges, "&", all_edges, "&", ego_ind, "\\")

# visitor
true_members <- hh_attribute_data %>%
  filter(HH_members) %>%
  select(household_id, individual_id) %>%
  unique()

house_individual_combo <- paste0(true_members$household_id, "_", true_members$individual_id)

visitor <- hh_attribute_data %>%
  filter(!HH_members | is.na(HH_members)) %>%
  mutate(house_person_combo_vis = paste0(household_id, "_", individual_id)) %>%
  filter(!(house_person_combo_vis %in% house_individual_combo)) %>%
  select(household_id, individual_id) %>%
  unique()

edge_visitor <- true_members %>%
  left_join(visitor, by = c("household_id"), relationship = "many-to-many") %>%
  set_colnames(c("compound_id", "individual_1_id", "individual_2_id")) %>%
  select(individual_1_id, individual_2_id) %>%
  drop_na(individual_2_id) %>%
  mutate(individual_1 = ifelse(individual_1_id < individual_2_id, individual_1_id, individual_2_id),
         individual_2 = ifelse(individual_1_id < individual_2_id, individual_2_id, individual_1_id)) %>%
  mutate(connection_string = paste0(individual_1, "_", individual_2)) %>%
  mutate(ego_1 = individual_1 %in% ego_individuals$individual_id,
         ego_2 = individual_2 %in% ego_individuals$individual_id) %>%
  filter(ego_1 | ego_2) %>%
  select(individual_1, individual_2, connection_string, ego_1, ego_2) %>%
  unique()

ee_edges <- nrow(edge_visitor %>% filter(ego_1 & ego_2))
ae_edges <- nrow(edge_visitor %>% rowwise() %>% filter(sum(ego_1, ego_2) ==1 ))
all_edges <- nrow(edge_visitor)

included_individuals <- unique(c(edge_visitor$individual_1, edge_visitor$individual_2))
ego_ind <- sum(included_individuals %in% ego_individuals$individual_id)
paste(ee_edges, "&", ae_edges, "&", all_edges, "&", ego_ind, "\\")

# mentioned

edge_mentioned_close <- ind_attribute_data %>%
  filter(close) %>%
  select(individual_1_id, individual_2_id) %>%
  mutate(individual_1 = ifelse(individual_1_id < individual_2_id, individual_1_id, individual_2_id),
         individual_2 = ifelse(individual_1_id < individual_2_id, individual_2_id, individual_1_id)) %>%
  mutate(ego_1 = individual_1 %in% ego_individuals$individual_id,
         ego_2 = individual_2 %in% ego_individuals$individual_id) %>%
  filter(ego_1 | ego_2) %>%
  mutate(connection_string = paste0(individual_1, "_", individual_2)) %>%
  select(individual_1, individual_2, connection_string, ego_1, ego_2) %>%
  unique()

ee_edges <- nrow(edge_mentioned_close %>% filter(ego_1 & ego_2))
ae_edges <- nrow(edge_mentioned_close %>% rowwise() %>% filter(sum(ego_1, ego_2) ==1 ))
all_edges <- nrow(edge_mentioned_close)

included_individuals <- unique(c(edge_mentioned_close$individual_1, edge_mentioned_close$individual_2))
ego_ind <- sum(included_individuals %in% ego_individuals$individual_id)
paste("&", ee_edges, "&", ae_edges, "&", all_edges, "&", ego_ind, "\\")


# colleagues
edge_colleagues <- ind_attribute_data %>%
  filter(colleagues) %>%
  select(individual_1_id, individual_2_id) %>%
  mutate(individual_1 = ifelse(individual_1_id < individual_2_id, individual_1_id, individual_2_id),
         individual_2 = ifelse(individual_1_id < individual_2_id, individual_2_id, individual_1_id)) %>%
  mutate(ego_1 = individual_1 %in% ego_individuals$individual_id,
         ego_2 = individual_2 %in% ego_individuals$individual_id) %>%
  filter(ego_1 | ego_2) %>%
  mutate(connection_string = paste0(individual_1, "_", individual_2)) %>%
  select(individual_1, individual_2, connection_string, ego_1, ego_2) %>%
  unique()

ee_edges <- nrow(edge_colleagues %>% filter(ego_1 & ego_2))
ae_edges <- nrow(edge_colleagues %>% rowwise() %>% filter(sum(ego_1, ego_2) ==1 ))
all_edges <- nrow(edge_colleagues)

included_individuals <- unique(c(edge_colleagues$individual_1, edge_colleagues$individual_2))
ego_ind <- sum(included_individuals %in% ego_individuals$individual_id)
paste("&", ee_edges, "&", ae_edges, "&", all_edges, "&", ego_ind, "\\")


# play


edge_playmates <- ind_attribute_data %>%
  filter(play) %>%
  select(individual_1_id, individual_2_id) %>%
  mutate(individual_1 = ifelse(individual_1_id < individual_2_id, individual_1_id, individual_2_id),
         individual_2 = ifelse(individual_1_id < individual_2_id, individual_2_id, individual_1_id)) %>%
  mutate(ego_1 = individual_1 %in% ego_individuals$individual_id,
         ego_2 = individual_2 %in% ego_individuals$individual_id) %>%
  filter(ego_1 | ego_2) %>%
  mutate(connection_string = paste0(individual_1, "_", individual_2)) %>%
  select(individual_1, individual_2, connection_string, ego_1, ego_2) %>%
  unique()

ee_edges <- nrow(edge_playmates %>% filter(ego_1 & ego_2))
ae_edges <- nrow(edge_playmates %>% rowwise() %>% filter(sum(ego_1, ego_2) ==1 ))
all_edges <- nrow(edge_playmates)

included_individuals <- unique(c(edge_playmates$individual_1, edge_playmates$individual_2))
ego_ind <- sum(included_individuals %in% ego_individuals$individual_id)
paste("&", ee_edges, "&", ae_edges, "&", all_edges, "&", ego_ind, "\\")


# study

edge_study_mates <- ind_attribute_data %>%
  filter(study_mates | child_coranic_playmate) %>%
  select(individual_1_id, individual_2_id) %>%
  mutate(individual_1 = ifelse(individual_1_id < individual_2_id, individual_1_id, individual_2_id),
         individual_2 = ifelse(individual_1_id < individual_2_id, individual_2_id, individual_1_id)) %>%
  mutate(ego_1 = individual_1 %in% ego_individuals$individual_id,
         ego_2 = individual_2 %in% ego_individuals$individual_id) %>%
  filter(ego_1 | ego_2) %>%
  mutate(connection_string = paste0(individual_1, "_", individual_2)) %>%
  select(individual_1, individual_2, connection_string, ego_1, ego_2) %>%
  unique()

ee_edges <- nrow(edge_study_mates %>% filter(ego_1 & ego_2))
ae_edges <- nrow(edge_study_mates %>% rowwise() %>% filter(sum(ego_1, ego_2) ==1 ))
all_edges <- nrow(edge_study_mates)

included_individuals <- unique(c(edge_study_mates$individual_1, edge_study_mates$individual_2))
ego_ind <- sum(included_individuals %in% ego_individuals$individual_id)


paste("&", ee_edges, "&", ae_edges, "&", all_edges, "&", ego_ind, "\\")

# coran

indiv_coran <- ind_ego_data %>% filter(coranic_sch == 1) %>%
  mutate(which_coran = ifelse(neighbourhood_coranicSchool_1, "coran_1", NA),
         which_coran = ifelse(neighbourhood_coranicSchool_2, "coran_2", which_coran),
         which_coran = ifelse(neighbourhood_coranicSchool_3, "coran_3", which_coran),
         which_coran = ifelse(neighbourhood_coranicSchool_4, "coran_4", which_coran),
         which_coran = ifelse(neighbourhood_coranicSchool_5, "coran_5", which_coran),
         which_coran = ifelse(neighbourhood_coranicSchool_6, "coran_6", which_coran),
         which_coran = ifelse(neighbourhood_coranicSchool_7, "coran_7", which_coran),
         goes_to_coran = TRUE) %>%
  filter(!is.na(which_coran)) %>%
  select(individual_id, goes_to_coran, which_coran) %>%
  
  coran <- hh_attribute_data %>% select(household_id, individual_id, child_coranic_attend) %>%
  left_join(age_sex, by = c("individual_id")) %>%
  filter(child_coranic_attend) %>%
  filter(age < 15) %>%
  left_join(hh_ego_data, by = c("household_id")) %>%
  mutate(nr_coran = neighbourhood_coran_sch_1 + neighbourhood_coran_sch_2 +
           neighbourhood_coran_sch_3 + neighbourhood_coran_sch_4 +
           neighbourhood_coran_sch_5 + neighbourhood_coran_sch_6 +
           neighbourhood_coran_sch_7 + neighbourhood_coran_sch_8) %>%
  select(household_id, individual_id, child_coranic_attend, age,
         neighbourhood_coran_sch_1, neighbourhood_coran_sch_2,
         neighbourhood_coran_sch_3, neighbourhood_coran_sch_4,
         neighbourhood_coran_sch_5, neighbourhood_coran_sch_6,
         neighbourhood_coran_sch_7, neighbourhood_coran_sch_8,
         nr_coran)

hh_coran <- coran %>%
  filter(nr_coran <= 1) %>%
  mutate(which_coran = ifelse(neighbourhood_coran_sch_1, "coran_1", NA),
         which_coran = ifelse(neighbourhood_coran_sch_2, "coran_2", which_coran),
         which_coran = ifelse(neighbourhood_coran_sch_3, "coran_3", which_coran),
         which_coran = ifelse(neighbourhood_coran_sch_4, "coran_4", which_coran),
         which_coran = ifelse(neighbourhood_coran_sch_5, "coran_5", which_coran),
         which_coran = ifelse(neighbourhood_coran_sch_6, "coran_6", which_coran),
         which_coran = ifelse(neighbourhood_coran_sch_7, "coran_7", which_coran),
         goes_to_coran = TRUE) %>%
  select(individual_id, goes_to_coran, which_coran)

coran_plus <- coran %>%
  filter(nr_coran > 1)

matches <- coran_plus[coran_plus$individual_id %in% indiv_coran$individual_id,]$individual_id
coran_plus <- coran_plus %>%
  filter(!(individual_id %in% matches))
indivatt <- ind_attribute_data %>% filter(child_coranic_playmate) %>%
  select(individual_1_id, individual_2_id)

indiv_list_1 <- coran_plus[coran_plus$individual_id %in% indivatt$individual_1_id,]$individual_id
indiv_list_2 <- coran_plus[coran_plus$individual_id %in% indivatt$individual_2_id,]$individual_id
friends <- indivatt[indivatt$individual_2_id %in% indiv_list_2,] 
friends$individual_1_id %in% indiv_coran$individual_id
fixed_coran <- friends %>%
  left_join(indiv_coran, by = c("individual_1_id" = "individual_id")) %>%
  select(individual_2_id, goes_to_coran, which_coran) %>%
  unique()

coran_plus <- coran_plus %>%
  filter(!(individual_id %in% fixed_coran$individual_2_id))

house_coran_1 <- coran_plus %>% filter(neighbourhood_coran_sch_1) %>%
  select(individual_id, child_coranic_attend) %>%
  mutate(which_coran = "coran_1")
house_coran_2 <- coran_plus %>% filter(neighbourhood_coran_sch_2) %>%
  select(individual_id, child_coranic_attend) %>%
  mutate(which_coran = "coran_2")
house_coran_3 <- coran_plus %>% filter(neighbourhood_coran_sch_3) %>%
  select(individual_id, child_coranic_attend) %>%
  mutate(which_coran = "coran_3")
house_coran_4 <- coran_plus %>% filter(neighbourhood_coran_sch_4) %>%
  select(individual_id, child_coranic_attend) %>%
  mutate(which_coran = "coran_4")
house_coran_5 <- coran_plus %>% filter(neighbourhood_coran_sch_5) %>%
  select(individual_id, child_coranic_attend) %>%
  mutate(which_coran = "coran_5")
house_coran_6 <- coran_plus %>% filter(neighbourhood_coran_sch_6) %>%
  select(individual_id, child_coranic_attend) %>%
  mutate(which_coran = "coran_6")
house_coran <- rbind(house_coran_1, house_coran_2, house_coran_3,
                     house_coran_4, house_coran_5, house_coran_6)
colnames(indiv_coran) <- c("individual_id", "goes_to_coran", "which_coran")
colnames(fixed_coran) <- c("individual_id", "goes_to_coran", "which_coran")
colnames(house_coran) <- c("individual_id", "goes_to_coran", "which_coran")

all_coran <- rbind(indiv_coran, fixed_coran, house_coran)
summary(as.factor(all_coran$which_coran))

edge_samen_coran <- all_coran %>%
  left_join(all_coran, by = c("which_coran"), relationship = "many-to-many") %>%
  select(individual_id.x, individual_id.y) %>%
  set_colnames(c("individual_1", "individual_2")) %>%
  filter(individual_1 < individual_2) %>%
  mutate(ego_1 = individual_1 %in% ego_individuals$individual_id,
         ego_2 = individual_2 %in% ego_individuals$individual_id) %>%
  filter(ego_1 | ego_2) %>%
  mutate(connection_string = paste0(individual_1, "_", individual_2)) %>%
  select(individual_1, individual_2, connection_string, ego_1, ego_2) %>%
  unique()

ee_edges <- nrow(edge_samen_coran %>% filter(ego_1 & ego_2))
ae_edges <- nrow(edge_samen_coran %>% rowwise() %>% filter(sum(ego_1, ego_2) ==1 ))
all_edges <- nrow(edge_samen_coran)

included_individuals <- unique(c(edge_samen_coran$individual_1, edge_samen_coran$individual_2))
ego_ind <- sum(included_individuals %in% ego_individuals$individual_id)
paste("&", ee_edges, "&", ae_edges, "&", all_edges, "&", ego_ind, "\\")


# french
survey_school <- ind_ego_data %>%
  filter(!is.na(schools)) %>%
  select(individual_id, schools) %>%
  set_colnames(c("individual_id", "school_name")) %>%
  mutate(which_school = ifelse(school_name == 1, "", NA),
         which_school = ifelse(school_name == 2, "", which_school),
         which_school = ifelse(school_name == 3, "", which_school),
         which_school = ifelse(school_name == 4, "", which_school),
         which_school = ifelse(school_name == 5, "", which_school),
         which_school = ifelse(school_name == 6, "", which_school),
         which_school = ifelse(school_name == 7, "", which_school),
         which_school = ifelse(school_name == 8, "", which_school)) %>%
  filter(!is.na(which_school)) %>%
  select(individual_id, which_school)

young_school <- hh_attribute_data %>%
  filter(child_french_school_attend) %>%
  mutate(which_school = ifelse(FrenchSchool_place_1, "", NA),
         which_school = ifelse(FrenchSchool_place_2, "", which_school),
         which_school = ifelse(FrenchSchool_place_3, "", which_school),
         which_school = ifelse(FrenchSchool_place_4, "", which_school),
         which_school = ifelse(FrenchSchool_place_5, "", which_school),
         which_school = ifelse(FrenchSchool_place_6, "", which_school),
         which_school = ifelse(FrenchSchool_place_7, "", which_school),
         which_school = ifelse(FrenchSchool_place_8, "", which_school)) %>%
  filter(!(individual_id %in% survey_school$individual_id)) %>%
  filter(!is.na(which_school)) %>%
  select(individual_id, which_school)

all_school <- rbind(young_school, survey_school)

edge_same_french <- all_school %>%
  left_join(all_school, by = c("which_school"), relationship = "many-to-many") %>%
  select(individual_id.x, individual_id.y) %>%
  set_colnames(c("individual_1", "individual_2")) %>%
  filter(individual_1 < individual_2) %>%
  mutate(ego_1 = individual_1 %in% ego_individuals$individual_id,
         ego_2 = individual_2 %in% ego_individuals$individual_id) %>%
  filter(ego_1 | ego_2) %>%
  mutate(connection_string = paste0(individual_1, "_", individual_2)) %>%
  select(individual_1, individual_2, connection_string, ego_1, ego_2) %>%
  unique()

ee_edges <- nrow(edge_same_french %>% filter(ego_1 & ego_2))
ae_edges <- nrow(edge_same_french %>% rowwise() %>% filter(sum(ego_1, ego_2) ==1 ))
all_edges <- nrow(edge_same_french)

included_individuals <- unique(c(edge_same_french$individual_1, edge_same_french$individual_2))
ego_ind <- sum(included_individuals %in% ego_individuals$individual_id)
paste("&", ee_edges, "&", ae_edges, "&", all_edges, "&", ego_ind, "\\")

# mosque
survey_mosque <- ind_ego_data %>%
  mutate(which_mosque = ifelse(mosque_1, "mosque_1", NA),
         which_mosque = ifelse(mosque_2, "mosque_2", which_mosque),
         which_mosque = ifelse(mosque_3, "mosque_3", which_mosque),
         which_mosque = ifelse(mosque_4, "mosque_4", which_mosque),
         which_mosque = ifelse(mosque_5, "mosque_5", which_mosque),
         which_mosque = ifelse(mosque_6, "mosque_6", which_mosque),
         which_mosque = ifelse(mosque_7, "mosque_7", which_mosque),
         which_mosque = ifelse(mosque_8, "mosque_8", which_mosque)) %>%
  filter(!is.na(which_mosque)) %>%
  select(individual_id, which_mosque) %>%
  set_colnames(c("individual_id", "which_mosque"))

edge_same_mosque <- survey_mosque %>%
  left_join(survey_mosque, by = c("which_mosque"), relationship = "many-to-many") %>%
  select(individual_id.x, individual_id.y) %>%
  set_colnames(c("individual_1", "individual_2")) %>%
  filter(individual_1 < individual_2) %>%
  mutate(ego_1 = individual_1 %in% ego_individuals$individual_id,
         ego_2 = individual_2 %in% ego_individuals$individual_id) %>%
  filter(ego_1 | ego_2) %>%
  mutate(connection_string = paste0(individual_1, "_", individual_2)) %>%
  select(individual_1, individual_2, connection_string, ego_1, ego_2) %>%
  unique()

ee_edges <- nrow(edge_same_mosque %>% filter(ego_1 & ego_2))
ae_edges <- nrow(edge_same_mosque %>% rowwise() %>% filter(sum(ego_1, ego_2) ==1 ))
all_edges <- nrow(edge_same_mosque)

included_individuals <- unique(c(edge_same_mosque$individual_1, edge_same_mosque$individual_2))
ego_ind <- sum(included_individuals %in% ego_individuals$individual_id)
paste("&", ee_edges, "&", ae_edges, "&", all_edges, "&", ego_ind, "\\")


# madrass
survey_madrass <- ind_ego_data %>%
  mutate(which_madrass = ifelse(neighbourhood_madrass_1, "madrass_1", NA),
         which_madrass = ifelse(neighbourhood_madrass_2, "madrass_2", which_madrass),
         which_madrass = ifelse(neighbourhood_madrass_3, "madrass_3", which_madrass),
         which_madrass = ifelse(neighbourhood_madrass_4, "madrass_4", which_madrass),
         which_madrass = ifelse(neighbourhood_madrass_5, "madrass_5", which_madrass),
         which_madrass = ifelse(neighbourhood_madrass_6, "madrass_6", which_madrass),
         which_madrass = ifelse(neighbourhood_madrass_7, "madrass_7", which_madrass)) %>%
  filter(!is.na(which_madrass)) %>%
  select(individual_id, which_madrass) %>%
  set_colnames(c("individual_id", "which_madrass"))

edge_same_madrass <- survey_madrass %>%
  left_join(survey_madrass, by = c("which_madrass"), relationship = "many-to-many") %>%
  select(individual_id.x, individual_id.y) %>%
  set_colnames(c("individual_1", "individual_2")) %>%
  filter(individual_1 < individual_2) %>%
  mutate(ego_1 = individual_1 %in% ego_individuals$individual_id,
         ego_2 = individual_2 %in% ego_individuals$individual_id) %>%
  filter(ego_1 | ego_2) %>%
  mutate(connection_string = paste0(individual_1, "_", individual_2)) %>%
  select(individual_1, individual_2, connection_string, ego_1, ego_2) %>%
  unique()


ee_edges <- nrow(edge_same_madrass %>% filter(ego_1 & ego_2))
ae_edges <- nrow(edge_same_madrass %>% rowwise() %>% filter(sum(ego_1, ego_2) ==1 ))
all_edges <- nrow(edge_same_madrass)

included_individuals <- unique(c(edge_same_madrass$individual_1, edge_same_madrass$individual_2))
ego_ind <- sum(included_individuals %in% ego_individuals$individual_id)
paste("&", ee_edges, "&", ae_edges, "&", all_edges, "&", ego_ind, "\\")
