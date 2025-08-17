proximity_data <- tibble(individual_id = character(), 
                         spatial_25 = list(), 
                         spatial_100 = list(),
                         network_1 = list(),
                         network_2 = list(),
                         network_3 = list(),
                         network_4 = list(),
                         length_n1 = numeric(),
                         length_n2 = numeric(),
                         length_n3 = numeric(),
                         length_n4 = numeric(),
                         social_in_spatial_n1_25 = numeric(),
                         social_in_spatial_n1_100 = numeric(),
                         social_in_spatial_n2_25 = numeric(),
                         social_in_spatial_n2_100 = numeric(),
                         social_in_spatial_n3_25 = numeric(),
                         social_in_spatial_n3_100 = numeric(),
                         social_in_spatial_n4_25 = numeric(),
                         social_in_spatial_n4_100 = numeric())


for (i in 1:nrow(individual_distance)){
  person <- rownames(individual_distance)[i]
  spatial_25_vec <- names(individual_distance[i, individual_distance[i,] <= 25])
  spatial_100_vec <- names(individual_distance[i, individual_distance[i,] <= 100])
  network_1_vec <- vector()
  n1_edges_1 <- n1_edges[n1_edges$individual_1 == person,]
  if(nrow(n1_edges_1 > 0)){
    network_1_vec <- c(network_1_vec, n1_edges_1$individual_2)
  }
  n1_edges_2 <- n1_edges[n1_edges$individual_2 == person,]
  if(nrow(n1_edges_2 > 0)){
    network_1_vec <- c(network_1_vec, n1_edges_2$individual_1)
  }
  network_2_vec <- vector()
  n2_edges_1 <- n2_edges[n2_edges$individual_1 == person,]
  if(nrow(n2_edges_1 > 0)){
    network_2_vec <- c(network_2_vec, n2_edges_1$individual_2)
  }
  n2_edges_2 <- n2_edges[n2_edges$individual_2 == person,]
  if(nrow(n2_edges_2 > 0)){
    network_2_vec <- c(network_2_vec, n2_edges_2$individual_1)
  }
  network_3_vec <- vector()
  n3_edges_1 <- n3_edges[n3_edges$individual_1 == person,]
  if(nrow(n3_edges_1 > 0)){
    network_3_vec <- c(network_3_vec, n3_edges_1$individual_2)
  }
  n3_edges_2 <- n3_edges[n3_edges$individual_2 == person,]
  if(nrow(n3_edges_2 > 0)){
    network_3_vec <- c(network_3_vec, n3_edges_2$individual_1)
  }
  network_4_vec <- vector()
  n4_edges_1 <- n4_edges[n4_edges$individual_1 == person,]
  if(nrow(n4_edges_1 > 0)){
    network_4_vec <- c(network_4_vec, n4_edges_1$individual_2)
  }
  n4_edges_2 <- n4_edges[n4_edges$individual_2 == person,]
  if(nrow(n4_edges_2 > 0)){
    network_4_vec <- c(network_4_vec, n4_edges_2$individual_1)
  }
  proximity_ind <- tibble(individual_id = person, 
                          spatial_25 = list(spatial_25_vec), 
                          spatial_100 = list(spatial_100_vec),
                          network_1 = list(network_1_vec),
                          network_2 = list(network_2_vec),
                          network_3 = list(network_3_vec),
                          network_4 = list(network_4_vec),
                          length_n1 = length(network_1_vec),
                          length_n2 = length(network_2_vec),
                          length_n3 = length(network_3_vec),
                          length_n4 = length(network_4_vec),
                          social_in_spatial_n1_25 = sum(network_1_vec %in% spatial_25_vec)/length(network_1_vec),
                          social_in_spatial_n1_100 = sum(network_1_vec %in% spatial_100_vec)/length(network_1_vec),
                          social_in_spatial_n2_25 = sum(network_2_vec %in% spatial_25_vec)/length(network_2_vec),
                          social_in_spatial_n2_100 = sum(network_2_vec %in% spatial_100_vec)/length(network_2_vec),
                          social_in_spatial_n3_25 = sum(network_3_vec %in% spatial_25_vec)/length(network_3_vec),
                          social_in_spatial_n3_100 = sum(network_3_vec %in% spatial_100_vec)/length(network_3_vec),
                          social_in_spatial_n4_25 = sum(network_4_vec %in% spatial_25_vec)/length(network_4_vec),
                          social_in_spatial_n4_100 = sum(network_4_vec %in% spatial_100_vec)/length(network_4_vec))
  proximity_data <- rbind(proximity_data, proximity_ind)
  
}

ego_individuals <- ego_individuals %>%
  mutate(age_group = case_when(
    age < 8 ~ "0-7",
    age >= 8 & age < 15 ~ "8-14",
    age >= 15 & age < 36 ~ "15-35",
    age >= 36 ~ "36+",
    .default = NA
  ))

ego_individuals$age_group <- factor(ego_individuals$age_group, levels = c("0-7", "8-14", "15-35", "36+"))

proximity_data <- proximity_data %>%
  left_join(ego_individuals, by = c("individual_id"))

proximity_data$quartier <- as.factor(proximity_data$quartier)



levels(proximity_data$quartier) <- c("", "", "", "", "", "", "",
                                     "District A", "District B", "District C", "District D", "District E", "District F", "District G")

proximity_data[proximity_data$quartier == "",]$quartier <- "District A"
proximity_data[proximity_data$quartier == "",]$quartier <- "District B"
proximity_data[proximity_data$quartier == "",]$quartier <- "District C"
proximity_data[proximity_data$quartier == "",]$quartier <- "District D"
proximity_data[proximity_data$quartier == "",]$quartier <- "District E"
proximity_data[proximity_data$quartier == "",]$quartier <- "District F"
proximity_data[proximity_data$quartier == "",]$quartier <- "District G"

ggplot(proximity_data, aes(x = length_n1, y = social_in_spatial_n1_25, color = quartier)) +
  geom_jitter(width = 1, height = 0.02, size = 0.2) +
  labs(x = "Number of contacts",
       y = "Proportion of contacts within 25m") +
  theme_minimal()

ggplot(proximity_data, aes(x = length_n1, y = social_in_spatial_n1_100, color = quartier)) +
  geom_jitter(width = 1, height = 0.02, size = 0.2) +
  labs(x = "Number of contacts",
       y = "Proportion of contacts within 100m") +
  theme_minimal()

ggplot(proximity_data, aes(x = social_in_spatial_n1_25)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ age_group) +
  labs(x = "Proportion of contacts within 25m",
       y = "Number of individuals") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

ggplot(proximity_data, aes(x = social_in_spatial_n1_100)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ age_group) +
  labs(x = "Proportion of contacts within 100m",
       y = "Number of individuals") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

ggplot(proximity_data, aes(x = length_n2, y = social_in_spatial_n2_25, color = quartier)) +
  geom_jitter(width = 1, height = 0.02, size = 0.2) +
  labs(x = "Number of contacts",
       y = "Proportion of contacts within 25m") +
  theme_minimal()

ggplot(proximity_data, aes(x = length_n2, y = social_in_spatial_n2_100, color = quartier)) +
  geom_jitter(width = 1, height = 0.02, size = 0.2) +
  labs(x = "Number of contacts",
       y = "Proportion of contacts within 100m") +
  theme_minimal()

ggplot(proximity_data, aes(x = social_in_spatial_n2_25)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ age_group) +
  labs(x = "Proportion of contacts within 25m",
       y = "Number of individuals") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

ggplot(proximity_data, aes(x = social_in_spatial_n2_100)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ age_group) +
  labs(x = "Proportion of contacts within 100m",
       y = "Number of individuals") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

ggplot(proximity_data, aes(x = length_n3, y = social_in_spatial_n3_25, color = quartier)) +
  geom_jitter(width = 1, height = 0.02, size = 0.2) +
  labs(x = "Number of contacts",
       y = "Proportion of contacts within 25m") +
  theme_minimal()

ggplot(proximity_data, aes(x = length_n3, y = social_in_spatial_n3_100, color = quartier)) +
  geom_jitter(width = 1, height = 0.02, size = 0.2) +
  labs(x = "Number of contacts",
       y = "Proportion of contacts within 100m") +
  theme_minimal()

ggplot(proximity_data, aes(x = social_in_spatial_n3_25)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ age_group) +
  labs(x = "Proportion of contacts within 25m",
       y = "Number of individuals") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

ggplot(proximity_data, aes(x = social_in_spatial_n3_100)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ age_group) +
  labs(x = "Proportion of contacts within 100m",
       y = "Number of individuals") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

ggplot(proximity_data, aes(x = length_n4, y = social_in_spatial_n4_25, color = quartier)) +
  geom_jitter(width = 1, height = 0.02, size = 0.2) +
  #scale_color_manual(values = my_colors) + 
  labs(x = "Number of contacts",
       y = "Proportion of contacts within 25m") +
  ylim(0,1) +
  theme_minimal()

ggplot(proximity_data, aes(x = length_n4, y = social_in_spatial_n4_100, color = quartier)) +
  geom_jitter(width = 1, height = 0.02, size = 0.2) +
  #scale_color_manual(values = my_colors) + 
  labs(x = "Number of contacts",
       y = "Proportion of contacts within 100m") +
  theme_minimal()

ggplot(proximity_data, aes(x = social_in_spatial_n4_25)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ age_group) +
  labs(x = "Proportion of contacts within 25m",
       y = "Number of individuals") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

ggplot(proximity_data, aes(x = social_in_spatial_n4_100)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ age_group) +
  labs(x = "Proportion of contacts within 100m",
       y = "Number of individuals") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))
