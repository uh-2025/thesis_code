# network 1
n1_edges <- rbind(edge_same_household, edge_eating_together, edge_shared_bedroom, 
                  edge_caretaker_link, edge_same_compound) %>%
  unique()

n1_ego_edges <- n1_edges %>%
  filter(ego_1 & ego_2) %>%
  mutate(same_household = ifelse(connection_string %in% edge_same_household$connection_string, TRUE, FALSE))

n1_mixed_edges <- n1_edges %>%
  rowwise() %>%
  mutate(ego_sum = sum(ego_1, ego_2)) %>%
  filter(ego_sum == 1)

n1_alter_connection <- n1_mixed_edges %>%
  mutate(ego_ind = ifelse(ego_1, individual_1, individual_2)) %>%
  pull(ego_ind) %>% unique()

n1_vertex_attributes <- ego_individuals %>%
  mutate(alter_connection = ifelse(individual_id %in% n1_alter_connection, TRUE, FALSE))

n1_vertex_attributes_ordered <- n1_vertex_attributes[match(rownames(individual_distance), n1_vertex_attributes$individual_id), ]

hh_matrix <- individual_distance
hh_matrix[,] <- as.logical(FALSE)

df_ego_edges_hh <- n1_ego_edges %>% filter(same_household)

for (i in seq_len(nrow(df_ego_edges_hh))) {
  a <- as.character(df_ego_edges_hh$individual_1[i])
  b <- as.character(df_ego_edges_hh$individual_2[i])
  hh_matrix[a, b] <- TRUE
  hh_matrix[b, a] <- TRUE
}

nrow(n1_ego_edges)
nrow(n1_mixed_edges)
nrow(n1_edges)

included_individuals <- unique(c(n1_edges$individual_1, n1_edges$individual_2))
length(included_individuals)
sum(included_individuals %in% ego_individuals$individual_id)
sum(included_individuals %in% alter_individuals$individual_id)


network1 <- network.initialize(2548, directed = FALSE)
network.vertex.names(network1) <- as.character(n1_vertex_attributes_ordered$individual_id)
network::set.vertex.attribute(network1, "age", n1_vertex_attributes_ordered$age)
network::set.vertex.attribute(network1, "sex", as.character(n1_vertex_attributes_ordered$sex))
network::set.vertex.attribute(network1, "alter_connection", n1_vertex_attributes_ordered$alter_connection)
network::set.vertex.attribute(network1, "survey_group", as.character(n1_vertex_attributes_ordered$survey_group))
network::set.vertex.attribute(network1, "quartier", as.character(n1_vertex_attributes_ordered$quartier))
network::set.vertex.attribute(network1, "house_size", n1_vertex_attributes_ordered$house_size)

vnames <- network.vertex.names(network1)
tail_verts <- match(n1_ego_edges$individual_1, vnames)
head_verts <- match(n1_ego_edges$individual_2, vnames)
n1_ego_edges$tail_verts <- tail_verts
n1_ego_edges$head_verts <- head_verts
network::add.edges(network1, tail_verts, head_verts)

# network 2

n2_edges <- rbind(edge_marriage_partners, 
                  edge_childparent_link,
                  edge_coparents, edge_siblings) %>%
  unique()

n2_ego_edges <- n2_edges %>%
  filter(ego_1 & ego_2)

n2_mixed_edges <- n2_edges %>%
  rowwise() %>%
  mutate(ego_sum = sum(ego_1, ego_2)) %>%
  filter(ego_sum == 1)
n2_alter_connection <- n2_mixed_edges %>%
  mutate(ego_ind = ifelse(ego_1, individual_1, individual_2)) %>%
  pull(ego_ind) %>% unique()

n2_vertex_attributes <- ego_individuals %>%
  mutate(alter_connection = ifelse(individual_id %in% n2_alter_connection, TRUE, FALSE))

n2_vertex_attributes_ordered <- n2_vertex_attributes[match(rownames(individual_distance), n2_vertex_attributes$individual_id), ]

ee_edges <- nrow(n2_ego_edges)
ae_edges <- nrow(n2_mixed_edges)
all_edges <- nrow(n2_edges)

included_individuals <- unique(c(n2_edges$individual_1, n2_edges$individual_2))
ego_ind <- sum(included_individuals %in% ego_individuals$individual_id)
paste("&", ee_edges, "&", ae_edges, "&", all_edges, "&", ego_ind, "\\")


network2 <- network.initialize(2548, directed = FALSE)
network.vertex.names(network2) <- as.character(n2_vertex_attributes_ordered$individual_id)
network::set.vertex.attribute(network2, "age", n2_vertex_attributes_ordered$age)
network::set.vertex.attribute(network2, "sex", as.character(n2_vertex_attributes_ordered$sex))
network::set.vertex.attribute(network2, "alter_connection", n2_vertex_attributes_ordered$alter_connection)
network::set.vertex.attribute(network2, "survey_group", as.character(n2_vertex_attributes_ordered$survey_group))
network::set.vertex.attribute(network2, "quartier", as.character(n2_vertex_attributes_ordered$quartier))
network::set.vertex.attribute(network2, "house_size", n2_vertex_attributes_ordered$house_size)
vnames <- network.vertex.names(network2)
tail_verts <- match(n2_ego_edges$individual_1, vnames)
head_verts <- match(n2_ego_edges$individual_2, vnames)

n2_ego_edges$tail_verts <- tail_verts
n2_ego_edges$head_verts <- head_verts
network::add.edges(network2, tail_verts, head_verts)


# network 3

n3_edges <- rbind(edge_mentioned_close, edge_colleagues,
                  edge_playmates, edge_study_mates) %>%
  unique()

n3_ego_edges <- n3_edges %>%
  filter(ego_1 & ego_2)

n3_mixed_edges <- n3_edges %>%
  rowwise() %>%
  mutate(ego_sum = sum(ego_1, ego_2)) %>%
  filter(ego_sum == 1)

n3_alter_connection <- n3_mixed_edges %>%
  mutate(ego_ind = ifelse(ego_1, individual_1, individual_2)) %>%
  pull(ego_ind) %>% unique()

n3_vertex_attributes <- ego_individuals %>%
  mutate(alter_connection = ifelse(individual_id %in% n3_alter_connection, TRUE, FALSE))

n3_vertex_attributes_ordered <- n3_vertex_attributes[match(rownames(individual_distance), n3_vertex_attributes$individual_id), ]

ee_edges <- nrow(n3_ego_edges)
ae_edges <- nrow(n3_mixed_edges)
all_edges <- nrow(n3_edges)

included_individuals <- unique(c(n3_edges$individual_1, n3_edges$individual_2))
ego_ind <- sum(included_individuals %in% ego_individuals$individual_id)
paste("&", ee_edges, "&", ae_edges, "&", all_edges, "&", ego_ind, "\\")


network3 <- network.initialize(2548, directed = FALSE)
network.vertex.names(network3) <- as.character(n3_vertex_attributes_ordered$individual_id)
network::set.vertex.attribute(network3, "age", n3_vertex_attributes_ordered$age)
network::set.vertex.attribute(network3, "sex", as.character(n3_vertex_attributes_ordered$sex))
network::set.vertex.attribute(network3, "alter_connection", n3_vertex_attributes_ordered$alter_connection)
network::set.vertex.attribute(network3, "survey_group", as.character(n3_vertex_attributes_ordered$survey_group))
network::set.vertex.attribute(network3, "quartier", as.character(n3_vertex_attributes_ordered$quartier))
network::set.vertex.attribute(network3, "house_size", n3_vertex_attributes_ordered$house_size)
vnames <- network.vertex.names(network3)

tail_verts <- match(n3_ego_edges$individual_1, vnames)
head_verts <- match(n3_ego_edges$individual_2, vnames)
n3_ego_edges$tail_verts <- tail_verts
n3_ego_edges$head_verts <- head_verts

network::add.edges(network3, tail_verts, head_verts)


# network 4

n4_edges <- rbind(edge_samen_coran, edge_same_french,
                  edge_same_mosque, edge_same_madrass) %>%
  unique()

n4_ego_edges <- n4_edges %>%
  filter(ego_1 & ego_2)
n4_mixed_edges <- n4_edges %>%
  rowwise() %>%
  mutate(ego_sum = sum(ego_1, ego_2)) %>%
  filter(ego_sum == 1)

n4_alter_connection <- n4_mixed_edges %>%
  mutate(ego_ind = ifelse(ego_1, individual_1, individual_2)) %>%
  pull(ego_ind) %>% unique()

n4_vertex_attributes <- ego_individuals %>%
  mutate(alter_connection = ifelse(individual_id %in% n4_alter_connection, TRUE, FALSE))

n4_vertex_attributes_ordered <- n4_vertex_attributes[match(rownames(individual_distance), n4_vertex_attributes$individual_id), ]

ee_edges <- nrow(n4_ego_edges)
ae_edges <- nrow(n4_mixed_edges)
all_edges <- nrow(n4_edges)

included_individuals <- unique(c(n4_edges$individual_1, n4_edges$individual_2))
ego_ind <- sum(included_individuals %in% ego_individuals$individual_id)
paste("&", ee_edges, "&", ae_edges, "&", all_edges, "&", ego_ind, "\\")


network4 <- network.initialize(2548, directed = FALSE)
network.vertex.names(network4) <- as.character(n4_vertex_attributes_ordered$individual_id)
network::set.vertex.attribute(network4, "age", n4_vertex_attributes_ordered$age)
network::set.vertex.attribute(network4, "sex", as.character(n4_vertex_attributes_ordered$sex))
network::set.vertex.attribute(network4, "alter_connection", n4_vertex_attributes_ordered$alter_connection)
network::set.vertex.attribute(network4, "survey_group", as.character(n4_vertex_attributes_ordered$survey_group))
network::set.vertex.attribute(network4, "quartier", as.character(n4_vertex_attributes_ordered$quartier))
network::set.vertex.attribute(network4, "house_size", n4_vertex_attributes_ordered$house_size)

vnames <- network.vertex.names(network4)
tail_verts <- match(n4_ego_edges$individual_1, vnames)
head_verts <- match(n4_ego_edges$individual_2, vnames)
n4_ego_edges$tail_verts <- tail_verts
n4_ego_edges$head_verts <- head_verts
network::add.edges(network4, tail_verts, head_verts)






