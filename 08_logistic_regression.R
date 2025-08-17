lepra <- read.xlsx(paste0(source_path, "")) %>%
  left_join(age_sex, by = c("indiv_ID_c" = "individual_id"))

hist(ego_individuals$age, breaks = 30, xlab = "Age", xlim = c(0,100), main = NULL)
hist(lepra$age, breaks = 30, xlab = "Age", xlim = c(0,100), main = NULL)

proximity_data <- proximity_data %>%
  rowwise() %>%
  mutate(n1_nr_lep_contacts = sum(network_1 %in% lepra$indiv_ID_c),
         n2_nr_lep_contacts = sum(network_2 %in% lepra$indiv_ID_c),
         n3_nr_lep_contacts = sum(network_3 %in% lepra$indiv_ID_c),
         n4_nr_lep_contacts = sum(network_4 %in% lepra$indiv_ID_c),
         m25_nr_lep_contacts = sum(spatial_25 %in% lepra$indiv_ID_c),
         m100_nr_lep_contacts = sum(spatial_100 %in% lepra$indiv_ID_c)) %>%
  mutate(n1_YN_lep_contacts = ifelse(n1_nr_lep_contacts > 0, TRUE, FALSE),
         n2_YN_lep_contacts = ifelse(n2_nr_lep_contacts > 0, TRUE, FALSE),
         n3_YN_lep_contacts = ifelse(n3_nr_lep_contacts > 0, TRUE, FALSE),
         n4_YN_lep_contacts = ifelse(n4_nr_lep_contacts > 0, TRUE, FALSE),
         m25_YN_lep_contacts = ifelse(m25_nr_lep_contacts > 0, TRUE, FALSE),
         m100_YN_lep_contacts = ifelse(m100_nr_lep_contacts > 0, TRUE, FALSE),
         positive = individual_id %in% lepra$indiv_ID_c)

proximity_data$quartier <- as.factor(proximity_data$quartier)
proximity_data$age_group <- as.factor(proximity_data$age_group)


shuffled_proximity_data <- proximity_data[sample(nrow(proximity_data)), ]

shuffled_proximity_data <- shuffled_proximity_data %>%
  mutate(log_n1_nr_contacts = log(length_n1 + 1),
         log_n2_nr_contacts = log(length_n2 + 1),
         log_n3_nr_contacts = log(length_n3 + 1),
         n4_nr_contacts = length_n4,
         n1_leprosy_contact_bin = case_when(
           n1_nr_lep_contacts == 0 ~ "none",
           n1_nr_lep_contacts == 1 ~ "single",
           n1_nr_lep_contacts > 1 ~ "several"),
         n2_leprosy_contact_bin = case_when(
           n2_nr_lep_contacts == 0 ~ "none",
           n2_nr_lep_contacts == 1 ~ "single",
           n2_nr_lep_contacts > 1 ~ "several"),
         n3_leprosy_contact_bin = case_when(
           n3_nr_lep_contacts == 0 ~ "none",
           n3_nr_lep_contacts == 1 ~ "single",
           n3_nr_lep_contacts > 1 ~ "several"),
         n4_leprosy_contact_bin = case_when(
           n4_nr_lep_contacts == 0 ~ "none",
           n4_nr_lep_contacts <= 10 ~ "several",
           n4_nr_lep_contacts > 10 ~ "many"),
         log_house_size = log(house_size))


xtabs(~positive + age_group, data = shuffled_proximity_data)

xtabs(~positive + n1_leprosy_contact_bin, data = shuffled_proximity_data)
xtabs(~positive + n2_leprosy_contact_bin, data = shuffled_proximity_data)
xtabs(~positive + n3_leprosy_contact_bin, data = shuffled_proximity_data)
xtabs(~positive + n4_leprosy_contact_bin, data = shuffled_proximity_data)

xtabs(~positive + sex, data = shuffled_proximity_data)
xtabs(~positive + quartier, data = shuffled_proximity_data)

model_data <- proximity_data %>%
  select(positive, sex, quartier, house_size, age_group, 
         n1_nr_lep_contacts, n2_nr_lep_contacts, 
         n3_nr_lep_contacts, n4_nr_lep_contacts)

model_data <- model_data %>%
  mutate(n1_nr_lep_contacts_tr = log(n1_nr_lep_contacts +1),
         n2_nr_lep_contacts_tr = log(n2_nr_lep_contacts +1),
         n3_nr_lep_contacts_tr = log(n3_nr_lep_contacts +1)) %>%
  select(positive, sex, quartier, house_size, age_group, 
         n1_nr_lep_contacts_tr, n2_nr_lep_contacts_tr, 
         n3_nr_lep_contacts_tr, n4_nr_lep_contacts)

shuffled_proximity_data <- proximity_data[sample(nrow(proximity_data)), ]


logit.01 <- glm(positive ~ age_group +
                  n1_leprosy_contact_bin +
                  n2_leprosy_contact_bin + log_n2_nr_contacts +
                  n3_leprosy_contact_bin +
                  n4_leprosy_contact_bin,
                data = shuffled_proximity_data, family = "binomial")
summary(logit.01)


logit.02 <- glm(positive ~ age_group +
                  n1_YN_lep_contacts +
                  n2_YN_lep_contacts + log_n2_nr_contacts +
                  n3_YN_lep_contacts +
                  n4_YN_lep_contacts,
                data = shuffled_proximity_data, family = "binomial")
summary(logit.02)



plot(residuals(logit.01), ylab = "residuals model 1")
plot(residuals(logit.02), ylab = "residuals model 2")

res <- data.frame(positive = shuffled_proximity_data$positive,
                  model1 = logit.01$fitted.values,
                  model2 = logit.02$fitted.values)
hist(logit.01$fitted.values)
hist(logit.02$fitted.values)

ggplot(res, aes(x=model1, color=positive)) +
  geom_histogram(fill="white", position="dodge")+
  theme(legend.position="top")

ggplot(res, aes(x=model2, color=positive)) +
  geom_histogram(fill="white", position="dodge")+
  theme(legend.position="top")

TF_color <- c("#cccccc", "#9900cc")
res_sorted <- res[order(res$positive), ]

ggplot(res_sorted, aes(x = model1, y = model2, color = positive, size = positive)) +
  geom_jitter(width = 0.005, height = 0.005) +
  scale_color_manual(values = TF_color) + 
  scale_size_manual(values = c(0.2, 0.3)) +
  theme_minimal() +
  ylab("fitted values model 2") +
  xlab("fitted values model 1") +
  labs(color = "leprosy", size = "leprosy")

proximity_data_ext <- proximity_data %>%
  mutate(n1_3 = list(unique(c(unlist(network_1), unlist(network_2), unlist(network_3))))) %>%
  mutate(n1_3_nr_lep_contacts =  sum(n1_3 %in% lepra$indiv_ID_c),
         length_n1_3 = length(unlist(n1_3)),
         length_25m = length(unlist(spatial_25)),
         length_100m = length(unlist(spatial_100))) %>%
  rowwise() %>%
  mutate(prop_n1_3 = n1_3_nr_lep_contacts/length_n1_3,
         prop_25m = m25_nr_lep_contacts/length_25m,
         prop_100m = m100_nr_lep_contacts/length_100m,
         prop_n1 = n1_nr_lep_contacts/length_n1,
         prop_n2 = n2_nr_lep_contacts/length_n2,
         prop_n3 = n3_nr_lep_contacts/length_n3,
         prop_n4 = n4_nr_lep_contacts/length_n4) %>%
  filter(positive)
hist(proximity_data_ext$n1_3_nr_lep_contacts)
plot(proximity_data_ext$prop_n1_3, proximity_data_ext$prop_25m)
plot(proximity_data_ext$prop_n1_3, proximity_data_ext$prop_100m)

sum(unique(unlist(proximity_data_ext$spatial_25)) %in% lepra$indiv_ID_c)
length(unique(unlist(proximity_data_ext$spatial_25)))
sum(unique(unlist(proximity_data_ext$spatial_100)) %in% lepra$indiv_ID_c)
length(unique(unlist(proximity_data_ext$spatial_100)))
sum(unique(unlist(proximity_data_ext$network_1)) %in% lepra$indiv_ID_c)
length(unique(unlist(proximity_data_ext$network_1)))
sum(unique(unlist(proximity_data_ext$network_2)) %in% lepra$indiv_ID_c)
length(unique(unlist(proximity_data_ext$network_2)))
sum(unique(unlist(proximity_data_ext$network_3)) %in% lepra$indiv_ID_c)
length(unique(unlist(proximity_data_ext$network_3)))
sum(unique(unlist(proximity_data_ext$network_4)) %in% lepra$indiv_ID_c)
length(unique(unlist(proximity_data_ext$network_4)))
sum(unique(unlist(proximity_data_ext$n1_3)) %in% lepra$indiv_ID_c)
length(unique(unlist(proximity_data_ext$n1_3)))

prop_df <- rbind(data.frame(prop_value = proximity_data_ext$prop_n1_3, prop_type = "network 1-3"),
                 data.frame(prop_value = proximity_data_ext$prop_25m, prop_type = "25 m"),
                 data.frame(prop_value = proximity_data_ext$prop_100m, prop_type = "100 m"),
                 data.frame(prop_value = proximity_data_ext$prop_n1, prop_type = "network 1"),
                 data.frame(prop_value = proximity_data_ext$prop_n2, prop_type = "network 2"),
                 data.frame(prop_value = proximity_data_ext$prop_n3, prop_type = "network 3"),
                 data.frame(prop_value = proximity_data_ext$prop_n4, prop_type = "network 4"))
prop_df$prop_type <- factor(prop_df$prop_type, levels = c("25 m", "100 m", "network 1",
                                                          "network 2", "network 3", "network 4",
                                                          "network 1-3"))
boxplot(prop_value ~ prop_type, data = prop_df, xlab = NULL,
        ylab = "proportion diagnosed individuals / total individuals")

b1 <- betweenness(network1, gmode = "graph", cmode = "undirected")
b2 <- betweenness(network2, gmode = "graph", cmode = "undirected")
b3 <- betweenness(network3, gmode = "graph", cmode = "undirected")
b4 <- betweenness(network4, gmode = "graph", cmode = "undirected")
ind_id <- network.vertex.names(network1)

between_df <- rbind(data.frame(bet_value = log(b1 + 1), network = "network 1", ind_id = ind_id),
                    data.frame(bet_value = log(b2 + 1), network = "network 2", ind_id = ind_id),
                    data.frame(bet_value = log(b3 + 1), network = "network 3", ind_id = ind_id),
                    data.frame(bet_value = log(b4 + 1), network = "network 4", ind_id = ind_id))
between_df_ext <- between_df %>%
  mutate(lepra = ifelse(ind_id %in% lepra$indiv_ID_c, TRUE, FALSE)) %>%
  select(bet_value, network, lepra)

boxplot(bet_value ~ lepra + network, data = between_df_ext, xlab = NULL,
        ylab = "betweenness (log scale)")
