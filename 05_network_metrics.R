library(GGally)

ggnet2(network1, 
       mode = "fruchtermanreingold", 
       node.size = 1,
       node.color = "black",
       edge.size = 0.3,
       edge.color = "chartreuse4")

ggnet2(network2, 
       mode = "fruchtermanreingold", 
       node.size = 1,
       node.color = "black",
       edge.size = 0.3,
       edge.color = "chartreuse4")

ggnet2(network3, 
       mode = "fruchtermanreingold", 
       node.size = 1,
       node.color = "black",
       edge.size = 0.3,
       edge.color = "chartreuse4")

ggnet2(network4, 
       mode = "fruchtermanreingold", 
       node.size = 1,
       node.color = "black",
       edge.size = 0.3,
       edge.color = "chartreuse4")




n1_comp_info <- component.dist(network1)
n2_comp_info <- component.dist(network2)
n3_comp_info <- component.dist(network3)
n4_comp_info <- component.dist(network4)

paste("Number of components &",
      length(n1_comp_info$csize),
      "&",
      length(n2_comp_info$csize),
      "&",
      length(n3_comp_info$csize),
      "&",
      length(n4_comp_info$csize),
      "\\")

paste("Nodes in largest component &",
      max(n1_comp_info$csize),
      "&",
      max(n2_comp_info$csize),
      "&",
      max(n3_comp_info$csize),
      "&",
      max(n4_comp_info$csize),
      "\\")

paste("Unconnected nodes &",
      sum(n1_comp_info$csize == 1),
      "&",
      sum(n2_comp_info$csize == 1),
      "&",
      sum(n3_comp_info$csize == 1),
      "&",
      sum(n4_comp_info$csize == 1),
      "\\")

paste("Mean degrees &",
      round(unname(summary(sna::degree(network1))["Mean"]),1),
      "&",
      round(unname(summary(sna::degree(network2))["Mean"]),1),
      "&",
      round(unname(summary(sna::degree(network3))["Mean"]),1),
      "&",
      round(unname(summary(sna::degree(network4))["Mean"]),1),
      "\\")

paste("1st quantile degrees &",
      round(unname(summary(sna::degree(network1))["1st Qu."]),1),
      "&",
      round(unname(summary(sna::degree(network2))["1st Qu."]),1),
      "&",
      round(unname(summary(sna::degree(network3))["1st Qu."]),1),
      "&",
      round(unname(summary(sna::degree(network4))["1st Qu."]),1),
      "\\")

paste("Median degrees &",
      round(unname(summary(sna::degree(network1))["Median"]),1),
      "&",
      round(unname(summary(sna::degree(network2))["Median"]),1),
      "&",
      round(unname(summary(sna::degree(network3))["Median"]),1),
      "&",
      round(unname(summary(sna::degree(network4))["Median"]),1),
      "\\")

paste("3rd quantile degrees &",
      round(unname(summary(sna::degree(network1))["3rd Qu."]),1),
      "&",
      round(unname(summary(sna::degree(network2))["3rd Qu."]),1),
      "&",
      round(unname(summary(sna::degree(network3))["3rd Qu."]),1),
      "&",
      round(unname(summary(sna::degree(network4))["3rd Qu."]),1),
      "\\")

geo1 <- sna::geodist(network1)$gdist
geo2 <- sna::geodist(network2)$gdist
geo3 <- sna::geodist(network3)$gdist
geo4 <- sna::geodist(network4)$gdist

paste("Mean geodesic &",
      round(unname(summary(as.vector(geo1[is.finite(geo1)]))["Mean"]),1),
      "&",
      round(unname(summary(as.vector(geo2[is.finite(geo2)]))["Mean"]),1),
      "&",
      round(unname(summary(as.vector(geo3[is.finite(geo3)]))["Mean"]),1),
      "&",
      round(unname(summary(as.vector(geo4[is.finite(geo4)]))["Mean"]),1),
      "\\")

paste("1st quantile geodesic &",
      round(unname(summary(as.vector(geo1[is.finite(geo1)]))["1st Qu."]),1),
      "&",
      round(unname(summary(as.vector(geo2[is.finite(geo2)]))["1st Qu."]),1),
      "&",
      round(unname(summary(as.vector(geo3[is.finite(geo3)]))["1st Qu."]),1),
      "&",
      round(unname(summary(as.vector(geo4[is.finite(geo4)]))["1st Qu."]),1),
      "\\")

paste("Median geodesic &",
      round(unname(summary(as.vector(geo1[is.finite(geo1)]))["Median"]),1),
      "&",
      round(unname(summary(as.vector(geo2[is.finite(geo2)]))["Median"]),1),
      "&",
      round(unname(summary(as.vector(geo3[is.finite(geo3)]))["Median"]),1),
      "&",
      round(unname(summary(as.vector(geo4[is.finite(geo4)]))["Median"]),1),
      "\\")

paste("3rd quantile geodesic &",
      round(unname(summary(as.vector(geo1[is.finite(geo1)]))["3rd Qu."]),1),
      "&",
      round(unname(summary(as.vector(geo2[is.finite(geo2)]))["3rd Qu."]),1),
      "&",
      round(unname(summary(as.vector(geo3[is.finite(geo3)]))["3rd Qu."]),1),
      "&",
      round(unname(summary(as.vector(geo4[is.finite(geo4)]))["3rd Qu."]),1),
      "\\")

paste("Diameter &",
      round(unname(summary(as.vector(geo1[is.finite(geo1)]))["Max."]),1),
      "&",
      round(unname(summary(as.vector(geo2[is.finite(geo2)]))["Max."]),1),
      "&",
      round(unname(summary(as.vector(geo3[is.finite(geo3)]))["Max."]),1),
      "&",
      round(unname(summary(as.vector(geo4[is.finite(geo4)]))["Max."]),1),
      "\\")

b1 <- summary(betweenness(network1, gmode = "graph", cmode = "undirected"))
b2 <- summary(betweenness(network2, gmode = "graph", cmode = "undirected"))
b3 <- summary(betweenness(network3, gmode = "graph", cmode = "undirected"))
b4 <- summary(betweenness(network4, gmode = "graph", cmode = "undirected"))

paste("Mean betweenness &",
      round(unname(b1["Mean"]),1),
      "&",
      round(unname(b2["Mean"]),1),
      "&",
      round(unname(b3["Mean"]),1),
      "&",
      round(unname(b4["Mean"]),1),
      "\\")

paste("1st quantile betweenness &",
      round(unname(b1["1st Qu."]),1),
      "&",
      round(unname(b2["1st Qu."]),1),
      "&",
      round(unname(b3["1st Qu."]),1),
      "&",
      round(unname(b4["1st Qu."]),1),
      "\\")

paste("Median betweenness &",
      round(unname(b1["Median"]),1),
      "&",
      round(unname(b2["Median"]),1),
      "&",
      round(unname(b3["Median"]),1),
      "&",
      round(unname(b4["Median"]),1),
      "\\")

paste("3rd quantile betweenness &",
      round(unname(b1["3rd Qu."]),1),
      "&",
      round(unname(b2["3rd Qu."]),1),
      "&",
      round(unname(b3["3rd Qu."]),1),
      "&",
      round(unname(b4["3rd Qu."]),1),
      "\\")

paste("Transitivity &",
      round(gtrans(network1, mode = graph, use.adjacency = FALSE),1),
      "&",
      round(gtrans(network2, mode = graph, use.adjacency = FALSE),1),
      "&",
      round(gtrans(network3, mode = graph, use.adjacency = FALSE),1),
      "&",
      round(gtrans(network4, mode = graph, use.adjacency = FALSE),1),
      "\\")

library(igraph)
library(intergraph)
inetwork1 <- asIgraph(network1)
inetwork2 <- asIgraph(network2)
inetwork3 <- asIgraph(network3)
inetwork4 <- asIgraph(network4)

i1 <- summary(transitivity(inetwork1, type = "local"))
i2 <- summary(transitivity(inetwork2, type = "local"))
i3 <- summary(transitivity(inetwork3, type = "local"))
i4 <- summary(transitivity(inetwork4, type = "local"))

paste("Mean clustering coefficient &",
      round(unname(i1["Mean"]),1),
      "&",
      round(unname(i2["Mean"]),1),
      "&",
      round(unname(i3["Mean"]),1),
      "&",
      round(unname(i4["Mean"]),1),
      "\\")

paste("1st quantile clustering coefficient &",
      round(unname(i1["1st Qu."]),1),
      "&",
      round(unname(i2["1st Qu."]),1),
      "&",
      round(unname(i3["1st Qu."]),1),
      "&",
      round(unname(i4["1st Qu."]),1),
      "\\")

paste("Median betweenness &",
      round(unname(i1["Median"]),1),
      "&",
      round(unname(i2["Median"]),1),
      "&",
      round(unname(i3["Median"]),1),
      "&",
      round(unname(i4["Median"]),1),
      "\\")

paste("3rd quantile betweenness &",
      round(unname(i1["3rd Qu."]),1),
      "&",
      round(unname(i2["3rd Qu."]),1),
      "&",
      round(unname(i3["3rd Qu."]),1),
      "&",
      round(unname(i4["3rd Qu."]),1),
      "\\")


detach("package:igraph", unload = TRUE)