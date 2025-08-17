model.01 <- ergm(network3 ~ edges +
                   nodecov("age") + absdiff("age") + 
                   nodefactor("sex") + nodematch("sex") +
                   nodecov("alter_connection") +
                   nodefactor("survey_group") +
                   nodefactor("quartier") + nodematch("quartier") +
                   nodecov("house_size") + absdiff("house_size") +
                   edgecov(individual_distance) +
                   gwdegree(0.8, fixed = TRUE), 
                 control = control.ergm(parallel = 4))


model.02 <- ergm(network3 ~ edges +
                   nodecov("age") + absdiff("age") + 
                   nodematch("sex") +
                   nodecov("alter_connection") +
                   nodefactor("survey_group") +
                   nodefactor("quartier") + nodematch("quartier") +
                   nodecov("house_size") + absdiff("house_size") +
                   edgecov(individual_distance) +
                   gwdegree(0.8, fixed = TRUE), 
                 control = control.ergm(parallel = 4))




