pars <- list()

pars[["latent_shape"]] <- 5
pars[["latent_sclae"]] <- 1
pars[["presymp_shape"]] <- 2.5
pars[["presymp_scale"]] <- 1
pars[["asymp_shape"]] <- 6
pars[["asymp_scale"]] <- 1
pars[["msymp_rate"]] <- 1/7
pars[["mtosev_shape"]] <- 7
pars[["mtosev_scale"]] <- 2
pars[["sevsymp_shape"]] <- 3
pars[["sevsymp_scale"]] <- 0.25
pars[["p_symp_age"]] <- c(0.03, 0.05, 0.3, 0.55, 0.61, 0.75, 0.89, 0.9, 0.9)
pars[["p_sevsymp_age"]] <- c(0.004, 0.004, 0.01, 0.04, 0.09, 0.13, 0.19, 0.2, 0.25)
pars[["p_mort_age"]] <- c(0.01, 0.0205, 0.031, 0.0475, 0.0785, 0.12, 0.186, 0.3, 0.45)
