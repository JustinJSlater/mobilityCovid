rm(list=ls())
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(rstan)
library(janitor)
library(xtable)
library(ggplot2)
library(sf)
library(mapmisc)
library(reshape2)

# Data import and cleaning =====================================================
## Case data ===================================================================
castilla_cases_covariates <- read.csv("data/castilla_cases_covariates_mar2020toFeb2021.csv") %>% 
  dplyr::select(hzone_code, total_pop, starts_with("infected") ) %>%
  pivot_longer(
    cols = starts_with("infected"),
    names_to = "time",
    values_to = "cases",
    values_drop_na = TRUE
  ) %>% mutate(time = as.numeric(str_sub(time, start=14)) ) %>% 
  mutate(hzone_code=ifelse(hzone_code %in% c(170813,170814), 170811, hzone_code )) %>% # codes 170813, 170814 are collapsed
  mutate(week = ceiling(time/7)) %>% 
  group_by(hzone_code, week) %>% 
  summarise(weekly_cases=sum(cases), total_pop=sum(unique(total_pop))) %>% 
  arrange(hzone_code)

y_castilla <- castilla_cases_covariates %>% ungroup %>% dplyr::select(hzone_code, weekly_cases,week) %>% data.frame() %>% 
  reshape(idvar = "hzone_code", timevar = "week",  direction = "wide") %>% arrange(hzone_code) %>% 
  dplyr::select(-hzone_code)

# create shifted series, mainly used for data exploration
shifted_y_castilla1 <- cbind(week1 = y_castilla[,1]/4, y_castilla)[,1:ncol(y_castilla)]
shifted_y_castilla2 <- cbind(week1 = shifted_y_castilla1[,1]/4, shifted_y_castilla1)[,1:ncol(shifted_y_castilla1)]

# Crude effective R for data exploration
crude_r_castilla = colSums(y_castilla)/colSums(shifted_y_castilla1)

## Population data  ============================================================
popByZone_castilla = castilla_cases_covariates %>% 
  group_by(hzone_code) %>% 
  summarise(pop = mean(total_pop)) %>% 
  arrange(hzone_code)

## Testing data ================================================================
test_data <- read.csv("data/Datos_Pruebas_Realizadas_Historico_11022022.csv", sep = ";") %>% 
  data.frame() %>% 
  clean_names() %>% 
  mutate(n_pcr = gsub(",","", n_pcr)) %>% 
  mutate(community = case_when(
    provincia %in% c("Ávila", "Burgos","León","Palencia","Salamanca","Segovia", "Soria", "Valladolid","Zamora")~ "castilla",
    provincia %in% c("Madrid") ~ "madrid",
    TRUE ~ "Other")) %>%
  filter(community %in% c("madrid","castilla")) %>% 
  mutate(new_date = as.Date(fecha_prueba, format = '%d%b%Y'),
         total_tests = as.numeric(n_ant)+ as.numeric(n_pcr) ,
         total_positive = n_ant_positivos+n_pcr_positivos ) %>% 
  mutate(time = as.numeric(new_date )-18321,
         week = ceiling(time/7))

test_data_castilla <- test_data %>% 
  filter(community == "castilla") %>% 
  group_by(week) %>% 
  summarise(total_tests = sum(total_tests),
            total_positive=sum(total_positive) ) %>% 
  filter(week < 54)

castilla_test_ts <- c(test_data_castilla$total_tests[4], #impute weeks 1 and 2 visually
                      test_data_castilla$total_tests[3],
                      test_data_castilla$total_tests)
ratio_tests_castilla <- castilla_test_ts/lag(castilla_test_ts) # relative change in testing
ratio_tests_castilla[1:3] =c(1.5,1.5,1.5)

## deaths data ===============
# used for corroborating under reporting

castilla_deaths_covariates <- read.csv("data/castilla_cases_covariates_mar2020toFeb2021.csv") %>% 
  dplyr::select(hzone_code, total_pop, starts_with("death") ) %>%
  pivot_longer(
    cols = starts_with("death"),
    names_to = "time",
    values_to = "cases",
    values_drop_na = TRUE
  ) %>% mutate(time = as.numeric(str_sub(time, start=11)) ) %>% 
  mutate(hzone_code=ifelse(hzone_code %in% c(170813,170814), 170811, hzone_code )) %>% # codes 170813, 170814 are collapsed
  mutate(week = ceiling(time/7)) %>% 
  group_by(hzone_code, week) %>% 
  summarise(weekly_cases=sum(cases), total_pop=sum(unique(total_pop))) %>% 
  arrange(hzone_code)

deaths_castilla <- castilla_deaths_covariates %>% ungroup %>% dplyr::select(hzone_code, weekly_cases,week) %>% data.frame() %>% 
  reshape(idvar = "hzone_code", timevar = "week",  direction = "wide") %>% arrange(hzone_code) %>% 
  dplyr::select(-hzone_code)

deaths_castilla_all = colSums(deaths_castilla)

## Population data  ============================================================
popByZone_castilla = castilla_cases_covariates %>% 
  group_by(hzone_code) %>% 
  summarise(pop = mean(total_pop)) %>% 
  arrange(hzone_code)

## Testing data ================================================================
test_data <- read.csv("data/Datos_Pruebas_Realizadas_Historico_11022022.csv", sep = ";") %>% 
  data.frame() %>% 
  clean_names() %>% 
  mutate(n_pcr = gsub(",","", n_pcr)) %>% 
  mutate(community = case_when(
    provincia %in% c("Ávila", "Burgos","León","Palencia","Salamanca","Segovia", "Soria", "Valladolid","Zamora")~ "castilla",
    provincia %in% c("Madrid") ~ "madrid",
    TRUE ~ "Other")) %>%
  filter(community %in% c("madrid","castilla")) %>% 
  mutate(new_date = as.Date(fecha_prueba, format = '%d%b%Y'),
         total_tests = as.numeric(n_ant)+ as.numeric(n_pcr) ,
         total_positive = n_ant_positivos+n_pcr_positivos ) %>% 
  mutate(time = as.numeric(new_date )-18321,
         week = ceiling(time/7))

test_data_castilla <- test_data %>% 
  filter(community == "castilla") %>% 
  group_by(week) %>% 
  summarise(total_tests = sum(total_tests),
            total_positive=sum(total_positive) ) %>% 
  filter(week < 54)

castilla_test_ts <- c(test_data_castilla$total_tests[4], #impute weeks 1 and 2 visually
                      test_data_castilla$total_tests[3],
                      test_data_castilla$total_tests)
ratio_tests_castilla <- castilla_test_ts/lag(castilla_test_ts) # relative change in testing
ratio_tests_castilla[1:3] =c(1.5,1.5,1.5)


## mobility data ===============================================================
files <- list.files(path = "data/Castilla Y Leon",pattern=".csv")

files_weeks <- cbind(files, seq(1, length(files))) %>% 
  data.frame() %>% 
  mutate(week = ceiling(as.numeric(V2)/7)) %>% 
  filter(week<54)

# Sum mobility matrices by week
weekly_mobility= array(dim=c(245, 245, max(castilla_cases_covariates$week))) #note that in this object, week 1 will empty

for (i in 1:max(castilla_cases_covariates$week)){
  tmp <- array(matrix(nrow =245, ncol = 245), dim = c(245, 245, 7) )
  matrices_idx <- files_weeks$V2[which(files_weeks$week==i)] %>% as.numeric()
  for (j in 1:7){
    tmp[,,j] <- read.csv(paste0("data/Castilla Y Leon/", files_weeks$files[matrices_idx[j]]))[,-1] %>% data.matrix
  }
  weekly_mobility[,,i] = rowSums(tmp, dims =2)
}

# if there is less than 1 trip on that day, set mobility = 0.
weekly_mobility[weekly_mobility<1]=0

# create mobility lags for implementation in stan
shifted_weekly_mobility1 = array(dim = dim(weekly_mobility))
shifted_weekly_mobility1[,,1:2] = weekly_mobility[,,1]
shifted_weekly_mobility1[,,3:53]= weekly_mobility[,,2:52]

shifted_weekly_mobility2 = array(dim = dim(shifted_weekly_mobility1))
shifted_weekly_mobility2[,,1:2] = shifted_weekly_mobility1[,,1]
shifted_weekly_mobility2[,,3:53]= shifted_weekly_mobility1[,,2:52]

shifted_weekly_mobility3 = array(dim = dim(shifted_weekly_mobility2))
shifted_weekly_mobility3[,,1:2] = shifted_weekly_mobility2[,,1]
shifted_weekly_mobility3[,,3:53]= shifted_weekly_mobility2[,,2:52]

total_mobility_castilla = weekly_mobility %>% colSums(dim=2)

y_all_castilla <- y_castilla %>% colSums()


## spatial data ================================================================
library(sf)
hzone <- st_read("data/castilla boundary") 
hzone <- hzone %>% arrange(hzcode)  
neighbor_mat <- spdep::poly2nb(hzone, row.names = hzone$hzcode)
output=NULL

for (i in 1:245){
  tmp <- matrix(ncol = 2, nrow = length(neighbor_mat[[i]]))
  tmp[,1] <- i
  tmp[,2] <- neighbor_mat[[i]]
  output <- rbind(output,tmp)
}

edges <- output %>% 
  data.frame() %>%
  rowwise() %>% 
  mutate(node1 = min(X1,X2), node2 = max(X1,X2)) %>% 
  dplyr::select(node1,node2) %>% ungroup() %>% 
  group_by(node1, node2) %>%
  filter(row_number() == 1) %>% ungroup() %>% 
  group_by(node1) %>% 
  mutate(num_neighbours = n())

el<- edges %>% dplyr::select(-num_neighbours) %>% as.matrix()

el[113,] = c(32, 44) # Condado de Trevino is completely surrounded by some other AC, force an edge to its closest neighbours.

adjacency <- matrix(0, 245,245)

for (i in 1: nrow(el)){
  adjacency[el[i,1], el[i,2]] <- 1
  adjacency[el[i,2], el[i,1]] <- 1
}

diag(adjacency) = 0 
spatial_weights = adjacency /rowSums(adjacency)


### Figure 2 mobility matrix plot =====

library(raster)
plot_mob_matrix = (weekly_mobility/7) |> 
  rowMeans(dim=2) 

row_sums <- rowSums(plot_mob_matrix)

# Sort matrix rows based on row sums
plot_mob_matrix<- plot_mob_matrix[order(row_sums, decreasing = TRUE), order(row_sums, decreasing = TRUE) ] |> 
  raster()


pdf(file = "Manuscript/AOAS/plots/castilla_mean_daily_mob.pdf", height = 7)
tm_shape(plot_mob_matrix)+
  tm_raster(title = "Trips", 
            style = "cont", breaks = c(0,1,10,100,1000,10000,100000,1e6),
            labels = c("0","1","10","100","1e3","1e4","1e5","1e6"),
            palette = "Greys")+
  tm_layout(legend.position = c(0.85,0.65),
            legend.width = 0.5,
            legend.height = 0.3,
            legend.text.size =3,
            legend.title.size = 4)
dev.off()


# Modelling ====================================================================

## Multivariate model: 2 mobility lags and no serial interval===================

dataForStan <- list(I = nrow(y_castilla[,4:53]),
                    T = ncol(y_castilla[,4:53]),
                    y = y_castilla[,4:53],
                    shifted_y = shifted_y_castilla1[,4:53],
                    shifted_weekly_mobility1 = shifted_weekly_mobility1[,,4:53],
                    shifted_weekly_mobility2 = shifted_weekly_mobility2[,,4:53],
                    spatial_weights = spatial_weights,
                    pops = popByZone_castilla$pop)

thin = 1
iter = thin * 1000
warmup= round(iter*0.5)
chains = 4

stan_code <- rstan::stanc(file = "stan models/multivariate_castilla_noSI.stan") # convert to C++ code
stan_model <- rstan::stan_model(stanc_ret = stan_code)     # compile generated code
castilla_samples_multivariate_noSI <- rstan::sampling(stan_model,
                                                      data = dataForStan,
                                                      iter = iter,
                                                      chains = 4,
                                                      cores =chains,
                                                      thin = thin,
                                                      warmup=warmup,
                                                      refresh = 50)
save(castilla_samples_multivariate_noSI,file = "results/final/multivariate_castilla_noSI.RData")


## Multivariate no SI testing==========================================================

dataForStan <- list(I = nrow(y_castilla[,4:53]),
                    T = ncol(y_castilla[,4:53]),
                    y = y_castilla[,4:53],
                    shifted_y = shifted_y_castilla1[,4:53],
                    shifted_weekly_mobility1 = shifted_weekly_mobility1[,,4:53],
                    shifted_weekly_mobility2 = shifted_weekly_mobility2[,,4:53],
                    spatial_weights = spatial_weights,
                    num_tests = castilla_test_ts[4:53]/max(castilla_test_ts[4:53]),
                    pops = popByZone_castilla$pop)

stan_code <- rstan::stanc(file = "stan models/multivariate_castilla_testing2.stan") # convert to C++ code
stan_model <- rstan::stan_model(stanc_ret = stan_code)     # compile generated code
castilla_samples_multivariate_noSI_testing <- rstan::sampling(stan_model,
                                                              data = dataForStan,
                                                              iter = iter,
                                                              chains = 4,
                                                              cores =chains,
                                                              thin = thin,
                                                              warmup=warmup,
                                                              refresh = 50)

save(castilla_samples_multivariate_noSI_testing,file = "results/final/multivariate_castilla_testing2.RData")

## multivariate three weeks ================================================================

dataForStan <- list(I = nrow(y_castilla[,4:53]),
                    T = ncol(y_castilla[,4:53]),
                    y = y_castilla[,4:53],
                    shifted_y1 = shifted_y_castilla1[,4:53],
                    shifted_weekly_mobility1 = shifted_weekly_mobility1[,,4:53],
                    shifted_weekly_mobility2 = shifted_weekly_mobility2[,,4:53],
                    shifted_weekly_mobility3 = shifted_weekly_mobility3[,,4:53],
                    spatial_weights = spatial_weights,
                    pops = popByZone_castilla$pop)

stan_code <- rstan::stanc(file = "stan models/multivariate_castilla_threelags.stan") # convert to C++ code
stan_model <- rstan::stan_model(stanc_ret = stan_code)     # compile generated code
castilla_samples_multivariate_threelags <- rstan::sampling(stan_model,
                                                           data = dataForStan,
                                                           iter = iter,
                                                           chains = 4,
                                                           cores =chains,
                                                           thin = thin,
                                                           warmup=warmup,
                                                           refresh = 50)

save(castilla_samples_multivariate_threelags, file = "results/final/multivariate_castilla_threelags.RData")





## multivariate SI ================================================================

dataForStan <- list(I = nrow(y_castilla[,4:53]),
                    T = ncol(y_castilla[,4:53]),
                    y = y_castilla[,4:53],
                    shifted_y1 = shifted_y_castilla1[,4:53],
                    shifted_y2 = shifted_y_castilla2[,4:53],
                    shifted_weekly_mobility1 = shifted_weekly_mobility1[,,4:53],
                    shifted_weekly_mobility2 = shifted_weekly_mobility2[,,4:53],
                    shifted_weekly_mobility3 = shifted_weekly_mobility3[,,4:53],
                    spatial_weights = spatial_weights,
                    ratio_tests = ratio_tests_castilla[4:53] - mean(ratio_tests_castilla[4:53]),
                    pops = popByZone_castilla$pop)

stan_code <- rstan::stanc(file = "stan models/multivariate_castilla_yesSI.stan") # convert to C++ code
stan_model <- rstan::stan_model(stanc_ret = stan_code)     # compile generated code
castilla_samples_multivariate_yesSI <- rstan::sampling(stan_model,
                                                       data = dataForStan,
                                                       iter = iter,
                                                       chains = 4,
                                                       cores =chains,
                                                       thin = thin,
                                                       warmup=warmup,
                                                       refresh = 50)

save(castilla_samples_multivariate_yesSI, file = "results/final/multivariate_castilla_yesSI.RData")

# Example traceplots of arbitrarily chosen parameters
pdf(file = "Manuscript/plots/castilla_traceplots.pdf", width = 6, height = 8)
grid.arrange(
  traceplot(castilla_samples_multivariate_threelags, pars = "alpha_move[2,25]")+labs(y="", title =expression(alpha["25,2"]^mob))+theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10)),
  traceplot(castilla_samples_multivariate_threelags, pars = "alpha_move[1,90]")+labs(y="", title =expression(alpha["90,1"]^mob))+theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10)),
  traceplot(castilla_samples_multivariate_threelags, pars = "alpha_move[3,168]")+labs(y="", title =expression(alpha["168,3"]^mob))+theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10)),
  traceplot(castilla_samples_multivariate_threelags, pars = "alpha_spat[22]")+labs(y="")+labs(y="", title =expression(alpha[22]^spat))+theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10)),
  traceplot(castilla_samples_multivariate_threelags, pars = "alpha_spat[52]")+labs(y="")+labs(y="", title =expression(alpha[52]^spat))+theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10)),
  traceplot(castilla_samples_multivariate_threelags, pars = "alpha_spat[231]")+labs(y="")+labs(y="", title =expression(alpha[231]^spat))+theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10)),
  traceplot(castilla_samples_multivariate_threelags, pars = "alpha_ar[21]")+labs(y="")+labs(y="", title =expression(alpha[21]^ar))+theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10)),
  traceplot(castilla_samples_multivariate_threelags, pars = "alpha_ar[167]")+labs(y="")+labs(y="", title =expression(alpha[167]^ar))+theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10)),
  traceplot(castilla_samples_multivariate_threelags, pars = "alpha_ar[201]")+labs(y="")+labs(y="", title =expression(alpha[201]^ar))+theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10)),
  traceplot(castilla_samples_multivariate_threelags, pars = "alpha_en[12]")+labs(y="")+labs(y="", title =expression(alpha[12]^en))+theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10)),
  traceplot(castilla_samples_multivariate_threelags, pars = "alpha_en[98]")+labs(y="")+labs(y="", title =expression(alpha[98]^en))+theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10)),
  traceplot(castilla_samples_multivariate_threelags, pars = "alpha_en[229]")+labs(y="")+labs(y="", title =expression(alpha[229]^en))+theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10)),
  nrow = 6)
dev.off()

# summarizing results ===========

# uncomment the following as needed
load(file = "results/final/multivariate_castilla_noSI.RData") 
load(file = "results/final/multivariate_castilla_testing2.RData")
load(file = "results/final/multivariate_castilla_yesSI.RData")
load(file = "results/final/multivariate_castilla_threelags.RData")

loo1= extract_log_lik(castilla_samples_multivariate_noSI) |> loo()
loo2= extract_log_lik(castilla_samples_multivariate_noSI_testing) |> loo()
loo3= extract_log_lik(castilla_samples_multivariate_yesSI) |> loo()
loo4= extract_log_lik(castilla_samples_multivariate_threelags) |> loo()


rho1 = rstan::extract(castilla_samples_multivariate_yesSI)$rho |> quantile(probs = c(0.025,0.5,0.975))


# make a date variable for plotting.
castilla_date <- seq.Date(as.Date("2020-03-01"),as.Date("2021-03-01"), by = "week")


## Figure 3 plots ==============================================================
tmp = rstan::extract(castilla_samples_multivariate_noSI)

# Whole region

tmp1 <- tmp$EP_AR %>% apply(MARGIN=1, FUN=colSums) %>% 
  apply(MARGIN = 1, FUN = quantile, probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[4:53]) %>% 
  data.frame() %>% 
  mutate(component = "Autoregressive")

tmp2 <- tmp$EP_move %>%apply(MARGIN=1, FUN=colSums) %>% 
  apply(MARGIN = 1, FUN = quantile, probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[4:53]) %>% 
  data.frame() %>% 
  mutate(component = "Mobility")

tmp3 <- tmp$EP_spat %>%apply(MARGIN=1, FUN=colSums) %>% 
  apply(MARGIN = 1, FUN = quantile, probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[4:53]) %>% 
  data.frame() %>% 
  mutate(component = "Spatial")

tmp4 <- tmp$mu %>% apply(MARGIN=1, FUN=colSums) %>% 
  apply(MARGIN = 1, FUN = quantile, probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[4:53]) %>% 
  data.frame() %>% 
  mutate(component = "Aggregate")

tmp5 <- tmp$EN %>% rowSums() %>% 
  quantile(probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[4:53]) %>% 
  data.frame() %>% 
  mutate(component = "Exogenous")

pdf(file = "Manuscript/AOAS/plots/castilla_multivariate_noSI.pdf", width = 7, height = 5)
ggplot()+
  geom_point(aes(x=castilla_date[4:53], y = as.numeric(colSums(y_castilla[,4:53]))))+
  geom_ribbon(data = rbind.data.frame(tmp1,tmp2,tmp3,tmp4,tmp5), 
              aes(ymin = X2.5., ymax = X97.5., x =date, fill = component, colour = component), alpha = 0.5, linetype=1)+
  scale_fill_manual(values=c("black","blue","orange","red","green"))+
  scale_colour_manual(values=c("black","blue","orange","red","green"))+
  labs(fill="", y= "Weekly Cases", x = "")+
  theme_bw()+
  theme(legend.position = c(0.35,0.7), 
        legend.background = element_rect("transparent"),
        text = element_text(size = 20))+
  guides(col = "none")
dev.off()

pdf(file = "Manuscript/AOAS/plots/castilla_multivariate_noSI_grey.pdf", width = 7, height = 5)
ggplot()+
  geom_point(aes(x=castilla_date[4:53], y = as.numeric(colSums(y_castilla[,4:53]))))+
  geom_ribbon(data = rbind.data.frame(tmp1,tmp2,tmp4), 
              aes(ymin = X2.5., ymax = X97.5., x =date, fill = component, colour = component), alpha = 0.8, linetype=1)+
  scale_fill_manual(values=c("grey10","gray40","gray80" ))+
  scale_colour_manual(values=c("grey10","gray40","gray80"))+
  labs(fill="", y= "Weekly Cases", x = "")+
  theme_bw()+
  theme(legend.position = c(0.35,0.8), 
        legend.background = element_rect("transparent"),
        text = element_text(size = 20))+
  guides(col = "none")
dev.off()

## mobility good ====
tmp1 <- tmp$EP_AR[, 71,] %>%
  apply(MARGIN = 2, FUN = quantile, probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[4:53]) %>% 
  data.frame() %>% 
  mutate(component = "Autoregressive")

tmp2 <- tmp$EP_move[, 71,] %>%
  apply(MARGIN = 2, FUN = quantile, probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[4:53]) %>% 
  data.frame() %>% 
  mutate(component = "Mobility")

tmp3 <- tmp$EP_spat[, 71,] %>%
  apply(MARGIN = 2, FUN = quantile, probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[4:53]) %>% 
  data.frame() %>% 
  mutate(component = "Spatial")

tmp4 <- tmp$mu[, 71,] %>%
  apply(MARGIN = 2, FUN = quantile, probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[4:53]) %>% 
  data.frame() %>% 
  mutate(component = "Aggregate")

tmp5 <- tmp$EN[, 71] %>%
  quantile(probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[4:53]) %>% 
  data.frame() %>% 
  mutate(component = "Exogenous")

pdf(file = "Manuscript/AOAS/plots/castilla_multivariate_mobilitygood.pdf", width = 7, height = 5)
ggplot()+
  geom_point(aes(x=castilla_date[4:53], y = as.numeric(y_castilla[71,4:53])))+
  geom_ribbon(data = rbind.data.frame(tmp1,tmp2,tmp4), 
              aes(ymin = X2.5., ymax = X97.5., x =date, fill = component, colour = component), alpha = 0.5)+
  scale_fill_manual(values=c("black","blue","red"))+
  scale_colour_manual(values=c("black","blue","red"))+
  labs(fill="", y= "Weekly Cases", x = "")+
  theme_bw()+
  theme(legend.position = c(0.35,0.8), 
        legend.background = element_rect("transparent"),
        text = element_text(size = 20))+
  guides(col = "none")
dev.off()


pdf(file = "Manuscript/AOAS/plots/castilla_multivariate_mobilitygood_grey.pdf", width = 7, height = 5)
ggplot()+
  geom_point(aes(x=castilla_date[4:53], y = as.numeric(y_castilla[71,4:53])))+
  geom_ribbon(data = rbind.data.frame(tmp1,tmp2,tmp4), 
              aes(ymin = X2.5., ymax = X97.5., x =date, fill = component, colour = component), alpha = 0.8)+
  scale_fill_manual(values=c("grey10","gray40","gray80" ))+
  scale_colour_manual(values=c("grey10","gray40","gray80"))+
  labs(fill="", y= "Weekly Cases", x = "")+
  theme_bw()+
  theme(legend.position = c(0.35,0.8), 
        legend.background = element_rect("transparent"),
        text = element_text(size = 20))+
  guides(col = "none")
dev.off()


# show region with a weak mobility effect

tmp1 <- tmp$EP_AR[, 191,] %>%
  apply(MARGIN = 2, FUN = quantile, probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[4:53]) %>% 
  data.frame() %>% 
  mutate(component = "Autoregressive")

tmp2 <-  tmp$EP_move[, 191,] %>%
  apply(MARGIN = 2, FUN = quantile, probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[4:53]) %>% 
  data.frame() %>% 
  mutate(component = "Mobility")

tmp3 <-  tmp$EP_spat[, 191,] %>%
  apply(MARGIN = 2, FUN = quantile, probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[4:53]) %>% 
  data.frame() %>% 
  mutate(component = "Spatial")

tmp4 <-  tmp$mu[, 191,] %>%
  apply(MARGIN = 2, FUN = quantile, probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[4:53]) %>% 
  data.frame() %>% 
  mutate(component = "Aggregate")

tmp5 <-  tmp$EN[,191] %>%
  quantile(probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[4:53]) %>% 
  data.frame() %>% 
  mutate(component = "Exogenous")

pdf(file = "Manuscript/AOAS/plots/castilla_multivariate_mobilitybad.pdf", width = 7, height = 5)
ggplot()+
  geom_point(aes(x=castilla_date[4:53], y = as.numeric(y_castilla[191,4:53])))+
  geom_ribbon(data = rbind.data.frame(tmp1,tmp2,tmp4), 
              aes(ymin = X2.5., ymax = X97.5., x =date, fill = component, colour = component), alpha = 0.5)+
  scale_fill_manual(values=c("black","blue","red"))+
  scale_colour_manual(values=c("black","blue","red"))+
  labs(fill="", y= "Weekly Cases", x = "")+
  theme_bw()+
  theme(legend.position = c(0.35,0.8), 
        legend.background = element_rect("transparent"),
        text = element_text(size = 20))+
  guides(col = "none")
dev.off() 

pdf(file = "Manuscript/AOAS/plots/castilla_multivariate_mobilitybad_grey.pdf", width = 7, height = 5)
ggplot()+
  geom_point(aes(x=castilla_date[4:53], y = as.numeric(y_castilla[191,4:53])))+
  geom_ribbon(data = rbind.data.frame(tmp1,tmp2,tmp4), 
              aes(ymin = X2.5., ymax = X97.5., x =date, fill = component, colour = component), alpha = 0.8)+
  scale_fill_manual(values=c("grey10","gray40","gray80" ))+
  scale_colour_manual(values=c("grey10","gray40","gray80"))+
  labs(fill="", y= "Weekly Cases", x = "")+
  theme_bw()+
  theme(legend.position = c(0.35,0.8), 
        legend.background = element_rect("transparent"),
        text = element_text(size = 20))+
  guides(col = "none")
dev.off() 


## PCAtM plots and calculations ================================================

# output PCAtM for no testing no SI
PCAtM <- ((tmp$EP_move %>% rowSums(dim=2))/
            (tmp$mu %>% rowSums(dim=2))) %>% 
  apply(FUN=quantile, MARGIN = 2, probs = c(0.025,0.5,0.975)) %>% t() %>% 
  data.frame()


pdf(file = "Manuscript/AOAS/plots/castilla_PCAtM_map_grey.pdf")
custom_greys <- gray(seq(0, 1, length.out = 11))
  try <- hzone %>% cbind.data.frame(PCAtM = PCAtM[,2])
  scale <-   mapmisc::colourScale(try$PCAtM, 
                                  breaks = c(seq(0,1,0.1)), 
                                  style = "fixed",
                                  col=custom_greys,
                                  rev=TRUE)
  par(mar = c(2, 2, 2, 2))
  plot(hzone$geometry, col=scale$plot)
  mapmisc::scaleBar(hzone, pos = "bottomright", seg.len=3, bty = "n", outer=TRUE, title.cex=1.2)
  legendBreaks("topright", breaks =scale, bty = "n", cex=0.8)
dev.off()



PCAtM_agg <- c()

PCAtM_agg <- ((rstan::extract(castilla_samples_multivariate_threelags)$EP_move %>% rowSums(dim=1))/
                (rstan::extract(castilla_samples_multivariate_threelags)$mu %>% rowSums(dim=1))) %>% 
  quantile(probs=c(0.025,0.5,0.975)) 

PCAtM_agg <- ((rstan::extract(castilla_samples_multivariate_yesSI)$EP_move %>% rowSums(dim=1))/
                (rstan::extract(castilla_samples_multivariate_yesSI)$mu %>% rowSums(dim=1))) %>% 
  quantile(probs=c(0.025,0.5,0.975)) %>% rbind(PCAtM_agg)

PCAtM_agg <- ((rstan::extract(castilla_samples_multivariate_noSI_testing)$EP_move %>% rowSums(dim=1))/
                (rstan::extract(castilla_samples_multivariate_noSI_testing)$mu %>% rowSums(dim=1))) %>% 
  quantile(probs=c(0.025,0.5,0.975)) %>% rbind(PCAtM_agg)

PCAtM_agg <- ((rstan::extract(castilla_samples_multivariate_noSI)$EP_move %>% rowSums(dim=1))/
                (rstan::extract(castilla_samples_multivariate_noSI)$mu %>% rowSums(dim=1)))%>% 
  quantile(probs=c(0.025,0.5,0.975)) %>% 
  rbind(PCAtM_agg)

PCAtM_agg <- PCAtM_agg %>% 
  data.frame() %>% 
  mutate(report = paste0(100*round(X50.,4), " (", 100*round(X2.5.,4), ", ", 100*round(X97.5.,4),")"  )   ) %>% 
  dplyr::select(report)

rownames(PCAtM_agg) <- c("No SI, No testing","No SI, testing","SI, additional lag, no testing", "No SI, additional lag, no testing")
colnames(PCAtM_agg) <- ("PCAtM (95% CrI)")

PCAtM_agg %>% 
  xtable(caption = "Percentage of cases attributable to movement (PCAtM). Posterior median and 95\\% CrI's are presented",
         label = "tab:PCAtM_castilla") %>% 
  print(file = "Manuscript/plots/PCAtM_agg_castilla.tex")


## Cases per infected trip =====================================================
tripsPerInfection <- (1/tmp$inf_per_trip_region) %>% 
  apply(FUN = quantile, MARGIN = 2, probs = c(0.025,0.5,0.975)) %>% 
  t()

pdf(file = "Manuscript/AOAS/plots/castilla_tripsPerInfection.pdf")
tripsPerInfection %>% 
  data.frame() %>% 
  arrange(X50.) %>% 
  cbind.data.frame(region = 1:245, popByZone_castilla) %>%
  ggplot(aes(x=region, y = X50.))+
  labs(x = "Region Indicator", y = "Trips Per Infection")+
  geom_point()+
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.)) +
  theme_bw()+
  scale_y_log10()
dev.off()  

pdf(file = "Manuscript/AOAS/plots/castilla_tripsPerInfection_map_grey.pdf")
  try <- hzone %>% cbind.data.frame(ratm = tripsPerInfection[,2])
  scale <-   mapmisc::colourScale(try$ratm, 
                                  breaks = c(1,5,10,20,50,100,200,500,1000,2000), 
                                  style = "fixed",
                                  col=custom_greys,
                                  rev=FALSE)
  par(mar = c(2, 2, 2, 2))
  plot(hzone$geometry, col=scale$plot)
  mapmisc::scaleBar(hzone, pos = "bottomright", seg.len=3, bty = "n", outer=TRUE, title.cex=1.2)
  legendBreaks("topright", breaks =scale, bty = "n", cex=0.8)
dev.off()  

pdf(file = "Manuscript/AOAS/plots/castilla_infPerTrip.pdf", width = 7, height = 5)
(1/rstan::extract(castilla_samples_multivariate_threelags)$inf_per_trip_agg) %>% 
  apply(FUN = quantile, MARGIN = 2, prob = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  data.frame() %>% 
  cbind.data.frame(date = castilla_date[4:53]) %>% 
  ggplot(aes(x=date))+
  geom_ribbon(aes (ymin = X2.5., ymax = X97.5.), alpha = 0.4) + 
  geom_line(aes(y = X50.))+
  theme_bw()+
  labs(y="Infected Trips per New Infection", x ="")
dev.off() 

## descriptive plots ===========================================================

# plot cases, mobility and tests over time
pdf(file = "Manuscript/AOAS/plots/castilla_descriptive.pdf", width = 5, height = 7)
  cbind.data.frame(dates = castilla_date, 
                   Cases=colSums(y_castilla)/1000, 
                   Tests=castilla_test_ts/1000, 
                   Trips = total_mobility_castilla/10000000) %>% 
    melt("dates") %>% 
    ggplot(aes(x = dates, y=value)) +
    geom_line()+
    facet_wrap(~variable, ncol = 1, scales = "free_y", strip.position = "left", 
               labeller = as_labeller(c(Cases = "Cases (Thousands)", 
                                        Trips = "Trips (Millions)", 
                                        Tests = "Tests (Thousands)")) )+
    labs(x=NULL, y = NULL)+
    theme_bw()+
    theme(strip.background = element_blank(),
          strip.placement = "outside")
dev.off()

# cross correlation between 
castilla_ccf <- ccf(total_mobility_castilla, crude_r_castilla)
pdf(file = "Manuscript/plots/castilla_ccf.pdf", width = 7, height = 5)
  plot(seq(14,0,-1), castilla_ccf$acf[1:15], pch = 16, ylab = "Cross Correlation", xlab = "Lag (h)")
dev.off()


pdf(file = "Manuscript/plots/castilla_test_adjust.pdf", width = 7, height = 7)
  lm_test <- lm(crude_r_castilla[4:53] ~ ratio_tests_castilla[4:53])
  plot(ratio_tests_castilla[4:53],crude_r_castilla[4:53])
  abline(a=lm_test$coefficients[1], b=lm_test$coefficients[2], col = "blue")
dev.off()

# exploratory plots pertaining to testing
corrected_r_castilla <- (1/(ratio_tests_castilla[4:53]))*budget_r_castilla[4:53]

lm1 <- lm(budget_r_castilla ~ mobility1_castilla)
lm2 <- lm(budget_r_castilla[4:53] ~ mobility1_castilla[4:53])
lm3 <- lm(corrected_r_castilla[4:53] ~ mobility1_castilla[4:53])

pdf(file = "Manuscript/plots/castilla_outlier.pdf", width = 7, height = 7)
plot( mobility1_castilla, budget_r_castilla, pch = 16, 
      ylab = "Crude Effective R", xlab = "Trips")
abline(a=lm1$coefficients[1], b=lm1$coefficients[2], col = "darkcyan", lwd = 2)
abline(a=lm2$coefficients[1], b=lm2$coefficients[2], col = "brown3", lwd = 2)
dev.off()

pdf(file = "Manuscript/plots/castilla_testing.pdf")
plot(mobility1_castilla[4:53], corrected_r_castilla[4:53], pch = 16)
abline(a=lm3$coefficients[1], b= lm3$coefficients[2], col = "purple")
dev.off()

## Effective Reproduction Number ===============================================

## R_0 with uncertainty ========================================================
alpha_move <- tmp$alpha_move[1:50,,]
alpha_spat <- tmp$alpha_spat[1:50,] 
alpha_ar <- tmp$alpha_ar[1:50,] 


Phi <- array(dim = c(dim(alpha_move)[1],dim(weekly_mobility[,,4:53])))

basic_R <- matrix(nrow = dim(Phi)[1], ncol = dim(Phi)[4])

for (sample in 1:dim(Phi)[1]){
  for (t in 1:dim(Phi)[4]){
    for (orig in 1:dim(Phi)[2]){
      for (dest in 1:dim(Phi)[3]){
        
        if(orig == dest){
          ar_part = alpha_ar[sample, dest]
        }else{
          ar_part=0
        }
        Phi[sample,orig,dest,t] = (alpha_move[sample, 1,dest]*shifted_weekly_mobility1[orig,dest,t]+
                                     alpha_move[sample, 2,dest]*shifted_weekly_mobility2[orig,dest,t]+
                                     alpha_spat[sample, dest]*spatial_weights[orig,dest])/popByZone_castilla$pop[orig] + 
          ar_part
      }
    }
    basic_R[sample,t] <- max(Mod(eigen(Phi[sample,,,t])$values))
  }
  print(sample)
}

save(basic_R,file = "results/final/castilla_DE.RData")

pdf(file = "Manuscript/AOAS/plots/DE_castilla.pdf", height= 5, width=7)
basic_R %>% 
  apply(MARGIN = 2, FUN=quantile, probs = c(0.025,0.5, 0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[4:53]) %>% 
  data.frame() %>% 
  ggplot()+
  geom_ribbon( aes(x = date, ymin = X2.5., ymax = X97.5.), alpha=0.3) +
  geom_line(aes(x=date, y = X50.)) +
  geom_hline(aes(yintercept = 1), lty = 2, col = "red")+
  labs(x=NULL, y = expression(R[eff]))+
  theme_bw()
dev.off()

# Appendix A plots =============================================================

# compute quantities for descriptive plots
y_all_castilla <- y_castilla %>% colSums()
y_all_castilla_shifted <- shifted_y_castilla1 %>% colSums()
y_all_castilla_shifted[1] <- y_all_castilla_shifted[2]/2

mobility1_castilla <- c(total_mobility_castilla[1], total_mobility_castilla[1:52])
mobility2_castilla <- c(mobility1_castilla[1], mobility1_castilla[1:52])  
mobility3_castilla <- c(mobility2_castilla[1], mobility2_castilla[1:52])  


ratio_tests_castilla <- test_data_castilla$total_tests/lag(test_data_castilla$total_tests)
ratio_tests_castilla[1] = 2
ratio_tests_castilla [1:3] =c(1.5,1.5,1.5)

castilla_test_ts <- c(test_data_castilla$total_tests[4],
                      test_data_castilla$total_tests[3],
                      test_data_castilla$total_tests)
ratio_tests_castilla <- castilla_test_ts/lag(castilla_test_ts)
ratio_tests_castilla[1:3] =c(1.5,1.5,1.5)

budget_r_castilla <- colSums(y_castilla)/colSums(shifted_y_castilla1)

data_absolute <- list(T = 53,
                      cases=as.numeric(y_all_castilla),
                      mobility1 = mobility1_castilla,
                      mobility2 = mobility2_castilla,
                      shifted_cases1=colSums(shifted_y_castilla1)
)

data_absolute_no3weeks <- list(T = 50,
                               cases=as.numeric(y_all_castilla[4:53]),
                               mobility1 = mobility1_castilla[4:53],
                               mobility2 = mobility2_castilla[4:53],
                               shifted_cases1=colSums(shifted_y_castilla1[4:53])
) 


chains = 4
thin = 1
iter = thin*1000

stan_code <- rstan::stanc(file = "stan models/univariate_castilla.stan") # convert to C++ code
stan_model <- rstan::stan_model(stanc_ret = stan_code)     # compile generated code

univariate_castilla_absolute<- rstan::sampling(stan_model,
                                               data = data_absolute,
                                               iter = iter,
                                               chains = chains,
                                               cores =4,
                                               thin = thin,
                                               refresh = 50)

univariate_castilla_absolute_no3weeks<- rstan::sampling(stan_model,
                                                        data = data_absolute_no3weeks,
                                                        iter = iter,
                                                        chains = chains,
                                                        cores =4,
                                                        thin = thin,
                                                        refresh = 50)

# save(univariate_castilla_absolute, univariate_castilla_absolute_no3weeks,
#      file = "results/castilla_results_univariate.RData")
#load(file = "results/castilla_results_univariate.RData")

## Univariate castilla, all time points =====
tmp1 <- rstan::extract(univariate_castilla_absolute)$ep_ar %>%
  apply(MARGIN = 2, FUN = quantile, probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[1:53]) %>% 
  data.frame() %>% 
  mutate(component = "Autoregressive")

tmp2 <- rstan::extract(univariate_castilla_absolute)$ep_move %>%
  apply(MARGIN = 2, FUN = quantile, probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[1:53]) %>% 
  data.frame() %>% 
  mutate(component = "Mobility")

tmp4 <- rstan::extract(univariate_castilla_absolute)$lambda %>%
  apply(MARGIN = 2, FUN = quantile, probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[1:53]) %>% 
  data.frame() %>% 
  mutate(component = "Aggregate")

tmp5 <- rstan::extract(univariate_castilla_absolute)$nu %>%
  quantile(probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[1:53]) %>% 
  data.frame() %>% 
  mutate(component = "Exogenous")


pdf(file = "Manuscript/AOAS/plots/castilla_univariate_absolute_grey.pdf", width = 7, height = 5)
ggplot()+
  geom_point(aes(x=castilla_date[1:53], y = as.numeric(colSums(y_castilla[,1:53]))))+
  geom_ribbon(data = rbind.data.frame(tmp1,tmp2,tmp4), 
              aes(ymin = X2.5., ymax = X97.5., x =date, fill = component, colour = component), alpha = 0.8)+
  scale_fill_manual(values=c("gray10","gray40","grey80"))+
  scale_colour_manual(values=c("gray10","gray40","grey80"))+
  labs(fill="", y= "Weekly Cases", x = "")+
  theme_bw()+
  theme(legend.position = c(0.35,0.8), 
        legend.background = element_rect("transparent"),
        text = element_text(size = 20))+
  guides(col = "none")
dev.off()

## Univariate no 3 weeks ====
tmp1 <- rstan::extract(univariate_castilla_absolute_no3weeks)$ep_ar %>%
  apply(MARGIN = 2, FUN = quantile, probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[4:53]) %>% 
  data.frame() %>% 
  mutate(component = "Autoregressive")

tmp2 <- rstan::extract(univariate_castilla_absolute_no3weeks)$ep_move %>%
  apply(MARGIN = 2, FUN = quantile, probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[4:53]) %>% 
  data.frame() %>% 
  mutate(component = "Mobility")

tmp4 <- rstan::extract(univariate_castilla_absolute_no3weeks)$lambda %>%
  apply(MARGIN = 2, FUN = quantile, probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[4:53]) %>% 
  data.frame() %>% 
  mutate(component = "Aggregate")

tmp5 <- rstan::extract(univariate_castilla_absolute_no3weeks)$nu %>%
  quantile(probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[4:53]) %>% 
  data.frame() %>% 
  mutate(component = "Exogenous")

pdf(file = "Manuscript/AOAS/plots/castilla_univariate_absolute_no3weeks_grey.pdf", width = 7, height = 5)
ggplot()+
  geom_point(aes(x=castilla_date[4:53], y = as.numeric(colSums(y_castilla[,4:53]))))+
  geom_ribbon(data = rbind.data.frame(tmp1,tmp2,tmp4), 
              aes(ymin = X2.5., ymax = X97.5., x =date, fill = component, colour = component), alpha = 0.8)+
  scale_fill_manual(values=c("gray10","gray40","gray80"))+
  scale_colour_manual(values=c("gray10","gray40","gray80"))+
  labs(fill="", y= "Weekly Cases", x = "")+
  theme_bw()+
  theme(legend.position = c(0.35,0.8), 
        legend.background = element_rect("transparent"),
        text = element_text(size = 20))+
  guides(col = "none")
dev.off()

pdf(file = "Manuscript/AOAS/plots/castilla_outlier.pdf", width = 6, height = 6)
  par(mfrow = c(1,1))
  mob_plot = mobility1_castilla/1000000
  lm_full <- lm(crude_r_castilla ~ mob_plot)
  lm_partial <- lm(crude_r_castilla[4:53] ~ mob_plot[4:53])
  plot(lag(mob_plot),crude_r_castilla, ylab = expression("Crude R"[t]), xlab = "Trips (Millions)", pch=16)
  abline(a=lm_full$coefficients[1], b=lm_full$coefficients[2], lwd=2)
  abline(a=lm_partial$coefficients[1], b=lm_partial$coefficients[2], lwd=2, lty = 2)
  legend("topleft",legend = c("LS line - with weeks 2 and 3","LS line - no weeks 2 and 3"), lty = c(1,2)) 
  text(mob_plot[c(2,3)]-c(1.8,-0.3), crude_r_castilla[c(2,3)], labels=c("Week 2", "Week 3"))
dev.off()


# Appendix E plots =======

load("under_reported_poisson.RData")

thin = 1
iter = thin * 600
warmup= round(iter*0.5)
chains = 4

# stan_code <- rstan::stanc(file = "stan models/mvt_p_ur_cc.stan") # convert to C++ code
# stan_model <- rstan::stan_model(stanc_ret = stan_code)     # compile generated code
# castilla_samples_poisson_under <- rstan::sampling(stan_model,
#                                                   data = dataForStan,
#                                                   iter = iter,
#                                                   chains = 4,
#                                                   cores =chains,
#                                                   thin = thin,
#                                                   warmup=warmup,
#                                                   refresh = 50,
#                                                   pars = c("shifted_scaled_mobility1",
#                                                            "shifted_scaled_mobility2",
#                                                            "spat_sum",
#                                                            "sd_latent",
#                                                            "sd_observed",
#                                                            "mean_observed"),http://127.0.0.1:25301/graphics/plot_zoom_png?width=791&height=607
#                                                   include = FALSE)

#save(castilla_samples_poisson_under, file="under_reported_poisson.RData")

load("under_reported_poisson.RData")

tmp = rstan::extract(castilla_samples_poisson_under)

reporting_prob = tmp$report_prob |> colMeans(dim=1)

tmp1 <- tmp$EP_AR %>% apply(MARGIN=1, FUN=colSums) %>% 
  apply(MARGIN = 1, FUN = quantile, probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[5:53]) %>% 
  data.frame() %>% 
  mutate(component = "Autoregressive")

tmp2 <- tmp$EP_move %>%apply(MARGIN=1, FUN=colSums) %>% 
  apply(MARGIN = 1, FUN = quantile, probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[5:53]) %>% 
  data.frame() %>% 
  mutate(component = "Mobility")

tmp3 <- tmp$EP_spat %>%apply(MARGIN=1, FUN=colSums) %>% 
  apply(MARGIN = 1, FUN = quantile, probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[5:53]) %>% 
  data.frame() %>% 
  mutate(component = "Spatial")

tmp4 <- tmp$mu %>% apply(MARGIN=1, FUN=colSums) %>% 
  apply(MARGIN = 1, FUN = quantile, probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[5:53]) %>% 
  data.frame() %>% 
  mutate(component = "Aggregate")

tmp5 <- tmp$EN %>% rowSums() %>% 
  quantile(probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  cbind.data.frame(date = castilla_date[5:53]) %>% 
  data.frame() %>% 
  mutate(component = "Exogenous")


pdf(file = "Manuscript/AOAS/plots/castilla_underreport.pdf")
ggplot()+
  geom_point(aes(x=castilla_date[5:53], y = as.numeric(colSums(y_castilla[,5:53]))))+
  geom_ribbon(data = rbind.data.frame(tmp1,tmp2,tmp3,tmp4,tmp5), 
              aes(ymin = X2.5., ymax = X97.5., x =date, fill = component, colour = component), alpha = 0.8, linetype=1)+
  scale_fill_manual(values=c("black","gray20","gray40","gray60","gray80"))+
  scale_colour_manual(values=c("black","gray20","gray40","gray60","gray80"))+
  labs(fill="", y= "Weekly Cases", x = "")+
  theme_bw()+
  theme(legend.position = c(0.35,0.7), 
        legend.background = element_rect("transparent"),
        text = element_text(size = 20))+
  guides(col = "none")
dev.off()

report_prob = tmp$report_prob %>% 
  apply(FUN=quantile, MARGIN=2, probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  data.frame()

pdf(file = "Manuscript/plots/castilla_pi.pdf")
ggplot(report_prob, aes(x=castilla_date[4:53], y=X50.))+
  geom_ribbon(aes(ymin= X2.5., ymax=X97.5.), alpha = 0.8)+
  labs(x="", y = expression(pi[t]))+
  theme_bw()
dev.off()

# Appendix F plots ====

bbox = st_bbox(c(xmin = 270000, 
                 xmax = 380000, 
                 ymin = 4600000,
                 ymax = 4770000),
               crs = "EPSG:25830") |> 
  st_as_sfc()


rth = hzone[c(71,191),] |> select(d_zbs) |> 
  mutate(d_zbs = str_to_title(d_zbs))

rth$d_zbs <- factor(rth$d_zbs, levels = c("Parquesol", "Eras De Renueva"))

castilla_border = st_union(hzone)

castilla_border_map = tm_shape(castilla_border)+
  tm_borders()+
  tm_fill(col = "grey")+
  tm_shape(bbox)+
  tm_borders(lwd = 3)


pdf(file = "Manuscript/AOAS/plots/castilla_zoom.pdf", height = 7)
tm_shape(where, bbox = bbox)+
  tm_borders(lwd = 0.3)+
  tm_shape(rth)+
  tm_fill("d_zbs", palette = c("grey10", "gray50"), title = "")+
  tm_layout(frame = FALSE, legend.text.size = 1.8,
            legend.width = 1,
            legend.position = c("left","top"))
print(castilla_border_map, vp = viewport(0.35, 0.2, width = 0.3, height = 0.3))

dev.off()






