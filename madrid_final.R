library(dplyr)
library(tidyr)
library(stringr)
library(rstan)
library(janitor)
library(xtable)
library(mapmisc)
library(sf)
library(loo)
library(ggplot2)
library(tmap)

# data import and cleaning =====================================================
## case data ===================================================================
a <- read.csv("data/madrid_cases_covariates_26Feb2020to02Mar2021.csv", encoding="latin1") |>
  dplyr::select(MunicipalityName, MunicipalityCode,TotalPopulation, starts_with("day") ) |>
  pivot_longer(
    cols = starts_with("day"),
    names_to = "time",
    values_to = "cases",
    values_drop_na = TRUE
  ) |> mutate(time = as.numeric(str_sub(time, start=5)) ) |> 
  filter(time > 5) |> mutate(time = time - 5) |> ###shifting time index to start at March 1
  mutate(week = ceiling(time/7)) |> 
  group_by(MunicipalityName,TotalPopulation, MunicipalityCode, week) |> 
  summarise(weekly_cases=sum(cases)) |> 
  filter(week<=15)

a$MunicipalityName[a$MunicipalityName == "Madrid-City"]="Madrid"

seq.dates <- seq(as.Date("2020/03/3"), as.Date("2021/12/22"), "week")
date.df <- cbind.data.frame(date = seq.dates, week = seq(1,length(seq.dates))) |> data.frame()
date.df$date[date.df$date == as.Date("2020-12-08")] = as.Date("2020-12-09")

add_data <- read.csv("data/covid19_muni_and_districts-26May20-21Dec21.csv", sep = ";", fileEncoding="latin1") |> 
  data.frame() |> 
  mutate(muni=ifelse(substr(municipality.district,1,6)=="Madrid","Madrid", municipality.district),
         date = as.Date(substr(date,1,10), format = "%d/%m/%Y")) |> #check this
  #date = substr(date,1,10)) |> 
  group_by(muni,date) |> 
  #summarise(cases = sum(confirmed.cases.last.14days)) |> 
  summarise(cases = sum(confirmed.cases.last.14days, na.rm = TRUE)) |> 
  left_join(date.df, by =c("date"))

names <- add_data$muni |> unique()
add_data$weekly_cases = NA

b <- NULL
for (name in names){
  new_data <- add_data |> filter(muni==name, week>15) |> arrange(week)
  
  new_data$weekly_cases[1] = new_data$cases[1] - 0.5*a$weekly_cases[a$MunicipalityName==name & a$week==15 ]
  
  for (i in 2:nrow(new_data)){
    new_data$weekly_cases[i] <- new_data$cases[i] - new_data$weekly_cases[i-1]
    
    new_data$weekly_cases[i] <- (new_data$weekly_cases[i]+new_data$weekly_cases[i-1])/2
  }
  b <- rbind(b,new_data)
}

b <- b |>dplyr::select(muni,week,weekly_cases) |>  
  rename(MunicipalityName = muni) |> 
  mutate(weekly_cases = round(weekly_cases))

madrid_cases_covariates <- rbind(a |> ungroup() |>  dplyr::select(MunicipalityName, week, weekly_cases), b) |> 
  data.frame() |> 
  left_join(a |>  dplyr::select(MunicipalityName, TotalPopulation,MunicipalityCode) |> unique(), by =  c("MunicipalityName")) |> 
  group_by(MunicipalityName,TotalPopulation, MunicipalityCode, week) |> 
  mutate(infectious_recovered = cumsum(weekly_cases),
         susceptible = TotalPopulation - infectious_recovered) |> 
  arrange(MunicipalityCode, week) |>
  filter(week<63)

pop_by_zone = madrid_cases_covariates |> group_by(MunicipalityCode) |> summarise(pop = mean(TotalPopulation))

#Make an outcome matrix where the row is the rowth region in the columnth time point
y_madrid <- madrid_cases_covariates |> 
  ungroup() |> 
  dplyr::select(MunicipalityCode, weekly_cases,week) |> 
  data.frame() |> 
  reshape(idvar = "MunicipalityCode", timevar = "week",  direction = "wide") |> 
  arrange(MunicipalityCode) |> 
  dplyr::select(-MunicipalityCode)

y_madrid[is.na(y_madrid)] = 0
y_madrid[y_madrid<0] = 0

shifted_y_madrid1 <- cbind(week1 = rep(0,nrow(y_madrid)),y_madrid)[,1:ncol(y_madrid)]
shifted_y_madrid1[1] <-  shifted_y_madrid1[2]/4

shifted_y_madrid2 <- cbind(week1 = rep(0,nrow(y_madrid)),y_madrid)[,1:ncol(y_madrid)]
shifted_y_madrid2[1] <-  shifted_y_madrid2[2]/4

susceptible <- madrid_cases_covariates |> 
  ungroup() |> 
  dplyr::select(MunicipalityCode, susceptible, week) |> 
  data.frame() |> 
  reshape(idvar="MunicipalityCode", timevar = "week", direction = "wide") |>  
  arrange(MunicipalityCode) |> 
  dplyr::select(-MunicipalityCode)

## Test Data ===================================================================
test_data <- read.csv("data/Datos_Pruebas_Realizadas_Historico_11022022.csv", sep = ";") |> 
  data.frame() |> 
  clean_names() |> 
  mutate(n_pcr = gsub(",","", n_pcr)) |> 
  mutate(community = case_when(
    provincia %in% c("Ávila", "Burgos","León","Palencia","Salamanca","Segovia", "Soria", "Valladolid","Zamora")~ "castilla",
    provincia %in% c("Madrid") ~ "madrid",
    TRUE ~ "Other")) |>
  filter(community %in% c("madrid","castilla")) |> 
  mutate(new_date = as.Date(fecha_prueba, format = '%d%b%Y'),
         total_tests = as.numeric(n_ant)+ as.numeric(n_pcr) ,
         total_positive = n_ant_positivos+n_pcr_positivos ) |> 
  mutate(time = as.numeric(new_date )-18321,
         week = ceiling(time/7))

test_data_madrid <- test_data |> filter(community == "madrid") |> group_by(week) |> 
  summarise(total_tests = sum(total_tests),
            total_positive=sum(total_positive) ) |> 
  filter(week < 63)


madrid_test_ts <- c(test_data_madrid$total_tests[4],
                    test_data_madrid$total_tests[3],
                    test_data_madrid$total_tests)
ratio_tests <- madrid_test_ts/lag(madrid_test_ts)
ratio_tests[1] <- 1

## vaccine Data =============================================================
vaccine_data <- read.csv("data/madrid_vaccine_data_ordered.csv") |> 
  dplyr::select(date, fully_vaccinated, partially_vaccinated) |> 
  mutate(time = as.numeric(as.Date(date), format = '%Y%b%d')-18321,
         week = ceiling(time/7)) |> 
  filter(week > 44) |> 
  mutate(fully_vaccinated = as.numeric(gsub("\\.","", fully_vaccinated)),
         partially_vaccinated=as.numeric(gsub("\\.","", partially_vaccinated))) |> 
  group_by(week) |> 
  slice(which.max(fully_vaccinated)) |> 
  dplyr::select(week, fully_vaccinated, partially_vaccinated) |> 
  filter(week<63) 

## Mobility Data =============================================================
files <- list.files(path = "data/Madrid",pattern=".csv")
files_weeks <- cbind(files, seq(1, length(files))) |> data.frame() |> mutate(week = ceiling(as.numeric(V2)/7)) 

#Sum mobility matrices by week
weekly_mobility= array(dim=c(179, 179, max(madrid_cases_covariates$week))) #note that in this object, week 1 will empty

for (i in 1:max(madrid_cases_covariates$week)){
  tmp <- array(matrix(nrow =179, ncol = 179), dim = c(179, 179, 7) )
  matrices_idx <- files_weeks$V2[which(files_weeks$week==i)] |> as.numeric()
  for (j in 1:7){
    tmp[,,j] <- read.csv(paste0("data//Madrid//", files_weeks$files[matrices_idx[j]]))[,-1] |> 
      data.matrix()
  }
  weekly_mobility[,,i] = rowSums(tmp, dims =2)
  print(i)
}
weekly_mobility[weekly_mobility<1]=0

total_mobility_madrid = weekly_mobility |> colSums(dim=2)

shifted_weekly_mobility1 = array(dim = dim(weekly_mobility))
shifted_weekly_mobility1[,,1:2] = weekly_mobility[,,1]
shifted_weekly_mobility1[,,3:62]= weekly_mobility[,,2:61]

shifted_weekly_mobility2 = array(dim = dim(shifted_weekly_mobility1))
shifted_weekly_mobility2[,,1:2] = shifted_weekly_mobility1[,,1]
shifted_weekly_mobility2[,,3:62]= shifted_weekly_mobility1[,,2:61]

shifted_weekly_mobility3 = array(dim = dim(shifted_weekly_mobility2))
shifted_weekly_mobility3[,,1:2] = shifted_weekly_mobility2[,,1]
shifted_weekly_mobility3[,,3:62]= shifted_weekly_mobility2[,,2:61]

## Spatial Component ===========================================================
library(sf)
sf::sf_use_s2(FALSE)
muni <- st_read("data/Shape File-Madrid-Municipalities") 
muni$natcode[which(muni$nameunit=="Madrid")] <- "34132828000" #Match spatial units to Jorge's coding
muni <- muni |> arrange(natcode)  
neighbor_mat <- spdep::poly2nb(muni, row.names = muni$natcode)
output=NULL

for (i in 1:179){
  tmp <- matrix(ncol = 2, nrow = length(neighbor_mat[[i]]))
  tmp[,1] <- i
  tmp[,2] <- neighbor_mat[[i]]
  output <- rbind(output,tmp)
}

edges <- output |> data.frame() |>
  rowwise() |> mutate(node1 = min(X1,X2), node2 = max(X1,X2)) |> 
  dplyr::select(node1,node2) |> ungroup() |> 
  group_by(node1, node2) |>
  filter(row_number() == 1) |> ungroup() |> 
  group_by(node1) |> 
  mutate(num_neighbours = n())

el<- edges |> dplyr::select(-num_neighbours) |> as.matrix()

adjacency <- matrix(0, 179,179)

for (i in 1: nrow(el)){
  adjacency[el[i,1], el[i,2]] <- 1
  adjacency[el[i,2], el[i,1]] <- 1
}

diag(adjacency) = 0 
spatial_weights = adjacency /rowSums(adjacency)

### Mobility plot =========
library(raster)
plot_mob_matrix = (weekly_mobility/7) |> 
  rowMeans(dim=2) 

row_sums <- rowSums(plot_mob_matrix)

# Sort matrix rows based on row sums
plot_mob_matrix<- plot_mob_matrix[order(row_sums, decreasing = TRUE), order(row_sums, decreasing = TRUE) ] |> 
  raster()

pdf(file = "Manuscript/AOAS/plots/madrid_mean_daily_mob.pdf", height = 7)
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



# Model - Multivariate =========================================================
## Three lags ============================
thin = 1
iter = thin *1000
warmup = round(iter/2)
chains = 4

dataForStan <- list(I = nrow(y_madrid[,4:62]),
                    T = ncol(y_madrid[,4:62]),
                    y = y_madrid[,4:62],
                    shifted_y1 = shifted_y_madrid1[,4:62],
                    shifted_weekly_mobility1 = shifted_weekly_mobility1[,,4:62],
                    shifted_weekly_mobility2 = shifted_weekly_mobility2[,,4:62],
                    shifted_weekly_mobility3 = shifted_weekly_mobility3[,,4:62],
                    pops = pop_by_zone$pop,
                    spatial_weights=spatial_weights[,],
                    num_tests = madrid_test_ts[4:62]/max(madrid_test_ts[4:62]),
                    vaccine_data = c(rep(0,42), vaccine_data$fully_vaccinated/sum(pop_by_zone$pop)))


stan_code <- rstan::stanc(file = "stan models/multivariate_madrid_threelags.stan") # convert to C++ code
stan_model <- rstan::stan_model(stanc_ret = stan_code)   # compile generated code

multivariate_samples_madrid_threelags <- rstan::sampling(stan_model,
                                                         data = dataForStan,
                                                         iter = iter,
                                                         chains = 4,
                                                         cores =chains,
                                                         thin = thin,
                                                         refresh = 10,
                                                         warmup=warmup,
                                                         control = list(adapt_delta=0.8,
                                                                        max_treedepth=10))

save(multivariate_samples_madrid_threelags, file = "results/final/multivariate_madrid_threelags.RData")



## Three lags SI ===============================================================

dataForStan <- list(I = nrow(y_madrid[,4:62]),
                    T = ncol(y_madrid[,4:62]),
                    y = y_madrid[,4:62],
                    shifted_y1 = shifted_y_madrid1[,4:62],
                    shifted_y2 = shifted_y_madrid2[,4:62],
                    shifted_weekly_mobility1 = shifted_weekly_mobility1[,,4:62],
                    shifted_weekly_mobility2 = shifted_weekly_mobility2[,,4:62],
                    shifted_weekly_mobility3 = shifted_weekly_mobility3[,,4:62],
                    pops = pop_by_zone$pop,
                    spatial_weights=spatial_weights,
                    num_tests = madrid_test_ts[4:62]/max(madrid_test_ts[4:62]))

stan_code <- rstan::stanc(file = "stan models/multivariate_madrid_SI.stan") # convert to C++ code
stan_model <- rstan::stan_model(stanc_ret = stan_code)     # compile generated code

multivariate_samples_madrid_SI <- rstan::sampling(stan_model,
                                                  data = dataForStan,
                                                  iter = iter,
                                                  chains = 4,
                                                  cores =chains,
                                                  thin = thin,
                                                  refresh = 10,
                                                  warmup=warmup,
                                                  control=list(adapt_delta=0.8))

save(multivariate_samples_madrid_SI, file = "results/final/multivariate_madrid_SI.RData")

extract_log_lik(multivariate_samples_madrid_threelags) |> loo()
extract_log_lik(multivariate_samples_madrid_SI) |> loo()


# Results summary ==============================================================

load("results/final/multivariate_madrid_threelags.RData")
load("results/final/multivariate_madrid_SI.RData")

madrid_date <- seq.Date(as.Date("2020-03-01"),as.Date("2021-05-02"), by = "week")
plot_date <- seq.Date(as.Date("2020-03-01"),as.Date("2021-05-01"), by = "month")


expit = function(x) exp(x)/(1+exp(x))


tmp = rstan::extract(multivariate_samples_madrid_SI)

probs <- tmp$log_prob |> exp()

mu <- array(dim = dim(tmp$mu))
EP_move <- array(dim = dim(tmp$mu))
EP_AR <- array(dim = dim(tmp$mu))
EP_spat <- array(dim = dim(tmp$mu))
EN <- array(dim = dim(tmp$mu))

mu_unadj <- tmp$mu
EP_move_unadj <- tmp$EP_move
EP_AR_unadj <- tmp$EP_AR
EP_spat_unadj <- tmp$EP_spat
EN_unadj <- tmp$EN

for (i in 1:dim(mu_unadj)[2]){
  mu[,i,] <- mu_unadj[,i,]*probs
  EP_move[,i,] <- EP_move_unadj[,i,]*probs
  EP_AR[,i,] <- EP_AR_unadj[,i,]*probs
  EP_spat[,i,] <- EP_spat_unadj[,i,]*probs
  EN[,i,] <- EN_unadj[,i]*probs
}

# clear RAM
mu_unadj=NULL
EP_move_unadj_unadj=NULL
EP_AR_unadj_unadj=NULL
EP_spat_unadj_unadj=NULL
EN_unadj_unadj=NULL

# multivariate noCity ==============
tmp1 <- EP_AR[,2:179,] |> apply(MARGIN=1, FUN=colSums) |> 
  apply(MARGIN = 1, FUN = quantile, probs = c(0.025,0.5,0.975)) |> 
  t() |> 
  cbind.data.frame(date = madrid_date[4:62]) |> 
  data.frame() |> 
  mutate(component = "Autoregressive")

tmp2 <- EP_move[,2:179,] |> apply(MARGIN=1, FUN=colSums) |> 
  apply(MARGIN = 1, FUN = quantile, probs = c(0.025,0.5,0.975)) |> 
  t() |> 
  cbind.data.frame(date = madrid_date[4:62]) |> 
  data.frame() |> 
  mutate(component = "Mobility")

tmp3 <- EP_spat[,2:179,] |> apply(MARGIN=1, FUN=colSums) |> 
  apply(MARGIN = 1, FUN = quantile, probs = c(0.025,0.5,0.975)) |> 
  t() |> 
  cbind.data.frame(date = madrid_date[4:62]) |> 
  data.frame() |> 
  mutate(component = "Spatial")

tmp4 <- mu[,2:179,] |> apply(MARGIN=1, FUN=colSums) |> 
  apply(MARGIN = 1, FUN = quantile, probs = c(0.025,0.5,0.975)) |> 
  t() |> 
  cbind.data.frame(date = madrid_date[4:62]) |> 
  data.frame() |> 
  mutate(component = "Aggregate")

tmp5 <- EN[,2:179,] |> apply(MARGIN=1, FUN=colSums) |> 
  apply(MARGIN = 1, FUN = quantile, probs = c(0.025,0.5,0.975)) |> 
  t() |> 
  cbind.data.frame(date = madrid_date[4:62]) |> 
  data.frame() |> 
  mutate(component = "Exogenous")

pdf(file = "Manuscript/AOAS/plots/madrid_multivariate_noCity.pdf", width = 7, height = 5)
ggplot()+
  geom_point(aes(x=madrid_date[4:62], y = as.numeric(colSums(y_madrid[2:179,4:62]))))+
  geom_ribbon(data = rbind.data.frame(tmp1,tmp2,tmp3,tmp4,tmp5), 
              aes(ymin = X2.5., ymax = X97.5., x =date, fill = component, colour = component), alpha = 0.5, linetype=1)+
  scale_fill_manual(values=c("black","blue","orange","red","green"))+
  scale_colour_manual(values=c("black","blue","orange","red","green"))+
  labs(fill="", y= "Weekly Cases", x = "")+
  theme_bw()+
  theme(legend.position = c(0.35,0.8), 
        legend.background = element_rect("transparent"),
        text = element_text(size = 20))+
  guides(col = "none")
dev.off()


pdf(file = "Manuscript/AOAS/plots/madrid_multivariate_noCity_grey.pdf", width = 7, height = 5)
ggplot()+
  geom_point(aes(x=madrid_date[4:62], y = as.numeric(colSums(y_madrid[2:179,4:62]))))+
  geom_ribbon(data = rbind.data.frame(tmp1,tmp2, tmp4), 
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


# mobility bad ======
tmp1 <- EP_AR[,7,] |> 
  apply(MARGIN = 2, FUN = quantile, probs = c(0.025,0.5,0.975)) |> 
  t() |> 
  cbind.data.frame(date = madrid_date[4:62]) |> 
  data.frame() |> 
  mutate(component = "Autoregressive")

tmp2 <- EP_move[,7,] |> 
  apply(MARGIN = 2, FUN = quantile, probs = c(0.025,0.5,0.975)) |> 
  t() |> 
  cbind.data.frame(date = madrid_date[4:62]) |> 
  data.frame() |> 
  mutate(component = "Mobility")

tmp3 <- EP_spat[,7,] |> 
  apply(MARGIN = 2, FUN = quantile, probs = c(0.025,0.5,0.975)) |> 
  t() |> 
  cbind.data.frame(date = madrid_date[4:62]) |> 
  data.frame() |> 
  mutate(component = "Spatial")

tmp4 <- mu[,7,] |> 
  apply(MARGIN = 2, FUN = quantile, probs = c(0.025,0.5,0.975)) |> 
  t() |> 
  cbind.data.frame(date = madrid_date[4:62]) |> 
  data.frame() |> 
  mutate(component = "Aggregate")

tmp5 <- EN[,7,] |> 
  apply(MARGIN = 2, FUN = quantile, probs = c(0.025,0.5,0.975)) |> 
  t() |> 
  cbind.data.frame(date = madrid_date[4:62]) |> 
  data.frame() |> 
  mutate(component = "Exogenous")

pdf(file = "Manuscript/AOAS/plots/madrid_multivariate_mobilityBad.pdf", width = 7, height = 5)
ggplot()+
  geom_point(aes(x=madrid_date[4:62], y = as.numeric(y_madrid[7,4:62])))+
  geom_ribbon(data = rbind.data.frame(tmp1,tmp2,tmp4), 
              aes(ymin = X2.5., ymax = X97.5., x =date, fill = component, colour = component), alpha = 0.5, linetype=1)+
  scale_fill_manual(values=c("black","blue","red"))+
  scale_colour_manual(values=c("black","blue","red"))+
  labs(fill="", y= "Weekly Cases", x = "")+
  theme_bw()+
  theme(legend.position = c(0.3,0.8), 
        legend.background = element_rect("transparent"),
        text = element_text(size = 20))+
  guides(col = "none")
dev.off()

pdf(file = "Manuscript/AOAS/plots/madrid_multivariate_mobilityBad_grey.pdf", width = 7, height = 5)
ggplot()+
  geom_point(aes(x=madrid_date[4:62], y = as.numeric(colSums(y_madrid[7,4:62]))))+
  geom_ribbon(data = rbind.data.frame(tmp1,tmp2,tmp4), 
              aes(ymin = X2.5., ymax = X97.5., x =date, fill = component, colour = component), alpha = 0.8, linetype=1)+
  scale_fill_manual(values=c("grey10","gray40","gray80"))+
  #scale_colour_manual(values=c("grey10","gray30","gray70","gray90"))+
  scale_colour_manual(values=c("grey10","gray40","gray80"))+
  labs(fill="", y= "Weekly Cases", x = "")+
  theme_bw()+
  theme(legend.position = c(0.35,0.8), 
        legend.background = element_rect("transparent"),
        text = element_text(size = 20))+
  guides(col = "none")
dev.off()

# mobility good =======
tmp1 <- EP_AR[,5,] |> 
  apply(MARGIN = 2, FUN = quantile, probs = c(0.025,0.5,0.975)) |> 
  t() |> 
  cbind.data.frame(date = madrid_date[4:62]) |> 
  data.frame() |> 
  mutate(component = "Autoregressive")

tmp2 <- EP_move[,5,] |> 
  apply(MARGIN = 2, FUN = quantile, probs = c(0.025,0.5,0.975)) |> 
  t() |> 
  cbind.data.frame(date = madrid_date[4:62]) |> 
  data.frame() |> 
  mutate(component = "Mobility")

tmp3 <- EP_spat[,5,] |> 
  apply(MARGIN = 2, FUN = quantile, probs = c(0.025,0.5,0.975)) |> 
  t() |> 
  cbind.data.frame(date = madrid_date[4:62]) |> 
  data.frame() |> 
  mutate(component = "Spatial")

tmp4 <- mu[,5,] |> 
  apply(MARGIN = 2, FUN = quantile, probs = c(0.025,0.5,0.975)) |> 
  t() |> 
  cbind.data.frame(date = madrid_date[4:62]) |> 
  data.frame() |> 
  mutate(component = "Aggregate")

tmp5 <- EN[,5,] |> 
  apply(MARGIN = 2, FUN = quantile, probs = c(0.025,0.5,0.975)) |> 
  t() |> 
  cbind.data.frame(date = madrid_date[4:62]) |> 
  data.frame() |> 
  mutate(component = "Exogenous")

pdf(file = "Manuscript/AOAS/plots/madrid_multivariate_mobilityGood.pdf", width = 7, height = 5)
ggplot()+
  geom_point(aes(x=madrid_date[4:62], y = as.numeric(y_madrid[5,4:62])))+
  geom_ribbon(data = rbind.data.frame(tmp1,tmp2,tmp4), 
              aes(ymin = X2.5., ymax = X97.5., x =date, fill = component, colour = component), alpha = 0.5, linetype=1)+
  scale_fill_manual(values=c("black","blue","red"))+
  scale_colour_manual(values=c("black","blue","red"))+
  labs(fill="", y= "Weekly Cases", x = "")+
  theme_bw()+
  theme(legend.position = c(0.3,0.65), 
        legend.background = element_rect("transparent"),
        text = element_text(size = 20))+
  guides(col = "none")
dev.off()

pdf(file = "Manuscript/AOAS/plots/madrid_multivariate_mobilityGood_grey.pdf", width = 7, height = 5)
ggplot()+
  geom_point(aes(x=madrid_date[4:62], y = as.numeric(colSums(y_madrid[5,4:62]))))+
  geom_ribbon(data = rbind.data.frame(tmp1,tmp2,tmp4), 
              aes(ymin = X2.5., ymax = X97.5., x =date, fill = component, colour = component), alpha = 0.8, linetype=1)+
  scale_fill_manual(values=c("grey10","gray40","gray80"))+
  #scale_colour_manual(values=c("grey10","gray30","gray70","gray90"))+
  scale_colour_manual(values=c("grey10","gray40","gray80"))+
  labs(fill="", y= "Weekly Cases", x = "")+
  theme_bw()+
  theme(legend.position = c(0.35,0.8), 
        legend.background = element_rect("transparent"),
        text = element_text(size = 20))+
  guides(col = "none")
dev.off()

### PCAtM ================
# output PCAtM 
PCAtM <- ((rstan::extract(multivariate_samples_madrid_SI)$EP_move |> rowSums(dim=2))/
  (rstan::extract(multivariate_samples_madrid_SI)$mu |> rowSums(dim=2))) |> 
  apply(FUN=quantile, MARGIN = 2, probs = c(0.025,0.5,0.975)) |> t() |> 
  data.frame()

pdf(file = "Manuscript/AOAS/plots/madrid_PCAtM_map.pdf")
  try <- muni |> cbind.data.frame(PCAtM = PCAtM[,2])
  scale <-   mapmisc::colourScale(try$PCAtM, 
                                  breaks = c(seq(0,1,0.1)), 
                                  style = "fixed",
                                  col=heat.colors,
                                  rev=TRUE)
  par(mar = c(2, 2, 2, 2))
  plot(muni$geometry, col=scale$plot)
  mapmisc::scaleBar(muni, pos = "bottomright", seg.len=3, bty = "n", outer=TRUE, title.cex=1.2)
  legendBreaks("topright", breaks =scale, bty = "n", cex=0.8)
dev.off()  

pdf(file = "Manuscript/AOAS/plots/madrid_PCAtM_map_grey.pdf")
custom_greys <- gray(seq(0, 1, length.out = 11))
try <- muni |> cbind.data.frame(PCAtM = PCAtM[,2])
  scale <-   mapmisc::colourScale(try$PCAtM, 
                                  breaks = c(seq(0,1,0.1)), 
                                  style = "fixed",
                                  col=custom_greys,
                                  rev=TRUE)
  par(mar = c(2, 2, 2, 2))
  plot(muni$geometry, col=scale$plot)
  mapmisc::scaleBar(muni, pos = "bottomright", seg.len=3, bty = "n", outer=TRUE, title.cex=1.2)
  legendBreaks("topright", breaks =scale, bty = "n", cex=0.8)
dev.off()


PCAtM_agg <- c()

PCAtM_agg <- ((rstan::extract(multivariate_samples_madrid_SI)$EP_move[,2:179,] |> rowSums(dim=1))/
                (rstan::extract(multivariate_samples_madrid_SI)$mu[,2:179,] |> rowSums(dim=1))) |> 
  quantile(probs=c(0.025,0.5,0.975)) 

PCAtM_agg <- ((rstan::extract(multivariate_samples_madrid_SI)$EP_move |> rowSums(dim=1))/
                (rstan::extract(multivariate_samples_madrid_SI)$mu |> rowSums(dim=1))) |> 
  quantile(probs=c(0.025,0.5,0.975)) |> rbind(PCAtM_agg)

PCAtM_agg <- ((rstan::extract(multivariate_samples_madrid_threelags)$EP_move |> rowSums(dim=1))/
                (rstan::extract(multivariate_samples_madrid_threelags)$mu |> rowSums(dim=1))) |> 
  quantile(probs=c(0.025,0.5,0.975)) |> rbind(PCAtM_agg)

PCAtM_agg <- PCAtM_agg |> 
  data.frame() |> 
  mutate(report = paste0(100*round(X50.,4), " (", 100*round(X2.5.,4), ", ", 100*round(X97.5.,4),")"  )   ) |> 
  dplyr::select(report)

rownames(PCAtM_agg) <- c("No SI","SI", "SI, No City")
colnames(PCAtM_agg) <- ("PCAtM (95% CrI)")

PCAtM_agg |> 
  xtable(caption = "Percentage of cases attributable to movement (PCAtM) for three models fit to the Madrid data. Posterior median and 95\\% CrI's are presented. SI = Serial Interval",
         label = "tab:PCAtM_madrid") |> 
  print(file = "Manuscript/plots/PCAtM_agg_madrid.tex")


tripsPerInfection <- (1/rstan::extract(multivariate_samples_madrid_SI)$inf_per_trip_region) |> 
  apply(FUN = quantile, MARGIN = 2, probs = c(0.025,0.5,0.975)) |> 
  t()


pdf(file = "Manuscript/AOAS/plots/madrid_tripsPerInfection_map.pdf")
  tpi <- muni |> cbind.data.frame(tpi = tripsPerInfection[,2])
  scale <-   mapmisc::colourScale(tpi$tpi, 
                                  breaks = c(1,2,5,10,20,50,100,200,500,1000,2000), 
                                  style = "fixed",
                                  col=heat.colors,
                                  rev=FALSE)
  par(mar = c(2, 2, 2, 2))
  plot(muni$geometry, col=scale$plot)
  mapmisc::scaleBar(muni, pos = "bottomright", seg.len=3, bty = "n", outer=TRUE, title.cex=1.2)
  legendBreaks("topright", breaks =scale, bty = "n", cex=0.8)
dev.off()  


pdf(file = "Manuscript/AOAS/plots/madrid_tripsPerInfection_map_grey.pdf")
custom_greys <- gray(seq(0, 1, length.out = 11))
  tpi <- muni |> cbind.data.frame(tpi = tripsPerInfection[,2])
  scale <-   mapmisc::colourScale(tpi$tpi, 
                                  breaks = c(1,2,5,10,20,50,100,200,500,1000,2000), 
                                  style = "fixed",
                                  col=custom_greys,
                                  rev=FALSE)
  par(mar = c(2, 2, 2, 2))
  plot(muni$geometry, col=scale$plot)
  mapmisc::scaleBar(muni, pos = "bottomright", seg.len=3, bty = "n", outer=TRUE, title.cex=1.2)
  legendBreaks("topright", breaks =scale, bty = "n", cex=0.8)
dev.off()  


## Trips per infection over time ====
pdf(file = "Manuscript/AOAS/plots/madrid_tripsPerInfection_temporal.pdf", width = 7, height= 5)
(1/rstan::extract(multivariate_samples_madrid_SI)$inf_per_trip_agg_noCity) |> 
  apply(FUN=quantile, MARGIN=2, probs = c(0.025,0.5,0.975)) |> 
  t() |> 
  data.frame() |> 
  cbind.data.frame(date = madrid_date[4:62]) |> 
  ggplot(aes(x=date, y = X50.))+
  #geom_point()+
  geom_ribbon(aes(ymin = X2.5., ymax = X97.5.), alpha = 0.5)+
  geom_line(aes(x=date, y = X50.))+
  theme_bw()+
  labs(x="", y = "Infected Trips per New Infection")
dev.off()


# Appendix C plots =====

tmp1 <- colSums( weekly_mobility[1,-1,]+weekly_mobility[-1,1,])
tmp2 <- weekly_mobility[2:179,2:179,] |> colSums(dim=2)
pdf(file = "Manuscript/AOAS/plots/madridCity_mobility.pdf", height=5, width=7)
ggplot()+
  geom_line(aes(x=madrid_date, y=weekly_mobility[1,1,]/1000000, linetype = "Within"),lwd = 1)+
  geom_line(aes(x=madrid_date, y = tmp1/1000000, linetype="Incoming/Outgoing"), lwd = 1) +
  theme_bw()+
  labs(col = NULL, y = "Trips (millions)", x = NULL, lty = NULL)+
  scale_linetype_manual(values = c(1,2)) +
  theme(legend.position=c(0.5,0.9),legend.text=element_text(size=14), 
        axis.text = element_text(size = 14),
        legend.key.size = unit(2.5,"line"),
        legend.background=element_blank(),
        axis.title = element_text(size = 14))
dev.off()

pdf(file = "Manuscript/AOAS/plots/madridNoCity_mobility.pdf", height=5, width=7)
ggplot()+
  geom_line(aes(x=madrid_date, y=tmp2/1000000), lwd = 1)+
  theme(legend.position=c(0.8,0.9))+
  labs(col = NULL, y = "Trips (millions)", x = NULL)+
  theme_bw()+
  theme(text = element_text(size =14))
dev.off()


y_all = colSums(y_madrid)
y_all_shifted1 = colSums(shifted_y_madrid1)
y_all_shifted1[1] = y_all_shifted1[2]/4

y_all_shifted2 = lag(y_all_shifted1)
y_all_shifted2[1] = y_all_shifted2[2]/4

y_all_shifted3 = lag(y_all_shifted2)
y_all_shifted3[1] = y_all_shifted3[2]/4

budget_r <- y_all/y_all_shifted1
mobility1_madrid <- c(total_mobility_madrid[1], total_mobility_madrid[1:61])
mobility1_madrid_millions = mobility1_madrid/1000000

corrected_r <- (1/(ratio_tests[4:62]))*budget_r[4:62]
lm1 <- lm(budget_r[2:62] ~ mobility1_madrid_millions[2:62])
lm2 <- lm(budget_r[4:62] ~ mobility1_madrid_millions[4:62])
lm3 <- lm(corrected_r[4:62] ~ mobility1_madrid_millions[4:62])

pdf(file = "Manuscript/AOAS/plots/madrid_outlier.pdf")
  par(las= 1)
  plot( mobility1_madrid[2:62]/1000000, budget_r[2:62], pch = 16, xlab = "Trips (millions)", ylab = expression("Crude R"[t]))
  abline(a=lm1$coefficients[1], b=lm1$coefficients[2],lwd = 3, lty = 1)
  abline(a=lm2$coefficients[1], b=lm2$coefficients[2],lwd = 3, lty = 2)
  legend("topleft",legend = c("LS line - with weeks 2 and 3","LS line - no weeks 2 and 3"), lty = c(1,2), lwd = c(3,3)) 
  text( c(62.5, 59.5), c(5.75, 2.25), labels=c("Week 2", "Week 3"))
dev.off()


pdf(file = "Manuscript/AOAS/plots/madrid_testing.pdf")
  par(las= 1)
  plot(mobility1_madrid[4:62]/1000000, corrected_r, pch = 16, 
       ylab = expression("Test Corrected Crude R"[t]), xlab = "Trips (millions)")
  abline(a=lm3$coefficients[1], b= lm3$coefficients[2], lwd=2)
dev.off()


# Appendix E Plots ======

tests = (madrid_test_ts[4:62]/max(madrid_test_ts[4:62]))
test_effect=rstan::extract(multivariate_samples_madrid_SI)$test_effect

plot(tests^test_effect, type = "l")

box = matrix(nrow = length(tests), ncol = length(test_effect))

for (i in 1:length(test_effect)) box[,i] = tests^test_effect[i]


pdf(file = "Manuscript/AOAS/plots/madrid_gamma.pdf")
box |> apply(FUN=quantile, MARGIN=1, probs = c(0.025,0.5,0.975)) |> 
  t() |> 
  data.frame() |> 
  ggplot()+
  geom_ribbon(aes(x = madrid_date[4:62] ,ymin = X2.5., ymax =X97.5.), alpha = 0.8)+
  labs(x = "", y = expression(gamma[t]))+
  theme_bw()
dev.off()


# Appendix F plot ====
library(tmap)
rth_madrid  = muni[c(5,7),]

madrid_border = st_union(muni)

bbox_madrid = st_bbox(c(xmin = -4.1, 
                        xmax = -3.5, 
                        ymin = 40.1,
                        ymax = 40.6),
                      crs = "EPSG:4326") |> 
  st_as_sfc()

madrid_border_map = tm_shape(madrid_border)+
  tm_borders()+
  tm_fill(col = "grey")+
  tm_shape(bbox_madrid)+
  tm_borders(lwd = 3)

pdf(file = "Manuscript/AOAS/plots/madrid_zoom.pdf", height = 7)
tm_shape(muni, bbox = bbox_madrid)+
  tm_borders(lwd = 0.3)+
  tm_shape(rth_madrid)+
  tm_fill("nameunit", palette = c("gray10", "gray50"), title = "")+
  tm_layout(frame = FALSE, legend.text.size = 1.8,
            legend.width = 1,
            legend.position = c("left","top"))

print(madrid_border_map, vp = viewport(0.7, 0.25, width = 0.3, height = 0.3))
dev.off()








