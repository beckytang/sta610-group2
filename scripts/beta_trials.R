load("cardio_clean.Rda")
set.seed(196)
require(tidyverse)
library(gridExtra)

head(cardio_clean)

cardio_clean <- cardio_clean %>%
  mutate(obs_mort_rate = Observed_Deaths/Total_Procedures,
         obs_std = (obs_mort_rate - Expected_Mortality_Rate)/sqrt(Expected_Mortality_Rate*(1-Expected_Mortality_Rate))) %>% 
  mutate(Hospital_Name = case_when(str_detect(Hospital_Name,"Loma Linda") ~ "Loma Linda",
                                   str_detect(Hospital_Name,"Sacred Heart") ~ "Providence Sacred Heart",
                                   Hospital_Name %>% str_detect("Doernbecher") ~ "Doernbecher Children's",
                                   TRUE ~ Hospital_Name),
         Hospital_Name = Hospital_Name %>% str_replace_all("University of Minnesota","UM"))

cardio_clean %>%
  filter(Procedure_Type %>% str_detect("5")) %>%
  ggplot(aes(x=obs_mort_rate)) +
  geom_histogram() +
  geom_vline(xintercept = .583,color = "red")

cardio_clean %>%
  filter(Procedure_Type %>% str_detect("4")) %>%
  ggplot(aes(x=obs_mort_rate)) +
  geom_histogram() +
  geom_vline(xintercept = .136,color = "red")


cardio_clean %>%
  filter(Procedure_Type %>% str_detect("5")) %>%
  ggplot(aes(x=obs_mort_rate)) +
  geom_histogram() +
  geom_vline(xintercept = cardio_clean$obs_std[cardio_clean$Hospital_Name == "UNC" & str_detect(cardio_clean$Procedure_Type,"5")],color = "red")




model <- function(){
  for(i in 1:length(y)){
    y[i] ~ dbin(p[i],n[i])
    p[i] ~ dbeta(exp[i]*alpha[procedure_type[i]],(1-exp[i])*alpha[procedure_type[i]]); T(1E-100,1)
  }

  for(i in 1:nprocedure_type){
    alpha_star[i] ~ dpois(lambda)
    alpha[i] = alpha_star[i] + 1
    #alpha[i] ~ dnorm(mu,tau);T(0,)
  }
  #mu ~ dnorm(0,.0001);T(0,)
  #tau ~ dgamma(1/2,1/2)
  lambda ~ dgamma(a,b)

  #alpha_star ~ dnorm(0,1/100)
}



jags_data <- list(y=cardio_clean$Observed_Deaths,
                  n=cardio_clean$Total_Procedures,
                  exp=cardio_clean$Expected_Mortality_Rate,
                  procedure_type = cardio_clean$Procedure_Type %>% str_sub(-1,-1) %>% as.numeric,
                  nprocedure_type = 5,
                  a = 1, b = 1)

jags_output <- R2jags::jags(model.file = model,data=jags_data,
                            parameters.to.save = c("p","alpha","lambda","mu","tau"),n.iter = 5000)

jags_output

jags_data2 <- list(y=cardio_clean$Observed_Deaths,
                  n=cardio_clean$Total_Procedures,
                  exp=cardio_clean$Expected_Mortality_Rate,
                  procedure_type = cardio_clean$Procedure_Type %>% str_sub(-1,-1) %>% as.numeric,
                  nprocedure_type = 5,
                  a = 0.1, b = 0.1)
jags_output2 <- R2jags::jags(model.file = model,data=jags_data2,
                            parameters.to.save = c("p","alpha","lambda","mu","tau"),n.iter = 5000)

model_normal <- function(){
  for(i in 1:length(y)){
    y[i] ~ dbin(p[i],n[i])
    p[i] ~ dbeta(exp[i]*alpha[procedure_type[i]],(1-exp[i])*alpha[procedure_type[i]]); T(1E-100,1)
  }

  for(i in 1:nprocedure_type){
    #alpha[i] ~ dunif(0,100)
    alpha[i] ~ dnorm(0, .001);T(0, )
    #alpha[i] ~ dgamma(10,10)
  }

  #alpha_star ~ dnorm(0,1/100)
}

jags_output_normal <- R2jags::jags(model.file = model_normal,data=jags_data,
                            parameters.to.save = c("p","alpha","lambda"),n.iter = 10000,n.thin = 5)

jags_output_normal

model_unif <- function(){
  for(i in 1:length(y)){
    y[i] ~ dbin(p[i],n[i])
    p[i] ~ dbeta(exp[i]*alpha[procedure_type[i]],(1-exp[i])*alpha[procedure_type[i]]); T(1E-100,1)
  }

  for(i in 1:nprocedure_type){
    alpha[i] ~ dunif(0,1000)
    #alpha[i] ~ dgamma(10,10)
  }

  #alpha_star ~ dnorm(0,1/100)
}

jags_output_unif <- R2jags::jags(model.file = model_unif,data=jags_data,
                                   parameters.to.save = c("p","alpha","lambda"),n.iter = 10000,n.thin = 5)

###########
### PPC ###
###########

pp_sim <- function(BUGSoutput){
  set.seed(28)
  I <- 1000
  reps <- matrix(NA, nrow = I, ncol = nrow(cardio_clean))
  for (i in 1:I){
    reps[i,] <-rbinom(n = nrow(cardio_clean),prob = BUGSoutput$mean$p,size = cardio_clean$Total_Procedures)
  }
  return(colMeans(reps))
}

sims <- list(jags_output$BUGSoutput, jags_output2$BUGSoutput, jags_output_normal$BUGSoutput,
             jags_output_unif$BUGSoutput) %>%
  lapply(pp_sim)

g1 <- ggplot() +
  geom_density(mapping = aes(x=sims[[1]]),fill = "red",alpha=.3) +
  geom_density(mapping = aes(x=cardio_clean$Observed_Deaths),fill = "blue",alpha=.3)+
  labs(title="Poisson-Gamma(1, 1)", x = "Deaths")+
  geom_vline(xintercept = max(sims[[1]]), col = "red",  alpha= 0.5)+
  geom_vline(xintercept = max(cardio_clean$Observed_Deaths), col = "blue", alpha= 0.5)

g2 <- ggplot() +
  geom_density(mapping = aes(x=sims[[2]]),fill = "red",alpha=.3) +
  geom_density(mapping = aes(x=cardio_clean$Observed_Deaths),fill = "blue",alpha=.3)+
  labs(title="Poisson-Gamma(0.1, 0.1)", x = "Deaths")+
  geom_vline(xintercept = max(sims[[2]]), col = "red",  alpha= 0.5)+
  geom_vline(xintercept = max(cardio_clean$Observed_Deaths), col = "blue",  alpha= 0.5)

g3 <- ggplot() +
  geom_density(mapping = aes(x=sims[[3]]),fill = "red",alpha=.3) +
  geom_density(mapping = aes(x=cardio_clean$Observed_Deaths),fill = "blue",alpha=.3)+
  labs(title="Half-Normal(0, 0.01)", x = "Deaths")+
  geom_vline(xintercept = max(sims[[3]]), col = "red",  alpha= 0.5)+
  geom_vline(xintercept = max(cardio_clean$Observed_Deaths), col = "blue",  alpha= 0.5)

g4 <- ggplot() +
  geom_density(mapping = aes(x=sims[[4]]),fill = "red",alpha=.3) +
  geom_density(mapping = aes(x=cardio_clean$Observed_Deaths),fill = "blue",alpha=.3)+
  labs(title="Uniform(0, 1000)", x = "Deaths")+
  geom_vline(xintercept = max(sims[[4]]), col = "red", alpha= 0.5)+
  geom_vline(xintercept = max(cardio_clean$Observed_Deaths), col = "blue", alpha = 0.5)

grid.arrange(g1, g2, g3, g4, nrow = 2, ncol = 2)

#############
### alpha ###
#############

alpha_names <- str_c("alpha[",1:5,"]")
make_summary <- function(table,digits=2){
  result <- rep(NA,nrow(table)*2)
  for(i in 1:nrow(table)){
    result[2*(i-1) + 1] <- table[i,1] %>% round(digits)
    result[2*(i-1) + 2] <- table[i,2] %>% round(digits) %>% {str_c("(",.,")")}
  }
  result
}

procedure_names <- lapply(1:5,function(m) c(m,"")) %>% unlist
cbind(procedure_names,
      jags_output$BUGSoutput$summary[alpha_names,c("mean","sd")] %>% make_summary(),
      jags_output$BUGSoutput$summary[alpha_names,c("mean","sd")] %>% make_summary(),
      jags_output_normal$BUGSoutput$summary[alpha_names,c("mean","sd")] %>% make_summary(),
      jags_output_unif$BUGSoutput$summary[alpha_names,c("mean","sd")] %>% make_summary()) %>%
  xtable::xtable() %>%
  xtable::print.xtable(include.rownames = F)


weights <- cardio_clean %>%
  group_by(Hospital_Name) %>%
  mutate(hosp_wts= Total_Procedures/ sum(Total_Procedures)) %>%
  ungroup() %>%
  group_by(Procedure_Type) %>% 
  mutate(proc_total = sum(Total_Procedures)) %>%
  ungroup() %>%
  mutate(overall_wts = proc_total / sum(Total_Procedures)) %>%
  dplyr::select(hosp_wts, overall_wts)

p_names <- str_c("p[",1:412,"]")
p_hosp_weighted <- t(jags_output$BUGSoutput$sims.matrix[,p_names] %*% diag(weights$hosp_wts)) 
p_overall_weighted <- t(jags_output$BUGSoutput$sims.matrix[,p_names] %*% diag(weights$overall_wts)) 

#using observed values in expectation
sims_oe <- 
  lapply(1:jags_output$BUGSoutput$n.sims,function(r){
    temp <- cardio_clean %>% 
      select(Hospital_Name,Observed_Deaths,Total_Procedures) %>% 
      mutate(p = jags_output$BUGSoutput$sims.matrix[r,p_names]) %>% 
      group_by(Hospital_Name) %>% 
      summarise(p_overall_hosp_wt = sum(p*Total_Procedures/sum(Total_Procedures)),
             oe_ratio = sum(Observed_Deaths)/sum(Total_Procedures)/p_overall_hosp_wt) %>%
      ungroup() %>%
      group_by(Procedure_Type) %>%
      mutate(proc_total = sum(Total_Procedures)) %>%
      ungroup() %>%
      mutate(overall_wts = proc_total / sum(Total_Procedures)) %>%
      summarise(p_overall_overall_wt = sum(p*overall_wts) )
    
    temp$oe_ratio
  }) %>% 
  do.call(what = "rbind")

sims_p <-
  lapply(1:jags_output$BUGSoutput$n.sims,function(r){
    temp <- cardio_clean %>% 
      dplyr::select(Hospital_Name,Observed_Deaths,Total_Procedures, Procedure_Type) %>% 
      mutate(p = jags_output$BUGSoutput$sims.matrix[r,p_names]) %>% 
      group_by(Hospital_Name) %>% 
      mutate(p_overall_hosp_wt = sum(p*Total_Procedures/sum(Total_Procedures))) %>%
      ungroup() %>%
      group_by(Procedure_Type) %>%
      mutate(proc_total = sum(Total_Procedures)) %>%
      ungroup() %>%
      mutate(overall_wts = proc_total / sum(Total_Procedures)) %>%
      mutate(p_overall_overall_wt = sum(p*overall_wts) )
    
    temp %>%
      dplyr::select(p_overall_hosp_wt, p_overall_overall_wt)
  }) %>% 
  do.call(what = "rbind")

#excluding observed values in expectation
types <- cardio_clean$Procedure_Type %>% str_sub(-1,-1) %>% as.numeric()
phi_means <- jags_output$BUGSoutput$mean$alpha[types]
nsims_prior <- 10000
p_prior_sims <- replicate(nsims_prior,rbeta(n=nrow(cardio_clean),
                                      shape1 = cardio_clean$Expected_Mortality_Rate*phi_means,
                                      shape2 = (1-cardio_clean$Expected_Mortality_Rate)*phi_means)) %>% 
  t

oe_prior_sims <- apply(p_prior_sims,1,function(r) cardio_clean$obs_mort_rate/r) %>% t
oe_prior_sims %>% colMeans()

sims_oe_prior <- 
  lapply(1:nsims_prior,function(r){
    temp <- cardio_clean %>% 
      select(Hospital_Name,Observed_Deaths,Total_Procedures) %>% 
      mutate(p = p_prior_sims[r,]) %>% 
      group_by(Hospital_Name) %>% 
      summarise(p_overall = sum(p*Total_Procedures/sum(Total_Procedures)),
                oe_ratio = sum(Observed_Deaths)/sum(Total_Procedures)/p_overall)
    
    temp$oe_ratio
  }) %>% 
  do.call(what = "rbind")
  
data.frame(Hospital = cardio_clean %>% group_by(Hospital_Name) %>% filter(row_number() == 1) %>% {.$Hospital_Name},
           mean = sims_oe %>% colMeans(),
           lb = sims_oe %>% apply(2,quantile,probs=.025),
           ub = sims_oe %>% apply(2,quantile,probs=.975)) %>% 
  mutate(star = case_when(lb>1 ~ 1,
                          ub<1 ~ 3,
                          TRUE ~ 2))


data.frame(Hospital = cardio_clean %>% group_by(Hospital_Name) %>% filter(row_number() == 1) %>% {.$Hospital_Name},
           p_hosp_wt_mean = sims_p %>% dplyr::select(p_overall_hosp_wt) %>% colMeans(),
           p_hosp_wt_lb = sims_p %>% dplyr::select(p_overall_hosp_wt)%>% apply(2,quantile,probs=.025),
           p_hosp_wt_ub = sims_p %>% dplyr::select(p_overall_hosp_wt) %>% apply(2,quantile,probs=.975)) 

li <- c(); ui <- c()
curr_hosp <- ""
count <- 1
for (i in 1:nrow(cardio_clean)){
  if (cardio_clean[i,]$Hospital_Name != curr_hosp){
    ui[count] <- i-1
    li[count] <- i
    curr_hosp <- cardio_clean[i,]$Hospital_Name
    count <- count + 1
  }
  if (i == nrow(cardio_clean)){
    ui[count] <- i
  }
}
ui <- ui[-1]
hosp_level <- cardio_clean %>%
  group_by(Hospital_Name) %>%
  mutate(total_obs = sum(Observed_Deaths), total_proc = sum(Total_Procedures)) %>%
  dplyr::select(id, Hospital_Name, total_obs, total_proc) %>%
  unique() 
n_hospitals <-nrow(hosp_level)

p_samps_hosp_wts <- oe_samps_hosp_wts <- matrix(NA, nrow = n_hospitals, ncol = ncol(p_hosp_weighted))
p_samps_overall_wts <- oe_samps_overall_wts <-  p_samps_hosp_wts 
for (i in 1:n_hospitals){
  p_samps_hosp_wts[i,] <- colSums(p_hosp_weighted[li[i]:ui[i],])
  #oe_samps_hosp_wts[i,] <- hosp_level$total_obs[i] /(overall_p_samps[i,] * hosp_level$total_proc[i])
  p_samps_overall_wts[i,] <- colSums(p_overall_weighted[li[i]:ui[i],])
}
p_samps_hosp_wts <- t(p_samps_hosp_wts) ; 
overall_oe_samps <- t(overall_oe_samps)
oe_post_mean <- colMeans(overall_oe_samps)
oe_post_lb <- apply(overall_oe_samps, 2, function(x){quantile(x, 0.025)})
oe_post_ub <- apply(overall_oe_samps, 2, function(x){quantile(x, 0.975)})

rating <- c()
for (i in 1:n_hospitals){
  if (oe_post_lb[i] > 1){
    rating[i] <- 1
  }
  else if (oe_post_ub[i] < 1){
    rating[i] <- 3
  }
  else{
    rating[i] <- 2
  }
}

ratings_table <- cbind(oe_post_mean, oe_post_lb, oe_post_ub, rating) %>%
  as.data.frame() %>%
  mutate(ID = hosp_level$id) %>%
  dplyr::select(ID, everything()) %>%
  arrange(desc(oe_post_mean)) %>%
  add_row(. , ID = 0)  
cbind(ratings_table[1:(nrow(ratings_table)/2),], 
      ratings_table[((nrow(ratings_table)/2) + 1):nrow(ratings_table),] ) %>%
  xtable::xtable() %>%
  print(include.rownames=FALSE)
#############
### Ranks ###
#############

get_rank_quantiles <- function(BUGSoutput,q){
  BUGSoutput$sims.matrix[,str_c("p[",1:412,"]")] %>% 
    t %>% 
    data.frame() %>% 
    split(base$Procedure_Type) %>% 
    lapply(function(d) apply(d,2,rank) %>% t %>% apply(2,quantile,probs=q)) %>% 
    {result <- c()
      for(i in 1:5){
        result <- c(result,.[[i]])
      }
      result
    } %>% 
    {.[order(names(.) %>% str_extract("[[:digit:]]+") %>% as.numeric)]}
}

get_rank_quantiles(jags_output$BUGSoutput,.975)


cardio_clean %>% 
  group_by(Procedure_Type) %>% 
  mutate(proc_total = sum(Total_Procedures)) %>%
  ungroup() %>%
  mutate(overall_wt = proc_total / sum(Total_Procedures)) %>%
  mutate()
  View()
  
ranking <- cardio_clean %>% 
  mutate(hospital_number = factor(Hospital_Name) %>% as.numeric()) %>% 
  mutate(exp_p = jags_output$BUGSoutput$mean$p,
         exp_var = (jags_output$BUGSoutput$sd$p)^2,
         #exp_p_2.5q = get_rank_quantiles(jags_output$BUGSoutput,.025),
         #exp_p_97.5q = get_rank_quantiles(jags_output$BUGSoutput,.975),
         exp_p_normal = jags_output_normal$BUGSoutput$mean$p,
         exp_p_unif = jags_output_unif$BUGSoutput$mean$p) %>% 
  group_by(Procedure_Type) %>% 
  mutate(procedure_rank = rank(exp_p), proc_total = sum(Total_Procedures)) %>% 
  ungroup() %>%
  mutate(overall_wt = proc_total / sum(Total_Procedures)) %>%
  group_by(Procedure_Type) %>%
  group_by(Hospital_Name) %>% 
  mutate(overall_p_hosp_wt = if_else(n() == 5,sum(exp_p*Total_Procedures/sum(Total_Procedures)),-1)) %>% 
  mutate(overall_p_overall_wt = if_else(n() == 5, sum(exp_p*overall_wt) , -1) ) %>%
  ungroup() %>%
  arrange(Procedure_Type,-exp_p) 

ranking %>% View

#bottom 5
#write("", "output/topn.tex", append=FALSE)
ranking %>% 
  group_by(Procedure_Type) %>% 
  top_n(3,procedure_rank) %>% 
  arrange(Procedure_Type,-exp_p) %>% 
  select(Hospital_Name,Total_Procedures,exp_p,procedure_rank,exp_p_2.5q,exp_p_97.5q) %>% 
  rename(N = Total_Procedures,`E[p]` = exp_p,Rank = procedure_rank,`Rank (2.5%)` = exp_p_2.5q,`Rank (97.5%)` = exp_p_97.5q) %>% 
  {split(.,.$Procedure_Type)} %>% 
  lapply(ungroup) %>% 
  do.call(what="rbind") %>% 
  mutate(Procedure_Type = str_sub(Procedure_Type,-1,-1)) %>% 
  rename(M = Procedure_Type) %>% 
  xtable::xtable(digits = c(0,0,0,0,3,0,0,0)) %>% 
  xtable::print.xtable(file = "output/worst_hospitals.tex",floating = F,append = F,include.rownames = F)
  # lapply(function(d) d %>% 
  #          select(-Procedure_Type) %>% 
  #          xtable::xtable(digits=0) %>% 
  #          xtable::print.xtable(file = "output/worst_hospitals.tex",floating = F,append = T))

# bottom 10 by hosp weights
ranking %>%
  group_by(Hospital_Name) %>%
  dplyr::select(id, Hospital_Name, overall_p_hosp_wt) %>%
  unique() %>%
  ungroup() %>%
  mutate(overall_rank = rank(overall_p_hosp_wt)) %>%
  top_n(10, overall_rank) %>%
  arrange(overall_rank)
# bottom 10 by overall weights
ranking %>%
  group_by(Hospital_Name) %>%
  dplyr::select(id, Hospital_Name, overall_p_overall_wt) %>%
  unique() %>%
  ungroup() %>%
  mutate(overall_rank = rank(overall_p_overall_wt)) %>%
  top_n(10, overall_rank) %>%
  arrange(overall_rank)

# top 10 by hosp weights
ranking %>%
  group_by(Hospital_Name) %>%
  dplyr::select(id, Hospital_Name, overall_p_hosp_wt) %>%
  unique() %>%
  filter(overall_p_hosp_wt != -1) %>%
  ungroup() %>%
  mutate(overall_rank = rank(overall_p_hosp_wt)) %>%
  top_n(10, -overall_rank) %>%
  arrange(overall_rank)

# top 10 by overall weights
ranking %>%
  group_by(Hospital_Name) %>%
  dplyr::select(id, Hospital_Name, overall_p_overall_wt) %>%
  unique() %>%
  filter(overall_p_overall_wt != -1) %>%
  ungroup() %>%
  mutate(overall_rank = rank(overall_p_overall_wt)) %>%
  top_n(10, -overall_rank) %>%
  arrange(overall_rank)

#top 5
ranking %>% 
  group_by(Procedure_Type) %>% 
  top_n(3,-procedure_rank) %>% 
  arrange(Procedure_Type,exp_p) %>% 
  select(Hospital_Name,Total_Procedures,exp_p,procedure_rank,exp_p_2.5q,exp_p_97.5q) %>% 
  rename(N = Total_Procedures,`E[p]` = exp_p,Rank = procedure_rank,`Rank (2.5%)` = exp_p_2.5q,`Rank (97.5%)` = exp_p_97.5q) %>% 
  {split(.,.$Procedure_Type)} %>% 
  lapply(ungroup) %>% 
  do.call(what="rbind") %>% 
  mutate(Procedure_Type = str_sub(Procedure_Type,-1,-1)) %>% 
  rename(M = Procedure_Type) %>% 
  xtable::xtable(digits = c(0,0,0,0,3,0,0,0)) %>% 
  xtable::print.xtable(file = "output/best_hospitals.tex",floating = F,append = F,include.rownames = F)

#O/E counts
ranking %>% 
  group_by(Hospital_Name) %>% 
  summarise(overall_p = sum(exp_p*Total_Procedures/sum(Total_Procedures)),
            overall_o = sum(Observed_Deaths)/sum(Total_Procedures),
            obs_oe = overall_o/overall_p) %>% 
  View


# lapply(function(d) d %>% 
#          select(-Procedure_Type) %>% 
#          xtable::xtable(digits=0) %>% 
#          xtable::print.xtable(file = "output/topn.tex",floating = F,append = T))


hospital_acronym <- function(str){
  if(str_length(str) > 9){
    str %>% {str_split(.," ")[[1]]} %>% str_sub(1,2) %>% str_c(collapse = "")
  } else{
    str
  }
}
hospital_acronym(cardio_clean$Hospital_Name[1])

cardio_clean$Hospital_Name[1] %>% {str_split(.," ")[[1]]}

cardio_clean$Hospital_Name %>%
  if_else(cardio_clean$Hospital_Name %>% str_length() %>% {.>8},)

#compute alternative metrics
ranking %>%
  mutate(exp_p_1 = (Observed_Deaths + Expected_Mortality_Rate)/(Total_Procedures + 1),
         N=Total_Procedures %>% as.character(),
         obs_mr = (obs_mort_rate*100) %>% round(2) %>% str_c("%"),
         exp_mr = (Expected_Mortality_Rate*100) %>% round(2) %>% str_c("%"),
         Type = str_sub(Procedure_Type,-1,-1),
         Hospital_Name = sapply(Hospital_Name,hospital_acronym)) %>%
  select(Type,Hospital_Name,N,obs_mr,exp_mr,starts_with("exp_p")) %>%
  group_by(Type) %>%
  mutate_at(vars(starts_with("exp_p")),rank,ties.method = "min") %>%
  arrange(Type,exp_p) %>%
  xtable::xtable() %>%
  xtable::print.xtable(include.rownames = F,tabular.environment = "longtable",floating = F,
                       file = "output/fullranking.tex")




