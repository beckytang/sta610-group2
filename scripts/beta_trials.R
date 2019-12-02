load("cardio_clean.Rda")
set.seed(196)
require(tidyverse)
library(gridExtra)

head(cardio_clean)

cardio_clean <- cardio_clean %>%
  mutate(obs_mort_rate = Observed_Deaths/Total_Procedures,
         obs_std = (obs_mort_rate - Expected_Mortality_Rate)/sqrt(Expected_Mortality_Rate*(1-Expected_Mortality_Rate)))

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
  lapply(., function(x){pp_sim(x)})

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
      jags_output2$BUGSoutput$summary[alpha_names,c("mean","sd")] %>% make_summary(),
      jags_output_normal$BUGSoutput$summary[alpha_names,c("mean","sd")] %>% make_summary(),
      jags_output_unif$BUGSoutput$summary[alpha_names,c("mean","sd")] %>% make_summary()) %>%
  xtable::xtable() %>%
  xtable::print.xtable(include.rownames = F)




#############
### Ranks ###
#############
p_names <- str_c("p[",1:length(n),"]")
df <- length(alpha_names)-1
ranking <- cardio_clean %>%
  mutate(exp_p = jags_output2$BUGSoutput$mean$p,
         exp_var = (jags_output2$BUGSoutput$sd$p)^2) %>%
  group_by(Procedure_Type) %>%
  mutate(procedure_rank = rank(exp_p)) %>%
  ungroup()%>%
  group_by(Hospital_Name) %>%
  mutate(overall_p = if_else(n() == 5,sum(exp_p*Total_Procedures/sum(Total_Procedures)),-1)) %>%
  mutate(overall_var = if_else(n() == 5,sum(exp_var * Total_Procedures^2)/(sum(Total_Procedures)^2),-1)) %>%
  ungroup() %>%
  mutate(overall_se = if_else(overall_p != -1, sqrt(overall_var / 5), -1) )%>%
  mutate(overall_lb = if_else(overall_p != -1, overall_p - qt(0.975, df)*overall_se,-1),
         overall_ub = if_else(overall_p != -1, overall_p + qt(0.975, df)*overall_se,-1))%>%
  arrange(Procedure_Type,-exp_p) %>%
  ungroup()

ranking %>% View

#bottom 5
ranking %>%
  group_by(Procedure_Type) %>%
  top_n(5,procedure_rank) %>%
  arrange(Procedure_Type,-exp_p) %>%
  {lapply(unique(.$Procedure_Type),function(p) .$Hospital_Name[.$Procedure_Type == p])} %>%
  do.call(what = "cbind") %>%
  xtable::xtable()

#top 5
ranking %>%
  group_by(Procedure_Type) %>%
  top_n(5,-procedure_rank) %>%
  arrange(Procedure_Type,-exp_p) %>%
  {lapply(unique(.$Procedure_Type),function(p) .$Hospital_Name[.$Procedure_Type == p])} %>%
  do.call(what = "cbind")


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
