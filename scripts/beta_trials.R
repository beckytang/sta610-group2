load("cardio_clean.Rda")
set.seed(196)
require(tidyverse)

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
    p[i] ~ dbeta(exp[i]*alpha[procedure_type[i]],(1-exp[i])*alpha[procedure_type[i]])
  }
  
  for(i in 1:nprocedure_type){
    alpha_star[i] ~ dpois(lambda)
    alpha[i] = alpha_star[i] + 1
    #alpha[i] ~ dnorm(mu,tau);T(0,)
  }
  #mu ~ dnorm(0,.0001);T(0,)
  #tau ~ dgamma(1/2,1/2)
  lambda ~ dgamma(.1,.1)
  
  #alpha_star ~ dnorm(0,1/100)
}



jags_data <- list(y=cardio_clean$Observed_Deaths,
                  n=cardio_clean$Total_Procedures,
                  exp=cardio_clean$Expected_Mortality_Rate,
                  procedure_type = cardio_clean$Procedure_Type %>% str_sub(-1,-1) %>% as.numeric,
                  nprocedure_type = 5)

jags_output <- R2jags::jags(model.file = model,data=jags_data,
                            parameters.to.save = c("p","alpha","lambda","mu","tau"),n.iter = 5000)

jags_output

model_normal <- function(){
  for(i in 1:length(y)){
    y[i] ~ dbin(p[i],n[i])
    p[i] ~ dbeta(exp[i]*alpha[procedure_type[i]],(1-exp[i])*alpha[procedure_type[i]])
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
    p[i] ~ dbeta(exp[i]*alpha[procedure_type[i]],(1-exp[i])*alpha[procedure_type[i]])
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

pp_sim <- function(){
  rbinom(n = nrow(cardio_clean),prob = jags_output$BUGSoutput$mean$p,size = cardio_clean$Total_Procedures)
}

ggplot() + 
  geom_density(mapping = aes(x=pp_sim()),bins=20,fill = "red",alpha=.3) + 
  geom_density(mapping = aes(x=cardio_clean$Observed_Deaths),bins=20,fill = "blue",alpha=.3)

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
      jags_output_normal$BUGSoutput$summary[alpha_names,c("mean","sd")] %>% make_summary(),
      jags_output_unif$BUGSoutput$summary[alpha_names,c("mean","sd")] %>% make_summary()) %>% 
  xtable::xtable() %>% 
  xtable::print.xtable(include.rownames = F)




#############
### Ranks ###
#############

ranking <- cardio_clean %>% 
  mutate(exp_p = jags_output$BUGSoutput$mean$p) %>% 
  group_by(Procedure_Type) %>% 
  mutate(procedure_rank = rank(exp_p)) %>% 
  group_by(Hospital_Name) %>% 
  mutate(overall_p = if_else(n() == 5,sum(exp_p*Total_Procedures/sum(Total_Procedures)),-1)) %>% 
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



#compute alternative metrics
ranking %>% 
  mutate(exp_p_phi1 = (Observed_Deaths + Expected_Mortality_Rate)/(Total_Procedures + 1)) %>% 
  select(Procedure_Type,Hospital_Name,exp_p,exp_p_phi1,obs_mort_rate) %>% 
  group_by(Procedure_Type) %>% 
  mutate_at(vars(-Procedure_Type,-Hospital_Name),rank,ties.method = "min") %>% 
  arrange(Procedure_Type,exp_p) %>% 
  View

data.frame(rank_obs = l5_data$Hospital_Name[order(l5_data$Observed_Deaths/l5_data$Total_Procedures)],
           #rank_exp = l5_data$Hospital_Name[order(l5_data$Expected_Mortality_Rate)],
           rank_bayes = l5_data$Hospital_Name[order(jags_output$BUGSoutput$mean$p)]) %>% View



