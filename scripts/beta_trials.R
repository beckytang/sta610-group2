load("cardio_clean.Rda")
require(tidyverse)

head(cardio)

cardio_clean <- cardio_clean %>% 
  mutate(obs_mort_rate = Observed_Deaths/Total_Procedures,
         obs_std = (obs_mort_rate - Expected_Mortality_Rate`)/sqrt(Expected_Mortality_Rate`*(1-`Expected_Mortality_Rate`)))

cardio %>% 
  filter(`Procedure Type` %>% str_detect("5")) %>% 
  ggplot(aes(x=obs_mort_rate)) + 
  geom_histogram() + 
  geom_vline(xintercept = .583,color = "red")

cardio %>% 
  filter(`Procedure Type` %>% str_detect("4")) %>% 
  ggplot(aes(x=obs_mort_rate)) + 
  geom_histogram() + 
  geom_vline(xintercept = .136,color = "red")







cardio %>% 
  filter(`Procedure Type` %>% str_detect("5")) %>% 
  ggplot(aes(x=obs_mort_rate)) + 
  geom_histogram() + 
  geom_vline(xintercept = cardio_clean$obs_std[cardio_clean$Hospital_Name == "UNC" & str_detect(cardio_clean$Procedure_Type,5)],color = "red")



##beta prior
l5_data <- cardio_clean %>% filter(str_detect(Procedure_Type,"4"))

betap_scale <- 1
l5_data <- l5_data %>% 
  mutate(post_alpha = Observed_Deaths + Expected_Mortality_Rate,
         post_beta = Total_Procedures - Observed_Deaths + 1-Expected_Mortality_Rate,
         post_mean = (post_alpha)/(post_alpha + post_beta))


model <- function(){
  for(i in 1:length(y)){
    y[i] ~ dbin(p[i],n[i])
    p[i] ~ dbeta(exp[i]*alpha,(1-exp[i])*alpha)
  }
  
  alpha ~ dgamma(3,3)
  #alpha_star ~ dnorm(0,1/100)
}

jags_data <- list(y=l5_data$Observed_Deaths,
                  n=l5_data$Total_Procedures,
                  exp=l5_data$Expected_Mortality_Rate)

jags_output <- R2jags::jags(model.file = model,data=jags_data,
                            parameters.to.save = c("p","alpha"),n.iter = 5000)

jags_output

ranking <- l5_data$Hospital_Name[order(jags_output$BUGSoutput$mean$p)]
ranking

jags_output$BUGSoutput$sims.matrix[,"alpha"] %>% plot

pp_sim <- rbinom(n = 80,prob = jags_output$BUGSoutput$mean$p,size = l5_data$Total_Procedures)

ggplot() + 
  geom_density(mapping = aes(x=pp_sim),bins=20,fill = "red",alpha=.3) + 
  geom_density(mapping = aes(x=l5_data$Observed_Deaths),bins=20,fill = "blue",alpha=.3)



data.frame(rank_obs = l5_data$Hospital_Name[order(l5_data$Observed_Deaths/l5_data$Total_Procedures)],
           #rank_exp = l5_data$Hospital_Name[order(l5_data$Expected_Mortality_Rate)],
           rank_bayes = l5_data$Hospital_Name[order(jags_output$BUGSoutput$mean$p)]) %>% View



