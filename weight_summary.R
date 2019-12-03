
data <- jags_output$BUGSoutput$sims.matrix[,8:419]

posterior_summary <- data.frame(Hospital_Name = cardio_clean$Hospital_Name,
                                id = cardio_clean$id,
                                Procedure_Type = cardio_clean$Procedure_Type,
                                post_mean = jags_output$BUGSoutput$mean$p,
                                q_0.025 = apply(data, 2, quantile, 0.025),
                                q_0.975 = apply(data, 2, quantile, 0.975),
                                Total_Procedures = cardio_clean$Total_Procedures,
                                Observed_Deaths = cardio_clean$Observed_Deaths,
                                Expected_Mortality_Rate = cardio_clean$Expected_Mortality_Rate)




marginals_by_cat <-posterior_summary %>%
  group_by(Procedure_Type) %>%
  summarize(n = sum(Total_Procedures)) %>%
  mutate(perc = n/sum(n)) %>%
  pull(perc)

posterior_summary %>%
  mutate(weight = case_when(Procedure_Type == "STAT Mortality Category 1" ~ marginals_by_cat[1],
                            Procedure_Type == "STAT Mortality Category 2" ~ marginals_by_cat[2],
                            Procedure_Type == "STAT Mortality Category 3" ~ marginals_by_cat[3],
                            Procedure_Type == "STAT Mortality Category 4" ~ marginals_by_cat[4],
                            Procedure_Type == "STAT Mortality Category 5" ~ marginals_by_cat[5])) %>%
  group_by(id) %>%
  mutate(post_oe_mean = Observed_Deaths/(Total_Procedures*post_mean),
         post_oe_025 = Observed_Deaths/(Total_Procedures*q_0.025),
         post_oe_975 = Observed_Deaths/(Total_Procedures*q_0.975)) %>%
  summarise(agg_oe_mean = weighted.mean(post_oe_mean, weight),
            agg_oe_02.5 = weighted.mean(post_oe_025, weight),
            agg_oe_97.5 = weighted.mean(post_oe_975, weight),
            agg_p_mean = weighted.mean(post_mean, weight),
            agg_p_02.5 = weighted.mean(q_0.025, weight),
            agg_p_97.5 = weighted.mean(q_0.975, weight)) %>%
  arrange(agg_p_mean) %>%
  save(file="posterior_summary.Rda")



load("posterior_summary.Rda")
