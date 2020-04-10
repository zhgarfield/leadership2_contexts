library(rstanarm)
library(bayesplot)
options(mc.cores = parallel::detectCores())

# stan_m_coauthor <- stan_glm(
#   female_leader_present2 ~
#     female_coauthor,
#   family = binomial(link = "logit"),
#   data = leader_text3,
#   prior_intercept = student_t(df=7,location=0,10),
#   prior = normal(0,1),
#   chains = 4,
#   iter = 40000)
#  
# summary(stan_m_coauthor, pars = "(Intercept)", "female_coauthorTRUE")
# posterior_stan_m_coauthor <- as.matrix(stan_m_coauthor)
# mcmc_areas(posterior_stan_m_coauthor,
#            pars = c("female_coauthorTRUE"),
#            prob = 0.95)
# 
# stan_mm_coauthor <- stan_glmer(
#   female_leader_present2 ~
#     female_coauthor +
#   (1|document_d_ID),
#   family = binomial(link = "logit"),
#   data = leader_text3,
#   prior_intercept = student_t(df=7,location=0),
#   prior = normal(0,1),
#   prior_covariance = decov(reg. = 1, conc. = 1, shape = 1, scale = 1),
#   chains = 4,
#   iter = 40000)
# 
# summary(stan_mm_coauthor, pars = "(Intercept)", "female_coauthorTRUE")
# posterior_stan_mm_coauthor <- as.matrix(stan_mm_coauthor)
# mcmc_areas(posterior_stan_mm_coauthor,
#            pars = c("female_coauthorTRUE"),
#            prob = 0.95)
# 
# prior_summary(stan_mm_coauthor)
# 
# 
# ## Adjusting covariance prior
# ### goal here was to replicate lme4 results with stan_glmer. Adusting prior on covariance matrix, via shape does it
# stan_mm_coauthor2 <- stan_glmer(
#   female_leader_present2 ~
#     female_coauthor +
#     (1|document_d_ID),
#   family = binomial(link = "logit"),
#   data = leader_text3,
#   prior_intercept = student_t(df=7,location=0),
#   prior = normal(0,1),
#   prior_covariance = decov(regularization = 1, 
#                            concentration = 1,
#                            shape = 19, 
#                            scale = 1),
#   chains = 4,
#   iter = 40000)
# 
# summary(stan_mm_coauthor2, pars = "(Intercept)", "female_coauthorTRUE")
# prior_summary(stan_mm_coauthor2)
# 
# posterior_stan_mm_coauthor2 <- as.matrix(stan_mm_coauthor2)
# mcmc_areas(posterior_stan_mm_coauthor2,
#            pars = c("female_coauthorTRUE"),
#            prob = 0.90)
# 
# library(brms)
# 
# prior1 <- c(set_prior("normal(0,1)", class = "b", coef = "female_coauthorTRUE"),
#             set_prior("student_t(7, 0, 10)", class = "Intercept"),
#             set_prior("lkj(regularization = 1, scale = 10, df = 1, autoscale = TRUE)", 
#                       class = "sd", group = "document_d_ID")
#             )
# 
#  
# bmr1 <- brm(as.numeric(female_leader_present2) ~
#               female_coauthor +
#               (1|document_d_ID),
#             bernoulli(link = "logit"),
#             prior = prior1,
#             warmup = 2000, iter = 4000,
#             data = leader_text3)

# summary(bmr1)
# prior_summary(bmr1)

# stan_mm_pubyear <- stan_glmer(
#   female_leader_present2 ~
#     `d_publication date`  +
#     (1|document_d_ID),
#   family = binomial(link = "logit"),
#   data = leader_text4,
#   prior_intercept = normal(0,1),
#   prior = normal(0,1), 
#   chains = 4, 
#   iter = 40000)
# 
# summary(stan_mm_pubyear, pars = "`d_publication date`")
# posterior_stan_mm_coauthor <- as.matrix(stan_mm_pubyear)
# mcmc_areas(posterior_stan_mm_coauthor,
#            pars = c("`d_publication date`"),
#            prob = 0.95)

## Both gender and publication year in one model
# ***********************
#  USE THIS MODEL????
# ***********************

# stan_mm_pubyear_gender <- stan_glmer(
#   female_leader_present2 ~
#     female_coauthor +
#     #(1|`d_publication date`)  +
#     (1|document_d_ID),
#   family = binomial(link = "logit"),
#   data = leader_text4,
#   prior_intercept = student_t(1,0,1),
#   prior = normal(0,1),
#   chains = 4,
#   iter = 20000)
# 
# summary(stan_mm_pubyear_gender, pars = c("female_coauthorTRUE", "(Intercept)"))
# posterior_stan_mm_coauthor_pubyear <- as.matrix(stan_mm_pubyear_gender)
# mcmc_areas(posterior_stan_mm_coauthor_pubyear,
#            pars = c("female_coauthorTRUE"),
#            prob = 0.95)

#launch_shinystan(stan_mm_pubyear_gender)

# ## Rstanarm models
# 
# options(mc.cores = parallel::detectCores())
# 
# # Qualities model
# 
# # qc_stan_m <- stan_glmer(
# #   qualities_component1 ~ 
# #     subsistence +
# #     c_cultural_complexity +
# #     pop_density +
# #     com_size +
# #     group.structure2 +
# #     (1|d_culture/doc_ID),
# #   prior = normal(0, 1),
# #   prior_intercept = normal(0, 1),
# #   algorithm = c("sampling"),
# #   family = gaussian(link = "identity"),
# #   iter=4000,
# #   data=leader_text2
# # )
# # 
# # prior_summary(qc_stan_m)
# # summary(qc_stan_m,
# #         pars = c(),
# #         probs = c(0.025, 0.975),
# #         digits = 2)
# # 
# # median(bayes_R2(qc_stan_m))
# # 
# # qc_m_post <- as.matrix(qc_stan_m)
# # prior_summary(qc_stan_m)
# # 
# # # Community size and pop density distributions
# # mcmc_areas(qc_m_post[,c(7:10)], prob_outer = .95)
# # 
# # # Group structure distritions
# # 
# # mcmc_areas(qc_m_post[,c(7:14)], 
# #            prob_outer = .95,
# #            point_est = "mean"
# #            )
# # 
# # # All effects
# # mcmc_intervals(qc_m_post[,c(2:20)])
# # 
# # launch_shinystan(qc_stan_m)
# # #mcmc_trace(qc_m_post[,c(1:5)])
# # 
# # # ggpubr plots
# # qc_m_post_df<-data.frame(qc_m_post)
# # ggdensity(qc_m_post_df, x="c_cultural_complexity", fill = "lightgrey",
# #           add = "mean", rug=T)
# # 
# # 
# # # tidybase plots
# # qc_m_post_df_plot<-qc_m_post_df[,c(2:14)]
# # qc_m_post_df_plot_long<-gather(qc_m_post_df_plot, variable, value)
# # 
# # theme_set(theme_tidybayes())
# # qc_bayes_model_post_plot <- qc_m_post_df_plot_long %>% 
# #   ggplot(aes(x=value, y=variable))+
# #   geom_halfeyeh(trim = TRUE)+
# #   vline_0() +
# #   xlim(-10,10) +
# #   scale_y_discrete(labels=rev(c("Subsistence:Pastoralists",
# #                             "Subsistence:Mixed",
# #                             "Subsistence:Hunter-gatherers",
# #                             "Subsistence:Horticulturalists",
# #                             "Population density",
# #                             "Group:State-level",
# #                             "Group:Social",
# #                             "Group:Religeous",
# #                             "Group:Political",
# #                             "Group:Other",
# #                             "Goupr:Military",
# #                             "Community size",
# #                             "Cultural complexity")))+
# #   labs(x="\nPosterior distribution", y="Covariate\n")
# #   
# # 
# # qc_bayes_model_post_plot
# # 
# # #Full tidybayes method
# # 
# # ggplot(qc_bayes_model_post_plot, aes(y = variable, x = value)) +
# #   geom_halfeyeh()
# # 
# # 
# # 
# # # Functions model
# # fc_stan_m <- stan_glmer(
# #   functions_component1 ~ 
# #     subsistence +
# #     c_cultural_complexity +
# #     pop_density2 +
# #     com_size2 +
# #     group.structure2 +
# #     (1|d_culture/doc_ID),
# #   prior_intercept = normal(0,1),
# #   prior = normal(0,1),
# #   algorithm = c("sampling"),
# #   family = gaussian(link = "identity"),
# #   iter=4000,
# #   data=leader_text2
# # )
# # 
# # summary(fc_stan_m)
# # median(bayes_R2(fc_stan_m))
# # 
# # fc_m_post <- as.matrix(fc_stan_m)
# # 
# # # Community size distributions
# # mcmc_areas(fc_m_post[,c(14:17)], prob_outer = .95)
# # 
# # # Group structure distributions
# # mcmc_areas(fc_m_post[,c(14:19)], prob_outer = .95)
# # 
# # mcmc_intervals(fc_m_post[,c(2:14)])
# # #launch_shinystan(fc_stan_m)
# # 
# # # Stan models
# # 
# 
# 
# 