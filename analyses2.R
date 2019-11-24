#Temporary analyses script for Leadership across context and cultures paper
#Eventually this will be added to the leadershipdata package, or probably it's own package. 
# Good stack overflow on PCA with binary data
# https://stats.stackexchange.com/questions/16331/doing-principal-component-analysis-or-factor-analysis-on-binary-data/16335#16335
# https://cran.r-project.org/web/packages/logisticPCA/vignettes/logisticPCA.html

library(conflicted)

# Load data library -------------------------------------------------------
library(leadershipdata)

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(forcats)
library(NMF)
library(pvclust)
library(dendextend)
library(logisticPCA)
library(gridExtra)
library(car)
library(visreg)
library(effects)
library(lme4)
library(patchwork)
library(pvclust)
library(magrittr)
library(modelr)
library(ggridges)
library(RColorBrewer)


# Load precomputed objects ------------------------------------------------

# First run initialcompute.R, which takes ~90 minutes on my machine,
# and saves computed objects in Leader2.Rdata

# Then load computed objects

load("Leader2.Rdata") 

# Heatmaps -----------------------------------------------------------------
heatmap_data<-leader_text2[,c(quality_vars, "group.structure2")]

#Temporary, -1 and 1 coudl 0 out
heatmap_data<-heatmap_data[rowSums(heatmap_data[,c(quality_vars)]) > 0, ]

# Rowv  <-
#   t(as.matrix(heatmap_data)) %>%
#   dist %>% # Cluster rows (variables) by Spearman rank correlation
#   hclust(method = 'ward.D') %>%
#   as.dendrogram %>%
#   #rotate(order= colnames(data[vars])[order(data[vars][1,])]) %>%
#   set("branches_k_color", k = 4)
#
# Colv  <- heatmap_data %>%
#   dist %>% # Cluster columns (participants) by Euclidean metric
#   hclust(method = 'ward.D') %>%
#   as.dendrogram %>%
#   #rotate(order = rownames(data)[order(-data$Respect)]) %>%
#   set("branches_k_color", k = 3)

aheatmap(t(as.matrix(heatmap_data[c(quality_vars)])),
         width = 15, height = 10,
         #Rowv  = Rowv,
         #Colv = Colv,
         distfun = "euclidean",
         hclustfun = "ward",
         cellheight = 5,
         annCol = list(
           Group = heatmap_data$group.structure2),
         # annColors = list(
         #   ),
         treeheight = 50,
         filename = 'heatmap_qualities.pdf')

# PCA Qualities ---------------------------------------------------------

# For two PCs only; use optimal m
m_lpca_qualk2 <- logisticPCA(pca_data_qualities2, k = 2, m = which.min(qual_cvlpcak2), main_effects = T)
plot(m_lpca_qualk2, type = 'scores')
logisticPCA_loadings_plot(m_lpca_qualk2, data = pca_data_qualities2)

# Add PC scores to df
pca_data_qualities$PC1k2 <- m_lpca_qualk2$PCs[,1]
pca_data_qualities$PC2k2 <- m_lpca_qualk2$PCs[,2]

# For optimal k, m determined by cv above
logpca_model_qualities = logisticPCA(pca_data_qualities2, k = kq, m = mq, main_effects = T)
plot(logpca_model_qualities, type = "scores")
logisticPCA_loadings_plot(logpca_model_qualities, data = pca_data_qualities2)

pca_data_qualities$PC1 <- logpca_model_qualities$PCs[,1]
pca_data_qualities$PC2 <- logpca_model_qualities$PCs[,2]


# Indicate group structure of log PCA model
plot(logpca_model_qualities, type = "scores") + 
  geom_point(aes(colour=pca_data_qualities$group.structure2)) + 
  stat_ellipse(aes(colour=pca_data_qualities$group.structure2)) + 
  ggtitle("Logistic PCA") +
  scale_colour_brewer(palette = "Set1")

# Indicate subsistence stype of log PCA model
plot(logpca_model_qualities, type = "scores") + 
  geom_point(aes(colour=pca_data_qualities$subsistence)) + 
  ggtitle("Logistic PCA") +
  scale_colour_brewer(palette = "Set1")


# PCA Functions -----------------------------------------------------------

# Logistic PCA on leader functions

m_lpca_funk2 <- logisticPCA(pca_data_functions2, k = 2, m = which.min(m_lpca_funk2cv), main_effects = T)
plot(m_lpca_funk2, type = 'score')

pca_data_functions$PC1k2 <- m_lpca_funk2$PCs[,1]
pca_data_functions$PC2k2 <- m_lpca_funk2$PCs[,2]

# For optimal k, m
logpca_model_functions = logisticPCA(pca_data_functions2, k = kf, m = mf, main_effects = T)
plot(logpca_model_functions, type = 'score')
logisticPCA_loadings_plot(logpca_model_functions, data = pca_data_functions2)

pca_data_functions$PC1 <- logpca_model_functions$PCs[,1]
pca_data_functions$PC2 <- logpca_model_functions$PCs[,2]

# Indicate group structure of log PCA model
plot(logpca_model_functions, type = "scores") + 
  geom_point(aes(colour=pca_data_functions$group.structure2)) + 
  ggtitle("Logistic PCA") +
  scale_colour_brewer(palette = "Set1")

# Indicate subsistence stype of log PCA model
plot(logpca_model_functions, type = "scores") + 
  geom_point(aes(colour=pca_data_functions$subsistence)) + 
  ggtitle("Logistic PCA") +
  scale_colour_brewer(palette = "Set1")


# PCA Qualities & Functions -----------------------------------------------

logpca_model_qfk2 = logisticPCA(pca_data_FQ[c(function_vars, quality_vars)], k = 2, mqf = which.min(logpca_cv_qfk2), main_effects = T)
plot(logpca_model_qfk2, type = 'scores')
logisticPCA_loadings_plot(logpca_model_qfk2, data = pca_data_FQ[,c(function_vars, quality_vars)])

pca_data_FQ$PC1k2 <- logpca_model_qfk2$PCs[,1]
pca_data_FQ$PC2k2 <- logpca_model_qfk2$PCs[,2]

logpca_model_qf = logisticPCA(pca_data_FQ[c(function_vars, quality_vars)], k = kqf, m = which.min(logpca_cv_qf), main_effects = T)
plot(logpca_model_qf, type = "scores")
logisticPCA_loadings_plot(logpca_model_qf, data = pca_data_FQ[c(function_vars, quality_vars)])

pca_data_FQ$PC1 <- logpca_model_qf$PCs[,1]
pca_data_FQ$PC2 <- logpca_model_qf$PCs[,2]

# Indicate group structure of log PCA model
plot(logpca_model_qf, type = "scores") + 
  geom_point(aes(colour=pca_data_FQ$group.structure2)) + 
  ggtitle("Logistic PCA") +
  scale_colour_brewer(palette = "Set1")

# Indicate subsistence stype of log PCA model
plot(logpca_model_qf, type = "scores") + 
  geom_point(aes(colour=pca_data_FQ$subsistence)) + 
  ggtitle("Logistic PCA") +
  scale_colour_brewer(palette = "Set1")

# Components df ----------------------------------------------------

components_data<- left_join(pca_data_qualities, pca_data_functions, by = "cs_textrec_ID")
components_data <- left_join(components_data, pca_data_FQ, by = "cs_textrec_ID")

components_data <- components_data[,c("cs_textrec_ID","qualities_component1",
                                      "functions_component1", 'qf_component1', "subsistence")]

leader_text2<-left_join(leader_text2, 
                        components_data[,c("cs_textrec_ID", "qualities_component1", "functions_component1","qf_component1")],
                        by="cs_textrec_ID")


# Exploring components ----------------------------------------------------

agg_data<-leader_text2[leader_text2$subsistence=="agriculturalists",]
agg_lm<-lm(functions_component1~qualities_component1, data=agg_data)

hg_data<-leader_text2[leader_text2$subsistence=="hunter gatherers",]
hg_lm<-lm(functions_component1~qualities_component1, data=hg_data)

hort_data<-leader_text2[leader_text2$subsistence=="horticulturalists",]
hort_lm<-lm(functions_component1~qualities_component1, data=hort_data)

past_data<-leader_text2[leader_text2$subsistence=="pastoralists",]
past_lm<-lm(functions_component1~qualities_component1, data=past_data)

#Plotting qualities and functions components and by subsistence
ggplot(leader_text2, aes(qualities_component1, functions_component1))+
  geom_point(aes(colour=subsistence))+
  #geom_smooth(method='lm')+
  geom_abline(slope = agg_lm$coefficients[2], intercept = agg_lm$coefficients[1], color="red") +
  geom_abline(slope = hg_lm$coefficients[2], intercept = hg_lm$coefficients[1], color="green") +
  geom_abline(slope = hort_lm$coefficients[2], intercept = hort_lm$coefficients[1], color="darkolivegreen3") +
  geom_abline(slope = past_lm$coefficients[2], intercept = past_lm$coefficients[1], color="purple") +
  labs(x="\nQualities component: Prestige - Dominance", 
       y="Functions component: Mediation - Organization\n")

#Plotting qualities and functions components by group type
ggplot(leader_text2, aes(qualities_component1, functions_component1))+
  geom_point(aes(colour=group.structure2))+
  #geom_smooth(method='lm')+
  #geom_abline(slope = agg_lm$coefficients[2], intercept = agg_lm$coefficients[1], color="red") +
  #geom_abline(slope = hg_lm$coefficients[2], intercept = hg_lm$coefficients[1], color="green") +
  #geom_abline(slope = hort_lm$coefficients[2], intercept = hort_lm$coefficients[1], color="darkolivegreen3") +
  #geom_abline(slope = past_lm$coefficients[2], intercept = past_lm$coefficients[1], color="purple") +
  labs(x="\nQualities component: Prestige - Dominance", 
       y="Functions component: Mediation - Organization\n")

# Cultural complexity

ggplot(leader_text2, aes(c_cultural_complexity, qualities_component1))+
  geom_point(aes(colour=subsistence))




##### Q & F component
#Qualities and functions component by settlement fixity
ggplot(leader_text2, aes(settlement_fixity, qf_component1))+
  geom_violin()+
  geom_jitter(height = 0, width = 0.1) +
  geom_boxplot(width=.15)


#Qualities and functions component by community size
ggplot(leader_text2, aes(com_size, qf_component1))+
  geom_violin()+
  geom_jitter(height = 0, width = 0.1) +
  geom_boxplot(width=.15)

#Qualities and functions component by community size
ggplot(leader_text2, aes(pop_density, qf_component1))+
  geom_violin()+
  geom_jitter(height = 0, width = 0.1) +
  geom_boxplot(width=.15)

##### Qualities component

#Qualities component by settlement fixity
ggplot(leader_text2, aes(settlement_fixity, qualities_component1))+
  geom_violin(trim=F) +
  geom_jitter(height = 0, width = 0.1) +
  geom_boxplot(width=.15)


#Qualities component by community size
ggplot(leader_text2, aes(com_size, qualities_component1))+
  geom_violin(trim=F) +
  geom_jitter(height = 0, width = 0.1) +
  geom_boxplot(width=.15)

#Qualities component by pop density
ggplot(leader_text2, aes(pop_density, qualities_component1))+
  geom_violin(trim=F) +
  geom_jitter(height = 0, width = 0.1) +
  geom_boxplot(width=.15)

#Qualities component by subsistence
ggplot(leader_text2, aes(subsistence, qualities_component1))+
  geom_violin(trim=F) +
  geom_jitter(height = 0, width = 0.1) +
  geom_boxplot(width=.15)

#Qualities component by group sructure
ggplot(leader_text2, aes(group.structure2, qualities_component1))+
  geom_violin(trim=F) +
  geom_jitter(height = 0, width = 0.1) +
  geom_boxplot(width=.15)

##### Functions component

#Functions component by settlement fixity
ggplot(leader_text2, aes(settlement_fixity, functions_component1))+
  geom_violin(trim=F) +
  geom_jitter(height = 0, width = 0.1) +
  geom_boxplot(width=.15)


#Functions component by community size
ggplot(leader_text2, aes(com_size, functions_component1))+
  geom_violin(trim=F) +
  geom_jitter(height = 0, width = 0.1) +
  geom_boxplot(width=.15)

#Functions component by pop density
ggplot(leader_text2, aes(pop_density, functions_component1))+
  geom_violin(trim=F) +
  geom_jitter(height = 0, width = 0.1) +
  geom_boxplot(width=.15)

#Functions component by group structure
ggplot(leader_text2, aes(group.structure2, functions_component1))+
  geom_violin(trim=F) +
  geom_jitter(height = 0, width = 0.1) +
  geom_boxplot(width=.15)


# Exploratory models ------------------------------------------------------

# qc_m<-glm(qualities_component1 ~ c_cultural_complexity + com_size + subsistence, 
#          data=leader_text2, family="gaussian")
# summary(qc_m)
# Anova(qc_m)
# vif(qc_m)
# plot(qc_m)
# visreg(qc_m)
# plot(allEffects(qc_m))

leader_text2$com_size <- 
  ordered(
    leader_text2$com_size,
    levels = c("< 99", "100-199", "200-399", "400-999", "> 1,000")
    )

leader_text2$pop_density <-
  ordered(
    leader_text2$pop_density,
    levels = c(
      "1 or less person / 1-5 sq. mile", 
      "1-5 persons / sq. mile",
      "1-25 persons / sq. mile", 
      "26-100 persons / sq. mile",
      "101-500 persons / sq. mile",
      "over 500 persons / sq. mile"
      )
  )



# #Set com size2 contrast to cubic function
# leader_text2$com_size2<-leader_text2$com_size
# contrasts(leader_text2$com_size2, 1) <- contr.poly(5)[,3]
# 
# # Set pop density2 contrast to quadratic function
# leader_text2$pop_density2<-leader_text2$pop_density
# contrasts(leader_text2$pop_density2, 1) <- contr.poly(4)[,2]

qc_m2 <- lmer(
  PC2k2 ~ 
    #functions_component1 +
    subsistence +
    #c_cultural_complexity +
    # pop_density +
    # com_size +
    group.structure2 +
    # warfare_freq +
    (1|d_culture/doc_ID),
  data=pca_data_qualities
  )
summary(qc_m2)
Anova(qc_m2)
AIC(qc_m2)
vif(qc_m2)
visreg(qc_m2)


plot(allEffects(qc_m2))
# visreg(qc_m2, by = 'c_cultural_complexity', xvar = 'pop_density')
# visreg(qc_m2, by = 'group.structure2', xvar = 'warfare_freq')

# Model function PCs

fun_m2 <- lmer(
  PC2k2 ~ 
    #functions_component1 +
    subsistence +
    #c_cultural_complexity +
    # pop_density +
    # com_size +
    group.structure2 +
    # warfare_freq +
    (1|d_culture/doc_ID),
  data=pca_data_functions
)
summary(fun_m2)
Anova(fun_m2)
AIC(fun_m2)
vif(fun_m2)
visreg(fun_m2)


by_culture2 <-
  leader_text2 %>% 
  group_by(d_culture) %>%
  dplyr::select(d_culture,
                qualities_component1,
                functions_component1,
                one_of(
                  c(quality_vars,
                    function_vars,
                    leader_benefit_vars,
                    leader_cost_vars,
                    follower_benefit_vars,
                    follower_cost_vars))) %>%
  summarise_all(mean, na.rm=T) %>% 
  left_join(leader_cult[c('d_culture', 'c_name', 'region', 'subsistence')]) %>% 
  mutate(
    c_name = fct_reorder(c_name, qualities_component1, mean)
  ) %>% 
  ggplot(aes(qualities_component1, c_name)) + geom_point()
by_culture2

# Leader functions

fc_m2 <- lmer(
  functions_component1 ~ 
    #qualities_component1 +
    subsistence +
    #c_cultural_complexity +
    #pop_density +
    #com_size +
    group.structure2 +
    # warfare_freq +
    (1|d_culture/doc_ID),
  data=leader_text2
)
summary(fc_m2)
Anova(fc_m2)
AIC(fc_m2)
visreg(fc_m2)
#visreg(fc_m2, xvar = 'warfare_freq', by = 'group.structure2')

# Predict top functions

m <- glmer(
  qualities.knowlageable.intellect ~
    subsistence +
    c_cultural_complexity +
    # pop_density +
    # com_size +
    # group.structure2 +
    # warfare_freq +
    (1|d_culture/doc_ID),
  family = binomial,
  # control = glmerControl(optimizer = c("Nelder_Mead")),
  data=leader_text2
)
summary(m)
Anova(m)
visreg(m, scale = 'response')

## Rstanarm models

options(mc.cores = parallel::detectCores())

# Qualities model

# qc_stan_m <- stan_glmer(
#   qualities_component1 ~ 
#     subsistence +
#     c_cultural_complexity +
#     pop_density +
#     com_size +
#     group.structure2 +
#     (1|d_culture/doc_ID),
#   prior = normal(0, 1),
#   prior_intercept = normal(0, 1),
#   algorithm = c("sampling"),
#   family = gaussian(link = "identity"),
#   iter=4000,
#   data=leader_text2
# )
# 
# prior_summary(qc_stan_m)
# summary(qc_stan_m,
#         pars = c(),
#         probs = c(0.025, 0.975),
#         digits = 2)
# 
# median(bayes_R2(qc_stan_m))
# 
# qc_m_post <- as.matrix(qc_stan_m)
# prior_summary(qc_stan_m)
# 
# # Community size and pop density distributions
# mcmc_areas(qc_m_post[,c(7:10)], prob_outer = .95)
# 
# # Group structure distritions
# 
# mcmc_areas(qc_m_post[,c(7:14)], 
#            prob_outer = .95,
#            point_est = "mean"
#            )
# 
# # All effects
# mcmc_intervals(qc_m_post[,c(2:20)])
# 
# launch_shinystan(qc_stan_m)
# #mcmc_trace(qc_m_post[,c(1:5)])
# 
# # ggpubr plots
# qc_m_post_df<-data.frame(qc_m_post)
# ggdensity(qc_m_post_df, x="c_cultural_complexity", fill = "lightgrey",
#           add = "mean", rug=T)
# 
# 
# # tidybase plots
# qc_m_post_df_plot<-qc_m_post_df[,c(2:14)]
# qc_m_post_df_plot_long<-gather(qc_m_post_df_plot, variable, value)
# 
# theme_set(theme_tidybayes())
# qc_bayes_model_post_plot <- qc_m_post_df_plot_long %>% 
#   ggplot(aes(x=value, y=variable))+
#   geom_halfeyeh(trim = TRUE)+
#   vline_0() +
#   xlim(-10,10) +
#   scale_y_discrete(labels=rev(c("Subsistence:Pastoralists",
#                             "Subsistence:Mixed",
#                             "Subsistence:Hunter-gatherers",
#                             "Subsistence:Horticulturalists",
#                             "Population density",
#                             "Group:State-level",
#                             "Group:Social",
#                             "Group:Religeous",
#                             "Group:Political",
#                             "Group:Other",
#                             "Goupr:Military",
#                             "Community size",
#                             "Cultural complexity")))+
#   labs(x="\nPosterior distribution", y="Covariate\n")
#   
# 
# qc_bayes_model_post_plot
# 
# #Full tidybayes method
# 
# ggplot(qc_bayes_model_post_plot, aes(y = variable, x = value)) +
#   geom_halfeyeh()
# 
# 
# 
# # Functions model
# fc_stan_m <- stan_glmer(
#   functions_component1 ~ 
#     subsistence +
#     c_cultural_complexity +
#     pop_density2 +
#     com_size2 +
#     group.structure2 +
#     (1|d_culture/doc_ID),
#   prior_intercept = normal(0,1),
#   prior = normal(0,1),
#   algorithm = c("sampling"),
#   family = gaussian(link = "identity"),
#   iter=4000,
#   data=leader_text2
# )
# 
# summary(fc_stan_m)
# median(bayes_R2(fc_stan_m))
# 
# fc_m_post <- as.matrix(fc_stan_m)
# 
# # Community size distributions
# mcmc_areas(fc_m_post[,c(14:17)], prob_outer = .95)
# 
# # Group structure distributions
# mcmc_areas(fc_m_post[,c(14:19)], prob_outer = .95)
# 
# mcmc_intervals(fc_m_post[,c(2:14)])
# #launch_shinystan(fc_stan_m)
# 
# # Stan models
# 



# Heatmaps -----------------------------------------------------------------
heatmap_data<-leader_text2[,c(quality_vars, "group.structure2")]

#Temporary, -1 and 1 coudl 0 out
heatmap_data<-heatmap_data[rowSums(heatmap_data[,c(quality_vars)]) > 0, ]

# Rowv  <-
#   t(as.matrix(heatmap_data)) %>%
#   dist %>% # Cluster rows (variables) by Spearman rank correlation
#   hclust(method = 'ward.D') %>%
#   as.dendrogram %>%
#   #rotate(order= colnames(data[vars])[order(data[vars][1,])]) %>%
#   set("branches_k_color", k = 4)
#
# Colv  <- heatmap_data %>%
#   dist %>% # Cluster columns (participants) by Euclidean metric
#   hclust(method = 'ward.D') %>%
#   as.dendrogram %>%
#   #rotate(order = rownames(data)[order(-data$Respect)]) %>%
#   set("branches_k_color", k = 3)

aheatmap(t(as.matrix(heatmap_data[,c(quality_vars)])),
         width = 15, height = 10,
         #Rowv  = Rowv,
         #Colv = Colv,
         distfun = "euclidean",
         hclustfun = "ward",
         cellheight = 5,
         annCol = list(
           Group = heatmap_data$group.structure2),
         # annColors = list(
         #   ),
         treeheight = 50,
         filename = 'heatmap_qualities.pdf')