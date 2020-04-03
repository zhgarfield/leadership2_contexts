#Temporary analyses script for Leadership across context and cultures paper
#Eventually this will be added to the leadershipdata package, or probably it's own package. 
# Good stack overflow on PCA with binary data
# https://stats.stackexchange.com/questions/16331/doing-principal-component-analysis-or-factor-analysis-on-binary-data/16335#16335
# https://cran.r-project.org/web/packages/logisticPCA/vignettes/logisticPCA.html

# library(conflicted)
# conflict_prefer("which", "base")

# Load data library -------------------------------------------------------
library(leadershipdata)

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(tidytext)
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
library(ggmosaic)
library(proxy)
# Load precomputed objects ------------------------------------------------

# First run initialcompute.R, which takes ~90 minutes on my machine,
# and saves computed objects in Leader2.Rdata

# Then load computed objects

load("Leader2.Rdata")

#+ fig.height=15, fig.width=15

# Functions ---------------------------------------------------------------

# loadings plot
logisticPCA_loadings_plot <- function(m, data){
  df <- data.frame(m$U)
  df$variable <- names(data)
  p1 <-
    ggplot(df, aes(X1, fct_reorder(variable, X1), colour=X1)) +
    ggalt::geom_lollipop(horizontal = T, size = 1, show.legend = FALSE) +
    scale_color_gradient2(low = 'red', mid = 'white', 'high' = 'blue', name = 'Loading') +
    theme_bw(15) +
    labs(title = "PC 1", x = "\nLoading", y = "")
  p2<-
    ggplot(df, aes(X2, fct_reorder(variable, X2), colour=X2)) +
    ggalt::geom_lollipop(horizontal = T, size = 1, show.legend = FALSE) +
    scale_color_gradient2(low = 'red', mid = 'white', 'high' = 'blue', name = 'Loading') +
    theme_bw(15) +
    labs(title = "PC 2", x = "\nLoading", y = "")
  p1 + p2
}


# Recode variables --------------------------------------------------------

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

all_study_vars <- c(function_vars, quality_vars, leader_benefit_vars, leader_cost_vars, follower_benefit_vars, follower_cost_vars)

# Variable support plots --------------------------------------------------

# Functions & qualities

plot.variable.support = ggplot(d_melt, aes(value, Variable, xmin=y_negse, xmax=y_se, colour=Type, shape=Type)) + 
  geom_errorbarh() + 
  geom_point() +
  scale_x_continuous(breaks=seq(0,1,.1), labels=scales::percent, limits=c(0,1)) +
  scale_colour_discrete(name='', labels=c('Cultures', 'Text records')) +
  facet_grid(Model~., scales = "free_y", space = "free_y") +
  theme(strip.text.y = element_text(angle=0)) +
  scale_shape_manual(name="", values=c(17,16), labels=c('Cultures', 'Text records'))+
  scale_fill_manual(name="", values=c("red", "blue"), labels=c('Cultures', 'Text records')) +
  labs(x="",y="") +
  theme_bw(15) +
  theme(strip.text.y = element_text(angle=0))
plot.variable.support

# Leader/follower benefits and costs

format_label <- function(lbl){
  relabel <- c(
    leader.benefits_fitness = 'Inclusive fitness benefit',
    leader.benefits_mating = 'Mating benefit',
    leader.benefits_other = 'Misc. non-material benefit',
    leader.benefits_reduced.risk.harm.conflict = 'Reduced risk of harm',
    leader.benefits_resource_food = 'Food benefit',
    leader.benefits_resource_other = 'Material resources',
    leader.benefits_social.services = 'Social services',
    leader.benefits_social.status.reputation = 'Increased social status',
    leader.benefits_territory = 'Territory',
    follower.benefits_fitness = 'Inclusive fitness benefit',
    follower.benefits_mating = 'Mating benefit',
    follower.benefits_other = 'Misc. non-material benefit',
    follower.benefits_reduced.risk.harm.conflict = 'Reduced risk of harm',
    follower.benefits_resource_food = 'Food benefit',
    follower.benefits_resource_other = 'Material resources',
    follower.benefits_social.services = 'Social services',
    follower.benefits_social.status.reputation = 'Increased social status',
    follower.benefits_territory = 'Territory',

    leader.costs_fitness.costs = 'Inclusive fitness cost',
    leader.costs_increased.risk.harm.conflict = 'Increased risk of harm',
    leader.costs_other = 'Misc. non-material cost',
    leader.costs_resource_food.cost = 'Food cost',
    leader.costs_resources_other.cost = 'Loss of material resources',
    leader.costs_social.status = 'Reduced social status',
    leader.costs_territory.cost = 'Loss of territory',
    leader.costs.mating.cost = 'Mating cost',
    leader.costs.social.services = 'Loss of social services',
    follower.costs_fitness = 'Inclusive fitness cost',
    follower.costs_increased.risk.harm.conflict = 'Increased risk of harm',
    follower.costs_mating = 'Mating cost',
    follower.costs_other = 'Misc. non-material cost',
    follower.costs_resource_food = 'Food cost',
    follower.costs_resource_other = 'Loss of material resources',
    follower.costs_social.services = 'Loss of social services',
    follower.costs_social.status = 'Reduced social status',
    follower.costs_territory = 'Loss of territory'
  )
  return(relabel[lbl])
}

plot.variable.support_costs_benefits = ggplot(d_melt_cb, aes(value, Variable, xmin=y_negse, xmax=y_se, colour=Type, shape=Type)) + 
  geom_errorbarh() + 
  geom_point() +
  scale_x_continuous(breaks=seq(0,1,.1), labels=scales::percent, limits=c(0, 0.8)) +
  scale_y_discrete(labels = format_label) +
  scale_colour_discrete(name='', labels=c('Cultures', 'Text records')) +
  facet_grid(Model~., scales = "free_y", space = "free_y") +
  scale_shape_manual(name="", values=c(17,16), labels=c('Cultures', 'Text records'))+
  scale_fill_manual(name="", values=c("red", "blue"), labels=c('Cultures', 'Text records'))+
  labs(x="",y="") +
  theme_bw(15) +
  theme(strip.text.y = element_text(angle=0))
plot.variable.support_costs_benefits

# Cluster plots -----------------------------------------------------------

pdf(file = "Figures/m_pvclust_qual.pdf", width=12, height=8)
plot(m_pvclust_qual)
pvrect(m_pvclust_qual, alpha = 0.9)
dev.off()

# pdf(file = "Figures/m_pvclust_qual_jaccard.pdf", width=12, height=8)
# plot(m_pvclust_qual_jaccard)
# pvrect(m_pvclust_qual_jaccard, alpha = 0.9)
# dev.off()

pdf(file = "Figures/m_pvclust_fun.pdf", width=12, height=8)
plot(m_pvclust_fun)
pvrect(m_pvclust_fun, alpha = 0.9)
dev.off()

# pdf(file = "Figures/m_pvclust_fun_jaccard.pdf", width=12, height=8)
# plot(m_pvclust_fun_jaccard)
# pvrect(m_pvclust_fun_jaccard, alpha = 0.9)
# dev.off()

# plot(qual_func_clust)
# pvrect(qual_func_clust, alpha = 0.9)

# PCA Qualities ---------------------------------------------------------

# For two PCs only; use optimal m
# m_lpca_qualk2 <- logisticPCA(pca_data_qualities2, k = 2, m = which.min(qual_cvlpcak2), main_effects = T)
# plot(m_lpca_qualk2, type = 'scores')
# logisticPCA_loadings_plot(m_lpca_qualk2, data = pca_data_qualities2)
# 
# # Add PC scores to df
# pca_data_qualities$qPC1k2 <- m_lpca_qualk2$PCs[,1]
# pca_data_qualities$qPC2k2 <- m_lpca_qualk2$PCs[,2]
# leader_text2 <- left_join(leader_text2, pca_data_qualities[c('cs_textrec_ID', 'qPC1k2', 'qPC2k2')])
# 
# # For optimal k, m determined by cv in initialcompute.R
# kq <- 8
# mq <- 12
# 
# logpca_model_qualities = logisticPCA(pca_data_qualities2, k = kq, m = mq, main_effects = T)
# plot(logpca_model_qualities, type = "scores")
# logisticPCA_loadings_plot(logpca_model_qualities, data = pca_data_qualities2)
# 
# pca_data_qualities$qPC1 <- logpca_model_qualities$PCs[,1]
# pca_data_qualities$qPC2 <- logpca_model_qualities$PCs[,2]
# leader_text2 <- left_join(leader_text2, pca_data_qualities[c('cs_textrec_ID', 'qPC1', 'qPC2')])
# 
# # Indicate group structure of log PCA model
# plot(logpca_model_qualities, type = "scores") + 
#   geom_point(aes(colour=pca_data_qualities$group.structure2)) + 
#   stat_ellipse(aes(colour=pca_data_qualities$group.structure2)) + 
#   ggtitle("Logistic PCA") +
#   scale_colour_brewer(palette = "Set1")
# 
# # Indicate subsistence stype of log PCA model
# plot(logpca_model_qualities, type = "scores") + 
#   geom_point(aes(colour=pca_data_qualities$subsistence)) + 
#   ggtitle("Logistic PCA") +
#   scale_colour_brewer(palette = "Set1")
# 
# # PCA Functions -----------------------------------------------------------
# 
# # Logistic PCA on leader functions
# 
# m_lpca_funk2 <- logisticPCA(pca_data_functions2, k = 2, m = which.min(m_lpca_funk2cv), main_effects = T)
# plot(m_lpca_funk2, type = 'score')
# logisticPCA_loadings_plot(m_lpca_funk2, data = pca_data_functions2)
# 
# pca_data_functions$fPC1k2 <- m_lpca_funk2$PCs[,1]
# pca_data_functions$fPC2k2 <- m_lpca_funk2$PCs[,2]
# leader_text2 <- left_join(leader_text2, pca_data_functions[c('cs_textrec_ID', 'fPC1k2', 'fPC2k2')])
# 
# # For optimal k, m
# kf <- 10 # elbow
# mf <- which.min(fun_cvlpca[kf,])
# 
# logpca_model_functions = logisticPCA(pca_data_functions2, k = kf, m = mf, main_effects = T)
# plot(logpca_model_functions, type = 'score')
# logisticPCA_loadings_plot(logpca_model_functions, data = pca_data_functions2)
# 
# pca_data_functions$fPC1 <- logpca_model_functions$PCs[,1]
# pca_data_functions$fPC2 <- logpca_model_functions$PCs[,2]
# leader_text2 <- left_join(leader_text2, pca_data_functions[c('cs_textrec_ID', 'fPC1', 'fPC2')])
# 
# # Indicate group structure of log PCA model
# plot(logpca_model_functions, type = "scores") + 
#   geom_point(aes(colour=pca_data_functions$group.structure2)) + 
#   ggtitle("Logistic PCA") +
#   scale_colour_brewer(palette = "Set1")
# 
# # Indicate subsistence stype of log PCA model
# plot(logpca_model_functions, type = "scores") + 
#   geom_point(aes(colour=pca_data_functions$subsistence)) + 
#   ggtitle("Logistic PCA") +
#   scale_colour_brewer(palette = "Set1")
# 
# # PCA Qualities & Functions -----------------------------------------------
# 
# logpca_model_qfk2 = logisticPCA(pca_data_FQ[c(function_vars, quality_vars)], k = 2, m = which.min(logpca_cv_qfk2), main_effects = T)
# plot(logpca_model_qfk2, type = 'scores')
# logisticPCA_loadings_plot(logpca_model_qfk2, data = pca_data_FQ[,c(function_vars, quality_vars)])
# # 
# # pca_data_FQ$fqPC1k2 <- logpca_model_qfk2$PCs[,1]
# # pca_data_FQ$fqPC2k2 <- logpca_model_qfk2$PCs[,2]
# # leader_text2 <- left_join(leader_text2, pca_data_FQ[c('cs_textrec_ID', 'fqPC1k2', 'fqPC2k2')])
# # 
# logpca_model_qf = logisticPCA(pca_data_FQ[c(function_vars, quality_vars)], k = kqf, m = mqf, main_effects = T)
# # plot(logpca_model_qf, type = "scores")
# logisticPCA_loadings_plot(logpca_model_qf, data = pca_data_FQ[c(function_vars, quality_vars)])
# # 
# pca_data_FQ$fqPC1 <- logpca_model_qf$PCs[,1]
# pca_data_FQ$fqPC2 <- logpca_model_qf$PCs[,2]
# leader_text2 <- left_join(leader_text2, pca_data_FQ[c('cs_textrec_ID', 'fqPC1', 'fqPC2')])
# 
# # Indicate group structure of log PCA model
# plot(logpca_model_qf, type = "scores") + 
#   geom_point(aes(colour=pca_data_FQ$group.structure2)) + 
#   ggtitle("Logistic PCA") +
#   scale_colour_brewer(palette = "Set1")
# 
# # Indicate subsistence stype of log PCA model
# plot(logpca_model_qf, type = "scores") + 
#   geom_point(aes(colour=pca_data_FQ$subsistence)) + 
#   ggtitle("Logistic PCA") +
#   scale_colour_brewer(palette = "Set1")


##### Q & F component
#Qualities and functions component by settlement fixity
# ggplot(leader_text2, aes(settlement_fixity, qf_component1))+
#   geom_violin()+
#   geom_jitter(height = 0, width = 0.1) +
#   geom_boxplot(width=.15)
# 
# 
# #Qualities and functions component by community size
# ggplot(leader_text2, aes(com_size, qf_component1))+
#   geom_violin()+
#   geom_jitter(height = 0, width = 0.1) +
#   geom_boxplot(width=.15)
# 
# #Qualities and functions component by community size
# ggplot(leader_text2, aes(pop_density, qf_component1))+
#   geom_violin()+
#   geom_jitter(height = 0, width = 0.1) +
#   geom_boxplot(width=.15)
# 
# ##### Qualities component
# 
# #Qualities component by settlement fixity
# ggplot(leader_text2, aes(settlement_fixity, qualities_component1))+
#   geom_violin(trim=F) +
#   geom_jitter(height = 0, width = 0.1) +
#   geom_boxplot(width=.15)
# 
# 
# #Qualities component by community size
# ggplot(leader_text2, aes(com_size, qualities_component1))+
#   geom_violin(trim=F) +
#   geom_jitter(height = 0, width = 0.1) +
#   geom_boxplot(width=.15)
# 
# #Qualities component by pop density
# ggplot(leader_text2, aes(pop_density, qualities_component1))+
#   geom_violin(trim=F) +
#   geom_jitter(height = 0, width = 0.1) +
#   geom_boxplot(width=.15)
# 
# #Qualities component by subsistence
# ggplot(leader_text2, aes(subsistence, qualities_component1))+
#   geom_violin(trim=F) +
#   geom_jitter(height = 0, width = 0.1) +
#   geom_boxplot(width=.15)
# 
# #Qualities component by group sructure
# ggplot(leader_text2, aes(group.structure2, qualities_component1))+
#   geom_violin(trim=F) +
#   geom_jitter(height = 0, width = 0.1) +
#   geom_boxplot(width=.15)
# 
# ##### Functions component
# 
# #Functions component by settlement fixity
# ggplot(leader_text2, aes(settlement_fixity, functions_component1))+
#   geom_violin(trim=F) +
#   geom_jitter(height = 0, width = 0.1) +
#   geom_boxplot(width=.15)
# 
# 
# #Functions component by community size
# ggplot(leader_text2, aes(com_size, functions_component1))+
#   geom_violin(trim=F) +
#   geom_jitter(height = 0, width = 0.1) +
#   geom_boxplot(width=.15)
# 
# #Functions component by pop density
# ggplot(leader_text2, aes(pop_density, functions_component1))+
#   geom_violin(trim=F) +
#   geom_jitter(height = 0, width = 0.1) +
#   geom_boxplot(width=.15)
# 
# #Functions component by group structure
# ggplot(leader_text2, aes(group.structure2, functions_component1))+
#   geom_violin(trim=F) +
#   geom_jitter(height = 0, width = 0.1) +
#   geom_boxplot(width=.15)
# 
# 
# # Exploratory models ------------------------------------------------------

# mqPC1 <- lmer(
#   qPC1 ~ 
#     subsistence + 
#     c_cultural_complexity +
#     com_size +
#     (1|d_culture/doc_ID), 
#   leader_text2
#   )
# summary(mqPC1)
# Anova(mqPC1)
# nobs(mqPC1)
# 
# mqPC1k2 <- lmer(
#   qPC1k2 ~ 
#     subsistence + 
#     c_cultural_complexity +
#     com_size +
#     (1|d_culture/doc_ID), 
#   leader_text2
# )
# summary(mqPC1k2)
# Anova(mqPC1k2)
# nobs(mqPC1k2)

# # qc_m<-glm(qualities_component1 ~ c_cultural_complexity + com_size + subsistence, 
# #          data=leader_text2, family="gaussian")
# # summary(qc_m)
# # Anova(qc_m)
# # vif(qc_m)
# # plot(qc_m)
# # visreg(qc_m)
# # plot(allEffects(qc_m))

# # #Set com size2 contrast to cubic function
# # leader_text2$com_size2<-leader_text2$com_size
# # contrasts(leader_text2$com_size2, 1) <- contr.poly(5)[,3]
# # 
# # # Set pop density2 contrast to quadratic function
# # leader_text2$pop_density2<-leader_text2$pop_density
# # contrasts(leader_text2$pop_density2, 1) <- contr.poly(4)[,2]
# 
# qc_m2 <- lmer(
#   PC2k2 ~ 
#     #functions_component1 +
#     subsistence +
#     #c_cultural_complexity +
#     # pop_density +
#     # com_size +
#     group.structure2 +
#     # warfare_freq +
#     (1|d_culture/doc_ID),
#   data=pca_data_qualities
#   )
# summary(qc_m2)
# Anova(qc_m2)
# AIC(qc_m2)
# vif(qc_m2)
# visreg(qc_m2)
# 
# 
# plot(allEffects(qc_m2))
# # visreg(qc_m2, by = 'c_cultural_complexity', xvar = 'pop_density')
# # visreg(qc_m2, by = 'group.structure2', xvar = 'warfare_freq')
# 
# # Model function PCs
# 
# fun_m2 <- lmer(
#   PC2k2 ~ 
#     #functions_component1 +
#     subsistence +
#     #c_cultural_complexity +
#     # pop_density +
#     # com_size +
#     group.structure2 +
#     # warfare_freq +
#     (1|d_culture/doc_ID),
#   data=pca_data_functions
# )
# summary(fun_m2)
# Anova(fun_m2)
# AIC(fun_m2)
# vif(fun_m2)
# visreg(fun_m2)
# 
# 
# by_culture2 <-
#   leader_text2 %>% 
#   group_by(d_culture) %>%
#   dplyr::select(d_culture,
#                 qPC1, qPC2,
#                 fPC1, fPC2,
#                 one_of(
#                   c(quality_vars,
#                     function_vars,
#                     leader_benefit_vars,
#                     leader_cost_vars,
#                     follower_benefit_vars,
#                     follower_cost_vars))) %>%
#   summarise_all(mean, na.rm=T) %>% 
#   left_join(leader_cult[c('d_culture', 'c_name', 'region', 'subsistence')]) 
# 
# p_culture_qPC1 <- by_culture2 %>%
#   mutate(
#     c_name = fct_reorder(c_name, qPC1)
#   ) %>% 
#   ggplot(aes(qPC1, c_name)) + geom_point()
# p_culture_qPC1
# 
# p_culture_qPC1_fPC1 <- by_culture2 %>% 
#   ggplot(aes(qPC1, fPC2, colour = subsistence)) + geom_point() + stat_ellipse()
# p_culture_qPC1_fPC1
# 
# # Leader functions

# m_fPC1 <- lmer(
#   fPC1 ~
#     #qualities_component1 +
#     subsistence +
#     #c_cultural_complexity +
#     #pop_density +
#     #com_size +
#     group.structure2 +
#     # warfare_freq +
#     (1|d_culture/doc_ID),
#   data=leader_text2
# )
# summary(m_fPC1)
# Anova(m_fPC1)
# AIC(m_fPC1)
# 
# 
# m_fPC1_sub_plot <- visreg(m_fPC1, "subsistence", type = "contrast", gg = T) +
#   labs(y = "Functions PC 1: \nSocial functions\n",
#        x = "\nSubsistence type")
# m_fPC1_group_plot <- visreg(m_fPC1, "group.structure2", type = "contrast", gg = T) + 
#   labs(y = "Functions PC 1: \nSocial functions\n",
#        x = "\nGroup type")
# 
# m_fPC1_sub_plot + m_fPC1_group_plot
# 
# 
# m_fPC2 <- lmer(
#   fPC2 ~
#     #qualities_component1 +
#     subsistence +
#     #c_cultural_complexity +
#     #pop_density +
#     #com_size +
#     group.structure2 +
#     # warfare_freq +
#     (1|d_culture/doc_ID),
#   data=leader_text2
# )
# summary(m_fPC2)
# Anova(m_fPC2)
# AIC(m_fPC2)
# 
# m_fPC2_sub_plot <- visreg(m_fPC2, "subsistence", type = "conditional", gg = T) +
#   labs(y = "Functions PC 2: \nOrganization vs. Mediation\n",
#        x = "\nSubsistence type")
# m_fPC2_group_plot <- visreg(m_fPC2, "group.structure2", type = "conditional", gg = T) +
#   labs(y = "Functions PC 2: \nOrganization vs. Mediation\n",
#        x = "\nGroup type")
# 
# m_fPC2_sub_plot + m_fPC2_group_plot
# 
# # Leader qualities
# 
# m_qPC1 <- lmer(
#   qPC1 ~
#     #qualities_component1 +
#     subsistence +
#     #c_cultural_complexity +
#     #pop_density +
#     #com_size +
#     group.structure2 +
#     # warfare_freq +
#     (1|d_culture/doc_ID),
#   data=leader_text2
# )
# summary(m_qPC1)
# Anova(m_qPC1)
# 
# m_qPC1_sub_plot <- visreg(m_qPC1, "subsistence", type = "contrast", gg = T) +
#   labs(y = "Qualities PC 1: \nPrestige vs. Dominance\n",
#        x = "\nSubsistence type")
# m_qPC1_group_plot <- visreg(m_qPC1, "group.structure2", type = "contrast", gg = T) +
#   labs(y = "Qualities PC 1: \nPrestige vs. Dominance\n",
#        x = "\nGroup type")
# 
# m_qPC1_sub_plot + m_qPC1_group_plot
# 
# m_qPC2 <- lmer(
#   qPC2 ~
#     #qualities_component1 +
#     subsistence +
#     #c_cultural_complexity +
#     #pop_density +
#     #com_size +
#     group.structure2 +
#     # warfare_freq +
#     (1|d_culture/doc_ID),
#   data=leader_text2
# )
# summary(m_qPC2)
# Anova(m_qPC2)
# 
# m_qPC2_sub_plot <- visreg(m_qPC2, "subsistence", type = "contrast", gg = T) + 
#   labs(y = "Qualities PC 2: \nIn-group favoritism vs. Social Status\n",
#        x = "\nSubsistence type")
# m_qPC2_group_plot <- visreg(m_qPC2, "group.structure2", type = "contrast", gg = T) +
#   labs(y = "Qualities PC 2: \nIn-group favoritism vs. Social Status\n",
#        x = "\nGroup type")
# 
# m_qPC2_sub_plot + m_qPC2_group_plot
# 
# 
# 
# # TMP models
# 
# tmp <- glmer(
#   function_resolve.conflcit ~
#     #qualities_component1 +
#     subsistence +
#     #c_cultural_complexity +
#     #pop_density +
#     #com_size +
#     group.structure2 +
#     # warfare_freq +
#     (1|d_culture/doc_ID),
#   family = binomial(link = "logit"),
#   data=leader_text2
# )
# summary(tmp)
# Anova(tmp)
# visreg(tmp)

# #visreg(m_fPC1, xvar = 'warfare_freq', by = 'group.structure2')
# 
# # Predict top functions
# 
# m <- glmer(
#   qualities.knowlageable.intellect ~
#     subsistence +
#     c_cultural_complexity +
#     # pop_density +
#     # com_size +
#     # group.structure2 +
#     # warfare_freq +
#     (1|d_culture/doc_ID),
#   family = binomial,
#   # control = glmerControl(optimizer = c("Nelder_Mead")),
#   data=leader_text2
# )
# summary(m)
# Anova(m)
# visreg(m, scale = 'response')
# 
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
# Heatmaps -----------------------------------------------------------------

# Heatmap of qualities with Euclidean distance

# heatmap(
#   t(as.matrix(pca_data_qualities2)),
#   hclustfun = function(x) hclust(x, method = 'ward.D'),
#   scale = 'none'
#   )
# 
# # aheatmap version
# 
# aheatmap(
#   t(as.matrix(pca_data_qualities2)),
#   distfun = "euclidean", 
#   hclustfun = "ward",
#   scale = "none",
#   filename = "Figures/heatmap_qualities_euc.pdf"
# )
# 
# aheatmap(
#   t(as.matrix(pca_data_qualities2)),
#   Rowv = c(distfun='correlation', hclustfun='ward'),
#   Colv = c(distfun='binary', hclustfun='ward'),
#   scale = "none",
#   filename = "Figures/heatmap_qualities_bin_cor.pdf"
# )
# 
# aheatmap(
#   t(as.matrix(pca_data_functions2)),
#   Rowv = c(distfun='correlation', hclustfun='ward'),
#   Colv = c(distfun='euclidean', hclustfun='ward'),
#   scale = "none",
#   filename = "Figures/heatmap_functions_bin_cor.pdf"
# )
# # # Heatmap of qualities with cor & binary distance
# 
# heatmap(
#   t(as.matrix(pca_data_qualities2)),
#   hclustfun = function(x) hclust(x, method = 'ward.D'),
#   distfun = function(x) proxy::dist(x, method = 'binary'),
#   scale = 'none'
#   )

# Heatmap of functions with Euclidean distance

# heatmap(
#   t(as.matrix(pca_data_functions2)),
#   hclustfun = function(x) hclust(x, method = 'ward.D'),
#   scale = 'none',
#   main = "Functions with Euclidean distance"
# )
# 
# # Heatmap of functions with cor distance
# heatmap(
#   t(as.matrix(pca_data_functions2)),
#   hclustfun = function(x) hclust(x, method = 'ward.D'),
#   distfun = function(x) proxy::dist(x, method = 'correlation'),
#   scale = 'none',
#   main = "Functions with Correlation distance"
# )

# heatmap_data<-leader_text2[,c(quality_vars, "group.structure2")]
# 
# #Temporary, -1 and 1 coudl 0 out
# heatmap_data<-heatmap_data[rowSums(heatmap_data[,c(quality_vars)]) > 0, ]
# 
# # Rowv  <-
# #   t(as.matrix(heatmap_data)) %>%
# #   dist %>% # Cluster rows (variables) by Spearman rank correlation
# #   hclust(method = 'ward.D') %>%
# #   as.dendrogram %>%
# #   #rotate(order= colnames(data[vars])[order(data[vars][1,])]) %>%
# #   set("branches_k_color", k = 4)
# #
# # Colv  <- heatmap_data %>%
# #   dist %>% # Cluster columns (participants) by Euclidean metric
# #   hclust(method = 'ward.D') %>%
# #   as.dendrogram %>%
# #   #rotate(order = rownames(data)[order(-data$Respect)]) %>%
# #   set("branches_k_color", k = 3)
# 
# aheatmap(t(as.matrix(heatmap_data[c(quality_vars)])),
#          width = 15, height = 10,
#          #Rowv  = Rowv,
#          #Colv = Colv,
#          distfun = "euclidean",
#          hclustfun = "ward",
#          cellheight = 5,
#          annCol = list(
#            Group = heatmap_data$group.structure2),
#          # annColors = list(
#          #   ),
#          treeheight = 50,
#          filename = 'heatmap_qualities.pdf')
# 
# heatmap_data<-leader_text2[,c(quality_vars, "group.structure2")]
# 
# #Temporary, -1 and 1 coudl 0 out
# heatmap_data<-heatmap_data[rowSums(heatmap_data[,c(quality_vars)]) > 0, ]
# 
# # Rowv  <-
# #   t(as.matrix(heatmap_data)) %>%
# #   dist %>% # Cluster rows (variables) by Spearman rank correlation
# #   hclust(method = 'ward.D') %>%
# #   as.dendrogram %>%
# #   #rotate(order= colnames(data[vars])[order(data[vars][1,])]) %>%
# #   set("branches_k_color", k = 4)
# #
# # Colv  <- heatmap_data %>%
# #   dist %>% # Cluster columns (participants) by Euclidean metric
# #   hclust(method = 'ward.D') %>%
# #   as.dendrogram %>%
# #   #rotate(order = rownames(data)[order(-data$Respect)]) %>%
# #   set("branches_k_color", k = 3)
# 
# aheatmap(t(as.matrix(heatmap_data[,c(quality_vars)])),
#          width = 15, height = 10,
#          #Rowv  = Rowv,
#          #Colv = Colv,
#          distfun = "euclidean",
#          hclustfun = "ward",
#          cellheight = 5,
#          annCol = list(
#            Group = heatmap_data$group.structure2),
#          # annColors = list(
#          #   ),
#          treeheight = 50,
#          filename = 'heatmap_qualities.pdf')
# 
# 


# NMF ---------------------------------------------------------------------

# library(NMF)
# m_nmf <- nmf(t(pca_data_qualities2), rank = 2:15)
# m_nmf5 <- nmf(t(pca_data_qualities2), rank = 5)
# m_nmfrandom <- nmf(randomize(t(pca_data_qualities2)), rank=2:15)

# pdf(file = 'consensusmap.pdf', width = 20)
# consensusmap(m_nmf)
# dev.off()

# Group structure by subsistence --------------------------------------------------

df_groups <- 
  leader_text2 %>% 
  dplyr::select(
    group.structure2,
    demo_sex,
    subsistence
  ) %>% 
  dplyr::filter(group.structure2 != 'other') %>% 
  mutate(
    demo_sex = factor(demo_sex, levels = c('male', 'female')),
    group =   factor(
      group.structure2,
      levels = c(
        'residential subgroup',
        'kin group',
        'economic group',
        'religious group',
        'military group',
        'political group (community)',
        'political group (supracommunity)'
      )
    ),
    subsistence = factor(
      subsistence,
      levels = c("hunter gatherers",
                 "pastoralists",
                 "mixed",
                 "horticulturalists",
                 "agriculturalists"
                 )
      )
)

plot_group_subsis <-
  ggplot(df_groups) +
  geom_mosaic(aes(x = product(group, subsistence), fill = group)) +
  labs(x="", y="", fill = "Group type") +
  guides(fill = guide_legend(reverse = T)) +
  theme_bw(15) 

# Leave off for now. Might not need this.
# + 
#   theme(axis.text.x = element_text(size=16),
#         axis.text.y = element_text(size=16)) +

# This code screws up the plot somehow
#   scale_y_productlist(labels = c("Residential subgroup",
#                                  "Kin group",
#                                  "Economic group",
#                                  "Political group\n(community)",
#                                  "Political group\n(supracommunity)",
#                                  "Military group",
#                                  "Religious group")) +
#   scale_x_productlist(labels = c("Hunter-gatherers",
#                                  "Pastoralists",
#                                  "Mixed",
#                                  "Horticulturalists",
#                                  "Agriculturalists")) + 
#   scale_fill_discrete(labels = c("Residential subgroup",
#                                   "Kin group",
#                                   "Economic group",
#                                   "Political group (community)",
#                                   "Political group (supracommunity)",
#                                   "Military group",
#                                   "Religious group"))

plot_group_subsis 


df_group_sex <- 
  df_groups %>% 
  dplyr::filter(
    demo_sex != 'both', demo_sex != 'unknown'
  )

plot_group_sex <- 
  ggplot(df_group_sex) + 
  geom_mosaic(aes(x = product(group, demo_sex), fill = group)) +
  # scale_fill_viridis_d(option = 'B') +
  labs(x="", y="", fill = "Group type") +
  guides(fill = guide_legend(reverse = T)) +
  theme_bw(15) + theme(axis.text.y=element_blank())
plot_group_sex


# Costs and benefits by group structure type ------------------------------
se <- function(x) sd(x)/sqrt(length(x))

# try to tidy data to: Group structure, Benefit type, Cost type, Status type (leader, follower), Mean, SE

# Group by group structure and calculate mean and SE
cb_grp <- leader_text2 %>% 
  group_by(group.structure2) %>% 
  select(contains(c("benefits", "costs"))) %>%
  summarise_each(funs(mean, se)) %>% 
  pivot_longer(cols=contains(c("benefits", "costs"))) # couldn't figure out how to pivot_longer on three vars

# label means and SE to split the data frame
cb_grp$value_type <- gsub("^.*\\_","",cb_grp$name)

#Create dataframe of means and fix names
cb_grp_means <- data.frame(split(cb_grp, cb_grp$value_type)[1])
cb_grp_means$name <- str_sub(cb_grp_means$mean.name, end=-6)
cb_grp_means$group.structure2 <- cb_grp_means$mean.group.structure2
cb_grp_means <- cb_grp_means[,c("mean.value","name","group.structure2")]

#Create dataframe of SEs and fix names
cb_grp_se <- data.frame(split(cb_grp, cb_grp$value_type)[2])
cb_grp_se$name <- str_sub(cb_grp_se$se.name, end=-4)
cb_grp_se$group.structure2 <- cb_grp_se$se.group.structure2
cb_grp_se <- cb_grp_se[,c("se.value","name","group.structure2")]

#Merge mean and SE data back together               
cb_grp_all <- full_join(cb_grp_means, cb_grp_se, by = c("name", "group.structure2"))

# Create status and type variables
cb_grp_all$status <- str_extract(cb_grp_all$name, ".+?(?=\\.)")
cb_grp_all$type <- str_match(cb_grp_all$name, "\\.(.*?)_")




# Compute values -----------------------------------------------------------

group_sex_tbl <- xtabs(~demo_sex+group.structure2, leader_text2)
female_residential_pct <- signif(group_sex_tbl['female', 'residential subgroup']/sum(leader_text$demo_sex == 'female'), 3)*100
male_residential_pct <- signif(group_sex_tbl['male', 'residential subgroup']/sum(leader_text$demo_sex == 'male'), 3)*100

group_sub_tbl <- xtabs(~subsistence+group.structure2, leader_text2)
hg_residential_pct <- signif(group_sub_tbl['hunter gatherers', 'residential subgroup']/sum(leader_text$subsistence == 'hunter gatherers'), 3)*100
hg_kin_pct <- signif(group_sub_tbl['hunter gatherers', 'kin group']/sum(leader_text$subsistence == 'hunter gatherers'), 3)*100
hort_kin_pct <- signif(group_sub_tbl['horticulturalists', 'kin group']/sum(leader_text$subsistence == 'horticulturalists'), 3)*100

final_record_count <- sum(rowSums(leader_text2[all_study_vars])>0)

male_leader_pct <- signif(100*sum(leader_text2$demo_sex=='male', na.rm=T)/nrow(leader_text2), 3)
female_leader_pct <- signif(100*sum(leader_text2$demo_sex=='female', na.rm=T)/nrow(leader_text2), 2)

intelltxts <- sum(leader_text2$qualities.knowlageable.intellect)
polytxts <- sum(leader_text2$qualities.polygynous)
statustxts <- sum(leader_text2$qualities_high.status)
intellpolytxts <- sum(leader_text2$qualities.polygynous & leader_text2$qualities.knowlageable.intellect)
statuspolytxts <- sum(leader_text2$qualities.polygynous & leader_text2$qualities_high.status)

# text analysis
# leader_text has 1000 rows
# need raw texts for all 1212 rows

textstats <- text_records %>% 
  dplyr::select(cs_textrec_ID, raw_text) %>% 
  unnest_tokens(word, raw_text) %>% 
  # dplyr::filter(is.na(as.numeric(word))) %>% # filters out numbers, some of which are page numbers
  group_by(cs_textrec_ID) %>% 
  summarise(count = n()) %>% 
  summarise(min = min(count), max = max(count), mean = mean(count), median = median(count), sd = sd(count)) %>% 
  round(1)

movement_pct <- signif(d_melt$value[d_melt$Variable == "Movement/migration" & d_melt$Type == "Cultures"], 3)*100
moral_pct <- signif(d_melt$value[d_melt$Variable == "Moral authority" & d_melt$Type == "Cultures"], 3)*100
fairness_pct <- signif(d_melt$value[d_melt$Variable == "Fairness" & d_melt$Type == "Cultures"], 3)*100
feared_pct <- signif(d_melt$value[d_melt$Variable == "Feared" & d_melt$Type == "Cultures"], 3)*100
attractive_pct <- signif(d_melt$value[d_melt$Variable == "Attractive" & d_melt$Type == "Cultures"], 3)*100

# # Violin plots of PCs by group vars ---------------------------------------
# 
# 
# ggplot(pca_data_qualities, aes(group.structure2, qPC1))+
#   geom_violin()+
#   geom_jitter(height = 0, width = 0.1) +
#   geom_boxplot(width=.15)
# 
# ggplot(pca_data_qualities, aes(group.structure2, qPC2))+
#   geom_violin()+
#   geom_jitter(height = 0, width = 0.1) +
#   geom_boxplot(width=.15)
# 
# ggplot(pca_data_qualities, aes(subsistence, qPC1))+
#   geom_violin()+
#   geom_jitter(height = 0, width = 0.1) +
#   geom_boxplot(width=.15)
# 
# ggplot(pca_data_qualities, aes(subsistence, qPC2))+
#   geom_violin()+
#   geom_jitter(height = 0, width = 0.1) +
#   geom_boxplot(width=.15)
# 
# ggplot(pca_data_functions, aes(group.structure2, fPC1))+
#   geom_violin()+
#   geom_jitter(height = 0, width = 0.1) +
#   geom_boxplot(width=.15)
# 
# ggplot(pca_data_functions, aes(subsistence, fPC1))+
#   geom_violin()+
#   geom_jitter(height = 0, width = 0.1) +
#   geom_boxplot(width=.15)
# 
# ggplot(pca_data_functions, aes(subsistence, fPC2))+
#   geom_violin()+
#   geom_jitter(height = 0, width = 0.1) +
#   geom_boxplot(width=.15)
# 
# ggplot(pca_data_functions, aes(group.structure2, fPC2))+
#   geom_violin()+
#   geom_jitter(height = 0, width = 0.1) +
#   geom_boxplot(width=.15)


# Female leaders by ethnographer gender -----------------------------------

# leader_text2$demo_sex
# authorship
# text_records

female_coauthor <- function(cs_textrec_ID){
  document_ID <- text_records$document_d_ID[text_records$cs_textrec_ID == cs_textrec_ID]
  author_genders <- authorship$author_gender[authorship$document_ID == document_ID]
  'female' %in% author_genders
}

leader_text2$female_coauthor <- sapply(leader_text2$cs_textrec_ID, female_coauthor)

leader_text3 <- 
  leader_text2 %>% 
  dplyr::select(
    cs_textrec_ID,
    demo_sex,
    female_coauthor
  ) %>% 
  mutate(
    female_leader_present = case_when(
      demo_sex == 'unknown' ~ 'unknown',
      demo_sex == 'male' ~ 'no',
      TRUE ~ 'yes'
    ),
    female_leader_present2 = female_leader_present == 'yes'
  ) %>% 
  dplyr::filter(
    demo_sex != 'unknown'
  ) %>% 
  left_join(
    text_records[c("document_d_ID", "cs_textrec_ID")]
  ) 

# Assuming each record is independent
tab_coauthor <- xtabs(~female_coauthor + female_leader_present, leader_text3)
summary(tab_coauthor)
# plot(tab_coauthor)

m_coauthor <- glm(
  female_leader_present2 ~
    female_coauthor,
  family = binomial,
  data = leader_text3
)

# Hierarchical model

mm_coauthor <- glmer(
  female_leader_present2 ~
    female_coauthor +
    (1|document_d_ID),
  family = binomial,
  data = leader_text3
)
mm_coauthorOR <- exp(fixef(mm_coauthor))[[2]]

library(rstanarm)
library(bayesplot)
options(mc.cores = parallel::detectCores())

# stan_mm_coauthor <- stan_glmer(
#   female_leader_present2 ~
#     female_coauthor +
#     (1|document_d_ID),
#   family = binomial(link = "logit"),
#   data = leader_text3,
#   prior_intercept = normal(0,1),
#   prior = normal(0,1), 
#   chains = 4, 
#   iter = 40000)
# 
# summary(stan_mm_coauthor, pars = "female_coauthorTRUE")
# posterior_stan_mm_coauthor <- as.matrix(stan_mm_coauthor)
# mcmc_areas(posterior_stan_mm_coauthor,
#            pars = c("female_coauthorTRUE"),
#            prob = 0.95)

# Female leaders by publication year --------------------------------------

leader_text4 <- left_join(leader_text3, documents, by = c("document_d_ID" = "d_ID"))

# mm_pubyear <- glmer(
#   female_leader_present2 ~
#     `d_publication date` +
#     (1|document_d_ID),
#   family = binomial,
#   data = leader_text4
# )
# mm_pubyearOR <- exp(fixef(mm_pubyear))[[2]]
# 
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
#    USE THIS MODEL????
# ***********************

# stan_mm_pubyear_gender <- stan_glmer(
#   female_leader_present2 ~
#     female_coauthor +
#     (1|`d_publication date`)  +
#     (1|document_d_ID),
#   family = binomial(link = "logit"),
#   data = leader_text4,
#   prior_intercept = normal(0,1),
#   prior = normal(0,1), 
#   chains = 4, 
#   iter = 40000)
# 
# summary(stan_mm_pubyear_gender, pars = c("female_coauthorTRUE"))
# posterior_stan_mm_coauthor_pubyear <- as.matrix(stan_mm_pubyear_gender)
# mcmc_areas(posterior_stan_mm_coauthor_pubyear,
#            pars = c("female_coauthorTRUE"),
#            prob = 0.95)

#launch_shinystan(stan_mm_pubyear_gender)

