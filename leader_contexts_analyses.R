#Temporary analyses script for Leadership across context and cultures paper
#Eventually this will be added to the leadershipdata package, or probably it's own package. 
# Good stack overflow on PCA with binary data
# https://stats.stackexchange.com/questions/16331/doing-principal-component-analysis-or-factor-analysis-on-binary-data/16335#16335

# Load data library -------------------------------------------------------
library(leadershipdata)

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(EGAnet)
library(glasso)
# library(qgraph)
library(NMF)
library(dendextend)


# Recode variables --------------------------------------------------------

# Collapse group structure types

leader_text2$group.structure2<-leader_text2$group.structure.coded

leader_text2$group.structure2[leader_text2$group.structure.coded=="criminal group"]="economic group"
leader_text2$group.structure2[leader_text2$group.structure.coded=="labor group"]="economic group"
leader_text2$group.structure2[leader_text2$group.structure.coded=="subsistence group"]="economic group"

leader_text2$group.structure2[leader_text2$group.structure.coded=="age-group"]="social group"
leader_text2$group.structure2[leader_text2$group.structure.coded=="domestic group"]="social group"
leader_text2$group.structure2[leader_text2$group.structure.coded=="kin group"]="social group"
leader_text2$group.structure2[leader_text2$group.structure.coded=="local group"]="social group"
leader_text2$group.structure2[leader_text2$group.structure.coded=="performance group"]="social group"



# Create named lists of variables by group --------------------------------

# Functions
function_vars = c("function_bestow.mate","function_organize.cooperation","function_political.appointments",
                  "function_resolve.conflcit","functions_construction.infrastructure","functions_control.calendar",
                  "functions_control.economics","functions_control.immigration","functions_council.member",
                  "functions_distribute.resources","functions_group.determination","functions_group.representative",
                  "functions_hospitality","functions_moral.authority","functions_new.settlement",
                  "functions_policymaking","functions_prosocial.investment","functions_protection",
                  "functions_provide.counsel","functions_provide.subsistence","functions_punishment",
                  "functions_ritual","functions_serve.leader","functions_social.functions","functions_strategic.planning")

# Qualities
quality_vars = c("qualities_artistic.performance","qualities_drug.consumption","qualities_exp.accomplished",
                 "qualities_generous", "qualities_high.status","qualities_wealthy","qualities.age","qualities.aggressive",
                 "qualities.ambition","qualities.attractive","qualities.bravery","qualities.charisma","qualities.coercive.authority",
                 "qualities.confident","qualities.culturally.conservative","qualities.culturally.progressive","qualities.decisive",
                 "qualities.fairness","qualities.favorable.personality","qualities.feared","qualities.high.quality.spouse",
                 "qualities.honest","qualities.humble","qualities.industriousness","qualities.ingroup.member","qualities.innovative",
                 "qualities.interpersonal.skills", "qualities.killer", "qualities.knowlageable.intellect", "qualities.loyalty",
                 "qualities.many.children","qualities.oratory.skill","qualities.physical.health","qualities.physically.strong",
                 "qualities.polygynous","qualities.proper.behavior","qualities.prosocial","qualities.social.contacts",
                 "qualities.strategic.nepotism","qualities.strategic.planner","qualities.supernatural","qualities.xenophobic")

# Leader benefits vars
leader_benefit_vars = c("leader.benefits_fitness",                     
                       "leader.benefits_mating","leader.benefits_other",                       
                       "leader.benefits_reduced.risk.harm.conflict","leader.benefits_resource_food",               
                       "leader.benefits_social.services","leader.benefits_social.status.reputation",    
                       "leader.benefits_territory", "leader.benefits_resource_other")
# Leader costs vars
leader_cost_vars = c("leader.costs_fitness.costs",                  
                    "leader.costs_increased.risk.harm.conflict","leader.costs_other",                          
                    "leader.costs_resource_food.cost","leader.costs_resources_other.cost",           
                    "leader.costs_social.status","leader.costs_territory.cost",                 
                    "leader.costs.mating.cost","leader.costs.social.services")

# Follower benefits vars
follower_benefit_vars = c("follower.benefits_fitness",                     
                        "follower.benefits_mating","follower.benefits_other",                       
                        "follower.benefits_reduced.risk.harm.conflict","follower.benefits_resource_food",               
                        "follower.benefits_social.services","follower.benefits_social.status.reputation",    
                        "follower.benefits_territory","follower.benefits_resource_other")
# Follower costs vars
follower_cost_vars = c("follower.costs_fitness",                  
                     "follower.costs_increased.risk.harm.conflict","follower.costs_other",                          
                     "follower.costs_resource_food","follower.costs_resource_other",           
                     "follower.costs_social.status","follower.costs_territory",                 
                     "follower.costs_mating","follower.costs_social.services")


# Aggregate at Culture level ----------------------------------------------

textID_docID<-leader_text_original[,c("cs_textrec_ID","doc_ID")]
docID_cultureID<-documents[,c("d_ID","d_culture")]
docID_cultureID$doc_ID<-docID_cultureID$d_ID

text_doc_cultureIDs<-left_join(textID_docID,docID_cultureID)

leader_text2<-left_join(leader_text2, text_doc_cultureIDs, by="cs_textrec_ID")


by_culture = leader_text2 %>% 
  group_by(d_culture) %>%
  dplyr::select(d_culture, one_of(c(quality_vars, function_vars,
                                    leader_benefit_vars, leader_cost_vars,
                                    follower_benefit_vars, follower_cost_vars))) %>% 
  summarise_each(funs(mean))



# Create data frames without all 0 rows -----------------------------------
#For quality variables only
d_q<-by_culture[quality_vars]

#Get rid of 'evidence against' for now...
#d_q[d_q==-1]<-0

#Get rid of rows with only 0s
d_q<-d_q[rowSums(d_q) > 0, ]


# PCA ---------------------------------------------------------



# PCA on leader qualities

# pca_qualities <-prcomp(d_q, scale=FALSE)
# summary(pca_qualities)
# plot(pca_qualities, main ="", col = "deepskyblue2")
# biplot(pca_qualities)
# 
# pca_qualities$rotation
# load=pca_qualities$rotation
# ##looking at a chart of variables and loadings
# sorted_pca1=load[order(load[,1]),1]
# plot.PCA1_dot<-dotchart(sorted_pca1, cex=.8, main="Dotchart of variable loadings\n for PCA 1", xlab="loadings")
# 
# sorted_pca2=load[order(load[,2]),2]
# plot.PCA2_dot<-dotchart(sorted_pca2, cex=.8, main="Dotchart of variable loadings\n for PCA 2", xlab="loadings")
# 
# sorted_pca3=load[order(load[,3]),3]
# plot.PCA2_dot<-dotchart(sorted_pca3, cex=.8, main="Dotchart of variable loadings\n for PCA 3", xlab="loadings")
# 
# sorted_pca4=load[order(load[,4]),4]
# plot.PCA2_dot<-dotchart(sorted_pca4, cex=.8, main="Dotchart of variable loadings\n for PCA 4", xlab="loadings")
# 
# 
# # EGA  --------------------------------------------------------------------
# #Create a correlation matrix of the quality variables
# q_cor<-cor(by_culture[quality_vars])
# 
# # Compute the EGA
# ega_q<-EGA(q_cor, n=59, model = "TMFG", plot.EGA = F)
# plot(ega_q, vsize = 6, label.prop = .5)
# 
# #CFA(ega_q, d_q, "WLSMV")
# 
# # Standardized loadings
# net.loads <- net.loads(A = ega_q)$std
# net.loads
# 
# # Network scores
# net.scores <- net.scores(data = d_q, A = ega_q)
# net.scores
# 
# # Dimension stability test
# ## Bootstrap item replicability
# ega_q_boot<-bootEGA(by_culture[quality_vars], n=500, model = "TMFG", type = "parametric", ncore=8)
# itemStability(ega_q_boot, orig.wc = ega_q$wc, item.freq = 0.2)
# plot(ega_q_boot)
# 
# 
# # Heatmaps -----------------------------------------------------------------
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




