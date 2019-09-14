#Temporary analyses script for Leadership across context and cultures paper
#Eventually this will be added to the leadershipdata package, or probably it's own package. 


# Load data library -------------------------------------------------------
library(leadershipdata)

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(EGAnet)
library(glasso)
library(qgraph)


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
follower_cost_vars = c("follower.costs_fitness.costs",                  
                     "follower.costs_increased.risk.harm.conflict","follower.costs_other",                          
                     "follower.costs_resource_food.cost","follower.costs_resources_other.cost",           
                     "follower.costs_social.status","follower.costs_territory.cost",                 
                     "follower.costs.mating.cost","follower.costs.social.services")


# Create data frames without all 0 rows -----------------------------------
#For quality variables only
d_q<-d[quality_vars]
#Get rid of 'evidence against' for now...
d_q[d_q==-1]<-0
#Get rid of rows with only 0s
d_q<-d_q[rowSums(d_q) > 0, ]


# Exploratory PCA ---------------------------------------------------------

# PCA on leader qualities

pca_qualities <-prcomp(d_q[quality_vars], scale=FALSE)
summary(pca_qualities)
plot(pca_qualities, main ="", col = "deepskyblue2")
biplot(pca_qualities)

pca_qualities$rotation
load=pca_qualities$rotation
##looking at a chart of variables and loadings
sorted_pca1=load[order(load[,1]),1]
plot.PCA1_dot<-dotchart(sorted_pca1, cex=.8, main="Dotchart of variable loadings\n for PCA 1", xlab="loadings")

sorted_pca2=load[order(load[,2]),2]
plot.PCA2_dot<-dotchart(sorted_pca2, cex=.8, main="Dotchart of variable loadings\n for PCA 2", xlab="loadings")

sorted_pca3=load[order(load[,3]),3]
plot.PCA2_dot<-dotchart(sorted_pca3, cex=.8, main="Dotchart of variable loadings\n for PCA 3", xlab="loadings")

sorted_pca4=load[order(load[,4]),4]
plot.PCA2_dot<-dotchart(sorted_pca4, cex=.8, main="Dotchart of variable loadings\n for PCA 4", xlab="loadings")


# EGA  --------------------------------------------------------------------

q_cor<-cor(d_q)
# Compute the EGA
ega_q<-EGA(q_cor, n=444, model = "TMFG", plot.EGA = F)
plot(ega_q, vsize = 6, label.prop = .5)

# Standardized loadings
net.loads <- net.loads(A = ega_q)$std
net.loads
# Network scores
net.scores <- net.scores(data = d_q, A = ega_q)
net.scores
# Dimension stability test
ega_q_boot<-bootEGA(d_q, n=1000, model = "glasso", type = "parametric", ncore=4)

itemStability(ega_q_boot, orig.wc = ega_q$wc, item.freq = 0.2)

plot(ega_q_boot)



