#Temporary analyses script for Leadership across context and cultures paper
#Eventually this will be added to the leadershipdata package, or probably it's own package. 
# Good stack overflow on PCA with binary data
# https://stats.stackexchange.com/questions/16331/doing-principal-component-analysis-or-factor-analysis-on-binary-data/16335#16335

# Load data library -------------------------------------------------------
library(leadershipdata)

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(NMF)
library(dendextend)
library(logisticPCA)
library(tibble)
library(gridExtra)


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





# PCA ---------------------------------------------------------
# Logistic PCA on leader qualities

# Create data frames without all 0 rows -----------------------------------

#Create dataframe of variables for logistic PCA of qualities
pca_data_qualities<-leader_text2[,c(quality_vars, "cs_textrec_ID", "group.structure2")]
#Remove -1s for now
pca_data_qualities[quality_vars==-1]<-0
#Remove rows with all 0s
pca_data_qualities<-pca_data_qualities[rowSums(pca_data_qualities[quality_vars])>0,]

# Add subsistence category and other vars in pca_data_qualities
pca_data_qualities <- left_join(pca_data_qualities, text_doc_cultureIDs)
pca_data_qualities <-left_join(pca_data_qualities, leader_cult)
pca_data_qualities$c_culture_code<-pca_data_qualities$d_culture
pca_data_qualities<-left_join(pca_data_qualities, leader_cult)
pca_data_qualities<-left_join(pca_data_qualities, leader_text, by="cs_textrec_ID")
pca_data_qualities$demo_sex[pca_data_qualities$demo_sex=="-1"]="unkown"
pca_data_qualities$demo_sex[is.na(pca_data_qualities$demo_sex)==TRUE]="unkown"


#Fit the SVD with k = 6. 6 is the minimum for 80% of the variance
logsvd_model = logisticSVD(pca_data_qualities[quality_vars], k = 6)
logsvd_model

#Cross validate optimal m
logpca_cv = cv.lpca(pca_data_qualities[quality_vars], ks = 6, ms = 1:10)
plot(logpca_cv)


logpca_model = logisticPCA(pca_data_qualities[quality_vars], k = 6, m = which.min(logpca_cv))
clogpca_model = convexLogisticPCA(pca_data_qualities[quality_vars], k = 6, m = which.min(logpca_cv))

#Plots


plot(clogpca_model, type = "trace")

plot(logsvd_model, type = "trace")


plot(logsvd_model, type = "scores")+ 
  geom_point(aes(colour=pca_data_qualities$subsistence.x)) + 
  ggtitle("Exponential Family PCA") +
  scale_colour_brewer(palette = "Set1")

# Indicate group structure of log PCA model
plot(logpca_model, type = "scores") + 
  geom_point(aes(colour=pca_data_qualities$group.structure2)) + 
  ggtitle("Logistic PCA") +
  scale_colour_brewer(palette = "Set1")

# Indicate subsistence stype of log PCA model
plot(logpca_model, type = "scores") + 
  geom_point(aes(colour=pca_data_qualities$subsistence.x)) + 
  ggtitle("Logistic PCA") +
  scale_colour_brewer(palette = "Set1")


plot(clogpca_model, type = "scores") + 
  geom_point(aes(colour=pca_data_qualities$subsistence.x)) + 
  ggtitle("Convex Logistic PCA")+
  scale_colour_brewer(palette = "Set1")


# Associate logistic PCA model with variables
logpca_model_loadings<-data.frame(logpca_model$U)
logpca_model_loadings$variable<-names(pca_data_qualities[quality_vars])

loadings<- gather(logpca_model_loadings, key = Component, value =  Loading, -variable)

ggplot(loadings[loadings], aes(Loading, variable)) +
  geom_point(aes(colour=Component)) +
  facet_wrap(vars(Component))

# Plot individual components
## Component 1
loadings_X1<-loadings[loadings$Component=="X1",]
X1_sort<-loadings_X1$variable[order(loadings_X1$Loading)]
loadings_X1$variable<-factor(loadings_X1$variable, levels =  X1_sort)

component1_plot<-ggplot(loadings_X1, aes(Loading, variable)) +
  geom_point(aes(colour=Component))+
  theme(legend.position = "none")+
  ggtitle("Component 1")

## Component 2
loadings_X2<-loadings[loadings$Component=="X2",]
X2_sort<-loadings_X2$variable[order(loadings_X2$Loading)]
loadings_X2$variable<-factor(loadings_X2$variable, levels =  X2_sort)

component2_plot<-ggplot(loadings_X2, aes(Loading, variable)) +
  geom_point(aes(colour=Component))+
  theme(legend.position = "none")+
  ggtitle("Component 2")

## Component 3
loadings_X3<-loadings[loadings$Component=="X3",]
X3_sort<-loadings_X3$variable[order(loadings_X3$Loading)]
loadings_X3$variable<-factor(loadings_X3$variable, levels =  X3_sort)

component3_plot<-ggplot(loadings_X3, aes(Loading, variable)) +
  geom_point(aes(colour=Component))+
  theme(legend.position = "none")+
  ggtitle("Component 3")

## Component 4
loadings_X4<-loadings[loadings$Component=="X4",]
X4_sort<-loadings_X4$variable[order(loadings_X4$Loading)]
loadings_X4$variable<-factor(loadings_X4$variable, levels =  X4_sort)

component4_plot<-ggplot(loadings_X4, aes(Loading, variable)) +
  geom_point(aes(colour=Component))+
  theme(legend.position = "none")+
  ggtitle("Component 4")

## Component 5
loadings_X5<-loadings[loadings$Component=="X5",]
X5_sort<-loadings_X5$variable[order(loadings_X5$Loading)]
loadings_X5$variable<-factor(loadings_X5$variable, levels =  X5_sort)

component5_plot<-ggplot(loadings_X5, aes(Loading, variable)) +
  geom_point(aes(colour=Component))+
  theme(legend.position = "none")+
  ggtitle("Component 5")

## Component 6
loadings_X6<-loadings[loadings$Component=="X6",]
X6_sort<-loadings_X6$variable[order(loadings_X6$Loading)]
loadings_X6$variable<-factor(loadings_X6$variable, levels =  X6_sort)

component6_plot<-ggplot(loadings_X6, aes(Loading, variable)) +
  geom_point(aes(colour=Component))+
  theme(legend.position = "none")+
  ggtitle("Component 6")


grid.arrange(component1_plot, component2_plot, component3_plot,
             component4_plot, component5_plot, component6_plot, nrow=2)




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




