#Temporary analyses script for Leadership across context and cultures paper
#Eventually this will be added to the leadershipdata package, or probably it's own package. 
# Good stack overflow on PCA with binary data
# https://stats.stackexchange.com/questions/16331/doing-principal-component-analysis-or-factor-analysis-on-binary-data/16335#16335
# https://cran.r-project.org/web/packages/logisticPCA/vignettes/logisticPCA.html

# Load data library -------------------------------------------------------
library(leadershipdata)

load("leader_text2.rda")

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
# library(tidybayes)
# library(cowplot)
# library(brms)
# library(ggstance)
# library(rstanarm)
# library(ggpubr)
# library(bayesplot)

library(magrittr)
library(modelr)
library(ggridges)
library(RColorBrewer)
# library(gganimate)


# Create functions --------------------------------------------------------
# Converts factors to charachter vecrtors in data frame
de_factor <- function(df){
df %>% dplyr::mutate_if(is.factor, as.character) -> df
}

# Recode variables --------------------------------------------------------

# Add sex
leader_text2 <- left_join(leader_text2, leader_text_original[c("cs_textrec_ID", "demo_sex")])
male_leader_pct <- signif(100*sum(leader_text2$demo_sex=='male', na.rm=T)/nrow(leader_text2), 3)
female_leader_pct <- signif(100*sum(leader_text2$demo_sex=='female', na.rm=T)/nrow(leader_text2), 2)

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

leader_text2$group.structure2[leader_text2$group.structure.coded=="multiple domains"]="other"
leader_text2$group.structure2[leader_text2$group.structure.coded=="unkown"]="other"


# Create named lists of variables by group --------------------------------

# Functions,
function_vars = c("function_bestow.mate" ,  "function_organize.cooperation"                ,             
"function_political.appointments"        ,   "function_resolve.conflcit"                   ,
"functions_construction.infrastructure"  ,  
"functions_control.calendar"             ,   "functions_control.economics"                 ,
"functions_control.immigration"          ,   "functions_council.member"                    ,
"functions_distribute.resources"         ,   "functions_group.determination"               ,
"functions_group.representative"         ,   "functions_hospitality"                       ,
"functions_medicinal"                    ,   "functions_military.command"                  ,
"functions_moral.authority"              ,   "functions_new.settlement"                    ,
"functions_policymaking"                 ,   "functions_prosocial.investment"              ,
"functions_protection"                   ,   "functions_provide.counsel"                   ,
"functions_provide.subsistence"          ,   "functions_punishment"                        ,
"functions_ritual"                       ,   "functions_serve.leader"                      ,
"functions_social.functions"             ,   "functions_strategic.planning")

# Qualities
quality_vars = c("qualities_artistic.performance"       ,        "qualities_drug.consumption"             ,       "qualities_exp.accomplished",                  
                 "qualities_generous"                   ,        "qualities_high.status"                  ,     
                 "qualities_wealthy"                    ,        "qualities.age"                          ,     
                 "qualities.aggressive"                 ,        "qualities.ambition"                     ,     
                 "qualities.attractive"                 ,        "qualities.bravery"                      ,     
                 "qualities.charisma"                   ,        "qualities.coercive.authority"           ,     
                 "qualities.confident"                  ,        "qualities.culturally.conservative"      ,     
                 "qualities.culturally.progressive"     ,        "qualities.decisive"                     ,     
                 "qualities.fairness"                   ,        "qualities.favorable.personality"        ,     
                 "qualities.feared"                     ,        "qualities.high.quality.spouse"          ,     
                 "qualities.honest"                     ,        "qualities.humble"                       ,     
                 "qualities.industriousness"            ,        "qualities.ingroup.member"               ,     
                 "qualities.innovative"                 ,        "qualities.interpersonal.skills"         ,     
                 "qualities.killer"                     ,        "qualities.knowlageable.intellect"       ,     
                 "qualities.loyalty"                    ,        "qualities.many.children"                ,     
                 "qualities.oratory.skill"              ,        "qualities.physical.health"              ,     
                  "qualities.physically.strong"         ,         "qualities.polygynous"                  ,      
                  "qualities.proper.behavior"           ,         "qualities.prosocial"                   ,      
                  "qualities.social.contacts"           ,         "qualities.strategic.nepotism"          ,     
                  "qualities.strategic.planner"         ,         "qualities.supernatural"                ,    
                  "qualities.xenophobic")

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

textID_docID<-de_factor(leader_text_original[,c("cs_textrec_ID","doc_ID")])
docID_cultureID<-de_factor(documents[,c("d_ID","d_culture")])
docID_cultureID$doc_ID<-docID_cultureID$d_ID
docID_cultureID<-docID_cultureID[,c("d_culture","doc_ID")]


text_doc_cultureIDs<-left_join(textID_docID,docID_cultureID, by="doc_ID")

leader_text2<-left_join(leader_text2, text_doc_cultureIDs, by="cs_textrec_ID")

by_culture <- leader_text2 %>% 
  dplyr::select(
    d_culture,
    one_of(
      c(quality_vars,
        function_vars,
        leader_benefit_vars,
        leader_cost_vars,
        follower_benefit_vars,
        follower_cost_vars)
      )
    ) %>%
  group_by(d_culture) %>%
  summarise_each(lst(mean))

# Culturel level variable manipulations -----------------------------------

# Rename culture name in leader_cult data
leader_cult$d_culture<-leader_cult$c_culture_code
leader_cult<-de_factor(leader_cult)
#Subset culture vars of interest
culture_vars<-leader_cult[,c("d_culture","subsistence","c_cultural_complexity", "settlement_fixity", "pop_density","com_size")]

leader_text2<-left_join(leader_text2, culture_vars)

# Add more SCCS variables
load("sccs.RData")
sccs<-data.frame(sccs)
sccs<-sccs[,c("SCCS.","V1648")] # ID, Frequency of warfare
sccs_ehraf_ID<-distinct(read.csv("sccs_ehraf_IDs.csv")) # Some dupes in csv file
sccs_ehraf_ID$c_culture_code<-sccs_ehraf_ID$OWC

sccs_ehraf_our_sample<-left_join(leader_cult, sccs_ehraf_ID)
sccs_ehraf_our_sample<-sccs_ehraf_our_sample[,c("c_name","c_culture_code",
                                                "SCCS.")]
sccs_ehraf_our_sample<-sccs_ehraf_our_sample[!is.na(sccs_ehraf_our_sample$SCCS.)==T,]

sccs_vars<-left_join(sccs, sccs_ehraf_ID[c('SCCS.', 'c_culture_code')], by = "SCCS.")
sccs_vars<-na.omit(sccs_vars) # Some eHRAF cultures do not map to SCCS cultures
sccs_vars<-sccs_vars[,c("c_culture_code","V1648")]



# Convert warfare levels to interval var
sccs_vars$warfare_freq <- as.numeric(sccs_vars$V1648)
sccs_vars$warfare_freq <- ifelse(sccs_vars$warfare_freq %in% c(1,20), NA, sccs_vars$warfare_freq)
sccs_vars$warfare_freq <- 19 - sccs_vars$warfare_freq

leader_text2<-left_join(leader_text2, sccs_vars, by=c("d_culture"="c_culture_code"))

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




# Variable support plots --------------------------------------------------

# Tabulate evidence for & against at the extract level (d1) and the culture level (d_ev2)

#Should have run first few data management code section of leader_contexts_analyses.R

#Create data frame of all variables

# Manage data 

authorID_textID<-leader_text_original[,c("author_ID","cs_textrec_ID")]

all_data <- left_join(leader_text2, authorID_textID)

# Remove negative values from vars
# all_data[all_data==-1]<-0 # Some vars are factors, so this code won't work

all_data <- all_data %>% 
  mutate_if(
    is.numeric, function(x) ifelse(x < 0, 0, x)
  )

#Groups of variables to model support
model_vars<-c(quality_vars, function_vars)

all_data$d_culture<-factor(all_data$d_culture)
all_data<-data.frame(all_data)

# Build functions 

# Inv. logit
logit.inv = function(x) exp(x)/(1+exp(x))

var.ev = function(v){
  
  return(sum(v==1)/length(v))
  
}

resample <- function(dat, cluster, replace) {
  
  # exit early for trivial data
  if(nrow(dat) == 1 || all(replace==FALSE))
    return(dat)
  
  # sample the clustering factor
  cls <- sample(unique(dat[[cluster[1]]]), replace=replace[1])
  
  # subset on the sampled clustering factors
  sub <- lapply(cls, function(b) subset(dat, dat[[cluster[1]]]==b))
  
  # sample lower levels of hierarchy (if any)
  if(length(cluster) > 1)
    sub <- lapply(sub, resample, cluster=cluster[-1], replace=replace[-1])
  
  # join and return samples
  do.call(rbind, sub)
  
}

clrs2 = function(x){
  
  m = mean(x[[1]])
  if (is.na(m) | m==0) return(0)
  return(1)
  
}

cult.sum = function(df) {
  v = as.numeric(by(df, factor(df$d_culture), clrs2, simplify=T))
  return(sum(v==1)/length(v))
}




# Qualities & Functions Fit models 
Model=character(0)
Variable=character(0)
Evidence=character(0)
Type=character(0)
value=numeric(0)
se=numeric(0)
y_se=numeric(0)
y_negse=numeric(0)

models = list(list('Qualities', quality_vars),
              list('Functions', function_vars))


library(lme4)
for (m in models){
  model = m[[1]]
  model_vars = m[[2]]
  
  glmer_models <- lapply(model_vars, function(x) { 
    print(x)
    glmer(substitute(i ~ 1 + (1|d_culture/author_ID), list(i = as.name(x))), family=binomial, data = all_data, nAGQ=0)
  })
  
  for (v in 1:length(model_vars)){
    print(v)
    Model = c(Model, model)
    Type = c(Type, 'Extracts')
    Variable = c(Variable, model_vars[v])
    
    m = glmer_models[[v]]
    cfs = summary(m)
    intercept = cfs$coefficients[[1]]
    se = cfs$coefficients[[2]]
    value = c(value, logit.inv(intercept))
    y_se = c(y_se, logit.inv(intercept + se))
    y_negse = c(y_negse, logit.inv(intercept - se))           
    
  }
}

for (m in models) {
  
  model = m[[1]]
  model_vars = m[[2]]
  for (v in 1:length(model_vars)){
    Model = c(Model, model)
    Variable = c(Variable, model_vars[v])
    Type = c(Type, 'Cultures')
    
    # Summarize at culture level
    
    d.tmp = all_data[, c(model_vars[v], 'd_culture', 'author_ID')]
    val = cult.sum(d.tmp)
    value = c(value, val)
    replicates = replicate(10, cult.sum(resample(d.tmp, cluster=c('d_culture', 'author_ID'), replace=c(T,F))))
    q=quantile(replicates, c(0.025, 0.975))
    y_se = c(y_se, q[[2]])
    y_negse = c(y_negse, q[[1]])
  }
}

d_melt = data.frame(Model=Model, Variable=Variable, Type=Type, value=value, y_se=y_se, y_negse=y_negse, stringsAsFactors = F)

var_names <- c(
  "function_bestow.mate" = "Bestow mates",                        
  "function_political.appointments" = "Political appointments",             
  "functions_construction.infrastructure"       = "Construction/infastructure",
  "functions_control.economics"                 = "Control economics",
  "functions_council.member"                    = "Council member",
  "functions_group.determination"              = "Group determiniation",
  "functions_hospitality"                       = "Hospitality",
  "functions_military.command"                  = "Military command",
  "functions_new.settlement"                    =  "Movement/migration",
  "functions_prosocial.investment"              = "Prosocial investment",
  "functions_provide.counsel"                   = "Provide counsel/direction",
  "functions_punishment"                        = "Punishment",
  "functions_serve.leader"                      = "Serve a leader",
  "functions_strategic.planning" = "Strategic planning",
  "function_organize.cooperation"  = "Organize cooperation",
  "function_resolve.conflcit"      = "Resolve conflict",
  "functions_control.calendar"     = "Control calander",
  "functions_control.immigration"  = "Control immigration",
  "functions_distribute.resources" = "Distribute resources",
  "functions_group.representative" = "Group representative",
  "functions_medicinal"           = "Medicinal functions",
  "functions_moral.authority"     = "Moral authority",
  "functions_policymaking"        =  "Policy making",
  "functions_protection"          = "Protection",
  "functions_provide.subsistence" = "Provide subsistence",
  "functions_ritual"              = "Ritual functions",
  "functions_social.functions" = "Social functions",
  "qualities_artistic.performance"     = "Artistic performance",
  "qualities_generous"                 = "Generosity",
  "qualities.age"                      = "Age",
  "qualities.attractive"              = "Attractive",
  "qualities.coercive.authority"      = "Coercive authority",
  "qualities.culturally.progressive"  = "Culturally progressive",
  "qualities.favorable.personality"   = "Favorable personality",
  "qualities.honest"                  = "Honesty",
  "qualities.ingroup.member"          = "Ingroup member",
  "qualities.killer"                  = "Killer",
  "qualities.many.children"           = "Many children",
  "qualities.physically.strong"       = "Physically formidable",
  "qualities.prosocial"               = "Prosocial",
  "qualities.strategic.planner"       = "Strategic planner",
  "qualities_drug.consumption"     = "Drug consumption",
  "qualities_high.status"            = "High status",
  "qualities.aggressive"             = "Aggressiveness",
  "qualities.bravery"               = "Bravery",
  "qualities.confident"             = "Confidence",
  "qualities.decisive"              = "Decisiveness/decision-making",
  "qualities.feared"                = "Feared",
  "qualities.humble"                = "Humility",
  "qualities.innovative"            = "Innovative",
  "qualities.knowlageable.intellect" = "Knowledgeable/intelligence",
  "qualities.oratory.skill"         = "Oratory skill",
  "qualities.polygynous"            = "Polygnous",
  "qualities.social.contacts"       = "Social contacts",
  "qualities.supernatural"    = "Supernatural",
  "qualities_exp.accomplished"       = "Experienced/accomplished",
  "qualities_wealthy"                = "Wealthy",
  "qualities.ambition"               = "Ambitious",
  "qualities.charisma"               = "Charisma",
  "qualities.culturally.conservative" = "Culturally conservative",
  "qualities.fairness"               = "Fairness",
  "qualities.high.quality.spouse"    = "High-quality spouse",
  "qualities.industriousness"        = "Industriousness",
  "qualities.interpersonal.skills"   = "Interpersonal skills",
  "qualities.loyalty"                = "Loyalty",
  "qualities.physical.health"        = "Physical health",
  "qualities.proper.behavior"        = "Proper behavior",
  "qualities.strategic.nepotism"     = "Strategic nepotism",
  "qualities.xenophobic"   = "Xenophia"
)

d_melt$Variable <- var_names[d_melt$Variable]

the_levels <- c(d_melt$Variable[d_melt$Type=='Cultures'][order(d_melt$value[d_melt$Type=='Cultures'])])
d_melt$Variable = factor(d_melt$Variable, levels=the_levels)

percent<-c("0%","10%","20%","30%","40%","50%","60%","70%","80%","90%","100%")
# Plot code
plot.variable.support = ggplot(d_melt, aes(value, Variable, xmin=y_negse, xmax=y_se, colour=Type, shape=Type)) + 
  geom_errorbarh() + 
  geom_point() +
  scale_x_continuous(breaks=seq(0,1,.1), labels=percent, limits=c(0,1)) +
  scale_colour_discrete(name='', labels=c('Cultures', 'Text records')) +
  #labs(x='\nPercent', y='') +
  facet_grid(Model~., scales = "free_y", space='free') +
  facet_wrap(~Model, scales = "free_y") +
  theme_bw() +
  theme(strip.text.y = element_text(angle=0))+
  scale_shape_manual(name="", values=c(17,16), labels=c('Cultures', 'Text records'))+
  scale_fill_manual(name="", values=c("red", "blue"), labels=c('Cultures', 'Text records')) +
  labs(x="\nValue",y="")
plot.variable.support



# Costs and benefits -
model_vars<-c(leader_benefit_vars, leader_cost_vars,
              follower_benefit_vars, follower_cost_vars)

Model=character(0)
Variable=character(0)
Evidence=character(0)
Type=character(0)
value=numeric(0)
se=numeric(0)
y_se=numeric(0)
y_negse=numeric(0)

models = list(list('Leadership benefits', leader_benefit_vars),
              list('Leadership costs', leader_cost_vars),
              list('Follower benefits', follower_benefit_vars),
              list('Follower costs', follower_cost_vars))


library(lme4)
for (m in models){
  model = m[[1]]
  model_vars = m[[2]]
  
  glmer_models <- lapply(model_vars, function(x) { 
    print(x)
    glmer(substitute(i ~ 1 + (1|d_culture/author_ID), list(i = as.name(x))), family=binomial, data = all_data, nAGQ=0)
  })
  
  for (v in 1:length(model_vars)){
    print(v)
    Model = c(Model, model)
    Type = c(Type, 'Extracts')
    Variable = c(Variable, model_vars[v])
    
    m = glmer_models[[v]]
    cfs = summary(m)
    intercept = cfs$coefficients[[1]]
    se = cfs$coefficients[[2]]
    value = c(value, logit.inv(intercept))
    y_se = c(y_se, logit.inv(intercept + se))
    y_negse = c(y_negse, logit.inv(intercept - se))           
    
  }
}

for (m in models) {
  
  model = m[[1]]
  model_vars = m[[2]]
  for (v in 1:length(model_vars)){
    Model = c(Model, model)
    Variable = c(Variable, model_vars[v])
    Type = c(Type, 'Cultures')
    
    # Summarize at culture level
    
    d.tmp = all_data[, c(model_vars[v], 'd_culture', 'author_ID')]
    val = cult.sum(d.tmp)
    value = c(value, val)
    replicates = replicate(10, cult.sum(resample(d.tmp, cluster=c('d_culture', 'author_ID'), replace=c(T,F))))
    q=quantile(replicates, c(0.025, 0.975))
    y_se = c(y_se, q[[2]])
    y_negse = c(y_negse, q[[1]])
  }
}

d_melt_cb = data.frame(Model=Model, Variable=Variable, Type=Type, value=value, y_se=y_se, y_negse=y_negse, stringsAsFactors = F)

# var_names <- c(
#   "leader.benefits_fitness"  = "Fitness",                  
#   "leader.benefits_other"     = "Other misc.",                 
#   "leader.benefits_resource_food" = "Food resources",             
#   "leader.benefits_social.status.reputation" = "Status/reputation",  
#   "leader.benefits_resource_other" = "Non-food Resources",
#   "leader.benefits_mating"      = "Mating",              
#   "leader.benefits_reduced.risk.harm.conflict" = "Reduced risk of harm/conflict",
#   "leader.benefits_social.services"           = "Social services",
#   "leader.benefits_territory" = "Territory",
#  "follower.benefits_fitness"   = "Fitness",                   
#  "follower.benefits_other"          = "Other misc.",            
#  "follower.benefits_resource_food"   = "Food resources",            
#  "follower.benefits_social.status.reputation"  = "Status/reputation",
#  "follower.benefits_resource_other"  = "Non-food Resources",
#  "follower.benefits_mating"           = "Mating",           
#  "follower.benefits_reduced.risk.harm.conflict" = "Reduced risk of harm/conflict",
#  "follower.benefits_social.services"    = "Social services",       
#  "follower.benefits_territory" = "Territory",
#  "leader.costs_fitness.costs"   = "Fitness",               
#  "leader.costs_other"                  = "Other misc.",      
#  "leader.costs_resources_other.cost"      =    "Non-food Resources",
#  "leader.costs_territory.cost"         = "Territory",       
#  "leader.costs.social.services" = "Social services",
# "leader.costs_increased.risk.harm.conflict" = "Increased risk of harm/conflict",
# "leader.costs_resource_food.cost"  =    "Food resources",     
# "leader.costs_social.status"              = "Status/reputation", 
# "leader.costs.mating.cost" = "Mating",  
# "follower.costs_fitness"    = "Fitness",                    
# "follower.costs_other"              = "Other misc.",          
# "follower.costs_resource_other"    = "Non-food Resources",           
# "follower.costs_territory"             = "Territory",       
# "follower.costs_social.services" = "Social services",
# "follower.costs_increased.risk.harm.conflict" = "Increased risk of harm/conflict",
# "follower.costs_resource_food"     =     "Food resources",      
# "follower.costs_social.status"      = "Status/reputation",         
# "follower.costs_mating"                      = "Mating"  
# )
# 
# d_melt_cb$Variable <- var_names[d_melt_cb$Variable]
# 
the_levels <- c(d_melt_cb$Variable[d_melt_cb$Type=='Cultures'][order(d_melt_cb$value[d_melt_cb$Type=='Cultures'])])
d_melt_cb$Variable = factor(d_melt_cb$Variable, levels=the_levels)

percent<-c("0%","10%","20%","30%","40%","50%","60%","70%","80%","90%","100%")
# Plot code
plot.variable.support_costs_benefits = ggplot(d_melt_cb, aes(value, Variable, xmin=y_negse, xmax=y_se, colour=Type, shape=Type)) + 
  geom_errorbarh() + 
  geom_point() +
  scale_x_continuous(breaks=seq(0,1,.1), labels=percent, limits=c(0,1)) +
  scale_colour_discrete(name='', labels=c('Cultures', 'Text records')) +
  #labs(x='\nPercent', y='') +
  facet_grid(Model~., scales = "free_y", space='free') +
  facet_wrap(~Model, scales = "free_y") +
  theme_bw() +
  theme(strip.text.y = element_text(angle=0))+
  scale_shape_manual(name="", values=c(17,16), labels=c('Cultures', 'Text records'))+
  scale_fill_manual(name="", values=c("red", "blue"), labels=c('Cultures', 'Text records'))+
  labs(x="\nValue",y="")
plot.variable.support_costs_benefits




# PCA Qualities ---------------------------------------------------------
# Logistic PCA on leader qualities

# Create data frames without all 0 rows

#Create dataframe of variables for logistic PCA of qualities
pca_data_qualities<-leader_text2[,c(quality_vars, "cs_textrec_ID", "group.structure2")]
#Remove -1s for now
# pca_data_qualities[quality_vars==-1]<-0 # This code makes no sense

pca_data_qualities <- pca_data_qualities %>% 
  mutate_if(
    is.numeric, function(x) ifelse(x < 0, 0, x) # Remove -1's
  )

#Remove rows with all 0s
pca_data_qualities<-pca_data_qualities[rowSums(pca_data_qualities[quality_vars])>0,]

# Add subsistence category and other vars in pca_data_qualities
pca_data_qualities <- left_join(pca_data_qualities, text_doc_cultureIDs, by = "cs_textrec_ID")
#pca_data_qualities$c_name<-pca_data_qualities$d_culture
pca_data_qualities <-left_join(pca_data_qualities, leader_cult, by = "d_culture")
#pca_data_qualities$c_culture_code<-pca_data_qualities$d_culture
#pca_data_qualities<-left_join(pca_data_qualities, leader_cult, by = "c_culture_code")
#pca_data_qualities<-left_join(pca_data_qualities, leader_text, by="cs_textrec_ID")
#pca_data_qualities$demo_sex[pca_data_qualities$demo_sex=="-1"]="unkown"
#pca_data_qualities$demo_sex[is.na(pca_data_qualities$demo_sex)==TRUE]="unkown"

# Fix names
pca_data_qualities2 <- pca_data_qualities[quality_vars]
names(pca_data_qualities2) <- var_names[names(pca_data_qualities2)]

# Cluster anaysis

m <- pvclust(pca_data_qualities2, method.hclust = 'ward', method.dist = 'correlation', nboot = 2000)
plot(m)
pvrect(m)

#Set components
k=3

logsvd_model_qualities = logisticSVD(pca_data_qualities2, k = k)
logsvd_model_qualities

#Cross validate optimal m
# logpca_cv_qualities = cv.lpca(pca_data_qualities2, ks = k, ms = 1:10)
# plot(logpca_cv_qualities)

# Need to cross validate both k and m
# This takes a long time
qual_cvlpca <- cv.lpca(pca_data_qualities2, ks = 1:20, ms = 5:10)
plot(qual_cvlpca)
# optimal values seem to be
k = 10
m = 12
logpca_model_qualities = logisticPCA(pca_data_qualities2, k = k, m = m, main_effects = T)
clogpca_model_qualities = convexLogisticPCA(pca_data_qualities2, k = k, m = which.min(logpca_cv_qualities))

#Plots

plot(logpca_model_qualities, type = "trace")
plot(clogpca_model_qualities, type = "trace")
plot(logsvd_model_qualities, type = "trace")


plot(logsvd_model_qualities, type = "scores")+ 
  geom_point(aes(colour=pca_data_qualities$subsistence)) + 
  stat_ellipse(aes(colour=pca_data_qualities$subsistence)) +
  ggtitle("Exponential Family PCA") +
  scale_colour_brewer(palette = "Set1")

# Indicate group structure of log PCA model
plot(logpca_model_qualities, type = "scores") + 
  geom_point(aes(colour=pca_data_qualities$group.structure2)) + 
  ggtitle("Logistic PCA") +
  scale_colour_brewer(palette = "Set1")

# Indicate subsistence stype of log PCA model
plot(logpca_model_qualities, type = "scores") + 
  geom_point(aes(colour=pca_data_qualities$subsistence)) + 
  ggtitle("Logistic PCA") +
  scale_colour_brewer(palette = "Set1")


plot(clogpca_model_qualities, type = "scores") + 
  geom_point(aes(colour=pca_data_qualities$group.structure2)) + 
  ggtitle("Convex Logistic PCA")+
  scale_colour_brewer(palette = "Set1")


# Associate logistic PCA model with variables
logpca_model_loadings<-data.frame(logpca_model_qualities$U)
logpca_model_loadings$variable<-names(pca_data_qualities2)

qual_loadings<- gather(logpca_model_loadings, key = Component, value =  Loading, -variable)

ggplot(qual_loadings, aes(Loading, variable)) +
  geom_point(aes(colour=Component)) +
  facet_wrap(vars(Component))

# Plot individual components
## Component 1
# qual_loadings_X1<-qual_loadings[qual_loadings$Component=="X1",]
# X1_sort<-qual_loadings_X1$variable[order(qual_loadings_X1$Loading)]
# qual_loadings_X1$variable<-factor(qual_loadings_X1$variable, levels =  X1_sort)

qualities_component1_plot <-
  ggplot(logpca_model_loadings, aes(X1, fct_reorder(variable, X1), colour=X1)) +
  ggalt::geom_lollipop(horizontal = T, size = 1, show.legend = FALSE) +
  scale_color_gradient2(low = 'red', mid = 'white', 'high' = 'blue', name = 'Loading') +
  theme_bw(15) +
  labs(title = "Leader qualities PC 1", x = "\nLoading", y = "")

qualities_component1_plot

## Component 2
# qual_loadings_X2<-qual_loadings[qual_loadings$Component=="X2",]
# X2_sort<-qual_loadings_X2$variable[order(qual_loadings_X2$Loading)]
# qual_loadings_X2$variable<-factor(qual_loadings_X2$variable, levels =  X2_sort)

qualities_component2_plot <-
  ggplot(logpca_model_loadings, aes(X2, fct_reorder(variable, X2), colour=X2)) +
  ggalt::geom_lollipop(horizontal = T, size = 1, show.legend = FALSE) +
  scale_color_gradient2(low = 'red', mid = 'white', 'high' = 'blue', name = 'Loading') +
  theme_bw(15) +
  labs(title = "Leader qualities PC 2", x = "\nLoading", y = "")

qualities_component2_plot

qualities_component1_plot + qualities_component2_plot

## Component 3
qual_loadings_X3<-qual_loadings[qual_loadings$Component=="X3",]
X3_sort<-qual_loadings_X3$variable[order(qual_loadings_X3$Loading)]
qual_loadings_X3$variable<-factor(qual_loadings_X3$variable, levels =  X3_sort)

qualities_component3_plot<-ggplot(qual_loadings_X3, aes(Loading, variable)) +
  geom_point(aes(colour=Component))+
  theme(legend.position = "none")+
  ggtitle("Component 3")



#plot(component1_plot + component2_plot + component3_plot)
grid.arrange(qualities_component1_plot, qualities_component2_plot, 
             qualities_component3_plot, nrow=1)

# PCA Functions -----------------------------------------------------------

# Logistic PCA on leader functions

#Create dataframe of variables for logistic PCA of qualities
pca_data_functions<-leader_text2[,c(function_vars, "cs_textrec_ID", "group.structure2")]
#Remove -1s for now
# pca_data_functions[function_vars==-1]<-0 # This code makes no sense

pca_data_functions <- pca_data_functions %>% 
  mutate_if(
    is.numeric, function(x) ifelse(x < 0, 0, x)
  )

#Remove rows with all 0s
pca_data_functions<-pca_data_functions[rowSums(pca_data_functions[function_vars])>0,]

# Add subsistence category and other vars in pca_data_qualities
pca_data_functions <- left_join(pca_data_functions, text_doc_cultureIDs, by = "cs_textrec_ID")

pca_data_functions <-left_join(pca_data_functions, culture_vars, by = "d_culture")

# Fix names
pca_data_functions2 <- pca_data_functions[function_vars]
names(pca_data_functions2) <- var_names[names(pca_data_functions2)]

# pvclust
m <- pvclust(pca_data_functions2, method.hclust = 'ward', method.dist = 'correlation', nboot = 2000)
plot(m)
pvrect(m, alpha = 0.9)

#Fit the SVD 
logsvd_model_functions = logisticSVD(pca_data_functions2, k = k)
logsvd_model_functions

#Cross validate optimal k, m
# Takes a long time
logpca_cv_function = cv.lpca(pca_data_functions2, ks = 1:20, ms = 1:15)
plot(logpca_cv_function)
# Optimal values?
k <- 10
m <- 11

logpca_model_functions = logisticPCA(pca_data_functions2, k = k, m = m, main_effects = T)
clogpca_model_functions = convexLogisticPCA(pca_data_functions2, k = k, m = m)

#Plots

plot(logpca_model_functions, type = "trace")
plot(clogpca_model_functions, type = "trace")
plot(logsvd_model_functions, type = "trace")


plot(logsvd_model_functions, type = "scores")+ 
  geom_point(aes(colour=pca_data_functions$subsistence)) + 
  ggtitle("Exponential Family PCA") +
  scale_colour_brewer(palette = "Set1")

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


plot(clogpca_model_functions, type = "scores") + 
  geom_point(aes(colour=pca_data_functions$group.structure2)) + 
  ggtitle("Convex Logistic PCA")+
  scale_colour_brewer(palette = "Set1")


# Associate logistic PCA model with variables
logpca_model_functions_loadings<-data.frame(logpca_model_functions$U)
logpca_model_functions_loadings$variable<-names(pca_data_functions2)

loadings_functions<- gather(logpca_model_functions_loadings, key = Component, value =  Loading, -variable)

ggplot(loadings_functions, aes(Loading, variable)) +
  geom_point(aes(colour=Component)) +
  facet_wrap(vars(Component))

# Plot individual components
## Component 1
# functions_loadings_X1<-loadings_functions[loadings_functions$Component=="X1",]
# functions_X1_sort<-functions_loadings_X1$variable[order(functions_loadings_X1$Loading)]
# functions_loadings_X1$variable<-factor(functions_loadings_X1$variable, levels =  functions_X1_sort)

functions_component1_plot <-
  ggplot(logpca_model_functions_loadings, aes(X1, fct_reorder(variable, X1), colour=X1)) +
  ggalt::geom_lollipop(horizontal = T, size = 1, show.legend = FALSE) +
  scale_color_gradient2(low = 'red', mid = 'white', 'high' = 'blue') +
  theme_bw(15) +
  labs(title = "Leader functions PC 1", x = "\nLoading", y = "")

functions_component1_plot

functions_component2_plot <-
  ggplot(logpca_model_functions_loadings, aes(X2, fct_reorder(variable, X2), colour=X2)) +
  ggalt::geom_lollipop(horizontal = T, size = 1, show.legend = FALSE) +
  scale_color_gradient2(low = 'red', mid = 'white', 'high' = 'blue') +
  theme_bw(15) +
  labs(title = "Leader functions PC 2", x = "\nLoading", y = "")

functions_component2_plot

functions_component1_plot + functions_component2_plot

# ## Component 2
# loadings_X2<-loadings[loadings$Component=="X2",]
# X2_sort<-loadings_X2$variable[order(loadings_X2$Loading)]
# loadings_X2$variable<-factor(loadings_X2$variable, levels =  X2_sort)
# 
# functions_component2_plot<-ggplot(loadings_X2, aes(Loading, variable)) +
#   geom_point(aes(colour=Component))+
#   theme(legend.position = "none")+
#   ggtitle("Component 2")
# 
# ## Component 3
# loadings_X3<-loadings[loadings$Component=="X3",]
# X3_sort<-loadings_X3$variable[order(loadings_X3$Loading)]
# loadings_X3$variable<-factor(loadings_X3$variable, levels =  X3_sort)
# 
# functions_component3_plot<-ggplot(loadings_X3, aes(Loading, variable)) +
#   geom_point(aes(colour=Component))+
#   theme(legend.position = "none")+
#   ggtitle("Component 3")
# 
# #plot(component1_plot + component2_plot + component3_plot)
# 
# grid.arrange(functions_component1_plot, functions_component2_plot,
#              functions_component3_plot, nrow=1)
# 



# PCA Qualities & Functions -----------------------------------------------

# Logistic PCA on leader functions and qualities

#Create dataframe of variables for logistic PCA of qualities
pca_data_FQ<-leader_text2[,c(function_vars, quality_vars, "cs_textrec_ID", "group.structure2")]
#Remove -1s for now
pca_data_FQ[function_vars==-1]<-0
pca_data_FQ[quality_vars==-1]<-0

#Remove rows with all 0s
pca_data_FQ<-pca_data_FQ[rowSums(pca_data_FQ[function_vars])>0,]

# Add subsistence category and other vars in pca_data_qualities
pca_data_FQ <- left_join(pca_data_FQ, text_doc_cultureIDs, by = "cs_textrec_ID")

pca_data_FQ <-left_join(pca_data_FQ, culture_vars, by = "d_culture")


#Fit the SVD 
logsvd_model_qf = logisticSVD(pca_data_FQ[,c(function_vars, quality_vars)], k = k)
logsvd_model_qf

#Cross validate optimal m
logpca_cv_qf = cv.lpca(pca_data_FQ[,c(function_vars, quality_vars)], ks = k, ms = 1:10)
plot(logpca_cv_qf)


logpca_model_qf = logisticPCA(pca_data_FQ[,c(function_vars, quality_vars)], k = k, m = which.min(logpca_cv_qf), main_effects = T)
#clogpca_model = convexLogisticPCA(pca_data_FQ[,c(function_vars, quality_vars)], k = k, m = which.min(logpca_cv_qf))

#Plots

plot(logpca_model_qf, type = "trace")
#plot(clogpca_model, type = "trace")
plot(logsvd_model_qf, type = "trace")


plot(logsvd_model_qf, type = "scores")+ 
  geom_point(aes(colour=pca_data_FQ$subsistence)) + 
  ggtitle("Exponential Family PCA") +
  scale_colour_brewer(palette = "Set1")

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


# plot(clogpca_model_qf, type = "scores") + 
#   geom_point(aes(colour=pca_data_FQ$group.structure2)) + 
#   ggtitle("Convex Logistic PCA")+
#   scale_colour_brewer(palette = "Set1")


# Associate logistic PCA model with variables
logpca_model_loadings<-data.frame(logpca_model_qf$U)
logpca_model_loadings$variable<-names(pca_data_FQ[,c(function_vars, quality_vars)])

loadings<- gather(logpca_model_loadings, key = Component, value =  Loading, -variable)

ggplot(loadings, aes(Loading, variable)) +
  geom_point(aes(colour=Component)) +
  facet_wrap(vars(Component))

# Plot individual components
## Component 1
loadings_X1<-loadings[loadings$Component=="X1",]
X1_sort<-loadings_X1$variable[order(loadings_X1$Loading)]
loadings_X1$variable<-factor(loadings_X1$variable, levels =  X1_sort)

qf_component1_plot<-ggplot(loadings_X1, aes(Loading, variable)) +
  geom_point(aes(colour=Component))+
  theme(legend.position = "none")+
  ggtitle("Component 1")

## Component 2
loadings_X2<-loadings[loadings$Component=="X2",]
X2_sort<-loadings_X2$variable[order(loadings_X2$Loading)]
loadings_X2$variable<-factor(loadings_X2$variable, levels =  X2_sort)

qf_component2_plot<-ggplot(loadings_X2, aes(Loading, variable)) +
  geom_point(aes(colour=Component))+
  theme(legend.position = "none")+
  ggtitle("Component 2")

## Component 3
loadings_X3<-loadings[loadings$Component=="X3",]
X3_sort<-loadings_X3$variable[order(loadings_X3$Loading)]
loadings_X3$variable<-factor(loadings_X3$variable, levels =  X3_sort)

qf_component3_plot<-ggplot(loadings_X3, aes(Loading, variable)) +
  geom_point(aes(colour=Component))+
  theme(legend.position = "none")+
  ggtitle("Component 3")


#plot(component1_plot + component2_plot + component3_plot)
grid.arrange(qf_component1_plot, qf_component2_plot, qf_component3_plot,nrow=1)


# Add components to DF ----------------------------------------------------

pca_data_qualities$qualities_component1 <- logpca_model_qualities$PCs[,1]
pca_data_functions$functions_component1 <- logpca_model_functions$PCs[,1]
pca_data_FQ$qf_component1 <- logpca_model_qf$PCs[,1]

components_data<- left_join(pca_data_qualities, pca_data_functions, by = "cs_textrec_ID")
components_data <- left_join(components_data, pca_data_FQ, by = "cs_textrec_ID")

components_data <- components_data[,c("cs_textrec_ID","qualities_component1",
                                      "functions_component1", 'qf_component1', "subsistence")]

leader_text2<-left_join(leader_text2, components_data, by="cs_textrec_ID")


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
  qualities_component1 ~ 
    #functions_component1 +
    subsistence +
    c_cultural_complexity +
    #pop_density +
    com_size +
    group.structure2 +
    #warfare_freq +
    (1|d_culture/doc_ID),
  data=leader_text2
  )
summary(qc_m2)
Anova(qc_m2)
AIC(qc_m2)
vif(qc_m2)
visreg(qc_m2)

plot(allEffects(qc_m2))
# visreg(qc_m2, by = 'c_cultural_complexity', xvar = 'pop_density')
# visreg(qc_m2, by = 'group.structure2', xvar = 'warfare_freq')


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
    c_cultural_complexity +
    #pop_density +
    com_size +
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


## Rstanarm models

options(mc.cores = parallel::detectCores())

# Qualities model

qc_stan_m <- stan_glmer(
  qualities_component1 ~ 
    subsistence +
    c_cultural_complexity +
    pop_density +
    com_size +
    group.structure2 +
    (1|d_culture/doc_ID),
  prior = normal(0, 1),
  prior_intercept = normal(0, 1),
  algorithm = c("sampling"),
  family = gaussian(link = "identity"),
  iter=4000,
  data=leader_text2
)

prior_summary(qc_stan_m)
summary(qc_stan_m,
        pars = c(),
        probs = c(0.025, 0.975),
        digits = 2)

median(bayes_R2(qc_stan_m))

qc_m_post <- as.matrix(qc_stan_m)
prior_summary(qc_stan_m)

# Community size and pop density distributions
mcmc_areas(qc_m_post[,c(7:10)], prob_outer = .95)

# Group structure distritions

mcmc_areas(qc_m_post[,c(7:14)], 
           prob_outer = .95,
           point_est = "mean"
           )

# All effects
mcmc_intervals(qc_m_post[,c(2:20)])

launch_shinystan(qc_stan_m)
#mcmc_trace(qc_m_post[,c(1:5)])

# ggpubr plots
qc_m_post_df<-data.frame(qc_m_post)
ggdensity(qc_m_post_df, x="c_cultural_complexity", fill = "lightgrey",
          add = "mean", rug=T)


# tidybase plots
qc_m_post_df_plot<-qc_m_post_df[,c(2:14)]
qc_m_post_df_plot_long<-gather(qc_m_post_df_plot, variable, value)

theme_set(theme_tidybayes())
qc_bayes_model_post_plot <- qc_m_post_df_plot_long %>% 
  ggplot(aes(x=value, y=variable))+
  geom_halfeyeh(trim = TRUE)+
  vline_0() +
  xlim(-10,10) +
  scale_y_discrete(labels=rev(c("Subsistence:Pastoralists",
                            "Subsistence:Mixed",
                            "Subsistence:Hunter-gatherers",
                            "Subsistence:Horticulturalists",
                            "Population density",
                            "Group:State-level",
                            "Group:Social",
                            "Group:Religeous",
                            "Group:Political",
                            "Group:Other",
                            "Goupr:Military",
                            "Community size",
                            "Cultural complexity")))+
  labs(x="\nPosterior distribution", y="Covariate\n")
  

qc_bayes_model_post_plot

#Full tidybayes method

ggplot(qc_bayes_model_post_plot, aes(y = variable, x = value)) +
  geom_halfeyeh()



# Functions model
fc_stan_m <- stan_glmer(
  functions_component1 ~ 
    subsistence +
    c_cultural_complexity +
    pop_density2 +
    com_size2 +
    group.structure2 +
    (1|d_culture/doc_ID),
  prior_intercept = normal(0,1),
  prior = normal(0,1),
  algorithm = c("sampling"),
  family = gaussian(link = "identity"),
  iter=4000,
  data=leader_text2
)

summary(fc_stan_m)
median(bayes_R2(fc_stan_m))

fc_m_post <- as.matrix(fc_stan_m)

# Community size distributions
mcmc_areas(fc_m_post[,c(14:17)], prob_outer = .95)

# Group structure distributions
mcmc_areas(fc_m_post[,c(14:19)], prob_outer = .95)

mcmc_intervals(fc_m_post[,c(2:14)])
#launch_shinystan(fc_stan_m)

# Stan models




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



# Save RData envrionment --------------------------------------------------
view(leadershipdata::documents)

save.image(file = "Leader2.Rdata")




