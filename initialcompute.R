
# This script takes about 90 minutes to run
# Saves all objects at the end, which are 
# required for the analysis2.R script

library(conflicted)
conflict_prefer("which", "base")

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

# Create functions --------------------------------------------------------

# Converts factors to character vectors in data frame
de_factor <- function(df){
  df %>% dplyr::mutate_if(is.factor, as.character) -> df
}

# Remove rare -1's from df
neg1to0 <- function(df){
  df %>% 
    mutate_if(
      is.numeric, function(x) ifelse(x == -1, 0, x) # Remove -1's
    )
}

# Create vectors of variable names by type --------------------------------

lt2vars <- names(leader_text2)

# This includes "functions_context" which is not included in original vector
function_vars <- lt2vars[str_detect(lt2vars, 'function')]
function_vars <- function_vars[function_vars != "functions_context"]

quality_vars <- lt2vars[str_detect(lt2vars, 'qualities')]
leader_benefit_vars <- lt2vars[str_detect(lt2vars, 'leader.benefits')]
leader_cost_vars <- lt2vars[str_detect(lt2vars, 'leader.costs')]
follower_benefit_vars <- lt2vars[str_detect(lt2vars, 'follower.benefits')]
follower_cost_vars <- lt2vars[str_detect(lt2vars, 'follower.costs')]

# Aggregate at Culture level ----------------------------------------------

# df that links the ids of texts, docs, and cultures
text_doc_cultureID <-
  leader_text_original %>% 
  dplyr::select(cs_textrec_ID, doc_ID) %>% 
  left_join(documents[c('d_ID', 'd_culture')], by = c("doc_ID" = "d_ID"))

by_culture <- leader_text2 %>% 
  left_join(text_doc_cultureID, by="cs_textrec_ID") %>% 
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
  summarise_all(mean)

# Culturel level variable manipulations -----------------------------------

# Rename culture name in leader_cult data
leader_cult$d_culture<-leader_cult$c_culture_code

#Subset culture vars of interest
culture_vars<-leader_cult[c("d_culture","subsistence","c_cultural_complexity", "settlement_fixity", "pop_density","com_size")]

leader_text2 <-
  leader_text2 %>% 
  left_join(text_doc_cultureID) %>% 
  left_join(culture_vars)

# Add more SCCS variables
load("sccs.RData")
sccs<-data.frame(sccs)
sccs<-sccs[c("SCCS.","V1648")] # ID, Frequency of warfare

sccs_ehraf_ID<-distinct(read.csv("sccs_ehraf_IDs.csv")) # Some dupes in csv file
sccs_ehraf_ID$c_culture_code<-sccs_ehraf_ID$OWC

sccs_vars<-left_join(sccs, sccs_ehraf_ID[c('SCCS.', 'c_culture_code')], by = "SCCS.")
sccs_vars<-na.omit(sccs_vars) # Some eHRAF cultures do not map to SCCS cultures
sccs_vars<-sccs_vars[c("c_culture_code","V1648")]

# Convert warfare levels to interval var
sccs_vars$warfare_freq <- as.numeric(sccs_vars$V1648)
sccs_vars$warfare_freq <- ifelse(sccs_vars$warfare_freq %in% c(1,20), NA, sccs_vars$warfare_freq)
sccs_vars$warfare_freq <- 19 - sccs_vars$warfare_freq

leader_text2<-left_join(leader_text2, sccs_vars, by=c("d_culture"="c_culture_code"))

# Variable support plot data prep --------------------------------------------------

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
  "qualities.knowlageable.intellect" = "Knowledgeable/intelligent",
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
  "qualities.xenophobic"   = "Xenophobia"
)

d_melt$Variable <- var_names[d_melt$Variable]

the_levels <- c(d_melt$Variable[d_melt$Type=='Cultures'][order(d_melt$value[d_melt$Type=='Cultures'])])
d_melt$Variable = factor(d_melt$Variable, levels=the_levels)

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

# Prepare data for cluster and PCA analyses -------------------------------

culture_level_vars <- c(
  "cs_textrec_ID",
  "doc_ID",
  "d_culture",
  "group.structure2", 
  "subsistence",
  "c_cultural_complexity",
  "settlement_fixity",
  "pop_density",
  "com_size",
  "warfare_freq"
)

# Qualities data frame

pca_data_qualities <- leader_text2[c(quality_vars, culture_level_vars)]
pca_data_qualities <- neg1to0(pca_data_qualities)

#Remove rows with all 0s
pca_data_qualities <- pca_data_qualities[rowSums(pca_data_qualities[quality_vars])>0,]

# Fix names
pca_data_qualities2 <- pca_data_qualities[quality_vars]
names(pca_data_qualities2) <- var_names[names(pca_data_qualities2)]

# Functions data frame

pca_data_functions <- leader_text2[c(function_vars, culture_level_vars)]
pca_data_functions <- neg1to0(pca_data_functions)

#Remove rows with all 0s
pca_data_functions <- pca_data_functions[rowSums(pca_data_functions[function_vars])>0,]

# Fix names
pca_data_functions2 <- pca_data_functions[function_vars]
names(pca_data_functions2) <- var_names[names(pca_data_functions2)]

# Functions and qualities

#Create dataframe of variables for logistic PCA of qualities
pca_data_FQ <- leader_text2[c(function_vars, quality_vars, culture_level_vars)]

#Remove -1s for now
pca_data_FQ <- neg1to0(pca_data_FQ)

#Remove rows with all 0s
pca_data_FQ <- pca_data_FQ[rowSums(pca_data_FQ[function_vars])>0,]

pca_data_FQ2 <- pca_data_FQ[c(quality_vars, function_vars)]
names(pca_data_FQ2) <- var_names[names(pca_data_FQ2)]

# Cluster analysis --------------------------------------------------------

m_pvclust_qual <- pvclust(
  pca_data_qualities2, 
  method.hclust = 'ward', 
  method.dist = 'correlation', 
  nboot = 10000,
  parallel = T
)

m_pvclust_fun <- 
  pvclust(
    pca_data_functions2, 
    method.hclust = 'ward', 
    method.dist = 'correlation', 
    nboot = 10000,
    parallel = T
  )

qual_func_vars <- leader_text2 %>% 
  select(matches("functions|qualities")) %>% 
  select(-functions_context) %>% 
  select(-contains("component"))

qual_func_clust <- pvclust(
  qual_func_vars, 
  method.hclust = 'ward', 
  method.dist = 'correlation', 
  nboot = 10000,
  parallel = T)

# Cross-validation for logisticPCA ----------------------------------------

# This takes a long time, so putting all code in one section

# Leader qualities
qual_cvlpca <- cv.lpca(pca_data_qualities2, ks = 1:20, ms = 5:15)
plot(qual_cvlpca)
which.min(qual_cvlpca[9,]) # optimal? k=9, m=11

# Plot all minima
x <- apply(qual_cvlpca, MARGIN = 1, which.min)
plot(1:20, qual_cvlpca[cbind(1:20, x)], type='l') # elbows at 9 & 14

kq <- 9
mq <- 11

# Assuming k=2, cross validate for optimal m
qual_cvlpcak2 <- cv.lpca(pca_data_qualities2, ks = 2, ms = 1:10)
plot(qual_cvlpcak2)
which.min(qual_cvlpcak2) # m = 7

# Leader functions

# Optimal k, m
fun_cvlpca = cv.lpca(pca_data_functions2, ks = 1:20, ms = 5:15)
plot(fun_cvlpca)

x <- apply(fun_cvlpca, MARGIN = 1, which.min)
plot(1:20, fun_cvlpca[cbind(1:20, x)], type='l') # elbows at 5, 10, 15

# Optimal values?
kf <- 10
which.min(fun_cvlpca[10,])
mf <- 13 

# For two PCs only (k=2), cv for optimal m
m_lpca_funk2cv <-  cv.lpca(pca_data_functions2, ks = 2, ms = 1:10)
plot(m_lpca_funk2cv)
which.min(m_lpca_funk2cv)

# Qualities and functions

logpca_cv_qf = cv.lpca(pca_data_FQ[c(function_vars, quality_vars)], ks = 1:20, ms = 5:15)
plot(logpca_cv_qf)
x <- apply(logpca_cv_qf, MARGIN = 1, which.min)
plot(1:20, logpca_cv_qf[cbind(1:20, x)], type='l') # elbows at 5, 16

kqf <- 16
mqf <- 10

# For k=2 only
logpca_cv_qfk2 = cv.lpca(pca_data_FQ[c(function_vars, quality_vars)], ks = 2, ms = 1:10)
plot(logpca_cv_qfk2)
which.min(logpca_cv_qfk2)

# Save all objects --------------------------------------------------

save(list=ls(), file = "Leader2.Rdata")
