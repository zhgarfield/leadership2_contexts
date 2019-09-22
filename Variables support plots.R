# Tabulate evidence for & against at the extract level (d1) and the culture level (d_ev2)

#Should have run first few data management code section of leader_contexts_analyses.R

#Create data frame of all variables

# Manage data -------------------------------------------------------------

authorID_textID<-leader_text_original[,c("author_ID","cs_textrec_ID")]

all_data <- left_join(leader_text2, authorID_textID)

# Remove negative values from vars
all_data[all_data==-1]<-0

#Groups of variables to model support
model_vars<-c(quality_vars, function_vars)


# Build functions ---------------------------------------------------------

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
  v = as.numeric(by(all_data, factor(all_data$d_culture), clrs2, simplify=T))
  return(sum(v==1)/length(v))
}




# Fit models --------------------------------------------------------------



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
    print(v)
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

# var_names <- c(
#   'dom_aggression' = 'Aggression',
#   'dom_assert.authority' = 'Assert authority',
#   'dom_avoid.dom' = 'Avoids dominance',
#   'dom_fear' = 'Feared',
#   'dom_fighting' = 'Fighting ability',
#   'dom_personality' = 'Dominant personality',
#   'dom_reputation' = 'Reputation',
#   'dom_strong' = 'Strong',
#   'dom_anti_aggression' = 'Anti-aggression',
#   'dom_no_coercive_authority' = 'Lack of coercion',
#   'dom_non_dominant_personality' = 'Non-dominant personality',
#   'prestige_counsel' = 'Counsel',
#   'prestige_emulated' = 'Emulated',
#   'prestige_expertise' = 'Expertise',
#   'prestige_family' = 'Family prestige',
#   'prestige_f.exp.success' = 'Expectation for success',
#   'prestige_likable' = 'Likable',
#   'prestige_respected' = 'Respected',
#   'prestige_no_family_prestige' = 'Lack of familiy prestige',
#   'prestige_not_respected' = 'Not respected',
#   'prestige_unlikeable' = 'Unlikable',
#   'neel_better.mates' = 'Better mates',
#   'neel_big.family' = 'Large family',
#   'neel_intelligence' = 'Intelligence',
#   'neel_polygynous' = 'Polygynous',
#   'hooper_performance' = 'Performance',
#   'hooper_sanction.freeriders' = 'Sanctions freeriders',
#   'hooper_payoff' = 'Payoff',
#   'hooper_group.size' = 'Leader in large group',
#   'hooper_coop.activities' = 'Leader prefered',
#   'hooper_egalitarian_large_group' = 'Leader not preffered',
#   'hooper_leaderless_large_group' = 'Leaderless large group',
#   'hooper_no_sanctioning' = 'No sanctioning')

#d_melt$Variable <- var_names[d_melt$Variable]

# the_levels <- c(d_melt$Variable[d_melt$Type=='Cultures'][order(d_melt$value[d_melt$Type=='Cultures'])], 'Female model score', 'Model score')
# d_melt$Variable = factor(d_melt$Variable, levels=the_levels)
# 
# the_models <- c('Dominance', 'Anti-dominance',
#                 'Prestige', 'Anti-prestige',
#                 'Collective action', 'Anti-collective action',
#                 'Intelligence &\n reproductive skew')
# d_melt$Model = factor(d_melt$Model, levels = the_models)
# 
# model_vars <- c(neel_vars, prest_vars, dom_vars) # Not sure what this line is for?

# Example code
# x <- leader_text[c('c_name', 'author_ID', neel_vars)]
# x$w <- length(neel_vars)
# x$prop <- rowSums(x[,3:6])/x$w

# Creating text level model score and CIs using GLMM

# Attempt to replicate from above

# Model = c()
# Score = c()
# Lower = c()
# Upper = c()
# 
# model_scores <- function(data, type){
#   for (m in models){
#     
#     model = m[[1]]
#     model_vars = m[[2]]
#     wgt = rep(length(model_vars), nrow(data))
#     
#     
#     if (sum(as.matrix(data[,model_vars])) != 0){
#       
#       m <- glmer(rowSums(data[,model_vars])/wgt ~ 1 + (1|c_name/author_ID), family = binomial, weights = wgt, data = data)
#       Model = c(Model, model)
#       Score = c(Score, logit.inv(fixef(m)[['(Intercept)']]))
#       ci=logit.inv(confint(m, method = "Wald")['(Intercept)',])
#       Lower = c(Lower, ci[[1]])
#       Upper = c(Upper, ci[[2]])
#       
#     } else {
#       
#       Model = c(Model, model)
#       Score = c(Score, 0)
#       Lower = c(Lower, 0)
#       Upper = c(Upper, 0)
#       
#     }
    
    # Model = c(Model, model)
    # Score = c(Score, logit.inv(fixef(m)[['(Intercept)']]))
    # ci=logit.inv(confint(m, method = "Wald")['(Intercept)',])
    # Lower = c(Lower, ci[[1]])
    # Upper = c(Upper, ci[[2]])
    
#   }
#   
#   data.frame(
#     Model = factor(Model, levels = the_models),
#     Variable = factor(rep(type, 7), levels = the_levels),
#     Type = rep(type, 7),
#     value = Score,
#     y_negse = Lower,
#     y_se = Upper,
#     stringsAsFactors = F
#   )
# }

# all_model_scores <- model_scores(leader_text, 'Model score')
# female_model_scores <- model_scores(leader_text[leader_text$demo_sex == 'female',], 'Female model score')

# d_melt2 <- rbind(d_melt, female_model_scores, all_model_scores)

# Plot code
plot.variable.support = ggplot(d_melt, aes(value, Variable, xmin=y_negse, xmax=y_se, colour=Type)) + 
  geom_errorbarh() + 
  geom_point() +
  #scale_x_continuous(breaks=seq(0,1,.1), labels=percent, limits=c(0,1)) +
  #scale_colour_discrete(name='', labels=c('Cultures', 'Text records', 'Female model score', 'Model score')) +
  #labs(x='\nPercent', y='') +
  facet_grid(Model~., scales = "free_y", space='free') +
  # facet_wrap(~Model, scales = "free_y") +
  theme_bw() +
  theme(strip.text.y = element_text(angle=0))
plot.variable.support

