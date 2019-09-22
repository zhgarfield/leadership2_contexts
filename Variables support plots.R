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

all_data$d_culture<-factor(all_data$d_culture)
all_data<-data.frame(all_data)

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
  v = as.numeric(by(df, factor(df$d_culture), clrs2, simplify=T))
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
  "function_bestow.mate" = "Bestow mate",                        
  "function_political.appointments" = "Polistical appointments",             
  "functions_construction.infrastructure"       = "Construction/infastructure",
  "functions_control.economics"                 = "Constrol economics",
  "functions_council.member"                    = "Council member",
   "functions_group.determination"              = "Group determiniation",
   "functions_hospitality"                       = "Hospitality",
   "functions_military.command"                  = "Military command",
   "functions_new.settlement"                    =  "Movement/migration",
   "functions_prosocial.investment"              = "Prosocial investment",
   "functions_provide.counsel"                   = "Provide counsel",
   "functions_punishment"                        = "Punishment",
   "functions_serve.leader"                      = "Serve a leader",
   "functions_strategic.planning" = "Strategic planning",
   "function_organize.cooperation"  = "Organize cooperation",
   "function_resolve.conflcit"      = "Resolve conflict",
   "functions_control.calendar"     = "Control calander",
   "functions_control.immigration"  = "Control immigration",
   "functions_distribute.resources" = "Distribute resources",
   "functions_group.representative" = "Group repsenetative",
   "functions_medicinal"           = "Medicinal",
   "functions_moral.authority"     = "Moral authority",
   "functions_policymaking"        =  "Policy making",
   "functions_protection"          = "Protection",
   "functions_provide.subsistence" = "Provide subsistence",
   "functions_ritual"              = "Ritual",
   "functions_social.functions" = "Social functions",
  "qualities_artistic.performance"     = "Artistic performance",
  "qualities_generous"                 = "Generous",
  "qualities.age"                      = "Age",
   "qualities.attractive"              = "Attractive",
   "qualities.coercive.authority"      = "Coercive authority",
   "qualities.culturally.progressive"  = "Culturally progressive",
   "qualities.favorable.personality"   = "Favorable personality",
   "qualities.honest"                  = "Honest",
   "qualities.ingroup.member"          = "Ingroup member",
   "qualities.killer"                  = "Killer",
   "qualities.many.children"           = "Many children",
   "qualities.physically.strong"       = "Physically formidable",
   "qualities.prosocial"               = "Prosocial",
   "qualities.strategic.planner"       = "Strategic planner",
   "qualities_drug.consumption"     = "Drug consumption",
   "qualities_high.status"            = "High status",
   "qualities.aggressive"             = "Aggressive",
   "qualities.bravery"               = "Bravery",
   "qualities.confident"             = "Confident",
   "qualities.decisive"              = "Decisive/decision making",
   "qualities.feared"                = "Feared",
   "qualities.humble"                = "Humble",
   "qualities.innovative"            = "Innovative",
   "qualities.knowlageable.intellect" = "Knowlageable/intelligent",
   "qualities.oratory.skill"         = "Oratory skill",
   "qualities.polygynous"            = "Polygnous",
   "qualities.social.contacts"       = "Social contacts",
   "qualities.supernatural"    = "Supernatural",
"qualities_exp.accomplished"       = "Experienced/accomplished",
"qualities_wealthy"                = "Wealthy",
"qualities.ambition"               = "Ambition",
 "qualities.charisma"               = "Charisma",
 "qualities.culturally.conservative" = "Culturally conservative",
 "qualities.fairness"               = "Fairness",
 "qualities.high.quality.spouse"    = "High quality spouse",
 "qualities.industriousness"        = "Industriousness",
 "qualities.interpersonal.skills"   = "Interpersonal skills",
 "qualities.loyalty"                = "Loyalty",
 "qualities.physical.health"        = "Physical health",
 "qualities.proper.behavior"        = "Proper behavior",
 "qualities.strategic.nepotism"     = "Strategic nepotism",
 "qualities.xenophobic"   = "Xenophic"
)

d_melt$Variable <- var_names[d_melt$Variable]

the_levels <- c(d_melt$Variable[d_melt$Type=='Cultures'][order(d_melt$value[d_melt$Type=='Cultures'])])
d_melt$Variable = factor(d_melt$Variable, levels=the_levels)

# Plot code
plot.variable.support = ggplot(d_melt, aes(value, Variable, xmin=y_negse, xmax=y_se, colour=Type)) + 
  geom_errorbarh() + 
  geom_point() +
  #scale_x_continuous(breaks=seq(0,1,.1), labels=percent, limits=c(0,1)) +
  scale_colour_discrete(name='', labels=c('Cultures', 'Text records')) +
  labs(x='\nPercent', y='') +
  facet_grid(Model~., scales = "free_y", space='free') +
   facet_wrap(~Model, scales = "free_y") +
  theme_bw() +
  theme(strip.text.y = element_text(angle=0))
plot.variable.support

