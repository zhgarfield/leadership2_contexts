# NMF ---------------------------------------------------------------------

library(NMF)
library(tidyverse)
load('Leader2.Rdata')

df_all <-
  leader_text2 %>% 
  dplyr::select(all_of(all_study_vars)) %>% 
  dplyr::filter(rowSums(.) > 0)

m_nmf <- nmf(t(df_all), rank = 2:15)
m_nmfrandom <- nmf(randomize(t(df_all)), rank=2:15)

pdf(file = 'consensusmap.pdf', width = 20)
consensusmap(m_nmf)
dev.off()

save(m_nmf, m_nmfrandom, file = 'NMF.RData')
# m_nmf5 <- nmf(t(pca_data_qualities2), rank = 5)

plot(m_nmf, m_nmfrandom)

library(NMF) # To run job from selection
m_nmf3 <- nmf(t(df_all), rank = 3, nrun = 100)
basismap(m_nmf3, filename = 'Figures/basismap3.pdf', hclustfun = 'ward', distfun='correlation')
coefmap(m_nmf3, hclustfun = 'ward', filename = 'Figures/coefmap3.pdf')

m_nmf4 <- nmf(t(df_all), rank = 4, nrun = 100)
basismap(m_nmf4, filename = 'Figures/basismap4.pdf', hclustfun = 'ward', distfun='correlation')
coefmap(m_nmf4, hclustfun = 'ward', distfun='correlation', filename = 'Figures/coefmap4.pdf')

save(m_nmf, m_nmfrandom, m_nmf3, m_nmf4, file = 'NMF.RData')

# plotting bases

df_basis_long <-
  m_nmf4 %>% 
  basis %>% 
  round(2) %>% # Set low values to 0
  as.data.frame %>% 
  mutate(
    Variable = rownames(.)
  ) %>% 
  gather(key = Component, value = 'Loading', -Variable) %>% 
  dplyr::filter(Loading > 0)

ggplot(df_basis_long, aes(Loading, reorder_within(Variable, Loading, Component))) +
  geom_point() +
  facet_wrap(~Component, scales = 'free_y')

# Analyzing coefs

df_coef_long <-
  m_nmf4 %>% 
  coef %>% 
  round(2) %>% # Set low values to 0
  t %>% 
  as.data.frame %>%
  rowwise() %>% 
  mutate(
    max = max(c(V1, V2, V3, V4)),
    quartile3 = quantile(c(V1, V2, V3, V4))[4]
  ) %>% 
  ungroup %>% 
  summarise_at(
    1:4,
    ~ mean(. > quartile3) # Approx. fraction of text records for each topic
  )
  
  
  
