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

m_nmf3 <- nmf(t(df_all), rank = 3)
basismap(m_nmf3, filename = 'Figures/basismap3.pdf', hclustfun = 'ward', distfun='correlation')
coefmap(m_nmf3, hclustfun = 'ward', filename = 'Figures/coefmap3.pdf')

m_nmf4 <- nmf(t(df_all), rank = 4)
basismap(m_nmf4, filename = 'Figures/basismap4.pdf', hclustfun = 'ward', distfun='correlation')
coefmap(m_nmf4, hclustfun = 'ward', filename = 'Figures/coefmap4.pdf')
