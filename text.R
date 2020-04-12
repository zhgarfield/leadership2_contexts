library(tidytext)
library(topicmodels)
library(ldatuning)

terms <-
  leader_words %>% 
  group_by(cs_textrec_ID, lemma) %>% 
  summarise(value = n())

dtm <- cast_dtm(terms, 'cs_textrec_ID', 'lemma', 'value')

lda_topicsnum <- FindTopicsNumber(
  dtm,
  topics = 2:15,
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  # control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(lda_topicsnum)

m_lda4 <- LDA(dtm, 8, method = "Gibbs")

top_terms <- 
  m_lda4 %>%
  tidy %>% 
  dplyr::filter(! term %in% c('chief', 'leader')) %>% 
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# top_terms

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term)) +
  geom_bar(stat = "identity") +
  scale_y_reordered() +
  facet_wrap(~ topic, scales = "free_y")
 