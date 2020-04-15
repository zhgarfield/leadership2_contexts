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

# Word treemaps

df_word_freq <-
  leader_words %>% 
  group_by(lemma) %>% 
  summarise(Frequency = n()) %>% 
  arrange(-Frequency) %>% 
  slice_head(n=50)

plot_word_freq <- 
  ggplot(df_word_freq, aes(area=Frequency, label = lemma)) +
  geom_treemap(fill='white') +
  geom_treemap_text(colour = 'black')
# plot_word_freq

# By subsistence type
df_word_freq_subsis <-
  leader_words %>% 
  left_join(leader_text2[c('cs_textrec_ID', 'subsistence')]) %>% 
  group_by(subsistence, lemma) %>% 
  summarise(Frequency = n()) %>% 
  arrange(-Frequency) %>% 
  slice_head(n=200)

plot_word_freq_subsis <- 
  ggplot(df_word_freq_subsis, aes(area=Frequency, label = lemma)) +
  geom_treemap(fill='white') +
  geom_treemap_text(colour = 'black') +
  facet_wrap(~subsistence)
# plot_word_freq_subsis

# NLP ecosystem
# http://www.bnosac.be/images/bnosac/blog/NLP-R-ecosystem.png

# Biterm
# http://www.bnosac.be/index.php/blog/98-biterm-topic-modelling-for-short-texts

library(udpipe)
library(stopwords)
library(BTM)
library(ggraph)
library(textplot)

text_records2 <-
  text_records %>% 
  dplyr::select(document_d_ID, raw_text) %>% 
  rename(
    doc_id = document_d_ID,
    text = raw_text
  ) %>% 
  mutate(
    text = str_to_lower(text),
    text = str_replace(text, "’", ""),
    text = str_replace(text, "'", ""),
    text = str_replace(text, "page", ""),
    text = str_replace(text, "\\d+", "")
  )

anno <- udpipe(text_records2, "english", trace = 10)

biterms <-
  anno %>% 
  group_by(doc_id) %>% 
  nest() %>% 
  mutate(
    cooc = map(
      data,
      ~ cooccurrence(
        x = .x$lemma,
        relevant = .x$upos %in% c("NOUN", "ADJ", "VERB") & nchar(.x$lemma) > 2 & !.x$lemma %in% stopwords("en"),
        skipgram = 3
      )
    ),
    cooc = map(cooc, as_tibble), # unnest doesn't seem to work with classes returned by cooccurrence
    data = NULL # Don't need these now
  ) %>% 
  unnest(cooc)

traindata <- subset(anno, upos %in% c("NOUN", "ADJ", "VERB") & !lemma %in% stopwords("en") & nchar(lemma) > 2)
traindata <- traindata[c("doc_id", "lemma")]
model     <- BTM(traindata, biterms = biterms, k = 20, iter = 2000, background = TRUE, trace = 100)
terms(model)


