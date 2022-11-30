pacotes <- c("wordcloud2","tidyverse","tm","topicmodels","dplyr","ggplot2",
            "tidytext","textstem", "writexl")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Criando o Corpus
ba_corpus <- Corpus(VectorSource(ba_1000_clean))
floripa_corpus <- Corpus(VectorSource(floripa_1000_clean))
stgo_corpus <- Corpus(VectorSource(stgo_1000_clean))

#Removendo HTML
remover_html <- function(texto){
  texto = gsub(pattern = '<.+//">', '', texto)
  texto = gsub(pattern = '</.+>', '', texto)
  texto = gsub("@\\w+", " ", texto)
  texto = gsub("<.*?>", "", texto)
  texto = gsub("http[^[:space:]]*", "", texto)
  return(texto)
}

#limpando o corpus
ba_corpus = ba_corpus %>% 
  tm_map(content_transformer(remover_html)) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeWords, stopwords('spanish'))

floripa_corpus = floripa_corpus %>% 
  tm_map(content_transformer(remover_html)) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeWords, stopwords('portuguese'))

stgo_corpus = stgo_corpus %>% 
  tm_map(content_transformer(remover_html)) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeWords, stopwords('spanish'))

#Fazendo o stemming  
ba_corpus_stem <- tm_map(ba_corpus, stemDocument, language = 'spanish') 
floripa_corpus_stem <- tm_map(floripa_corpus, stemDocument, language = 'portuguese') 
stgo_corpus_stem <- tm_map(stgo_corpus, stemDocument, language = 'spanish') 

#Criando uma Document Term Matrix e Dataframe
dtm_ba <- TermDocumentMatrix(ba_corpus_stem) %>% 
  as.matrix()
words = sort(rowSums(dtm_ba), decreasing = TRUE)
df_ba = data.frame(word = names(words), freq = words)

dtm_floripa <- TermDocumentMatrix(floripa_corpus_stem) %>% 
  as.matrix()
words = sort(rowSums(dtm_floripa), decreasing = TRUE)
df_floripa = data.frame(word = names(words), freq = words)

dtm_stgo <- TermDocumentMatrix(stgo_corpus_stem) %>% 
  as.matrix()
words = sort(rowSums(dtm_stgo), decreasing = TRUE)
df_stgo = data.frame(word = names(words), freq = words)

#Eliminando palavras especificas para as nuvens
df_floripa = df_floripa %>% 
  filter(nchar(as.character(word)) > 2,
         word != "rnrno")

#observando as nuvens

wordcloud2(df_ba)
wordcloud2(df_floripa)
wordcloud2(df_stgo)

#Criando o modelo LDA
DTM_ba<-DocumentTermMatrix(ba_corpus_stem)
DTM_floripa<-DocumentTermMatrix(floripa_corpus_stem)
DTM_santiago<-DocumentTermMatrix(stgo_corpus_stem)

Model_lda_ba<-LDA(DTM_ba, k=5,control=list(seed=1234))
Model_lda_ba
Model_lda_floripa<-LDA(DTM_floripa, k=5,control=list(seed=1234))
Model_lda_floripa
Model_lda_santiago<-LDA(DTM_santiago, k=5,control=list(seed=1234))
Model_lda_santiago

beta_topics_ba<-tidy(Model_lda_ba, matrix ="beta")
beta_topics_ba
beta_topics_floripa<-tidy(Model_lda_floripa, matrix ="beta")
beta_topics_floripa
beta_topics_santiago<-tidy(Model_lda_santiago, matrix ="beta")
beta_topics_santiago

#Grouping the terms by topic
beta_top_terms_ba <- beta_topics_ba %>%
  group_by(topic) %>%
  slice_max(beta, n = 8) %>% 
  ungroup() %>%
  arrange(topic, -beta)

beta_top_terms_floripa <- beta_topics_floripa %>%
  group_by(topic) %>%
  slice_max(beta, n = 8) %>% 
  ungroup() %>%
  arrange(topic, -beta)

beta_top_terms_santiago <- beta_topics_santiago %>%
  group_by(topic) %>%
  slice_max(beta, n = 8) %>% 
  ungroup() %>%
  arrange(topic, -beta)

#Display the grouped terms on the charts 
beta_top_terms_ba %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

beta_top_terms_floripa %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

beta_top_terms_santiago %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

