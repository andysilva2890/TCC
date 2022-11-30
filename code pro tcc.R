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
  tm_map(removeWords, stopwords('spanish')) %>% 
  tm_map(removeWords, c("todo", "toda", "ciudad", 
                        "hora", "nuevo", "nueva",
                        "debajo", "vivamoscultura","dia", "rnrn"))

floripa_corpus = floripa_corpus %>% 
  tm_map(content_transformer(remover_html)) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeWords, stopwords('portuguese')) %>% 
  tm_map(removeWords, c("florianópolis", "floripa", "prefeitura", 
                                   "vai", "chega", "ainda", "rnrn",
                                   "semana", "rnrna","rnrno", "nesta",
                                   "neste", "pra", "dos",
                                   "das", "sábado", "domingo",
                                   "florianopolis", "..."))

stgo_corpus = stgo_corpus %>% 
  tm_map(content_transformer(remover_html)) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeWords, stopwords('spanish')) %>% 
  tm_map(removeWords, c("hrs", "espacio", "dia", "alcadesa", "comuna",
                        "calle", "semana", "ahora",
                        "toda", "todo", "vivamosbiensantiago", "rnrnvivamosbiensantiag", "vecinos", "vecinas", "rnrn"))

#Fazendo a lematizacao  
ba_corpus_lem <- tm_map(ba_corpus, lemmatize_strings, language = 'spanish') 
floripa_corpus_lem <- tm_map(floripa_corpus, lemmatize_strings, language = 'portuguese') 
stgo_corpus_lem <- tm_map(stgo_corpus, lemmatize_strings, language = 'spanish') 

#Criando uma Document Term Matrix e Dataframe
dtm_ba <- TermDocumentMatrix(ba_corpus_lem) %>% 
  as.matrix()
words_ba = sort(rowSums(dtm_ba), decreasing = TRUE)
df_ba = data.frame(word = names(words_ba), freq = words_ba)

dtm_floripa <- TermDocumentMatrix(floripa_corpus_lem) %>% 
  as.matrix()
words_floripa = sort(rowSums(dtm_floripa), decreasing = TRUE)
df_floripa = data.frame(word = names(words_floripa), freq = words_floripa)

dtm_stgo <- TermDocumentMatrix(stgo_corpus_lem) %>% 
  as.matrix()
words_stgo = sort(rowSums(dtm_stgo), decreasing = TRUE)
df_stgo = data.frame(word = names(words_stgo), freq = words_stgo)


#observando as nuvens

wordcloud2(df_ba)
wordcloud2(df_floripa)
wordcloud2(df_stgo)

#Criando o modelo LDA
DTM_ba<-DocumentTermMatrix(ba_corpus_lem)
DTM_floripa<-DocumentTermMatrix(floripa_corpus_lem)
DTM_santiago<-DocumentTermMatrix(stgo_corpus_lem)

Model_lda_ba<-LDA(DTM_ba, k=3,control=list(seed=1234))
Model_lda_ba
Model_lda_floripa<-LDA(DTM_floripa, k=4,control=list(seed=1234))
Model_lda_floripa
Model_lda_santiago<-LDA(DTM_santiago, k=4,control=list(seed=1234))
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
  slice_max(beta, n = 12) %>% 
  ungroup() %>%
  arrange(topic, -beta)

beta_top_terms_floripa <- beta_topics_floripa %>%
  group_by(topic) %>%
  slice_max(beta, n = 12) %>% 
  ungroup() %>%
  arrange(topic, -beta)

beta_top_terms_santiago <- beta_topics_santiago %>%
  group_by(topic) %>%
  slice_max(beta, n = 12) %>% 
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

# contagem do numero de palavras usadas (top 50)
top_50_ba <- as.data.frame(dtm_ba)
colnames(top_50_ba) <-  c('freq')
top_50_ba[order(freq)]
write_xlsx(top_50_ba,"C:\\Users\\andys\\Desktop\\TCC MBA\\file name.xlsx")

top_50_fl <- as.data.frame(dtm_floripa)
colnames(top_50_fl) <-  c('freq')
top_50_fl[order(freq)]
write_xlsx(top_50_fl,"C:\\Users\\andys\\Desktop\\TCC MBA\\floripa.xlsx")

top_50_stgo <- as.data.frame(dtm_stgo)
colnames(top_50_stgo) <-  c('freq')
top_50_fl[order(freq)]
write_xlsx(top_50_fl,"C:\\Users\\andys\\Desktop\\TCC MBA\\floripa.xlsx")

