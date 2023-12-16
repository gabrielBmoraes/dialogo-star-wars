# imports ----
install.packages("syuzhet")
library(syuzhet)

install.packages("wordcloud")
library(wordcloud)

install.packages("stringr")
library(stringr)

install.packages("tm")
library(tm)

# análise dialogo - personagens ----
caminho = getwd()

episodioIV <- read.csv(paste(caminho, "raw/SW_EpisodeIV_enUS.txt", sep = ""), sep="")
episodioV <- read.csv(paste(caminho, "raw/SW_EpisodeV_enUS.txt", sep = ""), sep="")
episodioVI <- read.csv(paste(caminho, "raw/SW_EpisodeVI_enUS.txt", sep = ""), sep="")

todosEpisodios <- rbind(episodioIV,episodioV)

todosPersonagens <- table(todosEpisodios$character)

df_pers <- data.frame(todosPersonagens)

View(df_pers)

df_pers <- df_pers[order(df_pers$Freq, decreasing = TRUE),]

pers_prin = df_pers$Var1[1:5]

df = subset.data.frame(todosEpisodios, character == pers_prin[1])
2:length(pers_prin)

for(i in 2:length(pers_prin)){
  df <- rbind(df, subset.data.frame(todosEpisodios, character == pers_prin[i]))
}

barplot(table(df$character))

# detecção de emoções ----

texto1 <- tolower(readLines("C:/Users/rafae/OneDrive/Documentos/dialogo-star-wars/raw/SW_EpisodeIV_enUS.txt", warn = FALSE))
texto2 <- tolower(readLines("C:/Users/rafae/OneDrive/Documentos/dialogo-star-wars/raw/SW_EpisodeV_enUS.txt", warn = FALSE))
texto3 <- tolower(readLines("C:/Users/rafae/OneDrive/Documentos/dialogo-star-wars/raw/SW_EpisodeVI_enUS.txt", warn = FALSE))

dialogos <- c(texto1, texto2, texto3)

falas <- str_match(dialogos, "\"[0-9]+\" \"[a-z]+\" \"(.*)\"")[, 2]
falas <- gsub("\"", "", falas)
falas <- falas[!is.na(falas)]

# Corpus
corpus <- Corpus(VectorSource(falas))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)

sentimentos <- get_sentiment(falas, method = "syuzhet")

sentimentos[is.na(sentimentos)] <- 0

sentimentos <- sentimentos[!is.na(sentimentos) & sentimentos != 0]

limiar_positivo <- 0.5
limiar_negativo <- -0.5

rotulos_sentimento <- ifelse(sentimentos > limiar_positivo, "Positivo",
                             ifelse(sentimentos < limiar_negativo, "Negativo", NA))


rotulos_sentimento <- rotulos_sentimento[!is.na(rotulos_sentimento)]

# Gráfico de conotação das palavras - Positivo x Negativo
barplot(table(rotulos_sentimento))

# Wordcloud das falas
wordcloud(words = unlist(corpus),
          min.freq = 20, 
          scale = c(3, 0.5), 
          colors = brewer.pal(8, "Dark2"),
          random.order = FALSE)

