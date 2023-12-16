# imports ----
install.packages("syuzhet")
library(syuzhet)

install.packages("wordcloud")
library(wordcloud)

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

dialogos <- 