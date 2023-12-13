
install.packages("syuzhet")
library(syuzhet)

episodioIV <- read.csv("~/Documentos/projeto-star-wars-1CCO/raw/SW_EpisodeIV_enUS.txt", sep="")
episodioV <- read.csv("~/Documentos/projeto-star-wars-1CCO/raw/SW_EpisodeV_enUS.txt", sep="")
episodioVI <- read.csv("~/Documentos/projeto-star-wars-1CCO/raw/SW_EpisodeVI_enUS.txt", sep="")

todosEpisodios <- rbind(episodioIV,episodioV,episodioVI)

View(episodioIV)

todosPersonagens <- table(todosEpisodios$personagem)

df_pers <- data.frame(todosPersonagens)

View(df_pers)

df_pers <- df_pers[order(df_pers$Freq, decreasing = TRUE),]
typeof(df_pers)

pers_prin = df_pers$Var1[1:5]

pers_prin

df = subset.data.frame(todosEpisodios, personagem == pers_prin[1])
2:length(pers_prin)

for(i in 2:length(pers_prin)){
  df <- rbind(df, subset.data.frame(todosEpisodios, personagem == pers_prin[i]))
}

barplot(table(df$personagem))

#DETECÇÂO DE EMOÇÔES

