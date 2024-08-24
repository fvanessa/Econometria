
library(wooldridge)
library(tidyverse)

# 5. O conjunto de dados BWGHT contém informações de nascimentos nos 
# Estados Unidos. # As duas variáveis de interesse são bwght (peso do 
# recém-nascido em onças) e cigs (número médio de cigarros que a mãe 
# fumou por dia durante a gravidez). Regrida bwght sobre cigs e responda 
# às seguintes perguntas:
dados = as.data.frame(wooldridge::bwght)
modelo = lm(bwght ~ cigs, dados)
#summary(modelo)

# a) Qual é o peso do nascimento previsto quando cigs = 0? 
# E quando cigs = 20?
novos_dados = data.frame(cigs = c(0, 20))
previsoes = predict(modelo, novos_dados)

cat("O peso previsto do recém-nascido quando cigs = 0 é", round(previsoes[1],2), 
"e quando cigs = 20 é", round(previsoes[2],2), "\n")

# b) O MRLS necessariamente captura uma relação causal entre o peso do 
# nascimento da criança e os hábitos de fumar da mãe?
  # Não. Provavelmente existem outros fatores que influenciam no peso do 
  # recém-nascido que não foram incluídos no modelo.

# c) Para prever um peso de nascimento de 125 onças, qual deveria ser 
# a magnitude de cigs?
cigs_calculado = (125 - modelo$coefficients[[1]]) / modelo$coefficients[[2]]

cat("Para prever um peso de nascimento de 125 onças,",
"a magnitude de cigs deveria ser aproximadamente", 
round(cigs_calculado,2), "\n")

prev = predict(modelo, data.frame(cigs = c(cigs_calculado)))

cat("Veja que de fato quando cigs =", paste0(round(cigs_calculado,2), ","), 
"o peso previsto do recém-nascido é", paste0(prev, "."),
"Entretanto, fumar um número negativo de cigarros não é possível.", "\n")

# d) Verifique qual a proporção de mulheres que não fumaram durante 
# a gravidez na amostra. Sua conclusão no item anterior muda?
cigs0 = dados %>% filter(cigs == 0) %>% nrow()
prop = (cigs0 / nrow(dados)) * 100

cat("A proporção de mulheres na amostra que não fumaram durante a gravidez é de", 
paste0(round(prop,2), "%."), "Dessa forma, a conclusão no item anterior muda, pois",
"o ajuste do modelo é muito influenciado pela grande quantidade de mulheres que", 
"não fumaram durante a gravidez.")
