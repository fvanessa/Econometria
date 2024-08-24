
using WooldridgeDatasets
using DataFrames
using GLM

# 5. O conjunto de dados BWGHT contém informações de nascimentos nos 
# Estados Unidos. # As duas variáveis de interesse são bwght (peso do 
# recém-nascido em onças) e cigs (número médio de cigarros que a mãe 
# fumou por dia durante a gravidez). Regrida bwght sobre cigs e responda 
# às seguintes perguntas:

dados = DataFrame(wooldridge("bwght"))

modelo = lm(@formula(bwght ~ cigs), dados)

# a) Qual é o peso do nascimento previsto quando cigs = 0? 
# E quando cigs = 20?
novos_dados = DataFrame(cigs = [0, 20])
previsoes = predict(modelo, novos_dados)

println("O peso previsto do recém-nascido quando cigs = 0 é ", 
round(previsoes[1], digits=2), 
" e quando cigs = 20 é ", round(previsoes[2], digits=2), "\n")

# b) O MRLS necessariamente captura uma relação causal entre o peso do 
# nascimento da criança e os hábitos de fumar da mãe?
  # Não. Provavelmente existem outros fatores que influenciam no peso do 
  # recém-nascido que não foram incluídos no modelo.

# c) Para prever um peso de nascimento de 125 onças, qual deveria ser 
# a magnitude de cigs?
cigs_calculado = (125 - coef(modelo)[1]) / coef(modelo)[2]

println("Para prever um peso de nascimento de 125 onças,",
"a magnitude de cigs deveria ser aproximadamente ", 
round(cigs_calculado, digits=2))

prev = predict(modelo, DataFrame(cigs = cigs_calculado))

println("Veja que de fato quando cigs = ", round(cigs_calculado, digits=2), 
" o peso previsto do recém-nascido é ", prev[1], ". Entretanto, fumar um número",
" negativo de cigarros não é possível.")

# d) Verifique qual a proporção de mulheres que não fumaram durante 
# a gravidez na amostra. Sua conclusão no item anterior muda?
cigs0 = nrow(filter(row -> row.cigs == 0, dados))
prop = (cigs0 / nrow(dados)) * 100

println("A proporção de mulheres na amostra que não fumaram durante a gravidez é de ", 
round(prop, digits=2), "%. ", "Dessa forma, a conclusão no item anterior muda, pois ",
"o ajuste do modelo é muito influenciado pela grande quantidade de mulheres que ", 
"não fumaram durante a gravidez.")
