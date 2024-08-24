
import wooldridge
import statsmodels.api as sm
import pandas as pd

# 5. O conjunto de dados BWGHT contém informações de nascimentos nos 
# Estados Unidos. # As duas variáveis de interesse são bwght (peso do 
# recém-nascido em onças) e cigs (número médio de cigarros que a mãe 
# fumou por dia durante a gravidez). Regrida bwght sobre cigs e responda 
# às seguintes perguntas:
dados = wooldridge.data("bwght")
preditoras = sm.add_constant(dados.cigs) #para adicionar o intercepto no modelo
modelo = sm.OLS(dados.bwght, preditoras).fit()
#print(modelo.summary())

# a) Qual é o peso do nascimento previsto quando cigs = 0? 
# E quando cigs = 20?
novos_dados = sm.add_constant(pd.DataFrame({'cigs': [0, 20]}))
previsoes = modelo.predict(novos_dados)

print("O peso previsto do recém-nascido quando cigs = 0 é", round(previsoes[0],2), 
      "e quando cigs = 20 é", round(previsoes[1],2))

# b) O MRLS necessariamente captura uma relação causal entre o peso do 
# nascimento da criança e os hábitos de fumar da mãe?
  # Não. Provavelmente existem outros fatores que influenciam no peso do 
  # recém-nascido que não foram incluídos no modelo.

# c) Para prever um peso de nascimento de 125 onças, qual deveria ser 
# a magnitude de cigs?
cigs_calculado = (125 - modelo.params.iloc[0]) / modelo.params.iloc[1]

print("Para prever um peso de nascimento de 125 onças,",
"a magnitude de cigs deveria ser aproximadamente", 
round(cigs_calculado,2))

prev = modelo.predict([1, cigs_calculado])

print(f"Veja que de fato quando cigs = {round(cigs_calculado, 2)}, "
      f"o peso previsto do recém-nascido é {prev[0]:.2f}. "
      "Entretanto, fumar um número negativo de cigarros não é possível.")

# d) Verifique qual a proporção de mulheres que não fumaram durante 
# a gravidez na amostra. Sua conclusão no item anterior muda?
cigs0 = dados[dados.cigs == 0].shape[0]
prop = (cigs0 / dados.shape[0]) * 100

print(f"A proporção de mulheres na amostra que não fumaram durante a gravidez é de {round(prop, 2)}%. "
      "Dessa forma, a conclusão no item anterior muda, pois "
      "o ajuste do modelo é muito influenciado pela grande quantidade de mulheres que "
      "não fumaram durante a gravidez.")
