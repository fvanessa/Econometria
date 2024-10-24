
import wooldridge as woo
import statsmodels.formula.api as smf
import statsmodels.api as sm
import numpy as np

# 1. Utilize o dataset ceosal1 do pacote wooldridge e ajuste o modelo
# log(wage) = β0 + β1log(sales) + β2roe + β3rosneg + u,
# em que rosneg é uma dummy que é igual a um se ros < 0 e zero caso contrário. 
# Aplique o teste RESET e verifique se existe alguma evidência de má-especificação
# da forma funcional da equação. (ros: retorno sobre o capital das empresas)

dados1 = woo.data("ceosal1")
modelo1 = smf.ols(formula = 'np.log(salary) ~ np.log(sales) + roe + ros<0', data = dados1).fit()
modelo1_expandido = smf.ols(formula = 'np.log(salary) ~ np.log(sales) + roe + ros<0 + I(modelo1.fittedvalues**2) + I(modelo1.fittedvalues**3)', data = dados1).fit()
sm.stats.anova_lm(modelo1, modelo1_expandido)
# Não rejeitamos H0, ou seja, não temos evidência de que há má-especificação de forma funcional



# 2. Você e seu colega estão trabalhando com o dataset hprice1 do pacote wooldridge.
# Depois de várias análises, chegam nos seguintes modelos:
# Modelo 1: log(price) = β0 + β1lotsize + β2sqrft + β3bdrms + u.
# Modelo 2: log(price) = β0 + β1log(lotsize) + β2log(sqrft) + β3bdrms + u.
# Você e seu colega querem saber se algum dos modelos está mal-especificado. Utilize o teste
# de Davidson-MacKinnon para saber se existe ou não má-especificação da forma funcional em
# alguns dos modelos. Preferiria algum modelo frente ao outro? Compare os resultados com os
# obtidos em sala de aula com o teste de Mizon e Richard.

dados2 = woo.data("hprice1")
modelo2_a = smf.ols(formula = 'np.log(price) ~ lotsize + sqrft + bdrms', data = dados2).fit()
modelo2_b = smf.ols(formula = 'np.log(price) ~ np.log(lotsize) + np.log(sqrft) + bdrms', data = dados2).fit()

modelo2_a_auxiliar = smf.ols(formula = 'np.log(price) ~ lotsize + sqrft + bdrms + modelo2_b.fittedvalues', data = dados2).fit()
modelo2_a_auxiliar.pvalues["modelo2_b.fittedvalues"]
# p-valor < 0.05, rejeitamos H0 de que o coeficiente associado a modelo2_b.fittedvalues é zero
modelo2_b_auxiliar = smf.ols(formula = 'np.log(price) ~ np.log(lotsize) + np.log(sqrft) + bdrms + modelo2_a.fittedvalues', data = dados2).fit()
modelo2_b_auxiliar.pvalues["modelo2_a.fittedvalues"]
# p-valor > 0.05, não rejeitamos H0 de que o coeficiente associado a modelo2_a.fittedvalues é zero 
# Portanto, o modelo 1 está mal espeficicado e preferimos o modelo 2 (da mesma forma que foi concluído em aula)



# 3. Seja o modelo de interesse:
# log(wage) = β0 + β1educ + β2exper + β3tenure + β4married +
# β5south + β6urban + β7black + β8abil + β9educ ∗ abil + u.
# Contudo, abil é não observada e precisamos utilizar alguma variável proxy. 
# Utilize o dataset wage2 para ajustar o modelo. 

dados3 = woo.data("wage2")

# a. Use a variável QI como proxy para abil. Qual o retorno estimado para 
# a educação nesse caso? 
modelo3_a = smf.ols(formula = 'np.log(wage) ~ educ + exper + tenure + married + south + urban + black + IQ + I(educ*IQ)', data = dados3).fit()
modelo3_a.params.iloc[1]

# b. Use a variável KWW (conhecimento do mundo do trabalho) como proxy para abil. 
# Qual o retorno estimado para a educação nesse caso?
modelo3_b = smf.ols(formula = 'np.log(wage) ~ educ + exper + tenure + married + south + urban + black + KWW + I(educ*KWW)', data = dados3).fit()
modelo3_b.params.iloc[1]

# c. Use QI e KWW juntas como proxy para abil. O que acontece com o retorno 
# estimado para a educação? 
modelo3_c = smf.ols(formula = 'np.log(wage) ~ educ + exper + tenure + married + south + urban + black + IQ + KWW + I(educ*IQ) + I(educ*KWW)', data = dados3).fit()
modelo3_c.params.iloc[1]

# d. IQ e KWW sao individualmente significativas? Elas sao conjuntamente
# significativas?
sm.stats.anova_lm(modelo3_c, modelo3_a) # modelo3_c é o completo, modelo3_a só tem IQ
# rejeitamos H0, KWW é significativo individualmente 
sm.stats.anova_lm(modelo3_c, modelo3_b) # modelo3_c é o completo, modelo3_a só tem KWW
# rejeitamos H0, IQ é significativo individualmente
modelo3_d = smf.ols(formula = 'np.log(wage) ~ educ + exper + tenure + married + south + urban + black', data = dados3).fit()
sm.stats.anova_lm(modelo3_c, modelo3_d) 
# rejeitamos H0, IQ e KWW são significativos conjuntamente 
