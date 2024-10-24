
using WooldridgeDatasets
using GLM
using DataFrames
using Distributions

# 1. Utilize o dataset ceosal1 do pacote wooldridge e ajuste o modelo
# log(wage) = β0 + β1log(sales) + β2roe + β3rosneg + u,
# em que rosneg é uma dummy que é igual a um se ros < 0 e zero caso contrário. 
# Aplique o teste RESET e verifique se existe alguma evidência de má-especificação
# da forma funcional da equação. (ros: retorno sobre o capital das empresas)

dados1 = DataFrame(wooldridge("ceosal1"))
dados1.ros_neg = dados1.ros .< 0
modelo1 = lm(@formula(log(salary) ~ log(sales) + roe + ros_neg), dados1)
dados1.valores_ajustados = fitted(modelo1)
modelo1_expandido = lm(@formula(log(salary) ~ log(sales) + roe + ros_neg + (valores_ajustados^2) + (valores_ajustados^3)), dados1)

ftest(modelo1.model, modelo1_expandido.model)
# Não rejeitamos H0, ou seja, não temos evidência de que há má-especificação de forma funcional



# 2. Você e seu colega estão trabalhando com o dataset hprice1 do pacote wooldridge.
# Depois de várias análises, chegam nos seguintes modelos:
# Modelo 1: log(price) = β0 + β1lotsize + β2sqrft + β3bdrms + u.
# Modelo 2: log(price) = β0 + β1log(lotsize) + β2log(sqrft) + β3bdrms + u.
# Você e seu colega querem saber se algum dos modelos está mal-especificado. Utilize o teste
# de Davidson-MacKinnon para saber se existe ou não má-especificação da forma funcional em
# alguns dos modelos. Preferiria algum modelo frente ao outro? Compare os resultados com os
# obtidos em sala de aula com o teste de Mizon e Richard.

dados2 = DataFrame(wooldridge("hprice1"))
modelo2_a = lm(@formula(log(price) ~ lotsize + sqrft + bdrms), dados2)
modelo2_b = lm(@formula(log(price) ~ log(lotsize) + log(sqrft) + bdrms), dados2)

dados2.valores_ajustados_modelo2b = fitted(modelo2_b)
modelo2_a_auxiliar = lm(@formula(log(price) ~ lotsize + sqrft + bdrms + valores_ajustados_modelo2b), dados2)
# p-valor < 0.05, rejeitamos H0 de que o coeficiente associado a valores_ajustados_modelo2b é zero
dados2.valores_ajustados_modelo2a = fitted(modelo2_a)
modelo2_b_auxiliar = lm(@formula(log(price) ~ log(lotsize) + log(sqrft) + bdrms + valores_ajustados_modelo2a), dados2)
# p-valor > 0.05, não rejeitamos H0 de que o coeficiente associado a I(fitted(modelo2_a)) é zero 
# Portanto, o modelo 1 está mal espeficicado e preferimos o modelo 2 (da mesma forma que foi concluído em aula)



# 3. Seja o modelo de interesse:
# log(wage) = β0 + β1educ + β2exper + β3tenure + β4married +
# β5south + β6urban + β7black + β8abil + β9educ ∗ abil + u.
# Contudo, abil é não observada e precisamos utilizar alguma variável proxy. 
# Utilize o dataset wage2 para ajustar o modelo. 

dados3 = DataFrame(wooldridge("wage2"))

# a. Use a variável QI como proxy para abil. Qual o retorno estimado para 
# a educação nesse caso? 
modelo3_a = lm(@formula(log(wage) ~ educ + exper + tenure + married + south + urban + black + IQ + (educ*IQ)), dados3)
coef(modelo3_a)[2]

# b. Use a variável KWW (conhecimento do mundo do trabalho) como proxy para abil. 
# Qual o retorno estimado para a educação nesse caso?
modelo3_b = lm(@formula(log(wage) ~ educ + exper + tenure + married + south + urban + black + KWW + (educ*KWW)), dados3)
coef(modelo3_b)[2]

# c. Use QI e KWW juntas como proxy para abil. O que acontece com o retorno 
# estimado para a educação? 
modelo3_c = lm(@formula(log(wage) ~ educ + exper + tenure + married + south + urban + black + IQ + KWW + (educ*IQ) + (educ*KWW)), dados3)
coef(modelo3_c)[2]

# d. IQ e KWW sao individualmente significativas? Elas sao conjuntamente
# significativas?
ftest(modelo3_c.model, modelo3_a.model) # modelo3_c é o completo, modelo3_a só tem IQ
# rejeitamos H0, KWW é significativo individualmente 
ftest(modelo3_c.model, modelo3_b.model) # modelo3_c é o completo, modelo3_a só tem KWW
# rejeitamos H0, IQ é significativo individualmente
modelo3_d = lm(@formula(log(wage) ~ educ + exper + tenure + married + south + urban + black), dados3)
ftest(modelo3_c.model, modelo3_d.model) 
# rejeitamos H0, IQ e KWW são significativos conjuntamente 
