
library(wooldridge)

# 1. Utilize o dataset ceosal1 do pacote wooldridge e ajuste o modelo
# log(wage) = β0 + β1log(sales) + β2roe + β3rosneg + u,
# em que rosneg é uma dummy que é igual a um se ros < 0 e zero caso contrário. 
# Aplique o teste RESET e verifique se existe alguma evidência de má-especificação
# da forma funcional da equação. (ros: retorno sobre o capital das empresas)

dados1 = as.data.frame(wooldridge::ceosal1)
modelo1 = lm(log(salary) ~ log(sales) + roe + I(as.numeric(ros<0)), data = dados1)
modelo1_expandido = lm(log(salary) ~ log(sales) + roe + I(as.numeric(ros<0)) + I(fitted(modelo1)^2) + I(fitted(modelo1)^3), data = dados1)

cat("Como", round(anova(modelo1, modelo1_expandido)$Pr[2], 4), "> 0.05, não rejeitamos H0,", 
"ou seja, não temos evidência de que há má-especificação de forma funcional\n")



# 2. Você e seu colega estão trabalhando com o dataset hprice1 do pacote wooldridge.
# Depois de várias análises, chegam nos seguintes modelos:
# Modelo 1: log(price) = β0 + β1lotsize + β2sqrft + β3bdrms + u.
# Modelo 2: log(price) = β0 + β1log(lotsize) + β2log(sqrft) + β3bdrms + u.
# Você e seu colega querem saber se algum dos modelos está mal-especificado. Utilize o teste
# de Davidson-MacKinnon para saber se existe ou não má-especificação da forma funcional em
# alguns dos modelos. Preferiria algum modelo frente ao outro? Compare os resultados com os
# obtidos em sala de aula com o teste de Mizon e Richard.

dados2 = as.data.frame(wooldridge::hprice1)
modelo2_a = lm(log(price) ~ lotsize + sqrft + bdrms, data = dados2)
modelo2_b = lm(log(price) ~ log(lotsize) + log(sqrft) + bdrms, data = dados2)

# Teste de Davidson-MacKinnon
modelo2_a_auxiliar = lm(log(price) ~ lotsize + sqrft + bdrms + I(fitted(modelo2_b)), data = dados2)
summary(modelo2_a_auxiliar)$coefficients["I(fitted(modelo2_b))", "Pr(>|t|)"]
# p-valor < 0.05, rejeitamos H0 de que o coeficiente associado a I(fitted(modelo2_b)) é zero
modelo2_b_auxiliar = lm(log(price) ~ log(lotsize) + log(sqrft) + bdrms + I(fitted(modelo2_a)), data = dados2)
summary(modelo2_b_auxiliar)$coefficients["I(fitted(modelo2_a))", "Pr(>|t|)"]
# p-valor > 0.05, não rejeitamos H0 de que o coeficiente associado a I(fitted(modelo2_a)) é zero 
# Portanto, o modelo 1 está mal espeficicado e preferimos o modelo 2. Da mesma forma que foi concluído em aula:

# Teste de Mizon e Richard
modelo2_abrangente = lm(log(price) ~ log(lotsize) + lotsize + log(sqrft) + sqrft + bdrms, data = dados2)
anova(modelo2_a, modelo2_abrangente)$Pr[2] 
# Rejeitamos H0 de que os coefcientes associados a log(lotsize) e log(sqrft) são zero, o modelo 1 está mal especificado 
anova(modelo2_b, modelo2_abrangente)$Pr[2]
# Não rejeitamos H0 de que os coeficientes associados a lotsize e sqrft são zero, o modelo 2 não está mal espeficicado



# 3. Seja o modelo de interesse:
# log(wage) = β0 + β1educ + β2exper + β3tenure + β4married +
# β5south + β6urban + β7black + β8abil + β9educ ∗ abil + u.
# Contudo, abil é não observada e precisamos utilizar alguma variável proxy. 
# Utilize o dataset wage2 para ajustar o modelo. 

dados3 = as.data.frame(wooldridge::wage2)

# a. Use a variável QI como proxy para abil. Qual o retorno estimado para 
# a educação nesse caso? 
modelo3_a = lm(log(wage) ~ educ + exper + tenure + married + south + urban + black + IQ + I(educ*IQ), dados3)
summary(modelo3_a)$coefficients[2,1]

# b. Use a variável KWW (conhecimento do mundo do trabalho) como proxy para abil. 
# Qual o retorno estimado para a educação nesse caso?
modelo3_b = lm(log(wage) ~ educ + exper + tenure + married + south + urban + black + KWW + I(educ*KWW), dados3)
summary(modelo3_b)$coefficients[2,1]

# c. Use QI e KWW juntas como proxy para abil. O que acontece com o retorno 
# estimado para a educação? 
modelo3_c = lm(log(wage) ~ educ + exper + tenure + married + south + urban + black + IQ + KWW + I(educ*IQ) + I(educ*KWW), dados3)
summary(modelo3_c)$coefficients[2,1]

# d. IQ e KWW sao individualmente significativas? Elas sao conjuntamente
# significativas?
anova(modelo3_c, modelo3_a) # modelo3_c é o completo, modelo3_a só tem IQ
# rejeitamos H0, KWW é significativo individualmente 
anova(modelo3_c, modelo3_b) # modelo3_c é o completo, modelo3_a só tem KWW
# rejeitamos H0, IQ é significativo individualmente
modelo3_d = lm(log(wage) ~ educ + exper + tenure + married + south + urban + black, dados3)
anova(modelo3_c, modelo3_d) 
# rejeitamos H0, IQ e KWW são significativos conjuntamente 
