
library(wooldridge)

# 4. O dataset rdchem contém informação de 32 empresas da industria química. 
# Entre as informações coletadas, temos: rdintens (gastos com pesquisa e 
# desenvolvimento como uma porcentagem das vendas), sales (vendas 
# mensuradas em mlhões de dólares) e profmarg (lucros como uma porcentagem
# das vendas). Ajuste um modelo da forma 
# rdintens = beta0 + beta1*log(sales) + beta2*profmarg + u

dados1 = as.data.frame(wooldridge::rdchem)
modelo1 = lm(rdintens ~ log(sales) + profmarg, data = dados1)

# Assumindo que as hipóteses do modelos linear clássico acontecem:
# a) Interprete o coeficiente de log(sales).
cat("O coeficiente de log(sales) é", round(coef(modelo1)[2], 4), 
"Isso significa que quando sales aumenta em 1%, rdintens aumenta em aproximadamente", 
round((coef(modelo1)[2] / 100), 4), "\n")

# b) Teste a hipóteses de que a intensidade de P&D não varia com sales 
# contra a alternativa de que P&D aumenta com as vendas.
   # Queremos testar se beta1 = 0 vs beta1 > 0 (teste unilateral), então 
   # basta dividir por 2 o p-valor obtido no teste bilateral
cat("O p-valor para esse teste é", round(summary(modelo1)$coefficients[2,4] / 2, 4), 
"Então não rejeitamos a hipótese nula de que beta1=0\n")

# c) Interprete o coeficiente de profmarg, ele é economicamente grande?
cat("O coeficiente de profmarg é", round(coef(modelo1)[3], 4),
"Isso significa que quando profmarg aumenta em uma unidade, rdintens aumenta em aproximadamente",
round(coef(modelo1)[3], 4), 
"Portanto ele não é economicamente grande", "\n") 

# d) profmarg tem um efeito estatisticamente significativo sobre rdintens?
cat("O p-valor associado ao coeficiente de profmarg é", 
round(summary(modelo1)$coefficients[3, 4], 4), 
"Portanto, podemos dizer que essa variável não tem efeito estastisticamente significativo\n")



# 5. Utilizando o dataset gpa1, ajuste um modelo que explique a nota média 
# em um curso superior (colGPA) utilizando o número de faltas às aulas por 
# semana (skipped), horas de estudo semanais (hsGPA) e a nota do ACT 
# (equivalente ao vestitubular). 

dados2 = as.data.frame(wooldridge::gpa1)
modelo2 = lm(colGPA ~ skipped + hsGPA + ACT, data = dados2)

# Assumindo que as hipóteses do modelo linear clássico acontecem:
# a) Encontre um intervalo de confiança 95% para o coeficiente de hsGPA.
cat("Um intervalo de confiança de 95% para beta2 é", 
paste0("[", round(confint(modelo2)[3,][1], 4), ", ", 
round(confint(modelo2)[3,][2], 4), "] \n"))

# b) Teste H0: beta_hsGPA = 0.4 vs H1: beta_hsGPA diferente 0.4.
   # Sob a hipótese nula, beta2=0.4, então a estatística do teste vai ser 
   # (beta2_chapeu - 0.4) / sqrt(var(beta2_chapeu)), aí é só calcular o 
   # valor crítico e comparar com o valor observado da estatística do teste
t_obs = round( (summary(modelo2)$coefficients[3,1] - 0.4) / (summary(modelo2)$coefficients[3,2]) , 4)
c = round( pt(t_obs, modelo2$df.residual), 4)
cat("Como", abs(t_obs), "<", abs(c), "não temos evidência para rejeitar a hpótese nula\n")

# c) Você pode rejeitar a hipóteses H0: beta_hsGPA = 1 contra a alternativa bilateral?
   # Queremos testar beta2 = 1 vs beta2 diferente de 1, semelhante ao anterior
t_obs2 = round( (summary(modelo2)$coefficients[3,1] - 1) / (summary(modelo2)$coefficients[3,2]) , 4)
c2 = round( pt(t_obs2, modelo2$df.residual), 4)
cat("Como", abs(t_obs2), ">", abs(c2), "temos evidência para rejeitar a hpótese nula\n")

# d) Teste a hipótese nula de que, uma vez tendo sido controlado as horas de estudo
# semanais, o efeito de skipped e ACT sobre colGPA são, conjuntamente, nulos.
   # Queremos testar H0: beta1 = beta3 = 0 vs H1: H0 não é verdadeira 
modelo2_i = lm(colGPA ~ skipped + hsGPA + ACT, data = dados2)
modelo2_r = lm(colGPA ~ hsGPA, data = dados2)
SQRi = sum(residuals(modelo2_i)^2)
SQRr = sum(residuals(modelo2_r)^2)
n = nrow(dados2)
k = 3 #número total de variáveis
q = 2 #número de betas na hipótese nula
est_F = ((SQRr - SQRi)/SQRi)*((n - k - 1)/q)
c_critico = qf(p = 0.95, df1 = q, df2 = n - k - 1)
cat("Como", round(est_F, 4), ">", round(c_critico, 4), "rejeitamos a hipótese nula\n")
# Outra forma de fazer
anova(lm(colGPA ~ hsGPA, dados2), lm(colGPA ~ hsGPA + skipped + ACT, dados2))



# 6. Utilize o conjunto de dados wage2 e ajuste a regressão
# log(wage) = beta0 + beta1*educ + beta2*exper + beta3*tenure + u,
# em que wage é o salario-hora em dolares, educ são os anos de educação formal, 
# exper são os anos de experiência no mercado de trabalho e tenure são os anos
# de permanencia no emprego atual.

dados3 = as.data.frame(wooldridge::wage2)
modelo3 = lm(log(wage) ~ educ + exper + tenure, data = dados3)

# a) Teste a hipótede de significância geral do modelo.
summary(modelo3)
   # Como p-valor é menor que 2.2e-16, rejeitamos a hipótese nula de que 
   # todos os coeficientes são zero

# b) Teste a hipótese de que um ano a mais de experiência no mercado de trabalho tem 
# o mesmo efeito sobre log(wage) que mais um ano de permanencia no emprego atual.
   # Queremos testar se beta2 = beta3 (caso 4)
library(car)
lht(modelo3, c("exper = tenure"))
   # Como p-valor = 0.6805, não rejeitamos H0

# c) Teste a hipótese de que, controlado o número de anos de permanencia no emprego
# (tenure), educ e exper não tem efeito nenhum sobre log(wage).
    # Queremos testar se beta1 = beta2 = 0
    # Ou seja, se o modelo sem educ e exper é equivalente ao modelo completo
anova(lm(log(wage) ~ tenure, dados3), lm(log(wage) ~ educ + exper + tenure, data = dados3))
    # Como p-valor é menor que 2.2e-16, rejeitamos H0



# 7. Utilize o conjunto de dados htv e ajuste a regressão
# educ = beta0 + beta1*mothedu + beta2*fatheduc + beta3*abil + beta4*abil^2 + u. 

dados4 = as.data.frame(wooldridge::htv)
modelo4 = lm(educ ~ motheduc + fatheduc + abil + I(abil^2), data = dados4)

# a) Teste a hipóteses de que a influencia que motheduc e fatheduc exercem sobre 
# educ é a mesma.
   # Queremos testar se beta1 = beta2 (caso 4)
lht(modelo4, c("motheduc = fatheduc"))
   # Como p-valor = 0.05314, rejeitamos H0

# b) Teste a hipótese de que educ está linearmente relacionado com abil contra a 
# alternativa que diz que a relação é quadrática.
   # Testando se o modelo sem I(abil^2) é equivalente ao modelo com I(abil^2)
anova(lm(educ ~ motheduc + fatheduc + abil, data = dados4), modelo4) # Rejeita
   # Testando se um modelo só com abil é equivalente a um modelo com abil e I(abil^2)
anova(lm(educ ~ abil, data = dados4), lm(educ ~ abil + I(abil^2), data = dados4)) # Rejeita
   # Também poderíamos perceber que o coeficiente associado a I(abil^2) é significativo

# c) Um colega de trabalho diz que o modelo educ = beta0 + beta1*abil + beta2*abil^2 + u
# é suficiente, e que tanto motheduc e fatheduc não são importantes para o modelos. 
# Faça um teste de hipóteses para rejeitar ou não rejeitar a hipótese do seu colega.
    # Queremos testar se beta1 = beta2 = 0
    # Ou seja, se o modelo sem motheduc e fatheduc é equivalente ao modelo completo
anova(lm(educ ~ abil + I(abil^2), data = dados4), modelo4)
    # Como p-valor é menor do que 2.2e-16, rejeitamos H0, ou seja, 
    # o colega de trabalho está errado
