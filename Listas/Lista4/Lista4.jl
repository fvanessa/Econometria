
using WooldridgeDatasets
using GLM
using DataFrames
using Distributions

# 4. O dataset rdchem contém informação de 32 empresas da industria química. 
# Entre as informações coletadas, temos: rdintens (gastos com pesquisa e 
# desenvolvimento como uma porcentagem das vendas), sales (vendas 
# mensuradas em mlhões de dólares) e profmarg (lucros como uma porcentagem
# das vendas). Ajuste um modelo da forma 
# rdintens = beta0 + beta1*log(sales) + beta2*profmarg + u

dados1 = DataFrame(wooldridge("rdchem"))
modelo1 = lm(@formula(rdintens ~ log(sales) + profmarg), dados1)

# Assumindo que as hipóteses do modelos linear clássico acontecem:
# a) Interprete o coeficiente de log(sales).
println("O coeficiente de log(sales) é ", round(coef(modelo1)[2], digits = 4),
" Isso significa que quando sales aumenta em 1%, rdintens aumenta em aproximadamente ", 
round(coef(modelo1)[2]/100, digits = 4))

# b) Teste a hipóteses de que a intensidade de P&D não varia com sales 
# contra a alternativa de que P&D aumenta com as vendas.
   # Queremos testar se beta1 = 0 vs beta1 > 0 (teste unilateral), então 
   # basta dividir por 2 o p-valor obtido no teste bilateral
println("O p-valor para esse teste é ", round(coeftable(modelo1).cols[4][2] / 2, digits = 4), 
   " Então não rejeitamos a hipótese nula de que beta1=0")

# c) Interprete o coeficiente de profmarg, ele é economicamente grande?
println("O coeficiente de profmarg é ", round(coef(modelo1)[3], digits = 4),
" Isso significa que quando profmarg aumenta em uma unidade, rdintens aumenta em aproximadamente ",
round(coef(modelo1)[3], digits = 4), " Portanto ele não é economicamente grande") 

# d) profmarg tem um efeito estatisticamente significativo sobre rdintens?
println("O p-valor associado ao coeficiente de profmarg é ", 
round(coeftable(modelo1).cols[4][3], digits = 4), 
" Portanto, podemos dizer que essa variável não tem efeito estastisticamente significativo\n")



# 5. Utilizando o dataset gpa1, ajuste um modelo que explique a nota média 
# em um curso superior (colGPA) utilizando o número de faltas às aulas por 
# semana (skipped), horas de estudo semanais (hsGPA) e a nota do ACT 
# (equivalente ao vestitubular). 

dados2 = DataFrame(wooldridge("gpa1"))
modelo2 = lm(@formula(colGPA ~ skipped + hsGPA + ACT), dados2)

# Assumindo que as hipóteses do modelo linear clássico acontecem:
# a) Encontre um intervalo de confiança 95% para o coeficiente de hsGPA.
println("Um intervalo de confiança de 95% para beta2 é ", 
        round.(confint(modelo2, level=0.95)[3, :], digits = 4))

# b) Teste H0: beta_hsGPA = 0.4 vs H1: beta_hsGPA diferente 0.4.
round(2*cdf(TDist(dof_residual(modelo2)), -abs(coef(modelo2)[3] - 0.4) / stderror(modelo2)[3]), digits = 4)
   # Não rejeitamos   

# c) Você pode rejeitar a hipóteses H0: beta_hsGPA = 1 contra a alternativa bilateral?
round(2*cdf(TDist(dof_residual(modelo2)), -abs(coef(modelo2)[3] - 1) / stderror(modelo2)[3]), digits = 4)
   # Rejeitamos 

# d) Teste a hipótese nula de que, uma vez tendo sido controlado as horas de estudo
# semanais, o efeito de skipped e ACT sobre colGPA são, conjuntamente, nulos.
   # Queremos testar H0: beta1 = beta3 = 0 vs H1: H0 não é verdadeira 
ftest(lm(@formula(colGPA ~ hsGPA), dados2).model, modelo2.model)
   # Rejeitamos



# 6. Utilize o conjunto de dados wage2 e ajuste a regressão
# log(wage) = beta0 + beta1*educ + beta2*exper + beta3*tenure + u,
# em que wage é o salario-hora em dolares, educ são os anos de educação formal, 
# exper são os anos de experiência no mercado de trabalho e tenure são os anos
# de permanencia no emprego atual.

dados3 = DataFrame(wooldridge("wage2"))
modelo3 = lm(@formula(log(wage) ~ educ + exper + tenure), dados3)

# a) Teste a hipótede de significância geral do modelo.
ftest(modelo3.model) # Rejeitamos

# b) Teste a hipótese de que um ano a mais de experiência no mercado de trabalho tem 
# o mesmo efeito sobre log(wage) que mais um ano de permanencia no emprego atual.
   # Queremos testar se beta2 = beta3 (caso 4)
dados3.tenureexper = dados3.tenure + dados3.exper
ftest(lm(@formula(log(wage) ~ educ + tenureexper), dados3).model, modelo3.model)
   # Como p-valor = 0.6805, não rejeitamos H0

# c) Teste a hipótese de que, controlado o número de anos de permanencia no emprego
# (tenure), educ e exper não tem efeito nenhum sobre log(wage).
    # Queremos testar se beta1 = beta2 = 0
    # Ou seja, se o modelo sem educ e exper é equivalente ao modelo completo
ftest(lm(@formula(log(wage) ~ tenure), dados3).model, modelo3.model)
    # Rejeitamos H0



# 7. Utilize o conjunto de dados htv e ajuste a regressão
# educ = beta0 + beta1*mothedu + beta2*fatheduc + beta3*abil + beta4*abil^2 + u. 

dados4 = DataFrame(wooldridge("htv"))
modelo4 = lm(@formula(educ ~ motheduc + fatheduc + abil + abil^2), dados4)

# a) Teste a hipóteses de que a influencia que motheduc e fatheduc exercem sobre 
# educ é a mesma.
dados4.mothfatheduc = dados4.motheduc + dados4.fatheduc
ftest(lm(@formula(educ ~ mothfatheduc + abil + abil^2), dados4).model, modelo4.model) 
   # Rejeitamos

# b) Teste a hipótese de que educ está linearmente relacionado com abil contra a 
# alternativa que diz que a relação é quadrática.
   # Testando se o modelo sem I(abil^2) é equivalente ao modelo com I(abil^2)
ftest(lm(@formula(educ ~ motheduc + fatheduc + abil), dados4).model, modelo4.model) # Rejeitamos
   # Testando se um modelo só com abil é equivalente a um modelo com abil e I(abil^2)
ftest(lm(@formula(educ ~ abil), dados4).model, lm(@formula(educ ~ abil + abil^2), dados4).model) # Rejeitamos
   # Também poderíamos perceber que o coeficiente associado a I(abil^2) é significativo

# c) Um colega de trabalho diz que o modelo educ = beta0 + beta1*abil + beta2*abil^2 + u
# é suficiente, e que tanto motheduc e fatheduc não são importantes para o modelos. 
# Faça um teste de hipóteses para rejeitar ou não rejeitar a hipótese do seu colega.
    # Queremos testar se beta1 = beta2 = 0
    # Ou seja, se o modelo sem motheduc e fatheduc é equivalente ao modelo completo
ftest(lm(@formula(educ ~ abil + abil^2), dados4).model, modelo4.model)
    # Rejeitamos
