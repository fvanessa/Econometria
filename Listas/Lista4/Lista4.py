
import wooldridge as woo
import statsmodels.formula.api as smf
import statsmodels.api as sm
import numpy as np
from scipy import stats

# 4. O dataset rdchem contém informação de 32 empresas da industria química. 
# Entre as informações coletadas, temos: rdintens (gastos com pesquisa e 
# desenvolvimento como uma porcentagem das vendas), sales (vendas 
# mensuradas em mlhões de dólares) e profmarg (lucros como uma porcentagem
# das vendas). Ajuste um modelo da forma 
# rdintens = beta0 + beta1*log(sales) + beta2*profmarg + u

dados1 = woo.data("rdchem")
modelo1 = smf.ols(formula = 'rdintens ~ np.log(sales) + profmarg', data = dados1).fit()

# Assumindo que as hipóteses do modelos linear clássico acontecem:
# a) Interprete o coeficiente de log(sales).
print("O coeficiente de log(sales) é", round(modelo1.params[1],4),
      "Isso significa que quando sales aumenta em 1%, rdintens aumenta em aproximadamente",
      round(modelo1.params[1]/100, 4))

# b) Teste a hipóteses de que a intensidade de P&D não varia com sales 
# contra a alternativa de que P&D aumenta com as vendas.
   # Queremos testar se beta1 = 0 vs beta1 > 0 (teste unilateral), então 
   # basta dividir por 2 o p-valor obtido no teste bilateral
print("O p-valor para esse teste é", round(modelo1.pvalues[1] / 2, 4), 
      "Então não rejeitamos a hipótese nula de que beta1=0.")

# c) Interprete o coeficiente de profmarg, ele é economicamente grande?
print("O coeficiente de profmarg é", round(modelo1.params[2], 4),
      "Isso significa que quando profmarg aumenta em uma unidade, rdintens aumenta em aproximadamente",
      round(modelo1.params['profmarg'], 4), "Portanto ele não é economicamente grande.")

# d) profmarg tem um efeito estatisticamente significativo sobre rdintens?
print("O p-valor associado ao coeficiente de profmarg é", round(modelo1.pvalues[2],4),
     "Portanto, podemos dizer que essa variável não tem efeito estatisticamente significativo.")



# 5. Utilizando o dataset gpa1, ajuste um modelo que explique a nota média 
# em um curso superior (colGPA) utilizando o número de faltas às aulas por 
# semana (skipped), horas de estudo semanais (hsGPA) e a nota do ACT 
# (equivalente ao vestitubular). 

dados2 = woo.data("gpa1")
modelo2 = smf.ols(formula = 'colGPA ~ skipped + hsGPA + ACT', data = dados2).fit()

# Assumindo que as hipóteses do modelo linear clássico acontecem:
# a) Encontre um intervalo de confiança 95% para o coeficiente de hsGPA.
print("Um intervalo de confiança de 95% para beta2 é", 
      modelo2.conf_int().loc['hsGPA'].round(4).values)

# b) Teste H0: beta_hsGPA = 0.4 vs H1: beta_hsGPA diferente 0.4.
beta2 = modelo2.params[2]
sd_beta2 = modelo2.bse[2]
round(2*stats.t.cdf(-abs(beta2-0.4)/sd_beta2, modelo2.df_resid), 4) # Não rejeitamos

# c) Você pode rejeitar a hipóteses H0: beta_hsGPA = 1 contra a alternativa bilateral?
round(2*stats.t.cdf(-abs(beta2-1)/sd_beta2, modelo2.df_resid), 4) # Rejeitamos

# d) Teste a hipótese nula de que, uma vez tendo sido controlado as horas de estudo
# semanais, o efeito de skipped e ACT sobre colGPA são, conjuntamente, nulos.
   # Queremos testar H0: beta1 = beta3 = 0 vs H1: H0 não é verdadeira
round(sm.stats.anova_lm(smf.ols('colGPA ~ hsGPA', dados2).fit(), modelo2)['Pr(>F)'][1], 4)
   # Rejeitamos



# 6. Utilize o conjunto de dados wage2 e ajuste a regressão
# log(wage) = beta0 + beta1*educ + beta2*exper + beta3*tenure + u,
# em que wage é o salario-hora em dolares, educ são os anos de educação formal, 
# exper são os anos de experiência no mercado de trabalho e tenure são os anos
# de permanencia no emprego atual.

dados3 = woo.data("wage2")
modelo3 = smf.ols(formula = 'np.log(wage) ~ educ + exper + tenure', data = dados3).fit()

# a) Teste a hipótede de significância geral do modelo.
modelo3.f_pvalue # Rejeita

# b) Teste a hipótese de que um ano a mais de experiência no mercado de trabalho tem 
# o mesmo efeito sobre log(wage) que mais um ano de permanencia no emprego atual.
   # Queremos testar se beta2 = beta3 (caso 4)
modelo3.f_test(np.array([0,0,1,-1]))
   # Como p-valor = 0.6805, não rejeitamos H0

# c) Teste a hipótese de que, controlado o número de anos de permanencia no emprego
# (tenure), educ e exper não tem efeito nenhum sobre log(wage).
    # Queremos testar se beta1 = beta2 = 0
    # Ou seja, se o modelo sem educ e exper é equivalente ao modelo completo
sm.stats.anova_lm(smf.ols('np.log(wage) ~ tenure', dados3).fit(), modelo3)
    # Como p-valor é menor que 2.2e-16, rejeitamos H0



# 7. Utilize o conjunto de dados htv e ajuste a regressão
# educ = beta0 + beta1*mothedu + beta2*fatheduc + beta3*abil + beta4*abil^2 + u. 

dados4 = woo.data("htv")
modelo4 = smf.ols(formula = 'educ ~ motheduc + fatheduc + abil + I(abil**2)', data = dados4).fit()

# a) Teste a hipóteses de que a influencia que motheduc e fatheduc exercem sobre 
# educ é a mesma.
   # Queremos testar se beta1 = beta2 (caso 4)
modelo4.f_test(np.array([0,1,-1,0,0]))
   # Como p-valor = 0.05314, rejeitamos H0

# b) Teste a hipótese de que educ está linearmente relacionado com abil contra a 
# alternativa que diz que a relação é quadrática.
   # Testando se o modelo sem I(abil^2) é equivalente ao modelo com I(abil^2)
sm.stats.anova_lm(smf.ols('educ ~ motheduc + fatheduc + abil', dados4).fit(), modelo4)  # Rejeita
   # Testando se um modelo só com abil é equivalente a um modelo com abil e I(abil^2)
sm.stats.anova_lm(smf.ols('educ ~ abil', dados4).fit(), smf.ols('educ ~ abil + I(abil**2)', dados4).fit())                # Rejeita
   # Também poderíamos perceber que o coeficiente associado a I(abil^2) é significativo

# c) Um colega de trabalho diz que o modelo educ = beta0 + beta1*abil + beta2*abil^2 + u
# é suficiente, e que tanto motheduc e fatheduc não são importantes para o modelos. 
# Faça um teste de hipóteses para rejeitar ou não rejeitar a hipótese do seu colega.
    # Queremos testar se beta1 = beta2 = 0
    # Ou seja, se o modelo sem motheduc e fatheduc é equivalente ao modelo completo
sm.stats.anova_lm(smf.ols('educ ~ abil + I(abil**2)', dados4).fit(), modelo4)
    # Rejeitamos H0
