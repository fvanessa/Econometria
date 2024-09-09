
import wooldridge as woo
import statsmodels.api as sm

# Utilize o dataset HTV, estime o modelo de regresão
# educ = beta0 + beta1 motheduc + beta2 fatheduc + beta3 abil + beta4 abil^2 + u
# e interprete os resultados.

dados = woo.data("htv")
dados['abil2'] = dados['abil'] ** 2
X = dados[['motheduc', 'fatheduc', 'abil', 'abil2']]
Y = dados['educ']
X = sm.add_constant(X)

modelo = sm.OLS(Y, X).fit()

coeficientes = modelo.params
print("Os coeficientes do modelo são:\n", coeficientes.round(2))

# Isso significa que quando todas as variáveis estão fixas no modelo, 
# um ano a mais de estudo da mãe aumenta o tempo de educação em 0,19 e 
# um ano a mais de estudo do pai, aumenta o tempo de educação em 0,11.
# Por outro lado, como a forma quadrática da variável apil também está
# presente no modelo, para interpretar o seu efeito na variável resposta 
# precisamos derivar a equação em relação a ela, o que nos dá a seguinte
# expressão (beta3 + 2*beta4*abil) = (0,4 + 2*0,05*abil), ou seja, quando 
# abil = 1, o tempo de educação aumenta em 0,5, quando apil = 2, o tempo 
# aumenta em 0,6 e assim sucessivamente. 
