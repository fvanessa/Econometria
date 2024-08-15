
import pandas as pd
import statistics as sts
import math

########## Exercício 1

dados = pd.read_csv("C:/Users/vahfe/OneDrive/Área de Trabalho/Graduação/6º Semestre/ME715 - Econometria/Lista 1/meap01.csv")

# a) Encontre os maiores e menores valores de math4
print("Máximo:", max(dados["math4"]))
print("Mínimo:", min(dados["math4"]))

# b) Quantas escolas têm a taxa de aprovação em matemática de exatamente 50%?
print(sum(dados["math4"] == 50), "escola(s)")

# c) Compare as taxas médias de aprovação em matemática e leitura. 
# Qual teste tem aprovação mais difícil?
taxa_aprov_math = (dados["math4"]).mean()
taxa_aprov_read = (dados["read4"]).mean()

if taxa_aprov_math < taxa_aprov_read:
    print("Matemática tem aprovação mais difícil")
elif taxa_aprov_read < taxa_aprov_math:
    print("Leitura tem aprovação mais difícil")
else:
    print("A dificuldade de aprovação é igual")

# d) Encontre a correlação entre math4 e read4. O que pode concluir?
correlacao = dados["math4"].corr(dados["read4"])

if correlacao == 1:
    print("Correlação positiva perfeita")
elif correlacao >= 0.8 and correlacao < 1:
    print("Correlação positiva forte")
elif correlacao >= 0.5 and correlacao < 0.8:
    print("Correlação positiva moderada")
elif correlacao >= 0.1 and correlacao < 0.5:
    print("Correlação positiva fraca")
elif correlacao > 0 and correlacao < 0.1:
    print("Correlação positiva ínfima")
elif correlacao == -1:
    print("Correlação negativa perfeita")
elif correlacao <= -0.8 and correlacao > -1:
    print("Correlação negativa forte")
elif correlacao <= -0.5 and correlacao > -0.8:
    print("Correlação negativa moderada")
elif correlacao <= -0.1 and correlacao > -0.5:
    print("Correlação negativa fraca")
elif correlacao < 0 and correlacao > -0.1:
    print("Correlação negativa ínfima") 
else:
    print("Correlação nula")
    
# e) Avariável exppp são os gastos por aluno. Econtre o exppp médio e seu desvio padrão
print("Média:", sts.mean(dados["exppp"]))
print("Desvio padrão:", sts.stdev(dados["exppp"]))

#  f) Suponha que a escola A gaste USD$6.000 por estudante e a escola B gaste USD$5.500 por estudante. 
# Com que percentual os gastos da escola A superamos da escola B? 
# Compare isso a 100 × [log(6.000) − log(5.500)], que é a diferença percentual 
# aproximada baseada na diferença dos logaritmos.
print(100*(6000-5500)/5500)
print(100*(math.log(6000) - math.log(5500)))

########### Exercício 2

dados2 = pd.read_csv("C:/Users/vahfe/OneDrive/Área de Trabalho/Graduação/6º Semestre/ME715 - Econometria/Lista 1/econmath.csv")

# a) Quantos estudantes estão na amostra? 
# Quantos estudantes declaram ter frequentado um curso de economia no ensino médio?
print("Número de estudantes na amostra:", len(dados2))
print("Número de estudantes que cursaram economia no ensino médio:", sum(dados2["econhs"]))

# b) Encontre a nota média dos alunos que frequentaram um curso de economia do ensino médio. 
# Como se compara com a nota média daqueles que não o fizeram?
econhs1 = sts.mean(dados2[dados2['econhs'] == 1]['score'])
econhs0 = sts.mean(dados2[dados2['econhs'] == 0]['score'])

if econhs1 > econhs0:
    print("A nota média dos alunos que frequentaram um curso de economia no ensino médio é maior")
else:
    print("A nota média dos alunos que não frequentaram um curso de economia no ensino médio é maior")

# c) Os resultados encontrados dizem necessariamente alguma coisa sobre o efeito causal
# de cursar economia no ensino médio sobre o desempenho no curso universitário? (explique)
  # Não. Pode haver apenas uma associação entre essas duas variáveis, mas isso não implica causalidade.

# d) Se quiser obter uma boa estimativa causal do efeito de fazer um curso de economia
# no ensino médio utilizando a diferença de médias, que experiência faria?
  # Selecionaria um grupo de pessoas com características parecidas e designaria metade delas
  # para fazer um curso de economia no ensino médio e só depois compararia o desempenho delas no 
  # curso universitário com o desempenho daquelas que não fizeram o curso de economia no ensino médio. 

