
using CSV
using DataFrames
using Statistics

########## Exercício 1

dados = CSV.read("C:/Users/vahfe/OneDrive/Área de Trabalho/Graduação/6º Semestre/ME715 - Econometria/Lista 1/meap01.csv", DataFrame)

# a) Encontre os maiores e menores valores de math4.
println("Máximo: ", maximum(dados.math4))
println("Mínimo: ", minimum(dados.math4))

# b) Quantas escolas têm a taxa de aprovaçñao em matemática de exatamente 50%?
println(sum(dados.math4 .== 50))

# c) Compare as taxas médias de aprovação em matemática e leitura. Qual teste tem aprovação mais difícil?
taxa_aprov_math = mean(dados.math4)
taxa_aprov_read = mean(dados.read4)

if taxa_aprov_math < taxa_aprov_read
    println("Matemática tem aprovação mais difícil")
elseif taxa_aprov_read < taxa_aprov_math
    println("Leitura tem aprovação mais difícil")
else
    println("A dificuldade de aprovação é igual")
end

# d) Encontre a correlação entre math4 e read4. O que pode concluir?
correlacao = cor(dados.math4, dados.read4)

if correlacao == 1
    println("Correlação positiva perfeita")
elseif correlacao >= 0.8 && correlacao < 1
    println("Correlação positiva forte")
elseif correlacao >= 0.5 && correlacao < 0.8
    println("Correlação positiva moderada")
elseif correlacao >= 0.1 && correlacao < 0.5
    println("Correlação positiva fraca")
elseif correlacao > 0 && correlacao < 0.1
    println("Correlação positiva ínfima")
elseif correlacao == -1
    println("Correlação negativa perfeita")
elseif correlacao <= -0.8 && correlacao > -1
    println("Correlação negativa forte")
elseif correlacao <= -0.5 && correlacao > -0.8
    println("Correlação negativa moderada")
elseif correlacao <= -0.1 && correlacao > -0.5
    println("Correlação negativa fraca")
elseif correlacao < 0 && correlacao > -0.1
    println("Correlação negativa ínfima") 
else 
    println("Correlação nula")
end

# e) Avariável exppp são os gastos por aluno. Econtre o exppp médio e seu desvio padrão
println("Média: ", mean(dados.exppp))
println("Desvio padrão: ", std(dados.exppp))

# f) Suponha que a escola A gaste USD$6.000 por estudante e a escola B gaste USD$5.500 por estudante. 
# Com que percentual os gastos da escola A superamos da escola B? 
# Compare isso a 100 × [log(6.000) − log(5.500)], que é a diferença percentual 
# aproximada baseada na diferença dos logaritmos.
println(100*(6000-5500)/5500)
println(100*(log(6000) - log(5500)))

########## Exercício 2

dados2 = CSV.read("C:/Users/vahfe/OneDrive/Área de Trabalho/Graduação/6º Semestre/ME715 - Econometria/Lista 1/econmath.csv", DataFrame)

# a) Quantos estudantes estão na amostra? 
# Quantos estudantes declaram ter frequentado um curso de economia no ensino médio?
println("Número de estudantes na amostra: ", nrow(dados2))
println("Número de estudantes que cursaram economia no ensino médio: ", sum(dados2.econhs))

# b) Encontre a nota média dos alunos que frequentaram um curso de economia do ensino médio. 
# Como se compara com a nota média daqueles que não o fizeram?
econhs1 = mean(filter(row -> row[:econhs] == 1, dados2)[!, :score])
econhs0 = mean(filter(row -> row[:econhs] == 0, dados2)[!, :score])

if econhs1 > econhs0
    println("A nota média dos alunos que frequentaram um curso de economia no ensino médio é maior")
else
    println("A nota média dos alunos que não frequentaram um curso de economia no ensino médio é maior")
end

# outra forma:
# mean(dados2.score[dados2.econhs .== 1])
# mean(dados2.score[dados2.econhs .== 0])

# c) Os resultados encontrados dizem necessariamente alguma coisa sobre o efeito causal
# de cursar economia no ensino médio sobre o desempenho no curso universitário? (explique)
  # Não. Pode haver apenas uma associação entre essas duas variáveis, mas isso não implica causalidade.

# d) Se quiser obter uma boa estimativa causal do efeito de fazer um curso de economia
# no ensino médio utilizando a diferença de médias, que experiência faria?
  # Selecionaria um grupo de pessoas com características parecidas e designaria metade delas
  # para fazer um curso de economia no ensino médio e só depois compararia o desempenho delas no 
  # curso universitário com o desempenho daquelas que não fizeram o curso de economia no ensino médio. 

