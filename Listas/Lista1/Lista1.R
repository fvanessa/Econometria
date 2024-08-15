
library(tidyverse)
library(wooldridge)

########### Exercício 1

# Conjunto de dados com 1823 observações e 11 variáveis
# math4: porcentagem de alunos satisfatórios
# read4: porcentagem de alunos satisfatórios
# enroll: matrícula escolar
# expend: gasto total
# exppp: despesas por aluno (expend/enroll)

dados <- as.data.frame(wooldridge::meap01)

# a) Encontre os maiores e menores valores de math4
cat("Máximo:", max(dados$math4), "\n")
cat("Mínimo:", min(dados$math4), "\n")

# b) Quantas escolas têm a taxa de aprovação em matemática de exatamente 50%?
cat(sum(dados$math4 == 50), "escola(s)", "\n")

# c) Compare as taxas médias de aprovação em matemática e leitura. Qual teste tem aprovação mais difícil?
taxa_aprov_math = mean(dados$math4)
taxa_aprov_read = mean(dados$read4)

if (taxa_aprov_math < taxa_aprov_read) {
  cat("Mátemática tem aprovação mais difícil", "\n")
  } else if (taxa_aprov_read < taxa_aprov_math) {
    cat("Leitura tem aprovação mais difícil", "\n") 
    } else {cat("A dificuldade de aprovação é igual", "\n")
}

# d) Encontre a correlação entre math4 e read4. O que pode concluir?
correlacao = cor(dados$math4, dados$read4)

if (correlacao == 1) {
  cat(correlacao, "Correlação positiva perfeita", "\n")
} else if (correlacao >= 0.8 & correlacao < 1) {
  cat(correlacao, "Correlação positiva forte", "\n")
} else if (correlacao >= 0.5 & correlacao < 0.8) {
  cat(correlacao, "Correlação positiva moderada", "\n")
} else if (correlacao >= 0.1 & correlacao < 0.5) {
  cat(correlacao, "Correlação positiva fraca", "\n")
} else if (correlacao > 0 & correlacao < 0.1) {
  cat(correlacao, "Correlação positiva ínfima", "\n")
} else if (correlacao == -1) {
  cat(correlacao, "Correlação negativa perfeita", "\n")
} else if (correlacao <= -0.8 & correlacao > -1) {
  cat(correlacao, "Correlação negativa forte", "\n")
} else if (correlacao <= -0.5 & correlacao > -0.8) {
  cat(correlacao, "Correlação negativa moderada", "\n")
} else if (correlacao <= -0.1 & correlacao > -0.5) {
  cat(correlacao, "Correlação negativa fraca", "\n")
} else if (correlacao < 0 & correlacao > -0.1) {
  cat(correlacao, "Correlação negativa ínfima", "\n") 
} else {cat(correlacao, "Correlação nula", "\n")
}

# e) A variável exppp são os gastos por aluno. Econtre o exppp médio e seu desvio padrão
cat("Média:", mean(dados$exppp), "\n")
cat("Desvio padrão:", sd(dados$exppp), "\n")

#  f) Suponha que a escola A gaste USD$6.000 por estudante e a escola B gaste USD$5.500 por estudante. 
# Com que percentual os gastos da escola A superamos da escola B? 
# Compare isso a 100 × [log(6.000) − log(5.500)], que é a diferença percentual 
# aproximada baseada na diferença dos logaritmos.
cat(100*(6000-5500)/5500, "\n")
cat(100*(log(6000) - log(5500)), "\n")

########### Exercício 2

# Um conjunto de dados obtidos de estudantes de um grande curso universitário em introdução à microeconomia
# score: nota do final do curso
# econhs: variável binária que indica se um estudante fez um curso de economia no ensino médio

dados2 <- as.data.frame(wooldridge::econmath)

# a) Quantos estudantes estão na amostra? 
# Quantos estudantes declaram ter frequentado um curso de economia no ensino médio?
cat("Número de estudantes na amostra:", nrow(dados2), "\n")
cat("Número de estudantes que cursaram economia no ensino médio:", sum(dados2$econhs), "\n")

# b) Encontre a nota média dos alunos que frequentaram um curso de economia do ensino médio. 
# Como se compara com a nota média daqueles que não o fizeram?
econhs1 = dados2 %>% filter(econhs == 1) %>% summarise(econhs1 = mean(score)) %>% pull(econhs1)
econhs0 = dados2 %>% filter(econhs == 0) %>% summarise(econhs0 = mean(score)) %>% pull(econhs0)

if (econhs1 > econhs0){
  cat("A nota média dos alunos que frequentaram um curso de economia no ensino médio é maior", "\n")
} else {cat("A nota média dos alunos que não frequentaram um curso de economia no ensino médio é maior", "\n")
}

# outra forma:
# dados2 %>% group_by(econhs) %>% summarise(mean(score)) %>% ungroup()

# c) Os resultados encontrados dizem necessariamente alguma coisa sobre o efeito causal
# de cursar economia no ensino médio sobre o desempenho no curso universitário? (explique)
  # Não. Pode haver apenas uma associação entre essas duas variáveis, mas isso não implica causalidade.

# d) Se quiser obter uma boa estimativa causal do efeito de fazer um curso de economia
# no ensino médio utilizando a diferença de médias, que experiência faria?
  # Selecionaria um grupo de pessoas com características parecidas e designaria metade delas
  # para fazer um curso de economia no ensino médio e só depois compararia o desempenho delas no 
  # curso universitário com o desempenho daquelas que não fizeram o curso de economia no ensino médio. 

