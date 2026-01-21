# Homework_03
Trabalho de Estatística com o Software R

# Arquivos:
- q1.R                        # Análise do tempo de vida de computadores que descreve uma distribuição exponencial
- q2.R                        # Estudo sobre os dados de diferentes espécies de pinguins
- questions.R                 # Arquivo com todas as questões
- Homework_03.pdf             # Relatório do trabalho (PDF)
- README.md                   # Este arquivo

### PROCESSO
Nesse projeto, foram trabalhadas as competências em R relacionadas à análise estatística de dados, inferência estatística e modelagem probabilística, por meio de questões contextualizadas. A partir dos valores e das visualizações gráficas obtidas, foi possível fazer interpretações acerca do comportamento dos dados e como ele se traduziu nos contextos apresentados. Além disso, também foram realizados os cálculos teóricos das questões de modo a possibilitar uma comparação com os resultados obtidos pelos métodos utilizados pelo R.

Para isso, o projeto foi separado em dois arquivos de código em R (q1.R, q2.R), um para cada questão, os quais foram criados e programados pela plataforma RStudio conectada ao Git. Desse modo, os integrantes faziam suas partes simultaneamente, enviando as alterações feitas para o repositório remoto do gitHub, no qual conseguiam visualizar todo o trabalho.  
Ademais, o relatório do projeto foi feito na linguagem LATEX na plataforma Overleaf, que também permite uma edição simultânea dos textos. 

## INSTRUÇÕES PARA EXECUÇÃO
Para rodar o código, basta:
1. Clone o repositório:
- git clone 

2. Instale as Dependências #Execute no R/RStudio:
- dependencies <- c(
  "ggplot2",
  "palmerpenguins",
  "latex2exp"
)

install.packages(dependencies, dependencies = TRUE)


3. Para rodar o código execute no console do R:
- source("questions.R") 
