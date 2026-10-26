# ---------------------------------------------------------------------------
# CE423 - Econometria I
# 3. Modelos com Variaveis Binarias - Variaveis Binarias e Mudanca Estrutural
#
# Este arquivo e gerado a partir dos slides da aula. Roda de cima a baixo,
# sem alteracao.
#
# RODE A PARTIR DA PASTA "aulas", que e a que contem "code/" e "data/".
# Rodando de dentro de "code/", o script se corrige sozinho e avisa.
# ---------------------------------------------------------------------------
if (!file.exists("data/quartos_binarias.csv")) {
  if (file.exists("../data/quartos_binarias.csv")) {
    setwd("..")
    message("Diretorio de trabalho ajustado para ", getwd(), ".")
  } else {
    stop("Rode este script a partir da pasta 'aulas' da disciplina, ",
         "que e a que contem 'code/' e 'data/'. Diretorio atual: ", getwd())
  }
}

# Oitenta quartos em republicas: aluguel mensal (reais), area (m2), distancia
# a Unicamp (km), Q = 1 se o quarto e dividido e B = 1 se o banheiro e dividido.
quartos <- read.csv("data/quartos_binarias.csv")

m_dist <- lm(aluguel ~ dist, data = quartos)                   # so a distancia
m_int  <- lm(aluguel ~ area + dist + Q, data = quartos)        # binaria de intercepto
m_log  <- lm(log(aluguel) ~ area + dist + Q, data = quartos)   # semilogaritmica
m_bq   <- lm(aluguel ~ area + dist + Q + B + Q:B, data = quartos)           # interacao
m_incl <- lm(aluguel ~ area + dist + Q + B + Q:B + Q:dist, data = quartos)  # inclinacao
m_rest <- lm(aluguel ~ area + dist + B, data = quartos)        # sem nenhum termo com Q

summary(m_int)
exp(coef(m_log)["Q"]) - 1   # variacao relativa exata do aluguel do quarto dividido
summary(m_bq)
summary(m_incl)
anova(m_rest, m_incl)       # F da mudanca estrutural: os tres termos com Q

library(wooldridge); data("wage1")
# female: 1 para mulheres; married: 1 para casados
m_wage <- lm(log(wage) ~ educ + exper + tenure
             + female * married, data = wage1)
summary(m_wage)
# diferencas percentuais
exp(coef(m_wage)[c("female", "married")]) - 1
m_wage_r  <- lm(log(wage) ~ educ + exper + tenure,
                data = wage1)
m_wage_ur <- lm(log(wage) ~ educ + exper + tenure
                + female + educ:female, data = wage1)
anova(m_wage_r, m_wage_ur)
