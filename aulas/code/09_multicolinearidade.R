# ---------------------------------------------------------------------------
# CE423 - Econometria I
# 4. Multicolinearidade - Inflacao da Variancia e Medidas Paliativas
#
# Este arquivo e gerado a partir dos slides da aula. Roda de cima a baixo,
# sem alteracao. Usa os pacotes AER e wooldridge.
#
# RODE A PARTIR DA PASTA "aulas", que e a que contem "code/" e "data/".
# Rodando de dentro de "code/", o script se corrige sozinho e avisa.
# ---------------------------------------------------------------------------
if (!file.exists("data/trabalhadores_multicol.csv")) {
  if (file.exists("../data/trabalhadores_multicol.csv")) {
    setwd("..")
    message("Diretorio de trabalho ajustado para ", getwd(), ".")
  } else {
    stop("Rode este script a partir da pasta 'aulas' da disciplina, ",
         "que e a que contem 'code/' e 'data/'. Diretorio atual: ", getwd())
  }
}

# Macroeconomia dos EUA, trimestral, 1950-2000 (pacote AER)
data("USMacroG", package = "AER")
macro <- as.data.frame(USMacroG)
# Exportacoes liquidas pela identidade do PIB: PIB = C + I + G + NX
macro$nx <- macro$gdp - macro$consumption - macro$invest - macro$government
# Colinearidade perfeita: o PIB e a soma exata dos outros quatro regressores,
# e o R nao calcula o seu coeficiente (NA)
m_ident <- lm(unemp ~ consumption + invest + government + nx + gdp, data = macro)
# Multicolinearidade: renda disponivel e PIB andam juntos
m_cons <- lm(consumption ~ dpi + gdp, data = macro)
fiv_cons <- 1 / (1 - cor(macro$dpi, macro$gdp)^2)

# Trinta trabalhadores com ate seis anos desde a conclusao dos estudos:
# salario mensal (reais), anos de estudo, idade e experiencia efetiva (anos)
trab <- read.csv("data/trabalhadores_multicol.csv")
trab$exper_pot <- trab$idade - trab$educ - 6   # experiencia potencial
trab$afast <- trab$exper_pot - trab$exper      # anos fora do mercado

m_comp <- lm(log(salario) ~ educ + idade + exper, data = trab)   # completo
cor(trab[, c("educ", "idade", "exper")])                         # correlacoes

# Regressoes auxiliares: cada regressor contra os outros dois
aux_educ  <- lm(educ ~ idade + exper, data = trab)
aux_idade <- lm(idade ~ educ + exper, data = trab)
aux_exper <- lm(exper ~ educ + idade, data = trab)
r2_aux <- c(educ = summary(aux_educ)$r.squared,
            idade = summary(aux_idade)$r.squared,
            exper = summary(aux_exper)$r.squared)
fiv <- 1 / (1 - r2_aux)   # fator de inflacao da variancia
# O mesmo calculo pronto: car::vif(m_comp)

# Com a experiencia potencial, idade = educ + 6 + exper_pot: o R descarta o
# terceiro regressor (coeficiente NA), porque X'X nao tem inversa
m_pot <- lm(log(salario) ~ educ + idade + exper_pot, data = trab)

# Medidas paliativas
trab300 <- read.csv("data/trabalhadores_multicol_300.csv")
m_300 <- lm(log(salario) ~ educ + idade + exper, data = trab300)  # mais dados
m_rep <- lm(log(salario) ~ educ + exper + afast, data = trab)     # reparametrizado
m_sem <- lm(log(salario) ~ educ + exper, data = trab)             # sem a idade

summary(m_ident)
summary(m_cons)
summary(m_comp)
round(fiv, 2)
summary(m_pot)
summary(m_300)
summary(m_rep)
summary(m_sem)

library(wooldridge); data("hprice1")
m_casa <- lm(log(price) ~ log(lotsize) + log(sqrft)
             + bdrms, data = hprice1)
summary(m_casa)
# Fator de inflacao de cada regressor
X <- model.frame(m_casa)[, -1]
fiv_casa <- sapply(seq_along(X), function(j)
  1 / (1 - summary(lm(X[[j]] ~ ., data = X[-j]))$r.squared))
round(fiv_casa, 2)   # ou car::vif(m_casa)
