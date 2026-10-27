# ---------------------------------------------------------------------------
# CE423 - Econometria I
# 5. Problemas de Especificacao - Erros de Especificacao, Testes e Escolha
#    do Modelo
#
# Este arquivo e gerado a partir dos slides da aula. Roda de cima a baixo,
# sem alteracao. Os dados vem do pacote wooldridge:
#   install.packages("wooldridge")
# ---------------------------------------------------------------------------
library(wooldridge)

# Salario por hora (centavos de dolar) em 1976 de homens americanos de 24 a
# 34 anos (Card, 1995); ficam os 2040 com o QI e o KWW observados
data("card")
card2 <- subset(card, !is.na(IQ) & !is.na(KWW))

# Omissao da experiencia
curto <- lm(lwage ~ educ, data = card2)          # sem a experiencia
longo <- lm(lwage ~ educ + exper, data = card2)  # com a experiencia
aux <- lm(exper ~ educ, data = card2)            # experiencia contra educacao
b_curto <- coef(curto)[["educ"]]
b1 <- coef(longo)[["educ"]]; b2 <- coef(longo)[["exper"]]
delta <- coef(aux)[["educ"]]
b1 + b2 * delta                                  # igual a b_curto

# Forma funcional: salario em nivel, e logaritmo com a experiencia linear e
# com o seu quadrado
niv <- lm(wage ~ educ + exper + expersq + black + south + smsa, data = card2)
lin <- lm(lwage ~ educ + exper + black + south + smsa, data = card2)
qua <- update(lin, . ~ . + expersq)
com_qi <- update(qua, . ~ . + IQ)                # com o QI como proxy

# RESET: potencias 2 e 3 do valor ajustado como regressores novos
reset <- function(m, dados) {
  dados$yh <- fitted(m)
  aum <- update(m, . ~ . + I(yh^2) + I(yh^3), data = dados)
  r2 <- summary(m)$r.squared; r2a <- summary(aum)$r.squared
  gl <- df.residual(aum)
  F <- ((r2a - r2) / 2) / ((1 - r2a) / gl)
  c(R2 = r2, R2_aum = r2a, F = F, gl = gl, p = pf(F, 2, gl, lower.tail = FALSE))
}
rs_niv <- reset(niv, card2); rs_lin <- reset(lin, card2)
rs_qua <- reset(qua, card2); rs_qi <- reset(com_qi, card2)

# O mesmo teste pronto, no pacote lmtest:
# lmtest::resettest(lin, power = 2:3)

# Criterios de escolha
crit <- function(m) c(R2 = summary(m)$r.squared, R2aj = summary(m)$adj.r.squared,
                      AIC = AIC(m), BIC = BIC(m))

summary(curto); summary(longo); summary(aux)
summary(niv); summary(lin); summary(qua); summary(com_qi)
rbind(niv = rs_niv, lin = rs_lin, qua = rs_qua, com_qi = rs_qi)
rbind(lin = crit(lin), qua = crit(qua))

# Teste J: educacao em nivel (A) contra em logaritmo (B)
mA <- lm(lwage ~ educ + exper + expersq + black + south + smsa,
         data = card2)
mB <- update(mA, . ~ . - educ + log(educ))
yA <- fitted(mA); yB <- fitted(mB)
pJ <- function(m) summary(m)$coefficients["y", 4]
jt <- data.frame(p = c(
  pJ(update(mA, . ~ . + y, data = cbind(card2, y = yB))),
  pJ(update(mB, . ~ . + y, data = cbind(card2, y = yA)))))

f <- lwage ~ educ + exper + expersq + black + south + smsa
m_sem <- lm(f, data = card2)                   # sem proxy
m_qi <- update(m_sem, . ~ . + IQ)              # com o QI
m_qk <- update(m_qi, . ~ . + KWW)              # com o QI e o KWW
d_qi <- coef(lm(update(f, IQ ~ .), data = card2))[["educ"]]
