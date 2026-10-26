# ---------------------------------------------------------------------------
# CE423 - Econometria I
# 2. Regressao Linear Multipla - Correlacao Parcial
#
# Este arquivo e gerado a partir dos slides da aula: cada trecho aqui e um
# trecho que apareceu na tela. Roda de cima a baixo, sem alteracao.
#
# RODE A PARTIR DA PASTA "aulas", que e a que contem "code/" e "data/".
# Rodando de dentro de "code/", o script se corrige sozinho e avisa.
# ---------------------------------------------------------------------------
if (!file.exists("code/bloco_setup.R")) {
  if (file.exists("../code/bloco_setup.R")) {
    setwd("..")
    message("Diretorio de trabalho ajustado para ", getwd(), ".")
  } else {
    stop("Rode este script a partir da pasta 'aulas' da disciplina, ",
         "que e a que contem 'code/' e 'data/'. Diretorio atual: ", getwd())
  }
}
source("code/bloco_setup.R")
library(wooldridge); library(ggplot2)
options(lectures_decimal = ".")
m <- ctx_metais(); d <- m$dados
kL <- contribuicao(m, "x1")   # K depois de L
lK <- contribuicao(m, "x2")   # L depois de K
p  <- parcial(m)
data("wage1")
irr <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
c(ESSr = kL$ESSr, ESSur = kL$ESSu, contribuicao = kL$cont, F = kL$F)
painel <- function(x, y, titulo, rx, ry) {
  ggplot(data.frame(x = x, y = y), aes(x, y)) +
    geom_point(size = 1.4, colour = "grey20") +
    geom_smooth(method = "lm", se = FALSE, colour = "grey45", linewidth = 0.6,
                formula = y ~ x) +
    labs(title = titulo, x = rx, y = ry) +
    theme_minimal(base_size = 8) +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(size = 8))
}
p1 <- patchwork::wrap_plots(
  painel(m$x2, m$y, "Simples", "$\\log K$", "$\\log Y$"),
  painel(p$e21, p$ey1, "Parcial, descontado $\\log L$", "$\\hat u_{K.L}$", "$\\hat u_{Y.L}$"),
  ncol = 2)
eY <- residuals(lm(log(Y) ~ log(L), data = d))
eK <- residuals(lm(log(K) ~ log(L), data = d))
c(simples = cor(log(d$Y), log(d$K)), parcial = cor(eY, eK))
(p$rY2 - p$rY1 * p$r12) / sqrt((1 - p$r12^2) * (1 - p$rY1^2))
tK <- summary(lm(log(Y) ~ log(L) + log(K), data = d))$coefficients[3, "t value"]
tK^2 / (tK^2 + df.residual(lm(log(Y) ~ log(L) + log(K), data = d)))
eY <- residuals(lm(log(wage) ~ educ + exper, data = wage1))
eT <- residuals(lm(tenure ~ educ + exper, data = wage1))
tt <- summary(irr)$coefficients["tenure", "t value"]
gl <- df.residual(irr)
ex <- ctx_firmas_ex(); qx <- parcial(ex)
dn <- rbind(
  data.frame(painel = "Simples: $Y$ contra $L$", x = ex$x1, y = ex$y),
  data.frame(painel = "Parcial: res\\'iduos, descontado $K$", x = qx$e12, y = qx$ey2))
dn$painel <- factor(dn$painel, levels = c("Simples: $Y$ contra $L$", "Parcial: res\\'iduos, descontado $K$"))
p4 <- ggplot(dn, aes(x, y)) +
  geom_point(size = 1.6, colour = "grey20") +
  geom_smooth(method = "lm", se = FALSE, colour = "grey45", linewidth = 0.6, formula = y ~ x) +
  facet_wrap(~ painel, scales = "free") +
  expand_limits(y = 0) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 8) +
  theme(panel.grid.minor = element_blank())
