# ---------------------------------------------------------------------------
# CE423 - Econometria I
# 2. Regressao Linear Multipla - Contribuicao Marginal
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
mu <- lm(log(Y) ~ log(L) + log(K), data = d)
mL <- lm(log(Y) ~ log(L), data = d)
mK <- lm(log(Y) ~ log(K), data = d)
dec <- function(pv) if (pv < m$alpha) "rejeita $H_0$" else "não rejeita $H_0$"
data("wage1")
irr <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
res <- lm(log(wage) ~ educ, data = wage1)
av  <- anova(irr)
summary(mu)   # o ajuste das aulas de estimacao e de combinacoes lineares
c(TSS = m$TSS, ESS = m$ESS, RSS = m$RSS)
gd <- data.frame(x = seq(0.3, max(8, 1.8 * kL$Fc), length.out = 400))
gd$d <- df(gd$x, 1, m$gl)
gd$faixa <- factor(ifelse(gd$x < 1, "a", ifelse(gd$x < kL$Fc, "b", "c")), levels = c("a", "b", "c"))
p0 <- ggplot(gd, aes(x, d)) +
  geom_area(data = subset(gd, faixa == "b"), fill = "grey80") +
  geom_area(data = subset(gd, faixa == "c"), fill = "grey45") +
  geom_line(colour = "grey15", linewidth = 0.6) +
  geom_vline(xintercept = c(1, kL$Fc), linetype = "dashed", colour = "grey20") +
  annotate("text", x = (1 + kL$Fc) / 2, y = 0.42, size = 2.5, label = "$\\bar R^2$ sobe, o teste não rejeita") +
  annotate("text", x = kL$Fc + 0.15, y = 0.2, hjust = 0, size = 2.5, label = "o teste rejeita") +
  annotate("text", x = 1, y = -0.03, size = 2.4, label = "$1$") +
  annotate("text", x = kL$Fc, y = -0.03, size = 2.4, label = paste0("$F_{0.05} = ", nm(kL$Fc, 2), "$")) +
  labs(x = paste0("$F$ da contribuição, sob $H_0$: $F_{1,\\,", ni(m$gl), "}$"), y = "Densidade") +
  expand_limits(y = 0) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank())
anova(mL, mu)   # restrito contra irrestrito: Df = q, Res.Df = n - k - 1
c(ganho = kL$ESSu - kL$ESSr, perda = kL$RSSr - kL$RSSu)
c(ESSr = kL$ESSr, ESSur = kL$ESSu, acrescimo = kL$cont)
anova(mu)                                              # L, depois K
anova(lm(log(Y) ~ log(K) + log(L), data = d))          # K, depois L
p1 <- ggplot(d, aes(log(L), log(K))) +
  geom_point(size = 1.4, colour = "grey20") +
  geom_smooth(method = "lm", se = FALSE, colour = "grey45", linewidth = 0.6,
              formula = y ~ x) +
  labs(x = "$\\log L$", y = "$\\log K$") +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 1, margin = margin(r = 6)))
gr <- data.frame(x = seq(1.2, max(14, 1.15 * kL$F), length.out = 400))
gr$d <- df(gr$x, 1, m$gl)
reg <- subset(gr, x >= kL$Fc)
p3 <- ggplot(gr, aes(x, d)) +
  geom_area(data = reg, aes(x, d), fill = "grey72") +
  geom_line(colour = "grey15", linewidth = 0.6) +
  geom_vline(xintercept = kL$F, linetype = "dashed", colour = "grey10") +
  annotate("text", x = kL$F, y = 0.085, hjust = 1.08, size = 2.6,
           label = paste0("$F = ", nm(kL$F, 2), "$")) +
  annotate("text", x = kL$Fc, y = 0.038, hjust = 1.08, size = 2.6,
           label = paste0("$F_{0.05} = ", nm(kL$Fc, 2), "$")) +
  labs(x = paste0("$F_{1,\\,", ni(m$gl), "}$"), y = "Densidade") +
  expand_limits(y = 0) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank())
tt <- summary(irr)$coefficients["tenure", "t value"]
av2 <- anova(lm(log(wage) ~ tenure + exper + educ, data = wage1))
