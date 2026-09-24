# ---------------------------------------------------------------------------
# CE423 - Econometria I
# 2. Regressao Linear Multipla - Contribuicao Marginal e Correlacao Parcial
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
m <- ctx_firmas()
kL <- contribuicao(m, "x1")   # K depois de L
lK <- contribuicao(m, "x2")   # L depois de K
p  <- parcial(m)
dec <- function(pv) if (pv < m$alpha) "rejeita $H_0$" else "não rejeita $H_0$"
data("wage1")
irr <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
res <- lm(log(wage) ~ educ, data = wage1)
av  <- anova(irr)
e <- ctx_firmas_ex(); d <- e$dados
# So as duas contribuicoes. As parcelas grandes (ESS do primeiro regressor e
# RSS) estao na tabela do frame anterior; postas aqui, tornariam estas duas
# invisiveis -- 813 contra 31.014 nao se distingue de zero numa barra.
d <- data.frame(
  qual = factor(c("$K$, dado $L$", "$L$, dado $K$"),
                levels = c("$K$, dado $L$", "$L$, dado $K$")),
  valor = c(kL$cont, lK$cont))
p1 <- ggplot(d, aes(qual, valor)) +
  geom_col(width = 0.5, fill = "grey35") +
  geom_text(aes(label = nm(valor, 1)), vjust = -0.5, size = 2.7) +
  labs(x = NULL, y = "Soma de quadrados") +
  expand_limits(y = 0) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())
gr <- data.frame(x = seq(1.2, 14, length.out = 400))
gr$d <- df(gr$x, 1, m$gl)
reg <- subset(gr, x >= kL$Fc)
p1 <- ggplot(gr, aes(x, d)) +
  geom_area(data = reg, aes(x, d), fill = "grey72") +
  geom_line(colour = "grey15", linewidth = 0.6) +
  geom_vline(xintercept = kL$F, linetype = "dashed", colour = "grey10") +
  annotate("text", x = kL$F, y = 0.085, hjust = -0.12, size = 2.6,
           label = sprintf("$F = %s$", nm(kL$F, 2))) +
  annotate("text", x = kL$Fc, y = 0.038, hjust = 1.08, size = 2.6,
           label = sprintf("$F_{0{,}05} = %s$", nm(kL$Fc, 2))) +
  labs(x = sprintf("$F_{1,\\,%s}$", ni(m$gl)), y = "Densidade") +
  expand_limits(y = 0) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank())
d <- rbind(
  data.frame(painel = "Simples: $\\log Y$ contra $\\log K$", x = m$x2, y = m$y),
  data.frame(painel = "Parcial: res\\'iduos, descontado $L$", x = p$e21, y = p$ey1))
d$painel <- factor(d$painel, levels = c("Simples: $\\log Y$ contra $\\log K$",
                                        "Parcial: res\\'iduos, descontado $L$"))
p1 <- ggplot(d, aes(x, y)) +
  geom_point(size = 1.4, colour = "grey20") +
  geom_smooth(method = "lm", se = FALSE, colour = "grey45", linewidth = 0.6,
              formula = y ~ x) +
  facet_wrap(~ painel, scales = "free") +
  labs(x = NULL, y = NULL) +
  expand_limits(y = 0) +
  theme_minimal(base_size = 8) +
  theme(panel.grid.minor = element_blank())
tt <- summary(irr)$coefficients["tenure", "t value"]
gl <- df.residual(irr)
av2 <- anova(lm(log(wage) ~ tenure + exper + educ, data = wage1))
eY <- residuals(lm(log(wage) ~ educ + exper, data = wage1))
eT <- residuals(lm(tenure ~ educ + exper, data = wage1))
d <- e$dados
k <- contribuicao(e, "x1"); q <- parcial(e)
q <- parcial(e)
