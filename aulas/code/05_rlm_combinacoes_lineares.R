# ---------------------------------------------------------------------------
# CE423 - Econometria I
# 2. Regressao Linear Multipla - Testes para Combinacoes Lineares
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
library(car); library(wooldridge); library(ggplot2)
m <- ctx_metais(); d <- m$dados
cs <- combinacao(m, c(1, 1), 1); cd <- combinacao(m, c(1, -1))
mu <- lm(log(Y) ~ log(L) + log(K), data = d)
fr <- lm(log(Y) ~ I(log(L) + log(K)), data = d); RSSr_d <- sum(resid(fr)^2)
fi <- lm(log(Y / L) ~ log(K / L), data = d);     RSSr_s <- sum(resid(fi)^2)
si <- summary(fi)$coefficients
dec <- function(p) if (p < m$alpha) "rejeita" else "não rejeita"
data("wage1")
modelo <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
b <- coef(modelo); V <- vcov(modelo); gl <- df.residual(modelo)
dcb <- dados("cobb_douglas_rss.csv")
cb <- c(as.list(dcb), teste_F(dcb$rss_restrito, dcb$rss_irrestrito, dcb$q,
                              dcb$n - dcb$k - 1))
## Os 88 imoveis de wooldridge::hprice1, em logaritmo dos tres lados: preco de
## venda, area do terreno e area construida.
data("hprice1")
hp <- rlm2(log(hprice1$lotsize), log(hprice1$sqrft), log(hprice1$price))
hps <- combinacao(hp, c(1, 1), 1)
m$b0; m$b1; m$b2          # coeficientes estimados
c(t1 = m$t1, t2 = m$t2)   # t de cada coeficiente, um por vez
c(F = m$F, p = m$pF)      # F global, os dois de uma vez
m$b1 + m$b2     # a soma estimada
bc <- c(m$b1, m$b2)
Vd <- m$V             # a matriz estimada, com covariancia negativa
V0 <- diag(diag(Vd))  # a mesma variancia de cada coeficiente, sem covariancia
raio <- sqrt(2 * qf(0.95, 2, m$gl))
niveis <- c("Covariância negativa", "Covariância nula")

elipse <- function(V, cen) {
  E <- eigen(V); tt <- seq(0, 2 * pi, length.out = 181)
  d <- data.frame(t(bc + raio * E$vectors %*% diag(sqrt(E$values)) %*% rbind(cos(tt), sin(tt))))
  names(d) <- c("b1", "b2"); d$cen <- cen; d
}
nuvem <- function(V, cen, R = 400) {
  d <- data.frame(t(bc + t(chol(V)) %*% matrix(rnorm(2 * R), 2, R)))
  names(d) <- c("b1", "b2"); d$cen <- cen; d
}
## Meias-diagonais da elipse: ao longo de (1, 1) muda a soma dos dois
## coeficientes, e ao longo de (1, -1) muda a diferenca.
eixos <- function(V, cen) data.frame(
  cen = cen, s1 = raio * sqrt(V[1, 1]), s2 = raio * sqrt(V[2, 2]),
  hs = raio * sqrt(sum(V)) / 2,
  hd = raio * sqrt(V[1, 1] + V[2, 2] - 2 * V[1, 2]) / 2)

semente(20260924)
el <- rbind(elipse(Vd, niveis[1]), elipse(V0, niveis[2]))
nu <- rbind(nuvem(Vd, niveis[1]), nuvem(V0, niveis[2]))
ex <- rbind(eixos(Vd, niveis[1]), eixos(V0, niveis[2]))
el$cen <- factor(el$cen, levels = niveis)
nu$cen <- factor(nu$cen, levels = niveis)
ex$cen <- factor(ex$cen, levels = niveis)


p <- ggplot() +
  geom_point(data = nu, aes(b1, b2), size = 0.3, alpha = 0.3, colour = "grey50") +
  geom_path(data = el, aes(b1, b2), colour = "firebrick", linewidth = 0.6) +
  geom_segment(data = ex, aes(x = bc[1] - s1, xend = bc[1] + s1, y = bc[2], yend = bc[2]),
               colour = "grey25", linewidth = 0.4) +
  geom_segment(data = ex, aes(x = bc[1], xend = bc[1], y = bc[2] - s2, yend = bc[2] + s2),
               colour = "grey25", linewidth = 0.4) +
  geom_segment(data = ex, aes(x = bc[1] - hs, xend = bc[1] + hs, y = bc[2] - hs, yend = bc[2] + hs),
               linetype = "dashed", linewidth = 0.4) +
  geom_segment(data = ex, aes(x = bc[1] - hd, xend = bc[1] + hd, y = bc[2] + hd, yend = bc[2] - hd),
               linetype = "dashed", linewidth = 0.4) +
  geom_text(data = ex, aes(x = bc[1] + s1, y = bc[2], label = "$S_{\\hat\\beta_1}$"),
            hjust = -0.15, vjust = 1.4, size = 2.1) +
  geom_text(data = ex, aes(x = bc[1], y = bc[2] + s2, label = "$S_{\\hat\\beta_2}$"),
            hjust = -0.15, vjust = -0.3, size = 2.1) +
  facet_wrap(~ cen) +
  coord_equal() +
  labs(x = "$\\hat\\beta_1$", y = "$\\hat\\beta_2$",
       caption = "Elipse a 95\\% e 400 reamostragens do erro. Tracejadas, as direções da soma e da diferença.") +
  theme_minimal(base_size = 8) +
  theme(panel.grid.minor = element_blank(), plot.caption = element_text(size = 5.5),
        strip.text = element_text(size = 7),
        axis.title.y = element_text(angle = 0, vjust = 1))
## Quanto a soma e a diferenca variam, em cada cenario
ex[, c("cen", "hs", "hd")]
print(p)
Vh <- m$Se2 * m$XtXinv
diag(Vh)                       # variancias, uma por coeficiente
Vh[2, 3]                       # covariancia entre b1 e b2
m$Se2                          # variancia residual estimada
cs$var; cs$S    # variancia e erro padrao de b1 + b2
cd$var; cd$S    # variancia e erro padrao de b1 - b2
cd$S / cs$S     # quantas vezes a diferenca e menos precisa que a soma
m$Sb1           # erro padrao de b1 sozinho
cs$S            # erro padrao de b1 + b2
m$V[2, 2] + 2 * m$cov12   # negativo: e a condicao para a soma ser mais precisa
cd$theta        # b1 - b2
cd$S            # erro padrao da diferenca
cd$t            # estatistica t
m$tc            # valor critico bilateral a 5%
cs$theta        # b1 + b2
cs$S            # erro padrao da soma
cs$t            # (b1 + b2 - 1) / S
t0 <- si[2, 1] / si[2, 2]                 # H0: b2 = 0
t13 <- (si[2, 1] - 1 / 3) / si[2, 2]      # H0: b2 = 1/3
si                                        # coeficientes da forma intensiva
c(t0 = t0, t13 = t13)
2 * pt(-abs(c(t0, t13)), m$gl)            # p-valores
c(RSSr = RSSr_d, RSSur = m$RSS, custo = RSSr_d - m$RSS)
anova(fr, mu)   # o mesmo custo, pela funcao pronta
tf_d <- teste_F(RSSr_d, m$RSS, q = 1, gl = m$gl, alpha = m$alpha)
tf_d$F; tf_d$Fc; tf_d$p
## O residuo da forma intensiva, reescrito em log Y, e o mesmo residuo
b0i <- coef(fi)[1]; b2i <- coef(fi)[2]
r_logY <- log(d$Y) - (b0i + log(d$L) + b2i * (log(d$K) - log(d$L)))
tf_s <- teste_F(RSSr_s, m$RSS, q = 1, gl = m$gl, alpha = m$alpha)
all.equal(as.vector(resid(fi)), as.vector(r_logY))
c(F = tf_s$F, Fc = tf_s$Fc, p = tf_s$p)
b        # coeficientes
gl       # graus de liberdade do residuo
lam <- c(0, 0, 1, -1)              # a primeira posicao e a do intercepto
theta <- sum(lam * b)              # lambda' b
S <- sqrt(as.numeric(t(lam) %*% V %*% lam))   # raiz de lambda' V lambda
c(theta = theta, S = S, t = theta / S, tc = qt(0.975, gl))
tobs <- theta / S; tc <- qt(0.975, gl)
den <- data.frame(x = seq(-5, 5, length.out = 400))
den$y <- dt(den$x, gl)
p <- ggplot(den, aes(x, y)) +
  geom_area(data = subset(den, x <= -tc), fill = "grey80") +
  geom_area(data = subset(den, x >= tc), fill = "grey80") +
  geom_line(linewidth = 0.6) +
  geom_vline(xintercept = c(-tc, tc), linetype = "dashed", colour = "grey40",
             linewidth = 0.4) +
  geom_vline(xintercept = tobs, colour = "firebrick", linewidth = 0.7) +
  annotate("text", x = tobs, y = max(den$y) * 0.9,
           label = sprintf("$t = %s$", nm(tobs, 2)), colour = "firebrick",
           hjust = -0.12, size = 2.6) +
  annotate("text", x = 5, y = max(den$y) * 0.9,
           label = sprintf("$\\pm t_{0{,}025} = %s$", nm(tc, 2)),
           hjust = 1, size = 2.4) +
  expand_limits(y = 0) +
  labs(x = "$t$", y = NULL,
       caption = "Distribuição $t$ com os graus de liberdade do resíduo. Em cinza, as duas caudas de 2,5\\%.") +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank(), axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(), plot.caption = element_text(size = 6))
print(p)
tc <- qt(0.975, gl)
g <- rbind(
  data.frame(rotulo = "\\texttt{exper}", est = b[["exper"]],  S = sqrt(V["exper", "exper"])),
  data.frame(rotulo = "\\texttt{tenure}", est = b[["tenure"]], S = sqrt(V["tenure", "tenure"])),
  data.frame(rotulo = "\\texttt{exper} $-$ \\texttt{tenure}", est = theta, S = S))
g$rotulo <- factor(g$rotulo, levels = rev(g$rotulo))
g$ic_inf <- g$est - tc * g$S; g$ic_sup <- g$est + tc * g$S
p <- ggplot(g, aes(est, rotulo)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(xmin = ic_inf, xmax = ic_sup), orientation = "y", width = 0.12, linewidth = 0.7) +
  geom_point(size = 2.8) +
  labs(x = "Estimativa e intervalo de confiança a 95\\%", y = NULL,
       caption = "A terceira linha é a combinação. Ela tem erro padrão próprio, que não é a soma dos outros dois.") +
  theme_minimal(base_size = 9) +
  theme(plot.caption = element_text(size = 6), plot.margin = margin(2, 2, 2, 14))
g[, c("rotulo", "est", "S", "ic_inf", "ic_sup")]
print(p)
Rm <- rbind(c(0, 0, 1, 0), c(0, 0, 0, 1))   # uma linha por restricao
q0 <- c(0, 0)
Rb <- as.vector(Rm %*% b) - q0              # o quanto os dados se afastam de H0
M <- Rm %*% V %*% t(Rm)                     # a covariancia das duas discrepancias
Fobs <- as.numeric(t(Rb) %*% solve(M) %*% Rb) / nrow(Rm)
c(F = Fobs, Fc = qf(0.95, 2, gl), p = pf(Fobs, 2, gl, lower.tail = FALSE))
Fc <- qf(0.95, 2, gl)
den <- data.frame(x = seq(0.02, 6, length.out = 400))
den$y <- df(den$x, 2, gl)
p <- ggplot(den, aes(x, y)) +
  geom_area(data = subset(den, x >= Fc), fill = "grey75") +
  geom_line(linewidth = 0.6) +
  geom_vline(xintercept = Fc, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  annotate("text", x = Fc, y = max(den$y) * 0.75,
           label = sprintf("$F_{0{,}05} = %s$", nm(Fc, 2)), hjust = -0.1, size = 2.4) +
  annotate("segment", x = 5.2, xend = 6, y = max(den$y) * 0.45,
           yend = max(den$y) * 0.45, colour = "firebrick",
           arrow = arrow(length = unit(0.06, "in"))) +
  annotate("text", x = 5.1, y = max(den$y) * 0.45, colour = "firebrick",
           hjust = 1, size = 2.4,
           label = sprintf("$F = %s$, fora da escala", nm(Fobs, 2))) +
  expand_limits(y = 0) +
  labs(x = "$F$", y = NULL,
       caption = "Densidade $F$ com 2 e os graus de liberdade do resíduo. Em cinza, a cauda de 5\\%.") +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank(), axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(), plot.caption = element_text(size = 6))
print(p)
library(wooldridge); library(car); data("wage1")
modelo <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
linearHypothesis(modelo, "exper = tenure")
## A mesma conta: estimar o restrito e comparar os dois RSS
restrito <- lm(log(wage) ~ educ + I(exper + tenure), data = wage1)
anova(restrito, modelo)
