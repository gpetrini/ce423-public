# ---------------------------------------------------------------------------
# CE423 - Econometria I
# 2. Regressao Linear Multipla - Teste de Hipoteses para os Parametros
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
dq <- dados("quartos_republicas.csv")
a <- rlmk(dq[, c("area", "dist", "idade")], dq$aluguel,
          rot = c("\\text{área}", "\\text{dist}", "\\text{idade}"))
a$dados <- dq
## Processo gerador, exibido no frame de revelacao. Os valores sao os do bloco
## `gera-quartos`, abaixo, que e quem grava o CSV.
a$dgp <- list(b = c(450, 35, -80, -1.5), sigma = 90)
## Valor de referencia da hipotese economica: cada quilometro a mais de
## distancia reduziria o aluguel em cem reais por mes.
a$c_econ <- -100
options(lectures_decimal = ".")
m <- ctx_metais()
options(lectures_decimal = ".")
e <- ctx_firmas_ex(); d <- dados("firmas_exercicio.csv")
options(lectures_decimal = ".")
data("wage1")
modelo <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
options(lectures_decimal = ".")
q <- dados("anpec2021_q15.csv")
options(lectures_decimal = ".")
semente(7)
x <- seq(1, 15, length.out = 15)
y_sim <- 2 + 1.2 * x + rnorm(15, 0, 1.6)
paineis <- c("$Y_i - \\bar Y$: a variação total", "$\\hat Y_i - \\bar Y$: a parcela explicada", "$Y_i - \\hat Y_i$: o resíduo")
g <- rbind(data.frame(x = x, y = y_sim, painel = paineis[1]),
           data.frame(x = x, y = y_sim, painel = paineis[2]),
           data.frame(x = x, y = y_sim, painel = paineis[3]))
g$painel <- factor(g$painel, levels = paineis)
## Media, ajuste e as tres parcelas de cada ponto, painel a painel.
g <- do.call(rbind, lapply(split(g, g$painel), function(d) {
  f <- lm(y ~ x, data = d)
  d$my <- mean(d$y); d$fi <- fitted(f); d$b0 <- coef(f)[1]; d$b1 <- coef(f)[2]
  d
}))
p1 <- subset(g, painel == paineis[1]); p2 <- subset(g, painel == paineis[2]); p3 <- subset(g, painel == paineis[3])
p <- ggplot(g, aes(x, y)) +
  geom_hline(aes(yintercept = my), linetype = "dashed", colour = "grey45") +
  geom_segment(data = p1, aes(xend = x, y = my, yend = y), colour = "grey40", linewidth = 0.7) +
  geom_segment(data = p2, aes(xend = x, y = my, yend = fi), colour = "firebrick", linewidth = 0.8) +
  geom_segment(data = p3, aes(xend = x, y = fi, yend = y), colour = "steelblue4", linewidth = 0.8) +
  geom_abline(data = rbind(p2, p3)[!duplicated(rbind(p2, p3)$painel), ], aes(intercept = b0, slope = b1),
              colour = "grey15", linewidth = 0.5) +
  geom_point(size = 1.3, colour = "grey20") +
  geom_text(data = g[!duplicated(g$painel), ], aes(x = 15, y = my, label = "$\\bar Y$"),
            vjust = -0.4, hjust = 1, size = 2.4, colour = "grey45") +
  facet_wrap(~ painel) +
  expand_limits(y = 0) +
  labs(x = "$X$", y = "$Y$") +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(face = "bold", size = 8),
        panel.grid.minor = element_blank(), axis.text = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 1))
## Tres observacoes bastam para mostrar a degeneracao: a terceira coluna de
## X e o dobro da segunda, e X'X herda a proporcao entre a segunda e a
## terceira linhas -- determinante zero, sem inversa.
x1 <- c(1, 2, 3); Xs <- cbind(1, x1, 2 * x1); XtXs <- t(Xs) %*% Xs
semente(19)
rs <- c(0.3, 1); n <- 80
paineis <- sprintf("$r = %s$", nm(rs, 1))
am <- do.call(rbind, lapply(seq_along(rs), function(i) {
  x1 <- rnorm(n); x2 <- rs[i] * x1 + sqrt(1 - rs[i]^2) * rnorm(n)
  data.frame(x1 = x1, x2 = x2, painel = paineis[i])
}))
am$painel <- factor(am$painel, levels = paineis)
lim <- 3.2; alt <- 1.1
marg <- do.call(rbind, lapply(paineis, function(pn) {
  d <- am[am$painel == pn, ]
  d1 <- density(d$x1, from = -lim, to = lim); d2 <- density(d$x2, from = -lim, to = lim)
  rbind(data.frame(x1 = d1$x, x2 = lim + 0.15 + alt * d1$y, painel = pn, qual = "x1"),
        data.frame(x1 = lim + 0.15 + alt * d2$y, x2 = d2$x, painel = pn, qual = "x2"))
}))
marg$painel <- factor(marg$painel, levels = paineis)
## A intuicao, em segmentos: no painel de r = 0,3 um segmento horizontal a X2
## fixo; no de r = 1, um segmento sobre a reta.
seg <- data.frame(painel = factor(paineis, levels = paineis),
                  x = c(-1.6, -1.6), xend = c(1.6, 1.6), y = c(-2.4, -1.6), yend = c(-2.4, 1.6),
                  rot = c("a $X_2$ fixo, $X_1$ varia", "mover $X_1$ é mover $X_2$"),
                  rx = c(0, 0.7), ry = c(-2.9, -2.6))
p <- ggplot(am, aes(x1, x2)) +
  geom_point(size = 0.8, colour = "grey45", alpha = 0.8) +
  geom_path(data = marg, aes(x1, x2, group = qual), colour = "firebrick", linewidth = 0.55) +
  geom_hline(yintercept = lim + 0.15, colour = "grey80", linewidth = 0.3) +
  geom_vline(xintercept = lim + 0.15, colour = "grey80", linewidth = 0.3) +
  geom_segment(data = seg, aes(x = x, xend = xend, y = y, yend = yend),
               colour = "black", linewidth = 0.7,
               arrow = arrow(ends = "both", length = unit(0.12, "cm"))) +
  geom_text(data = seg, aes(x = rx, y = ry, label = rot), size = 2.4, colour = "black") +
  facet_wrap(~ painel) +
  labs(x = "$X_1$", y = "$X_2$") +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(face = "bold", size = 8),
        panel.grid = element_blank(), axis.text = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 1))
dq6 <- head(a$dados, 4)
tb <- data.frame(ni(dq6$quarto), ni(dq6$area), nm(dq6$dist, 1), ni(dq6$idade), ni(dq6$aluguel))
names(tb) <- c("Quarto", "Área (m$^2$)", "Distância (km)", "Idade (anos)", "Aluguel (R\\$)")
paineis <- c("Área (m$^2$)", "Distância (km)", "Idade (anos)")
dq <- a$dados
pl <- rbind(data.frame(x = dq$area, y = dq$aluguel, painel = paineis[1]),
            data.frame(x = dq$dist, y = dq$aluguel, painel = paineis[2]),
            data.frame(x = dq$idade, y = dq$aluguel, painel = paineis[3]))
pl$painel <- factor(pl$painel, levels = paineis)
p <- ggplot(pl, aes(x, y)) +
  geom_point(size = 1.4, colour = "grey25") +
  facet_wrap(~ painel, scales = "free_x") +
  expand_limits(y = 0) +
  labs(x = NULL, y = "Aluguel (R\\$)") +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(face = "bold", size = 7),
        axis.title.y = element_text(angle = 0, vjust = 1, margin = margin(r = 6)),
        panel.grid.minor = element_blank())
dq <- a$dados
## Duas primeiras observacoes, reticencias e a ultima: a matriz inteira nao
## cabe, e o que o frame estabelece e a forma, nao a conta.
linha <- function(i) c("1", ni(dq$area[i]), nm(dq$dist[i], 1), ni(dq$idade[i]))
X <- rbind(linha(1), linha(2), rep("\\vdots", 4), linha(a$n))
y <- c(ni(dq$aluguel[1]), ni(dq$aluguel[2]), "\\vdots", ni(dq$aluguel[a$n]))
u <- c("u_1", "u_2", "\\vdots", sprintf("u_{%s}", ni(a$n)))
diag_box <- function(M, d) {
  C <- matrix(nm(M, d), nrow(M)); diag(C) <- sprintf("\\boxed{%s}", diag(C)); C
}
semente(3)
paineis <- c("Dispersão constante, (P3)", "Dispersão que cresce com $\\hat Y$")
esc <- (a$aj - min(a$aj)) / diff(range(a$aj))
g <- rbind(data.frame(aj = a$aj, u = a$u, painel = paineis[1]),
           data.frame(aj = a$aj, u = rnorm(a$n, 0, 25 + 160 * esc), painel = paineis[2]))
g$painel <- factor(g$painel, levels = paineis)
p <- ggplot(g, aes(aj, u)) +
  geom_hline(yintercept = 0, colour = "grey45") +
  geom_segment(aes(xend = aj, yend = 0), colour = "steelblue4", linewidth = 0.4) +
  geom_point(size = 1.2, colour = "grey20") +
  facet_wrap(~ painel) +
  labs(x = "$\\hat Y_i$ (R\\$)", y = "$\\hat u_i$") +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(face = "bold", size = 8),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 1, margin = margin(r = 6)))
C <- matrix(nm(a$XtXinv, 5), 4); C[3, 3] <- sprintf("\\boxed{%s}", C[3, 3])
## Diagonal em \boxed{}, como na inversa: e dela que saem os erros padrao.
diag_box <- function(M, d) {
  C <- matrix(nm(M, d), nrow(M)); diag(C) <- sprintf("\\boxed{%s}", diag(C)); C
}
us <- function(j, d) sprintf("\\underset{(%s)}{%s}", nm(a$Sb[j], d), nm(abs(a$b[j]), d))
sg <- function(j) ifelse(a$b[j] < 0, "-", "+")
j <- 2:4
pv <- function(p) ifelse(p < 0.001, "$< 0.001$", paste0("$", nm(p, 3), "$"))
g <- a$gl; lim <- ceiling(max(abs(a$t[2:4]), a$tc) + 0.8)
## constante da densidade t avaliada no R: pgfplots so precisa da forma fechada
cte <- gamma((g + 1) / 2) / (sqrt(g * pi) * gamma(g / 2))
dens <- sprintf("%s*(1+x^2/%s)^(-%s)", signif(cte, 6), g, (g + 1) / 2)
alt <- "Densidade t com as duas caudas de rejeicao sombreadas: as estatisticas da area e da distancia caem longe dentro das caudas, e a da idade cai no centro, fora delas."
rot <- c("$\\beta_1$ (área)", "$\\beta_2$ (dist)", "$\\beta_3$ (idade)")
j <- 2:4
g <- data.frame(termo = factor(rot, levels = rev(rot)), est = a$b[j],
                lo = a$ic[j, 1], hi = a$ic[j, 2])
p <- ggplot(g, aes(est, termo, colour = termo)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = 0.15, linewidth = 0.8) +
  geom_point(size = 2.6) +
  scale_colour_manual(values = c("grey35", "steelblue4", "firebrick"), guide = "none") +
  ## O porcento vai escapado: com o motor tikz o rotulo e escrito no .tex.
  labs(x = "Estimativa e intervalo de confiança a 95\\% (R\\$ por mês, por unidade do regressor)", y = NULL) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank())
c0 <- a$c_econ
tc0 <- (a$b[3] - c0) / a$Sb[3]
tcu <- qt(1 - a$alpha, a$gl)
## A mesma densidade t do frame da regiao de rejeicao, agora com as duas
## regioes sobrepostas: as duas caudas do bilateral (claras, alpha/2 cada) e a
## cauda esquerda do unilateral (escura, alpha inteiro), com t da idade
## marcado. Estatica, em pgfplots, como a anterior.
g <- a$gl; lim <- 4
tcu <- qt(1 - a$alpha, g)
cte <- gamma((g + 1) / 2) / (sqrt(g * pi) * gamma(g / 2))
dens <- sprintf("%s*(1+x^2/%s)^(-%s)", signif(cte, 6), g, (g + 1) / 2)
alt <- "Densidade t com as duas caudas do teste bilateral em vermelho e, entre o critico unilateral e o bilateral da esquerda, uma faixa azul que so o teste unilateral rejeita; a estatistica da idade cai fora das duas regioes."
j <- 2:4
dec <- function(p) ifelse(p < a$alpha, "rejeita", "não rejeita")
pv <- function(p) ifelse(p < 0.001, "$< 0.001$", paste0("$", nm(p, 3), "$"))
bt <- a$dgp$b
## Fator que multiplica a variancia do coeficiente, 1/(1 - r^2), contra r^2.
## Curva analitica; os pontos marcam valores de referencia citados em aula.
## Sem titulo no eixo vertical: o rotulo com fracao transbordava a margem, e
## a prosa diz o que a curva e.
g <- data.frame(r2 = seq(0, 0.96, by = 0.005))
g$f <- 1 / (1 - g$r2)
marc <- data.frame(r2 = c(0.5, 0.8, 0.9, 0.95)); marc$f <- 1 / (1 - marc$r2)
p <- ggplot(g, aes(r2, f)) +
  geom_line(colour = "grey15", linewidth = 0.6) +
  geom_point(data = marc, colour = "firebrick", size = 1.8) +
  geom_text(data = marc, aes(label = sprintf("$\\times %s$", ni(f))), colour = "firebrick",
            hjust = 1.3, vjust = 0.2, size = 2.4) +
  labs(x = "$r_{12}^2$", y = NULL) +
  expand_limits(y = 0) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank())
## Conceitual: as duas distribuicoes amostrais de beta_1 chapeu, uma sob cada
## modelo. Com X_2 no modelo, centrada no parametro e dispersa; sem X_2,
## concentrada e deslocada pelo vies de omissao. A legenda traz a forma
## funcional de cada curva, e nao ha rotulo dentro do painel.
beta <- 1; vies <- 1.3
modelos <- c("com $X_2$", "sem $X_2$")
x <- seq(-2.5, 4.5, length.out = 500)
g <- rbind(data.frame(x = x, f = dnorm(x, beta, 1.0), modelo = modelos[1]),
           data.frame(x = x, f = dnorm(x, beta + vies, 0.45), modelo = modelos[2]))
g$modelo <- factor(g$modelo, levels = modelos)
p <- ggplot(g, aes(x, f, colour = modelo, fill = modelo)) +
  geom_area(alpha = 0.25, position = "identity") +
  geom_line(linewidth = 0.6) +
  geom_vline(xintercept = beta, linetype = "dashed", colour = "grey30") +
  annotate("text", x = beta, y = 0.9, label = "$\\beta_1$", size = 2.8, hjust = -0.3) +
  scale_colour_manual(values = c("grey25", "firebrick"), name = NULL) +
  scale_fill_manual(values = c("grey60", "firebrick"), name = NULL) +
  labs(x = NULL, y = NULL) +
  expand_limits(y = 0) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank(), axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(), legend.position = c(0.12, 0.8),
        legend.text = element_text(size = 8), legend.key.height = unit(0.35, "cm"),
        legend.background = element_blank())
j <- 2:4
pv <- function(p) ifelse(p < 0.001, "$< 0.001$", paste0("$", nm(p, 3), "$"))
tb <- data.frame(
  Regressor = c("área", "dist", "idade"),
  Unidade = c("m$^2$", "km", "anos"),
  `$\\hat\\beta_j$ (R\\$/mês por unidade)` = paste0("$", nm(a$b[j], 2), "$"),
  `$p$-valor` = pv(a$p[j]),
  `Significativo?` = ifelse(abs(a$t[j]) > a$tc, "sim", "não"),
  check.names = FALSE)
## A mesma estimativa da idade em amostras cada vez maiores, com S_e e a
## dispersao do regressor fixos nos valores desta amostra: o erro padrao cai
## com a raiz de n - 1. A decisao de cada linha sai dos numeros.
j <- 4; nn <- c(a$n, 100, 500, 5000)
Sn <- a$Sb[j] * sqrt((a$n - 1) / (nn - 1)); tn <- a$b[j] / Sn
tcn <- qt(1 - a$alpha / 2, nn - a$k - 1)
Sb0 <- sqrt(m$Se2 * m$XtXinv[1, 1])
us <- function(b, s) sprintf("\\underset{(%s)}{%s}", nm(s, 3), nm(abs(b), 2))
rot <- c("$\\beta_1$ ($\\log L$)", "$\\beta_2$ ($\\log K$)")
g <- data.frame(termo = factor(rot, levels = rev(rot)),
                est = c(m$b1, m$b2), lo = c(m$ic1[1], m$ic2[1]), hi = c(m$ic1[2], m$ic2[2]))
p <- ggplot(g, aes(est, termo)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = 0.12, linewidth = 0.7) +
  geom_point(size = 2.8) +
  labs(x = "Estimativa e intervalo de confiança a 95\\%", y = NULL) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank())
co <- summary(modelo)$coefficients
us <- function(j) sprintf("\\underset{(%s)}{%s}", nm(co[j, 2], 3), nm(abs(co[j, 1]), 3))
sg <- function(j) ifelse(co[j, 1] < 0, "-", "+")
co <- summary(modelo)$coefficients
estrela <- function(p) ifelse(p < 0.001, "^{\\star\\star\\star}", ifelse(p < 0.01, "^{\\star\\star}", ifelse(p < 0.05, "^{\\star}", "")))
rot <- c("Escolaridade (anos)", "Experiência (anos)", "Tempo no emprego atual (anos)", "Constante")
ord <- c(2, 3, 4, 1)
linhas <- sprintf("$%s%s$ ($%s$)", nm(co[ord, 1], 3), estrela(co[ord, 4]), nm(co[ord, 2], 3))
tb <- data.frame(c(rot, "$n$", "$R^2$"),
                 c(linhas, ni(nobs(modelo)), nm(summary(modelo)$r.squared, 3)))
names(tb) <- c("", "$\\log(\\text{salário-hora})$")
ic <- confint(modelo)[-1, , drop = FALSE]
## Nome de variavel do R sai em maquina de escrever, como nos slides.
rot <- sprintf("\\texttt{%s}", rownames(ic))
g <- data.frame(termo = factor(rot, levels = rev(rot)),
                est = coef(modelo)[-1], lo = ic[, 1], hi = ic[, 2])
p <- ggplot(g, aes(est, termo)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = 0.12, linewidth = 0.7) +
  geom_point(size = 2.8) +
  labs(x = "Estimativa e intervalo de confiança a 95\\%", y = NULL,
       caption = "Mesma leitura da tabela, sem ler número por número.") +
  theme_minimal(base_size = 9) +
  theme(plot.caption = element_text(size = 6))
itens <- c(
  sprintf("Um coeficiente cuja razão entre estimativa e erro padrão vale $%s$ é significativo a $%s\\%%$ em um teste bilateral.",
          nm(q$razao_avaliada, 1), ni(100 * q$alpha_item)),
  "A soma de quadrados dos resíduos pode ser obtida como $(1-R^2)\\,\\text{TSS}$.",
  "Ao acrescentar um regressor irrelevante, o $R^2$ não cai e o $\\bar R^2$ pode cair.",
  "A estatística $F$ de significância da regressão testa a hipótese de que todos os coeficientes de inclinação são nulos simultaneamente.",
  "Em um modelo com $\\log(\\text{preço})$ como dependente, o coeficiente de uma variável binária multiplicado por $100$ aproxima a variação percentual do preço associada à característica.")
j <- 2:4; rot <- c("área", "distância", "idade")
tc <- qt(1 - q$alpha_item / 2, q$n - q$k - 1)
## 500 amostras de n = 30, y = 1 + x1 + x2 + u, com correlacao 0,95 entre x1
## e x2; os regressores ficam fixos entre as amostras e so u e sorteado de
## novo. Cada ponto e o par (b1, b2) estimado numa amostra, com as duas
## medias tracejadas: os quadrantes superior esquerdo e inferior direito
## concentram os pontos, e o produto dos desvios e negativo na maioria das
## amostras. Niveis declarados.
sim_col <- local({
  semente(20260915)
  n <- 30; R <- 500; r <- 0.95
  x1 <- rnorm(n); x2 <- r * x1 + sqrt(1 - r^2) * rnorm(n)
  b <- t(replicate(R, coef(lm(I(1 + x1 + x2 + rnorm(n)) ~ x1 + x2))[2:3]))
  data.frame(r = r, b1 = b[, 1], b2 = b[, 2])
})
g <- sim_col
mb <- colMeans(g[, c("b1", "b2")])
g$sinal <- factor(ifelse((g$b1 - mb[1]) * (g$b2 - mb[2]) < 0, "produto negativo", "produto positivo"),
                  levels = c("produto negativo", "produto positivo"))
cv <- cov(g$b1, g$b2)
p <- ggplot(g, aes(b1, b2, colour = sinal)) +
  geom_vline(xintercept = mb[1], linetype = "dashed", colour = "grey50") +
  geom_hline(yintercept = mb[2], linetype = "dashed", colour = "grey50") +
  geom_point(size = 0.7, alpha = 0.6) +
  scale_colour_manual(values = c("firebrick", "grey45"), name = NULL) +
  labs(x = "$\\hat\\beta_1$", y = "$\\hat\\beta_2$") +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank(), legend.position = "right",
        legend.text = element_text(size = 7),
        axis.title.y = element_text(angle = 0, vjust = 1))
