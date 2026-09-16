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
## Separador decimal: ponto, neste deck (decisao do professor, 2026-09-15).
## O formatador nm() da camada compartilhada le esta opcao; sem ela sai a
## virgula, que e o padrao dos demais decks.
options(lectures_decimal = ".")
source("code/bloco_setup.R"); m <- ctx_metais()
options(lectures_decimal = ".")
source("code/bloco_setup.R"); e <- ctx_firmas_ex(); d <- dados("firmas_exercicio.csv")
options(lectures_decimal = ".")
source("code/bloco_setup.R")
library(wooldridge); data("wage1")
modelo <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
options(lectures_decimal = ".")
source("code/bloco_setup.R"); q <- dados("anpec2021_q15.csv")
options(lectures_decimal = ".")
eq(sprintf("\\widehat{\\log Y_i} = %s %s \\log L_i %s \\log K_i, \\qquad R^2 = %s, \\qquad F = %s.",
           nm(m$b0, 2), ns(m$b1, 2), ns(m$b2, 2), nm(m$R2, 3), nm(m$F, 1)))
library(ggplot2)
## Figura conceitual, sorteada aqui e nao lida de dados: uma amostra, tres
## paineis. O primeiro mostra o desvio de cada Y_i em torno da media; o
## segundo, a parcela que a curva ajustada explica (da media ao ajustado); o
## terceiro, o residuo (do ajustado ao observado). Niveis declarados
## (CLAUDE.md 5.14).
set.seed(7)
x <- seq(1, 15, length.out = 15)
y_sim <- 2 + 1.2 * x + rnorm(15, 0, 1.6)
## A MESMA amostra nos tres paineis, na mesma escala: e uma decomposicao, e
## nao tres casos (decisao do professor, 2026-09-15).
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
fig_salva("anova_x_explica_y.pdf", p, largura = 5.2, altura = 1.5,
          alt = "A mesma amostra em tres paineis com a media de Y tracejada: no primeiro, segmentos cinza ligam cada ponto a media; no segundo, segmentos vermelhos vao da media a curva ajustada; no terceiro, segmentos azuis vao da curva ajustada ao ponto.")
## Tres observacoes bastam para mostrar a degeneracao: a terceira coluna de
## X e o dobro da segunda, e X'X herda a proporcao entre a segunda e a
## terceira linhas -- determinante zero, sem inversa.
x1 <- c(1, 2, 3); Xs <- cbind(1, x1, 2 * x1); XtXs <- t(Xs) %*% Xs
eq(sprintf("\\mathbf{X} = %s, \\qquad \\mathbf{X}^\\top\\mathbf{X} = %s, \\qquad \\det(\\mathbf{X}^\\top\\mathbf{X}) = %s.",
           mat(Xs), mat(XtXs), ni(det(XtXs))))
library(ggplot2)
## Uma amostra de n = 80 pares (X1, X2) por painel, com correlacao 0,3 e 1.
## As marginais sao densidades por nucleo DA PROPRIA AMOSTRA, desenhadas na
## margem oposta a cada eixo, como num jointplot do seaborn: a de X1 no alto,
## a de X2 a direita. Com r = 1 os pontos caem sobre uma reta, e as marginais
## continuam parecidas com as do outro painel: cada regressor varia igual, e
## e a conjunta que degenera. Os dois segmentos anotados sao a intuicao:
## a X2 fixo, X1 varia (r = 0,3); com r = 1 nao ha como mover X1 sem mover X2.
## Niveis declarados (CLAUDE.md 5.14).
set.seed(19)
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
fig_salva("conjunta_regressores_correlacao.pdf", p, largura = 5.2, altura = 1.55,
          alt = "Dois paineis de dispersao de X2 contra X1 com a densidade de X1 desenhada na margem superior e a de X2 na margem direita: com correlacao 0,3 a nuvem e redonda e um segmento horizontal marca a variacao de X1 a X2 fixo; com correlacao 1 os pontos caem sobre uma reta, as marginais continuam parecidas, e um segmento sobre a reta diz que mover X1 e mover X2.")
dq6 <- head(a$dados, 4)
tb <- data.frame(ni(dq6$quarto), ni(dq6$area), nm(dq6$dist, 1), ni(dq6$idade), ni(dq6$aluguel))
names(tb) <- c("Quarto", "Área (m$^2$)", "Distância (km)", "Idade (anos)", "Aluguel (R\\$)")
tab(tb, caption = "Base de dados sintética ($n = 30$)", tamanho = "scriptsize")
library(ggplot2)
## Niveis declarados, na ordem das colunas de X (CLAUDE.md 5.14).
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
  ## Figura baixa: o titulo do eixo vertical vai na horizontal (CLAUDE.md 5.34).
  theme(strip.text = element_text(face = "bold", size = 7),
        axis.title.y = element_text(angle = 0, vjust = 1, margin = margin(r = 6)),
        panel.grid.minor = element_blank())
fig_salva("dispersao_quartos.pdf", p, largura = 5.2, altura = 1.55,
          alt = "Tres paineis de dispersao do aluguel contra a area, a distancia e a idade: associacao positiva com a area, negativa com a distancia, e nuvem sem inclinacao visivel contra a idade.")
dq <- a$dados
## Duas primeiras observacoes, reticencias e a ultima: a matriz inteira nao
## cabe, e o que o frame estabelece e a forma, nao a conta.
linha <- function(i) c("1", ni(dq$area[i]), nm(dq$dist[i], 1), ni(dq$idade[i]))
X <- rbind(linha(1), linha(2), rep("\\vdots", 4), linha(a$n))
y <- c(ni(dq$aluguel[1]), ni(dq$aluguel[2]), "\\vdots", ni(dq$aluguel[a$n]))
u <- c("u_1", "u_2", "\\vdots", sprintf("u_{%s}", ni(a$n)))
eq(sprintf("\\underbrace{%s}_{\\mathbf{y}\\;(%s \\times 1)} = \\underbrace{%s}_{\\mathbf{X}\\;(%s \\times %s)} \\underbrace{%s}_{\\bm\\beta\\;(%s \\times 1)} + \\underbrace{%s}_{\\mathbf{u}\\;(%s \\times 1)}",
           mat(y), ni(a$n), mat(X), ni(a$n), ni(a$k + 1),
           mat(c("\\beta_0", "\\beta_1", "\\beta_2", "\\beta_3")), ni(a$k + 1),
           mat(u), ni(a$n)))
## Cinco casas: a entrada da idade e da ordem de 10^-4, e com quatro casas a
## matriz exibiria zeros onde ha numero.
## A diagonal principal sai em \boxed{}: e a unica parte da inversa que a
## aula usa, e o destaque e visual (CLAUDE.md da CE423, "Enfase").
diag_box <- function(M, d) {
  C <- matrix(nm(M, d), nrow(M)); diag(C) <- sprintf("\\boxed{%s}", diag(C)); C
}
eq(sprintf("(\\mathbf{X}^\\top\\mathbf{X})^{-1} = %s", mat(diag_box(a$XtXinv, 5))))
## A seta marca a coordenada de cada parametro, na ordem das colunas de X.
eq(sprintf("\\hat{\\bm\\beta} = (\\mathbf{X}^\\top\\mathbf{X})^{-1}\\mathbf{X}^\\top\\mathbf{y} = %s \\; \\begin{matrix} \\leftarrow \\hat\\beta_0 \\\\ \\leftarrow \\hat\\beta_1 \\\\ \\leftarrow \\hat\\beta_2 \\\\ \\leftarrow \\hat\\beta_3 \\end{matrix}",
           cx(mat(a$b, fmt = function(z) nm(z, 2)))))
library(ggplot2)
## Esquerda: os residuos do exemplo contra o ajustado, com dispersao igual em
## toda a faixa -- e o que (P3) afirma, e o que autoriza um unico S_e^2.
## Direita: contrafactual simulado com o MESMO ajustado, em que a dispersao
## cresce com o valor ajustado; um unico numero nao descreve o erro. O nome
## do fenomeno fica para aulas futuras. Niveis declarados (CLAUDE.md 5.14).
set.seed(3)
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
fig_salva("sigma2_residuos_quartos.pdf", p, largura = 5.2, altura = 1.3,
          alt = "Dois paineis de residuos contra o valor ajustado, cada residuo ligado ao zero por um segmento: a esquerda a dispersao e a mesma em toda a faixa; a direita ela cresce da esquerda para a direita, em funil.")
eq(sprintf("S_e^2 = \\frac{\\sum \\hat u_i^2}{n - k - 1} = \\frac{%s}{%s} = %s, \\qquad S_e = %s \\text{ reais.}",
           nm(a$RSS, 0), ni(a$gl), nm(a$Se2, 1), nm(a$Se, 1)))
C <- matrix(nm(a$XtXinv, 5), 4); C[3, 3] <- sprintf("\\boxed{%s}", C[3, 3])
## Duas equacoes: matriz e leitura na mesma linha transbordam 85 pt.
eq(sprintf("(\\mathbf{X}^\\top\\mathbf{X})^{-1} = %s", mat(C)))
eq(sprintf("\\Var(\\hat\\beta_2) = \\sigma^2 \\times %s.", nm(a$XtXinv[3, 3], 5)))
eq(sprintf("S_e^2 = %s = %s = %s, \\qquad S_e = %s.",
           frac("\\text{RSS}", "n-k-1"), frac(nm(a$RSS, 0), ni(a$gl)), nm(a$Se2, 1), nm(a$Se, 1)))
## Diagonal em \boxed{}, como na inversa: e dela que saem os erros padrao.
diag_box <- function(M, d) {
  C <- matrix(nm(M, d), nrow(M)); diag(C) <- sprintf("\\boxed{%s}", diag(C)); C
}
eq(sprintf("S_e^2(\\mathbf{X}^\\top\\mathbf{X})^{-1} = %s", mat(diag_box(a$V, 2))))
eq(sprintf("S_{\\hat\\beta_1} = \\sqrt{%s} = %s, \\qquad S_{\\hat\\beta_2} = \\sqrt{%s} = %s, \\qquad S_{\\hat\\beta_3} = \\sqrt{%s} = %s.",
           nm(a$V[2, 2], 2), nm(a$Sb[2], 2), nm(a$V[3, 3], 2), nm(a$Sb[3], 2),
           nm(a$V[4, 4], 2), nm(a$Sb[4], 2)))
## O sinal fica FORA do \underset (CLAUDE.md 5.33): dentro dele o TeX suprime o
## espaco de operador binario.
us <- function(j, d) sprintf("\\underset{(%s)}{%s}", nm(a$Sb[j], d), nm(abs(a$b[j]), d))
sg <- function(j) ifelse(a$b[j] < 0, "-", "+")
eq(sprintf("\\widehat{\\text{aluguel}}_i = %s %s %s\\,\\text{área}_i %s %s\\,\\text{dist}_i %s %s\\,\\text{idade}_i",
           us(1, 2), sg(2), us(2, 2), sg(3), us(3, 2), sg(4), us(4, 2)))
j <- 2:4
## Colunas numericas alinhadas a direita pelo proprio tab(): com o mesmo
## numero de casas, o separador decimal fica na mesma coluna e o sinal
## negativo nao desloca nada (Lectures/CLAUDE.md 5.9).
pv <- function(p) ifelse(p < 0.001, "$< 0.001$", paste0("$", nm(p, 3), "$"))
tab(data.frame(Coeficiente = c("$\\hat\\beta_1$ (área)", "$\\hat\\beta_2$ (dist)", "$\\hat\\beta_3$ (idade)"),
               Estimativa = paste0("$", nm(a$b[j], 2), "$"),
               `Erro padrão` = paste0("$", nm(a$Sb[j], 2), "$"),
               `$t$` = paste0("$", nm(a$t[j], 2), "$"),
               `$p$-valor` = pv(a$p[j]),
               `$|t| > t_c$?` = ifelse(abs(a$t[j]) > a$tc, "sim", "não"),
               check.names = FALSE),
    caption = "Estatísticas $t$ e $p$-valores dos coeficientes de inclinação", tamanho = "small")
cat(sprintf("\nO valor crítico é $t_{%s}(%s) = %s$.\n", nm(a$alpha / 2, 3), ni(a$gl), nm(a$tc, 2)))
g <- a$gl; lim <- ceiling(max(abs(a$t[2:4]), a$tc) + 0.8)
## constante da densidade t avaliada no R: pgfplots so precisa da forma fechada
cte <- gamma((g + 1) / 2) / (sqrt(g * pi) * gamma(g / 2))
dens <- sprintf("%s*(1+x^2/%s)^(-%s)", signif(cte, 6), g, (g + 1) / 2)
alt <- "Densidade t com as duas caudas de rejeicao sombreadas: as estatisticas da area e da distancia caem longe dentro das caudas, e a da idade cai no centro, fora delas."
cat(sprintf("\\altfig{%s}{%%\n", alt))
cat("\\centering\n\\begin{tikzpicture}\n")
cat(sprintf("\\begin{axis}[width=0.86\\textwidth, height=4.6cm, domain=%s:%s, samples=200,\n", -lim, lim))
cat(sprintf("  axis lines=middle, ymin=0, ymax=0.42, xlabel={$t$}, ylabel={},\n  xtick={%s,0,%s}, xticklabels={$%s$,,$%s$},\n",
            round(-a$tc, 2), round(a$tc, 2), nm(-a$tc, 2), nm(a$tc, 2)))
cat("  ytick=\\empty, clip=false, x tick label style={font=\\tiny}]\n")
cat(sprintf("  \\addplot[thick, black] {%s};\n", dens))
for (lado in list(c(round(a$tc, 2), lim), c(-lim, round(-a$tc, 2))))
  cat(sprintf("  \\addplot[draw=none, fill=catcolor, fill opacity=0.28, domain=%s:%s] {%s} \\closedcycle;\n",
              lado[1], lado[2], dens))
rot <- c("\\text{área}", "\\text{dist}", "\\text{idade}"); alt_y <- c(0.30, 0.30, 0.22)
for (j in 1:3)
  cat(sprintf("  \\draw[thick, %s] (axis cs:%s,0) -- (axis cs:%s,%s) node[above, font=\\scriptsize] {$t_{%s} = %s$};\n",
              if (abs(a$t[j + 1]) > a$tc) "catcolor" else "black",
              round(a$t[j + 1], 3), round(a$t[j + 1], 3), alt_y[j], rot[j], nm(a$t[j + 1], 2)))
cat("\\end{axis}\n\\end{tikzpicture}\n}\n")
eq(sprintf("|t(\\hat\\beta_3)| = %s < %s = t_{%s}(%s) \\qquad\\Longrightarrow\\qquad \\text{não se rejeita } H_0: \\beta_3 = 0.",
           nm(abs(a$t[4]), 2), nm(a$tc, 2), nm(a$alpha / 2, 3), ni(a$gl)))
cat(sprintf("\nA estimativa é $%s$ reais por mês a cada ano de idade do imóvel, tem o sinal esperado, e ainda assim não é distinguível de zero a $%s\\%%$.\n",
            nm(a$b[4], 2), ni(100 * a$alpha)))
eq(sprintf("\\hat\\beta_3 \\pm t_{%s}(%s)\\, S_{\\hat\\beta_3} = %s \\pm %s \\times %s = %s.",
           nm(a$alpha / 2, 3), ni(a$gl), nm(a$b[4], 2), nm(a$tc, 2), nm(a$Sb[4], 2), iv(a$ic[4, ])))
library(ggplot2)
## Os tres intervalos num mesmo eixo, cada um com a sua cor: e o eixo comum
## que mostra a diferenca de escala entre os regressores. Niveis declarados,
## na ordem das colunas de X (CLAUDE.md 5.14).
rot <- c("$\\beta_1$ (área)", "$\\beta_2$ (dist)", "$\\beta_3$ (idade)")
j <- 2:4
g <- data.frame(termo = factor(rot, levels = rev(rot)), est = a$b[j],
                lo = a$ic[j, 1], hi = a$ic[j, 2])
p <- ggplot(g, aes(est, termo, colour = termo)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.15, linewidth = 0.8) +
  geom_point(size = 2.6) +
  scale_colour_manual(values = c("grey35", "steelblue4", "firebrick"), guide = "none") +
  ## O porcento vai escapado: com o motor tikz o rotulo e escrito no .tex.
  labs(x = "Estimativa e intervalo de confiança a 95\\% (R\\$ por mês, por unidade do regressor)", y = NULL) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank())
fig_salva("ic_coeficientes_quartos.pdf", p, largura = 5.2, altura = 1.45,
          alt = "Os tres intervalos de confianca num mesmo eixo, um por cor, com uma linha tracejada no zero: o da area fica a direita do zero, o da distancia bem a esquerda, e o da idade e curto e atravessa o zero.")
cat(sprintf("A hipótese nula não precisa ser zero. Suponha que um anúncio em um grupo de alunos afirme que cada quilômetro a mais de distância à Unicamp reduz o aluguel em \\textrm{R\\$}\\,%s por mês:\n\n",
            ni(abs(a$c_econ))))
c0 <- a$c_econ
tc0 <- (a$b[3] - c0) / a$Sb[3]
eq(sprintf("H_0: \\beta_2 = %s \\qquad t = %s = %s, \\qquad |t| %s %s.",
           ni(c0), frac(sprintf("%s - (%s)", nm(a$b[3], 2), ni(c0)), nm(a$Sb[3], 2)),
           nm(tc0, 2), if (abs(tc0) > a$tc) ">" else "<", nm(a$tc, 2)))
cat(sprintf("\n%s $H_0$ a $%s\\%%$. A hipótese $\\beta_2 = 0$ foi rejeitada; a hipótese $\\beta_2 = %s$ %s. A mesma estimativa responde de forma diferente a perguntas diferentes.\n",
            if (abs(tc0) > a$tc) "Rejeita-se" else "Não se rejeita", ni(100 * a$alpha), ni(c0),
            if (abs(tc0) > a$tc) "também é" else "não é"))
tcu <- qt(1 - a$alpha, a$gl)
cat(sprintf("A estatística é a mesma. Muda o valor crítico: toda a área $\\alpha$ vai para uma cauda só, logo $-t_{%s}(%s) = %s$ em vez de $%s$.\n\n",
            nm(a$alpha, 2), ni(a$gl), nm(-tcu, 2), nm(-a$tc, 2)))
cat(sprintf("Com $t = %s %s %s$, aqui %s.\n", nm(a$t[4], 2),
            if (a$t[4] < -tcu) "<" else ">", nm(-tcu, 2),
            if (a$t[4] < -tcu) "se rejeitaria" else "também não se rejeita"))
## A mesma densidade t do frame da regiao de rejeicao, agora com as duas
## regioes sobrepostas: as duas caudas do bilateral (claras, alpha/2 cada) e a
## cauda esquerda do unilateral (escura, alpha inteiro), com t da idade
## marcado. Estatica, em pgfplots, como a anterior.
g <- a$gl; lim <- 4
tcu <- qt(1 - a$alpha, g)
cte <- gamma((g + 1) / 2) / (sqrt(g * pi) * gamma(g / 2))
dens <- sprintf("%s*(1+x^2/%s)^(-%s)", signif(cte, 6), g, (g + 1) / 2)
alt <- "Densidade t com as duas caudas do teste bilateral em vermelho e, entre o critico unilateral e o bilateral da esquerda, uma faixa azul que so o teste unilateral rejeita; a estatistica da idade cai fora das duas regioes."
cat(sprintf("\\altfig{%s}{%%\n", alt))
cat("\\centering\n\\begin{tikzpicture}\n")
cat(sprintf("\\begin{axis}[width=0.86\\textwidth, height=4.4cm, domain=%s:%s, samples=200,\n", -lim, lim))
cat("  axis lines=middle, ymin=0, ymax=0.42, xlabel={$t$}, ylabel={},\n")
## Os dois criticos da esquerda distam 0,35: os rotulos vao empilhados em
## alturas diferentes, e nao nos ticks, onde se sobrepunham.
cat(sprintf("  xtick={%s,0,%s}, xticklabels={$%s$,,$%s$}, ytick=\\empty, clip=false, x tick label style={font=\\tiny}]\n",
            round(-a$tc, 2), round(a$tc, 2), nm(-a$tc, 2), nm(a$tc, 2)))
cat(sprintf("  \\addplot[thick, black] {%s};\n", dens))
## unilateral a esquerda primeiro, em azul; as caudas bilaterais depois, em
## vermelho OPACO por cima: onde as duas se sobrepoem fica so o vermelho, e o
## azul aparece na faixa entre os dois criticos (decisao do professor).
cat(sprintf("  \\addplot[draw=none, fill=blue!35!white, domain=%s:%s] {%s} \\closedcycle;\n",
            -lim, round(-tcu, 2), dens))
for (lado in list(c(round(a$tc, 2), lim), c(-lim, round(-a$tc, 2))))
  cat(sprintf("  \\addplot[draw=none, fill=catcolor!45!white, domain=%s:%s] {%s} \\closedcycle;\n",
              lado[1], lado[2], dens))
cat(sprintf("  \\draw[blue!45!black, dashed] (axis cs:%s,0) -- (axis cs:%s,0.16) node[above, font=\\tiny] {$-t_{%s} = %s$};\n",
            round(-tcu, 3), round(-tcu, 3), nm(a$alpha, 2), nm(-tcu, 2)))
cat(sprintf("  \\draw[thick, black] (axis cs:%s,0) -- (axis cs:%s,0.30) node[above, font=\\scriptsize] {$t_{\\text{idade}} = %s$};\n",
            round(a$t[4], 3), round(a$t[4], 3), nm(a$t[4], 2)))
cat(sprintf("  \\node[font=\\tiny, text=blue!45!black, anchor=south west] at (axis cs:%s,0.06) {unilateral: $\\alpha = %s$ numa cauda};\n",
            -lim, nm(a$alpha, 2)))
cat(sprintf("  \\node[font=\\tiny, text=catcolor!60!black, anchor=south east] at (axis cs:%s,0.06) {bilateral: $\\alpha/2 = %s$ em cada cauda};\n",
            lim, nm(a$alpha / 2, 3)))
cat("\\end{axis}\n\\end{tikzpicture}\n}\n")
eq(sprintf("F = %s = %s, \\qquad p %s.", frac("\\text{ESS}/k", "\\text{RSS}/(n-k-1)"),
           nm(a$F, 1), if (a$pF < 0.001) "< 0.001" else sprintf("= %s", nm(a$pF, 3))))
j <- 2:4
dec <- function(p) ifelse(p < a$alpha, "rejeita", "não rejeita")
pv <- function(p) ifelse(p < 0.001, "$< 0.001$", paste0("$", nm(p, 3), "$"))
tab(data.frame(
  Teste = c("$H_0: \\beta_1 = 0$", "$H_0: \\beta_2 = 0$", "$H_0: \\beta_3 = 0$",
            "$H_0: \\beta_1 = \\beta_2 = \\beta_3 = 0$"),
  Estatística = c(rep("$t$", 3), "$F$"),
  Valor = paste0("$", c(nm(a$t[j], 2), nm(a$F, 1)), "$"),
  `$p$-valor` = pv(c(a$p[j], a$pF)),
  `Decisão` = dec(c(a$p[j], a$pF)),
  check.names = FALSE),
    caption = "Os testes individuais e o teste conjunto", tamanho = "small")
bt <- a$dgp$b
eq(sprintf("\\text{aluguel}_i = %s %s\\,\\text{área}_i %s\\,\\text{dist}_i %s\\,\\text{idade}_i + u_i, \\qquad u_i \\sim \\mathcal{N}(0;\\ %s^2)",
           ni(bt[1]), ns(bt[2], 0), ns(bt[3], 0), ns(bt[4], 1), ni(a$dgp$sigma)))
tab(data.frame(
  Parâmetro = c("$\\beta_0$", "$\\beta_1$ (área)", "$\\beta_2$ (dist)", "$\\beta_3$ (idade)", "$\\sigma$"),
  Verdadeiro = paste0("$", nm(c(bt, a$dgp$sigma), 2), "$"),
  Estimado = paste0("$", nm(c(a$b, a$Se), 2), "$"),
  Diferença = paste0("$", ns(c(a$b, a$Se) - c(bt, a$dgp$sigma), 2), "$"),
  `IC 95\\%` = c(apply(a$ic, 1, iv), ""),
  `$t$` = c("", paste0("$", nm(a$t[2:4], 2), "$"), ""),
  check.names = FALSE),
    caption = "Parâmetros do processo gerador e suas estimativas", tamanho = "tiny")
## A conclusao sai dos numeros, e nao da prosa: o coeficiente da idade e
## diferente de zero no processo gerador, e o teste nao o distinguiu de zero.
cat(sprintf("\n$\\beta_3 = %s$ no processo gerador, e o teste \\alert{não o distinguiu de zero} ($t = %s$, $p = %s$).\n",
            nm(bt[4], 1), nm(a$t[4], 2), nm(a$p[4], 3)))
library(ggplot2)
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
fig_salva("fator_inflacao_variancia.pdf", p, largura = 5.0, altura = 1.0,
          alt = "Curva crescente do fator um sobre um menos r quadrado, quase plana ate a metade e que dispara perto de um, com os multiplicadores dois, cinco, dez e vinte marcados.")
library(ggplot2)
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
  ## Legenda DENTRO do painel, a esquerda, onde nao ha curva (CLAUDE.md 5.21).
  theme(panel.grid.minor = element_blank(), axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(), legend.position = c(0.12, 0.8),
        legend.text = element_text(size = 8), legend.key.height = unit(0.35, "cm"),
        legend.background = element_blank())
fig_salva("imprecisao_vies.pdf", p, largura = 5.0, altura = 1.5,
          alt = "Duas densidades do estimador, com legenda no canto superior esquerdo: a cinza, larga e centrada no parametro, do modelo com o segundo regressor; a vermelha, estreita e deslocada para a direita, do modelo sem ele.")
j <- 2:4
pv <- function(p) ifelse(p < 0.001, "$< 0.001$", paste0("$", nm(p, 3), "$"))
tb <- data.frame(
  Regressor = c("área", "dist", "idade"),
  Unidade = c("m$^2$", "km", "anos"),
  `$\\hat\\beta_j$ (R\\$/mês por unidade)` = paste0("$", nm(a$b[j], 2), "$"),
  `$p$-valor` = pv(a$p[j]),
  `Significativo?` = ifelse(abs(a$t[j]) > a$tc, "sim", "não"),
  check.names = FALSE)
tab(tb, caption = "Cada coeficiente, na sua unidade, e o seu teste", tamanho = "small")
## A mesma estimativa da idade em amostras cada vez maiores, com S_e e a
## dispersao do regressor fixos nos valores desta amostra: o erro padrao cai
## com a raiz de n - 1. A decisao de cada linha sai dos numeros.
j <- 4; nn <- c(a$n, 100, 500, 5000)
Sn <- a$Sb[j] * sqrt((a$n - 1) / (nn - 1)); tn <- a$b[j] / Sn
tcn <- qt(1 - a$alpha / 2, nn - a$k - 1)
tab(data.frame(`$n$` = ni(nn),
               `$\\hat\\beta_3$` = paste0("$", nm(rep(a$b[j], 4), 2), "$"),
               `$S_{\\hat\\beta_3}$` = paste0("$", nm(Sn, 2), "$"),
               `$t$` = paste0("$", nm(tn, 2), "$"),
               `IC 95\\%` = sapply(seq_along(nn), function(i) iv(a$b[j] + c(-1, 1) * tcn[i] * Sn[i])),
               `Decisão` = ifelse(abs(tn) > tcn, "rejeita", "não rejeita"),
               check.names = FALSE),
    caption = "O mesmo coeficiente da idade em amostras maiores", tamanho = "scriptsize")
Sb0 <- sqrt(m$Se2 * m$XtXinv[1, 1])
us <- function(b, s) sprintf("\\underset{(%s)}{%s}", nm(s, 3), nm(abs(b), 2))
eq(sprintf("\\widehat{\\log Y_i} = %s %s %s\\,\\log L_i %s %s\\,\\log K_i",
           us(m$b0, Sb0), ifelse(m$b1 < 0, "-", "+"), us(m$b1, m$Sb1),
           ifelse(m$b2 < 0, "-", "+"), us(m$b2, m$Sb2)))
library(ggplot2)
rot <- c("$\\beta_1$ ($\\log L$)", "$\\beta_2$ ($\\log K$)")
g <- data.frame(termo = factor(rot, levels = rev(rot)),
                est = c(m$b1, m$b2), lo = c(m$ic1[1], m$ic2[1]), hi = c(m$ic1[2], m$ic2[2]))
p <- ggplot(g, aes(est, termo)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.12, linewidth = 0.7) +
  geom_point(size = 2.8) +
  labs(x = "Estimativa e intervalo de confiança a 95\\%", y = NULL) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank())
fig_salva("ic_coeficientes_metais.pdf", p, largura = 5.0, altura = 1.05,
          alt = "Intervalos de confianca das elasticidades do trabalho e do capital, ambos inteiramente a direita da linha tracejada no zero.")
co <- summary(modelo)$coefficients
us <- function(j) sprintf("\\underset{(%s)}{%s}", nm(co[j, 2], 3), nm(abs(co[j, 1]), 3))
sg <- function(j) ifelse(co[j, 1] < 0, "-", "+")
eq(sprintf("\\widehat{\\log(\\text{salário})}_i = %s %s %s\\,\\text{educ}_i %s %s\\,\\text{exper}_i %s %s\\,\\text{tenure}_i",
           us(1), sg(2), us(2), sg(3), us(3), sg(4), us(4)))
co <- summary(modelo)$coefficients
## Tabela no formato corrente em artigo: uma coluna por modelo, a estimativa
## com as estrelas de significancia e o erro padrao entre parenteses, e as
## estatisticas do modelo no rodape. As estrelas saem do p-valor de cada linha.
## Estrelas (\star), e nao asteriscos, por decisao do professor (2026-09-15).
estrela <- function(p) ifelse(p < 0.001, "^{\\star\\star\\star}", ifelse(p < 0.01, "^{\\star\\star}", ifelse(p < 0.05, "^{\\star}", "")))
rot <- c("Escolaridade (anos)", "Experiência (anos)", "Tempo no emprego atual (anos)", "Constante")
ord <- c(2, 3, 4, 1)
linhas <- sprintf("$%s%s$ ($%s$)", nm(co[ord, 1], 3), estrela(co[ord, 4]), nm(co[ord, 2], 3))
tb <- data.frame(c(rot, "$n$", "$R^2$"),
                 c(linhas, ni(nobs(modelo)), nm(summary(modelo)$r.squared, 3)))
names(tb) <- c("", "$\\log(\\text{salário-hora})$")
tab(tb, caption = "Equação de Mincer, $n = 526$", tamanho = "scriptsize")
library(ggplot2)
ic <- confint(modelo)[-1, , drop = FALSE]
## Nome de variavel do R sai em maquina de escrever, como nos slides.
rot <- sprintf("\\texttt{%s}", rownames(ic))
g <- data.frame(termo = factor(rot, levels = rev(rot)),
                est = coef(modelo)[-1], lo = ic[, 1], hi = ic[, 2])
p <- ggplot(g, aes(est, termo)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.12, linewidth = 0.7) +
  geom_point(size = 2.8) +
  labs(x = "Estimativa e intervalo de confiança a 95\\%", y = NULL,
       caption = "Mesma leitura da tabela, sem ler número por número.") +
  theme_minimal(base_size = 9) +
  theme(plot.caption = element_text(size = 6))
fig_salva("ic_coeficientes_wage1.pdf", p, largura = 5.2, altura = 1.60,
          alt = "Intervalos de confianca dos coeficientes de educ, exper e tenure, com uma linha tracejada no zero.")
tab_serie("$L_i$" = d$L, "$K_i$" = d$K, "$Y_i$" = d$Y,
    caption = "As cinco firmas do exercício da aula passada", tamanho = "scriptsize")
## A inversa vai em linha com a frase, e nao em display: o frame ja leva a
## tabela e os quatro itens, e um display a mais transborda.
cat(sprintf("\nJá se sabe: $\\hat Y = %s %s L %s K$, $\\text{RSS} = %s$ e $(\\mathbf{X}^\\top\\mathbf{X})^{-1} = \\bigl(%s\\bigr)$.\n",
            ni(e$b0), ns(e$b1, 0), ns(e$b2, 0), ni(e$RSS),
            mat(e$XtXinv, fmt = function(z) nm(z, 3), env = "smallmatrix")))
cat(sprintf("Estimou-se por OLS, com $n = %s$ imóveis, um modelo em logaritmos do preço contra características do imóvel. Reporta-se $R^2 = %s$, $\\text{TSS} = %s$ e $S_e = %s$, com erros padrão entre parênteses sob cada coeficiente. Julgue:\n\n",
            ni(q$n), nm(q$r2, 2), nm(q$tss, 2), nm(q$se, 2)))
itens <- c(
  sprintf("Um coeficiente cuja razão entre estimativa e erro padrão vale $%s$ é significativo a $%s\\%%$ em um teste bilateral.",
          nm(q$razao_avaliada, 1), ni(100 * q$alpha_item)),
  "A soma de quadrados dos resíduos pode ser obtida como $(1-R^2)\\,\\text{TSS}$.",
  "Ao acrescentar um regressor irrelevante, o $R^2$ não cai e o $\\bar R^2$ pode cair.",
  "A estatística $F$ de significância da regressão testa a hipótese de que todos os coeficientes de inclinação são nulos simultaneamente.",
  "Em um modelo com $\\log(\\text{preço})$ como dependente, o coeficiente de uma variável binária multiplicado por $100$ aproxima a variação percentual do preço associada à característica.")
cat("\\begin{enumerate}\n")
cat(sprintf("  \\item[(%d)] %s\n", seq_along(itens) - 1, itens), sep = "")
cat("\\end{enumerate}\n")
j <- 2:4; rot <- c("área", "distância", "idade")
cat(sprintf("No exemplo, com $\\alpha = %s$: %s.\n", nm(a$alpha, 2),
            paste(sprintf("%s, $p %s$, %s", rot,
                          ifelse(a$p[j] < 0.001, "< 0.001", paste0("= ", nm(a$p[j], 3))),
                          ifelse(a$p[j] < a$alpha, "rejeita-se $H_0$", "não se rejeita $H_0$")),
                  collapse = "; ")))
cat(sprintf("\\alert{Item 1.} $S_e^2 = %s/%s = %s$. A entrada de $\\hat\\beta_1$ é a segunda da diagonal, depois do intercepto, e a de $\\hat\\beta_2$ a terceira:\n",
            ni(e$RSS), ni(e$gl), ni(e$Se2)))
eq(sprintf("S_{\\hat\\beta_1} = \\sqrt{S_e^2 \\times %s} = %s, \\qquad S_{\\hat\\beta_2} = \\sqrt{S_e^2 \\times %s} = %s.",
           nm(e$XtXinv[2, 2], 3), nm(e$Sb1, 3), nm(e$XtXinv[3, 3], 3), nm(e$Sb2, 3)))
cat(sprintf("\n\\alert{Item 2.} $t(\\hat\\beta_1) = %s$ e $t(\\hat\\beta_2) = %s$, contra $t_{%s}(%s) = %s$. Não se rejeita para $L$; rejeita-se para $K$.\n",
            nm(e$t1, 2), nm(e$t2, 2), nm(e$alpha / 2, 3), ni(e$gl), nm(e$tc, 2)))
cat(sprintf("\n\\alert{Item 3.} $%s \\pm %s \\times %s = %s$, que contém zero, coerente com não rejeitar.\n",
            ni(e$b1), nm(e$tc, 2), nm(e$Sb1, 3), iv(e$ic1)))
cat(sprintf("\\alert{Item 4.} O modelo é conjuntamente significativo ($F = %s$, $p = %s$) com um coeficiente individualmente insignificante. Aqui $r_{LK} = %s$, e o fator $1/(1-%s) = %s$ é modesto: a causa principal é o tamanho da amostra, com apenas %s graus de liberdade.\n",
            ni(e$F), nm(e$pF, 3), nm(e$r12, 1), nm(e$r12^2, 2),
            nm(1 / (1 - e$r12^2), 2), ni(e$gl)))
tc <- qt(1 - q$alpha_item / 2, q$n - q$k - 1)
## A terceira coluna e paragrafo (p{}), senao a tabela sai 110 pt mais larga
## que o slide: verificado no .log de 2026-09-15.
tab(data.frame(
  Item = sprintf("(%d)", 0:4),
  Resposta = c("F", "V", "V", "V", "V"),
  `Por quê` = c(
    sprintf("Com $n=%s$ e $k=%s$, o crítico bilateral a $%s\\%%$ é $%s$; $%s$ não alcança.",
            ni(q$n), ni(q$k), ni(100 * q$alpha_item), nm(tc, 2), nm(q$razao_avaliada, 1)),
    sprintf("$R^2 = 1 - \\text{RSS}/\\text{TSS}$, logo $\\text{RSS} = (1-R^2)\\text{TSS} = %s$.",
            nm((1 - q$r2) * q$tss, 3)),
    "$R^2$ nunca cai; $\\bar R^2$ cai se o ganho não paga o grau de liberdade.",
    "É a definição do $F$ global.",
    "É a semi-elasticidade da aula sobre anamorfose; a aproximação piora para coeficientes grandes."),
  check.names = FALSE),
    caption = "Gabarito do Exercício 2", align = c("l", "l", "p{9cm}"), tamanho = "scriptsize")
library(ggplot2)
## 500 amostras de n = 30, y = 1 + x1 + x2 + u, com correlacao 0,95 entre x1
## e x2; os regressores ficam fixos entre as amostras e so u e sorteado de
## novo. Cada ponto e o par (b1, b2) estimado numa amostra, com as duas
## medias tracejadas: os quadrantes superior esquerdo e inferior direito
## concentram os pontos, e o produto dos desvios e negativo na maioria das
## amostras. Niveis declarados.
sim_col <- local({
  set.seed(20260915)
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
fig_salva("covariancia_estimadores.pdf", p, largura = 5.0, altura = 1.3,
          alt = "Nuvem dos pares de coeficientes estimados em quinhentas amostras com regressores muito correlacionados, cortada pelas duas medias em quatro quadrantes: os pontos se concentram nos quadrantes em que um coeficiente esta acima da sua media e o outro abaixo.")
cat(sprintf("\nNas quinhentas amostras com $r = 0.95$, $\\Cov(\\hat\\beta_1, \\hat\\beta_2) = %s$: quando um sorteio dá $\\hat\\beta_1$ acima da média, $\\hat\\beta_2$ tende a sair abaixo, e a soma dos dois varia menos que cada um.\n",
            nm(cv, 2)))
