# ---------------------------------------------------------------------------
# CE423 - Econometria I
# 2. Regressao Linear Multipla - Estimacao e ANOVA
#
# Este arquivo e gerado a partir dos slides da aula: cada trecho aqui e um
# trecho que apareceu na tela. Roda de cima a baixo, sem alteracao.
#
# RODE A PARTIR DA PASTA "aulas", que e a que contem "code/" e "data/".
# No RStudio, o caminho curto e abrir "aulas" como projeto. Rodando de dentro
# de "code/", o script se corrige sozinho e avisa.
# ---------------------------------------------------------------------------

# O erro comum e chamar o script de dentro de "code/", onde ele esta, e nao de
# "aulas/", a que os caminhos se referem. Corrige sozinho quando reconhece essa
# estrutura, e AVISA -- silencioso seria pior, porque esconderia de onde os
# arquivos passaram a ser lidos. Fora dessas duas situacoes, para aqui com uma
# mensagem que diz de onde rodar, em vez de falhar adiante numa leitura de CSV
# com mensagem que nao explica a causa.
if (!file.exists("code/bloco_setup.R")) {
  if (file.exists("../code/bloco_setup.R")) {
    setwd("..")
    message("Diretorio de trabalho ajustado para ", getwd(), ".")
  } else {
    stop("Rode este script a partir da pasta 'aulas' da disciplina, ",
         "que e a que contem 'code/' e 'data/'. Diretorio atual: ", getwd())
  }
}
source("code/bloco_setup.R"); m <- ctx_firmas()
## Recorte de uma serie longa, para que a tabela caiba no slide: as primeiras
## `k` observacoes, reticencia, e as ultimas `f`. As somas exibidas em qualquer
## frame sao SEMPRE da amostra inteira -- a tabela e que e truncada.
recorte <- function(v, k = 6, f = 2)
  c(as.character(v)[1:k], "$\\cdots$",
    as.character(v)[(length(v) - f + 1):length(v)])
## A curva ajustada, escrita uma vez. Tres frames a citam.
ajuste <- sprintf("\\hat Y_i = %s %s L_i %s K_i",
                  nm(m$b0, 2), ns(m$b1, 2), ns(m$b2, 2))
## O processo gerador, para o frame que o revela.
dgp <- sprintf("Y_i = %s + %s\\,L_i + %s\\,K_i + u_i, \\qquad u_i \\sim \\mathcal{N}(0, %s^2)",
               ni(m$dgp$b0), ni(m$dgp$b1), ni(m$dgp$b2), ni(m$dgp$sigma))
source("code/bloco_setup.R"); e <- ctx_firmas_ex()
source("code/bloco_setup.R")
dq <- dados("obs_matricial.csv")
q <- matricial(rlm2(dq$X1, dq$X2, dq$Y, r1 = "X_1", r2 = "X_2"))
q$dados <- dq
source("code/bloco_setup.R")
library(wooldridge); data("wage1")
simples  <- lm(log(wage) ~ educ, data = wage1)
multipla <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
library(ggplot2)
## Niveis declarados, como manda o CLAUDE.md 5.14, mesmo com a ordem alfabetica
## coincidindo com a desejada: e uma renomeacao futura que inverte a figura em
## silencio.
d <- m$dados
paineis <- c("$Y_i$ contra $L_i$", "$Y_i$ contra $K_i$")
pl <- rbind(data.frame(x = d$L, y = d$Y, painel = paineis[1]),
            data.frame(x = d$K, y = d$Y, painel = paineis[2]))
pl$painel <- factor(pl$painel, levels = paineis)
p <- ggplot(pl, aes(x, y)) +
  geom_point(size = 1.5) +
  facet_wrap(~ painel, scales = "free_x") +
  labs(x = NULL, y = "$Y_i$") +
  expand_limits(y = 0) +
  theme_minimal(base_size = 9) +
  ## Figura baixa: o titulo do eixo vertical vai na horizontal, senao encosta
  ## nos numeros do eixo (CLAUDE.md 5.34).
  theme(strip.text = element_text(face = "bold", size = 7),
        axis.title.y = element_text(angle = 0, vjust = 1, margin = margin(r = 4)),
        panel.grid.minor = element_blank())
fig_salva("dispersao_firmas.pdf", p, largura = 5.0, altura = 1.35,
          alt = "Dois paineis de dispersao da producao contra o trabalho e contra o capital, ambos com associacao positiva.")
eq(sprintf("S_{LL} = %s, \\quad S_{KK} = %s, \\quad S_{LK} = %s, \\quad S_{LY} = %s, \\quad S_{KY} = %s.",
           nm(m$S11, 1), nm(m$S22, 1), nm(m$S12, 1), nm(m$S1y, 1), nm(m$S2y, 1)))
library(ggplot2)
## Os quadrantes sombreados sao os de produto positivo. Niveis declarados,
## como manda o CLAUDE.md 5.14, mesmo com a ordem alfabetica coincidindo.
rot <- c("$\\ell_i \\kappa_i > 0$", "$\\ell_i \\kappa_i < 0$")
d <- data.frame(l = m$x1 - m$mx1, kp = m$x2 - m$mx2)
d$sinal <- factor(ifelse(d$l * d$kp >= 0, rot[1], rot[2]), levels = rot)
p <- ggplot(d, aes(l, kp)) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf, fill = "grey90") +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0, fill = "grey90") +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = "grey45", linewidth = 0.4) +
  geom_point(aes(shape = sinal), size = 1.7) +
  scale_shape_manual(values = c(19, 1)) +
  labs(x = "$\\ell_i = L_i - \\bar L$", y = "$\\kappa_i = K_i - \\bar K$", shape = NULL) +
  theme_minimal(base_size = 9) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
fig_salva("produto_cruzado_firmas.pdf", p, largura = 3.0, altura = 1.8,
          alt = "Dispersao dos desvios de trabalho e de capital, com os quadrantes de produto positivo sombreados e as firmas concentradas neles.")
## `:results output latex`, e nao raw: o resultado em cache de um bloco raw que
## e um paragrafo solto nao tem delimitador, o Org nao consegue apaga-lo, e a
## exportacao seguinte emite a frase DUAS vezes -- sem erro. Ver MISTAKES.md.
cat(sprintf("Nas firmas, $S_{LK} = %s$ e a correlação entre trabalho e capital é $%s$, de modo que os dois \\alert{variam juntos}.\n",
            nm(m$S12, 1), nm(m$r12, 2)))
## As vinte observacoes, sem truncar, com a ordem de cada matriz sob ela.
##
## O corpo vai em \tiny, e por isso NAO usa `eq()`: envolver \begin{equation*}
## num grupo TeX faz a equacao sumir do PDF sem erro de compilacao, do mesmo
## modo que \begin{table} (Lectures/CLAUDE.md 5.9, verificado aqui em
## 2026-09-02). Matematica em linha dentro do grupo obedece ao \tiny; ambiente
## de display, nao.
ordem <- function(mtx, rotulo, dim)
  sprintf("\\underbrace{%s}_{%s\\;(%s)}", mtx, rotulo, dim)
cat(sprintf("\\begin{center}{\\tiny $\\displaystyle %s, \\qquad %s$}\\end{center}\n",
            ordem(mat(cbind(rep(1, m$n), m$x1, m$x2)), "\\mathbf{X}",
                  sprintf("%s \\times %s", ni(m$n), ni(m$k + 1))),
            ordem(mat(m$y), "\\mathbf{y}", sprintf("%s \\times 1", ni(m$n)))))
eq(sprintf("\\mathbf{X}'\\mathbf{X} = %s, \\qquad \\mathbf{X}'\\mathbf{y} = %s.",
           mat(m$XtX), mat(m$Xty)))
eq(sprintf("(\\mathbf{X}'\\mathbf{X})^{-1} = %s",
           mat(m$XtXinv, fmt = function(z) nm(z, 4))))
## A seta marca a coordenada de cada parametro. A ordem e a das colunas de X,
## e por isso o intercepto vem primeiro.
eq(sprintf("%s = %s \\; \\begin{matrix} \\leftarrow \\hat\\beta_0 \\\\ \\leftarrow \\hat\\beta_1 \\\\ \\leftarrow \\hat\\beta_2 \\end{matrix}",
           "\\hat{\\bm\\beta} = (\\mathbf{X}'\\mathbf{X})^{-1}\\mathbf{X}'\\mathbf{y}",
           cx(mat(m$bvec, fmt = function(z) nm(z, 2)))))
cat(sprintf("Aplicando $%s$ a cada firma:\n", ajuste))
tab_serie("$L_i$" = recorte(m$x1), "$K_i$" = recorte(m$x2), "$Y_i$" = recorte(m$y),
          "$\\hat Y_i$" = recorte(nm(m$aj, 1)), "$\\hat u_i$" = recorte(ns(m$u, 1)),
    caption = "Valores ajustados e resíduos", tamanho = "small")
d <- m$dados
## Grafico de base, e nao ggplot2: o ggplot nao compoe tres dimensoes. O
## fig_salva aceita uma funcao justamente para este caso.
gl <- seq(min(d$L) - 1, max(d$L) + 1, length.out = 26)
gk <- seq(min(d$K) - 1, max(d$K) + 1, length.out = 26)
z  <- outer(gl, gk, function(a, b) m$b0 + m$b1 * a + m$b2 * b)
desenho <- function() {
  par(mar = c(0.2, 0.2, 0.2, 0.2))
  vt <- persp(gl, gk, z, theta = 38, phi = 22, expand = 0.62,
              col = "grey93", border = "grey65", ticktype = "detailed",
              nticks = 4, cex.axis = 0.5, cex.lab = 0.8,
              xlab = "$L$", ylab = "$K$", zlab = "$\\hat Y$")
  ## Haste vertical de cada observacao ate o plano: o segmento E o residuo.
  for (i in seq_len(nrow(d)))
    lines(grDevices::trans3d(rep(d$L[i], 2), rep(d$K[i], 2),
                             c(m$aj[i], d$Y[i]), vt), col = "grey15", lwd = 1.1)
  points(grDevices::trans3d(d$L, d$K, d$Y, vt), pch = 19, cex = 0.75)
}
fig_salva("plano_ajustado_firmas.pdf", desenho, largura = 3.0, altura = 2.3,
          alt = "Plano ajustado sobre os eixos de trabalho e capital, com as firmas ligadas ao plano por segmentos verticais.")
eq(ajuste)
eqs(sprintf("\\frac{\\partial \\hat Y}{\\partial L} &= \\hat\\beta_1 = %s", nm(m$b1, 2)),
    sprintf("\\frac{\\partial \\hat Y}{\\partial K} &= \\hat\\beta_2 = %s", nm(m$b2, 2)))
cat("\nCada derivada parcial mantém a outra variável fixa, e é isso que a palavra \\alert{parcial} nomeia.\n")
cat(sprintf("\nO intercepto, $%s$, é a produção prevista para uma firma com $L = 0$ e $K = 0$, configuração que não existe na amostra.\n",
            nm(m$b0, 2)))
Xs <- cbind(1, m$x1)
bs <- as.vector(solve(t(Xs) %*% Xs) %*% (t(Xs) %*% m$y))
eq(sprintf("\\hat{\\bm\\beta}^{\\text{simples}} = (\\mathbf{X}_s'\\mathbf{X}_s)^{-1}\\mathbf{X}_s'\\mathbf{y} = %s, \\qquad \\hat{\\bm\\beta} = %s.",
           mat(bs, fmt = function(z) nm(z, 2)),
           mat(m$bvec, fmt = function(z) nm(z, 2))))
tab(data.frame(
  Modelo = c("Só $L$", "$L$ e $K$"),
  `$\\hat\\beta_1$` = paste0("$", nm(c(m$b1s, m$b1), 2), "$"),
  `Mantém constante` = c("nada", "o estoque de capital"),
  check.names = FALSE),
    caption = "O coeficiente do trabalho nos dois modelos", tamanho = "scriptsize")
cat(sprintf("\\correctwrong{Correto}{``Mantendo o estoque de capital constante, uma unidade adicional de trabalho está associada a %s unidades a mais de produção.''}{Errado}{``Se esta firma contratar mais uma unidade de trabalho, sua produção sobe %s unidades.''}\n",
            nm(m$b1, 2), nm(m$b1, 2)))
eq(sprintf("%s, \\qquad \\delta = %s = %s.",
           cx("\\hat\\beta_1^{\\text{simples}} = \\hat\\beta_1 + \\hat\\beta_2 \\cdot \\delta"),
           frac("S_{LK}", "S_{LL}"), nm(m$delta, 2)))
cat(sprintf("\nNas firmas: $%s + %s \\times %s = %s$.\n",
            nm(m$b1, 2), nm(m$b2, 2), nm(m$delta, 2), nm(m$b1s, 2)))
sob <- function(termo, rotulo, valor)
  sprintf("\\underbrace{%s}_{\\substack{\\text{%s} \\\\[2pt] = %s}}",
          termo, rotulo, valor)
eq(sprintf("%s = %s + %s",
           sob("\\sum (Y_i - \\bar Y)^2", "TSS", nm(m$TSS, 1)),
           sob("\\sum (\\hat Y_i - \\bar Y)^2", "ESS", nm(m$ESS, 1)),
           sob("\\sum \\hat u_i^2", "RSS", nm(m$RSS, 1))))
eq(sprintf("\\text{RSS} = \\hat{\\mathbf{u}}'\\hat{\\mathbf{u}} = \\sum \\hat u_i^2 = %s, \\qquad S_e^2 = %s = %s = %s.",
           nm(m$RSS, 1), frac("\\text{RSS}", "n-k-1"),
           frac(nm(m$RSS, 1), sprintf("%s-%s-1", ni(m$n), ni(m$k))), nm(m$Se2, 2)))
cat(sprintf("\nCada equação normal impõe uma restrição sobre os resíduos, de modo que os %s parâmetros estimados consomem %s graus de liberdade, e sobram $%s$.\n",
            ni(m$k + 1), ni(m$k + 1), ni(m$gl)))
cat("\nNa regressão simples eram dois, e o denominador era $n-2$.\n")
yty <- sum(m$y^2); bXty <- as.numeric(t(m$bvec) %*% m$Xty); nY2 <- m$n * m$my^2
eq(sprintf("\\mathbf{y}'\\mathbf{y} = %s, \\qquad \\hat{\\bm\\beta}'\\mathbf{X}'\\mathbf{y} = %s, \\qquad n\\bar Y^2 = %s.",
           nm(yty, 1), nm(bXty, 1), nm(nY2, 1)))
tab(data.frame(
  Fonte = c("Regressão", "Resíduo", "Total"),
  `Soma de quadrados` = paste0("$\\text{", c("ESS", "RSS", "TSS"), "} = ",
                               nm(c(m$ESS, m$RSS, m$TSS), 1), "$"),
  `g.l.` = paste0("$", c(sprintf("k = %s", ni(m$k)),
                         sprintf("n-k-1 = %s", ni(m$gl)),
                         sprintf("n-1 = %s", ni(m$n - 1))), "$"),
  `Quadrado médio` = c(paste0("$", nm(m$ESS / m$k, 1), "$"),
                       paste0("$S_e^2 = ", nm(m$Se2, 2), "$"), ""),
  `$F$` = c(paste0("$", nm(m$F, 1), "$"), "", ""),
  check.names = FALSE),
    caption = "Tabela ANOVA do exemplo", tamanho = "small")
eq(sprintf("F = %s = %s = %s \\;\\sim\\; F_{k,\\,n-k-1}",
           frac("\\text{ESS}/k", "\\text{RSS}/(n-k-1)"),
           frac(nm(m$ESS / m$k, 1), nm(m$Se2, 2)), nm(m$F, 1)))
cat(sprintf("\nCom $\\alpha = %s$ o valor crítico é $F_{%s;\\,%s} = %s$, e o $p$-valor do exemplo é inferior a $0{,}001$.\n",
            nm(m$alpha, 2), ni(m$k), ni(m$gl), nm(m$Fc, 2)))
eq(sprintf("R^2 = %s = %s = %s.", frac("\\text{ESS}", "\\text{TSS}"),
           frac(nm(m$ESS, 1), nm(m$TSS, 1)), nm(m$R2, 3)))
d <- m$dados
s1 <- summary(lm(Y ~ L, data = d)); s2 <- summary(lm(Y ~ L + K, data = d))
tab(data.frame(
  Modelo = c("só $L$", "$L$ e $K$"),
  `$R^2$` = paste0("$", nm(c(s1$r.squared, s2$r.squared), 3), "$"),
  `$\\bar R^2$` = paste0("$", nm(c(s1$adj.r.squared, s2$adj.r.squared), 3), "$"),
  check.names = FALSE),
    caption = "$R^2$ e $R^2$ ajustado nos dois modelos", tamanho = "small")
eq(dgp)
tab(data.frame(
  Parâmetro = c("$\\beta_0$", "$\\beta_1$", "$\\beta_2$", "$\\sigma$"),
  `No processo gerador` = paste0("$", ni(unlist(m$dgp)), "$"),
  Estimado = paste0("$", nm(c(m$b0, m$b1, m$b2, m$Se), 2), "$"),
  check.names = FALSE),
    caption = "Parâmetros do processo gerador e suas estimativas", tamanho = "scriptsize")
co <- summary(multipla)$coefficients
tab(data.frame(
  Termo = c("Intercepto", "\\texttt{educ}", "\\texttt{exper}", "\\texttt{tenure}"),
  Estimativa = nm(co[, 1], 4),
  `Erro padrão` = nm(co[, 2], 4),
  `$t$` = nm(co[, 3], 2),
  `$p$-valor` = ifelse(co[, 4] < 0.001, "$< 0{,}001$", paste0("$", nm(co[, 4], 3), "$")),
  check.names = FALSE),
    caption = "Modelo múltiplo estimado sobre \\texttt{wage1}", tamanho = "small")
cat(sprintf("\n$n = %s$, $R^2 = %s$, $\\bar R^2 = %s$.\n",
            ni(nobs(multipla)), nm(summary(multipla)$r.squared, 3),
            nm(summary(multipla)$adj.r.squared, 3)))
b1s <- coef(simples)[["educ"]]; b1m <- coef(multipla)[["educ"]]
cat(sprintf(paste("O coeficiente de \\texttt{educ} passa de $%s$, no modelo simples, a $%s$ na RLM.",
                  "\\alert{Mantendo constantes} a experiência e o tempo no emprego atual,",
                  "um ano a mais de estudo está associado a $%s\\%%$ a mais de salário.\n"),
            nm(b1s, 4), nm(b1m, 4), nm(100 * b1m, 1)))
cat(sprintf("- $\\hat\\beta_2 = %s$ é grande, ou é ruído amostral?\n", nm(m$b2, 2)))
cat("- Qual o erro padrão de um coeficiente parcial?\n")
cat(sprintf("- O $F = %s$ rejeita o quê, exatamente?\n", nm(m$F, 1)))
eq(sprintf("\\hat Y_i = %s %s L_i %s K_i", ni(e$b0), ns(e$b1, 0), ns(e$b2, 0)))
tab_serie("$L_i$" = e$x1, "$K_i$" = e$x2, "$Y_i$" = e$y,
    caption = "Dados do exercício", tamanho = "small")
## Centrado dentro da coluna, como a tabela acima: um paragrafo solto encosta
## na barra de progresso da margem esquerda.
cat(sprintf("\n\\begin{center}\\small $S_{LL} = %s$, $S_{KK} = %s$, $S_{LK} = %s$,\n$S_{LY} = %s$, $S_{KY} = %s$.\\end{center}\n",
            ni(e$S11), ni(e$S22), ni(e$S12), ni(e$S1y), ni(e$S2y)))
tab(data.frame(
  Fonte = c("Regressão", "Resíduo", "Total"),
  `Soma de quadrados` = c("\\rule{1.4cm}{0.4pt}", paste0("$", ni(e$RSS), "$"),
                          paste0("$", ni(e$TSS), "$")),
  `g.l.` = c("\\rule{1.4cm}{0.4pt}", "\\rule{1.4cm}{0.4pt}",
             paste0("$", ni(e$n - 1), "$")),
  `Quadrado médio` = c("\\rule{1.4cm}{0.4pt}", "\\rule{1.4cm}{0.4pt}", ""),
  check.names = FALSE),
    caption = "Tabela ANOVA a completar", tamanho = "scriptsize")
tab_serie("$X_{1i}$" = q$x1, "$X_{2i}$" = q$x2, "$Y_i$" = q$y,
    caption = "Dados do exercício de notação matricial")
## A amostra INTEIRA, e nao um recorte: e a partir desta tabela que o aluno
## refaz a conta. Vinte linhas nao cabem na altura do slide, entao a serie e
## dobrada em quatro blocos de cinco, lado a lado.
d <- m$dados; blocos <- split(seq_len(nrow(d)), rep(1:4, each = nrow(d) / 4))
corpo <- do.call(cbind, lapply(blocos, function(i)
  data.frame(Firma = d$firma[i], `$L_i$` = d$L[i], `$K_i$` = d$K[i],
             `$Y_i$` = d$Y[i], check.names = FALSE)))
names(corpo) <- rep(c("Firma", "$L_i$", "$K_i$", "$Y_i$"), 4)
tab(corpo, caption = sprintf("Base de dados sintética ($n = %s$)", ni(m$n)),
    align = paste(rep("r", 16), collapse = ""), tamanho = "scriptsize")
eq(sprintf("\\begin{cases} %s\\,\\hat\\beta_1 + %s\\,\\hat\\beta_2 = %s \\\\ %s\\,\\hat\\beta_1 + %s\\,\\hat\\beta_2 = %s \\end{cases} \\quad \\det = %s",
           nm(m$S11, 1), nm(m$S12, 1), nm(m$S1y, 1),
           nm(m$S12, 1), nm(m$S22, 1), nm(m$S2y, 1), nm(m$det, 1)))
eq(sprintf("\\hat\\beta_1 = %s = %s, \\qquad \\hat\\beta_2 = %s = %s.",
           frac("S_{KK} S_{LY} - S_{LK} S_{KY}", "\\det"), nm(m$b1, 2),
           frac("S_{LL} S_{KY} - S_{LK} S_{LY}", "\\det"), nm(m$b2, 2)))
eq(sprintf("\\hat\\beta_0 = \\bar Y - \\hat\\beta_1 \\bar L - \\hat\\beta_2 \\bar K = %s.",
           nm(m$b0, 2)))
av <- anova(lm(Y ~ L + K, data = m$dados))
tab(data.frame(
  ` ` = paste0("\\texttt{", rownames(av), "}"),
  `\\texttt{Df}` = ni(av$Df),
  `\\texttt{Sum Sq}` = nm(av$`Sum Sq`, 1),
  `\\texttt{Mean Sq}` = nm(av$`Mean Sq`, 1),
  `\\texttt{F value}` = c(nm(av$`F value`[1:2], 1), ""),
  check.names = FALSE),
    caption = "Decomposição sequencial que o R produz", tamanho = "small")
av <- anova(lm(Y ~ L + K, data = m$dados)); sq <- av$`Sum Sq`
eq(sprintf("%s = %s = \\text{ESS}.",
           paste(nm(sq[1:2], 1), collapse = " + "), nm(m$ESS, 1)))
cat(sprintf("\nA primeira parcela é a ESS da regressão \\alert{simples} de $Y$ contra $L$, que vale $S_{LY}^2/S_{LL}$.\n"))
fs <- summary(lm(Y ~ L + K, data = m$dados))$fstatistic
cat(sprintf("\\texttt{value} $= %s$, \\texttt{numdf} $= %s$, \\texttt{dendf} $= %s$ --- o $F$ global vive no \\texttt{summary}, e não em \\texttt{anova}.\n",
            nm(fs[["value"]], 1), ni(fs[["numdf"]]), ni(fs[["dendf"]])))
d <- m$dados
a1 <- anova(lm(Y ~ L + K, data = d)); a2 <- anova(lm(Y ~ K + L, data = d))
tab(data.frame(
  Linha = c("1ª", "2ª", "Res."),
  ## Os rotulos sao nomes de termo do R, e vao em \texttt: em modo matematico
  ## `Residuals` sai como produto de letras italicas, sem espacamento.
  `\\texttt{lm(Y \\textasciitilde{} L + K)}` = sprintf("\\texttt{%s}: $%s$", rownames(a1), nm(a1$`Sum Sq`, 1)),
  `\\texttt{lm(Y \\textasciitilde{} K + L)}` = sprintf("\\texttt{%s}: $%s$", rownames(a2), nm(a2$`Sum Sq`, 1)),
  check.names = FALSE),
    caption = "Somas de quadrados sob duas ordens de entrada",
    tamanho = "small")
cat(sprintf(paste("\\alert{Item 1.} Mantendo o estoque de capital constante, uma unidade a mais de trabalho está associada a $%s$ unidades a mais de produção;",
                  "mantendo o trabalho constante, uma unidade a mais de capital, a $%s$ unidades a mais.\n"),
            ni(e$b1), ni(e$b2)))
cat(sprintf("\n\\alert{Item 2.} $\\hat\\beta_1^{\\text{simples}} = S_{LY}/S_{LL} = %s/%s = %s$. O trabalho não parece ter efeito.\n",
            ni(e$S1y), ni(e$S11), ni(e$b1s)))
cat(sprintf("\n\\alert{Item 3.} $\\delta = S_{LK}/S_{LL} = %s$, e o viés é $\\hat\\beta_2\\delta = %s(%s) = %s$, de modo que $%s %s = %s$.\n",
            nm(e$delta, 1), ni(e$b2), nm(e$delta, 1), ni(e$b2 * e$delta),
            ni(e$b1), ns(e$b2 * e$delta, 0), ni(e$b1s)))
tab(data.frame(
  Fonte = c("Regressão", "Resíduo", "Total"),
  `Soma de quadrados` = paste0("$", ni(c(e$ESS, e$RSS, e$TSS)), "$"),
  `g.l.` = paste0("$", ni(c(e$k, e$gl, e$n - 1)), "$"),
  `Quadrado médio` = c(paste0("$", ni(e$ESS / e$k), "$"),
                       paste0("$", ni(e$Se2), "$"), ""),
  check.names = FALSE),
    caption = "Tabela ANOVA completa do exercício", tamanho = "scriptsize")
eq(sprintf("F = %s = %s \\quad (p\\text{-valor} = %s), \\qquad R^2 = %s = %s.",
           frac(ni(e$ESS / e$k), ni(e$Se2)), ni(e$F), nm(e$pF, 3),
           frac(ni(e$ESS), ni(e$TSS)), nm(e$R2, 3)))
cat(sprintf("\nCom $\\alpha = %s$ e $F_{%s;\\,%s} = %s$, o valor observado excede o crítico e rejeita-se $H_0$.\n",
            nm(e$alpha, 2), ni(e$k), ni(e$gl), nm(e$Fc, 2)))
eq(sprintf("\\mathbf{X}_{%s \\times %s} = %s, \\qquad \\mathbf{y}_{%s \\times 1} = %s.",
           ni(q$n), ni(q$k + 1), mat(q$X), ni(q$n), mat(q$vy)))
eq(sprintf("\\mathbf{X}'\\mathbf{X} = %s, \\qquad \\mathbf{X}'\\mathbf{y} = %s.",
           mat(q$XtX), mat(q$Xty)))
eq(sprintf("\\begin{cases} %s\\,\\hat\\beta_1 + %s\\,\\hat\\beta_2 = %s \\\\ %s\\,\\hat\\beta_1 + %s\\,\\hat\\beta_2 = %s \\end{cases} \\qquad \\det = %s",
           ni(q$S11), ni(q$S12), ni(q$S1y), ni(q$S12), ni(q$S22), ni(q$S2y), ni(q$det)))
eq(sprintf("%s, \\qquad \\hat{\\mathbf{u}} = %s, \\qquad \\mathbf{X}'\\hat{\\mathbf{u}} = %s.",
           cx(sprintf("\\hat{\\bm\\beta} = %s", mat(q$bvec))),
           mat(q$uvec), mat(round(t(q$X) %*% q$uvec))))
