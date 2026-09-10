source("code/bloco_setup.R"); m <- ctx_firmas()
eq(sprintf("\\hat Y_i = %s %s L_i %s K_i", nm(m$b0, 2), ns(m$b1, 0), ns(m$b2, 0)))

source("code/bloco_setup.R"); m <- ctx_firmas()
tab(data.frame(`$S_{LL}$` = nm(m$S11, 1), `$S_{KK}$` = nm(m$S22, 1), `$S_{LK}$` = nm(m$S12, 1),
               `$\\det$` = nm(m$det, 1), `RSS` = nm(m$RSS, 1), `$S_e^2$` = nm(m$Se2, 2),
               `$n-k-1$` = ni(m$gl), check.names = FALSE),
    caption = "Somas de desvios do exemplo")
cat(sprintf("\nTambém: $\\text{ESS} = %s$, $\\text{TSS} = %s$, $R^2 = %s$ e $F = %s$.\n",
            nm(m$ESS, 1), nm(m$TSS, 1), nm(m$R2, 3), nm(m$F, 1)))

source("code/bloco_setup.R"); m <- ctx_firmas()
cat(sprintf("Com $r_{LK} = %s$, o fator $1/(1-%s) = %s$: a variância é cerca de $%s$ vezes a que seria com regressores não correlacionados.\n",
            nm(m$r12, 1), nm(m$r12^2, 2), nm(1 / (1 - m$r12^2), 2),
            nm(1 / (1 - m$r12^2), 1)))

source("code/bloco_setup.R"); m <- ctx_firmas()
eq(sprintf("S_{\\hat\\beta_1} = \\sqrt{%s} = %s, \\qquad S_{\\hat\\beta_2} = \\sqrt{%s} = %s.",
           frac("S_e^2 \\, S_{KK}", "\\det"), nm(m$Sb1, 3),
           frac("S_e^2 \\, S_{LL}", "\\det"), nm(m$Sb2, 3)))

source("code/bloco_setup.R"); m <- ctx_firmas(); d <- m$dados
co <- summary(lm(Y ~ L + K, data = d))$coefficients
tab(data.frame(Coeficiente = c("$\\hat\\beta_0$", "$\\hat\\beta_1$ ($L$)", "$\\hat\\beta_2$ ($K$)"),
               Estimativa = paste0("$", nm(co[, 1], 2), "$"),
               `Erro padrão` = paste0("$", nm(co[, 2], 3), "$"), check.names = FALSE),
    caption = "Estimativas e erros padrão", tamanho = "scriptsize")

source("code/bloco_setup.R"); library(ggplot2)
m <- ctx_firmas(); d <- m$dados
co <- summary(lm(Y ~ L + K, data = d))$coefficients
ic <- confint(lm(Y ~ L + K, data = d))
## Com o motor tikz o rotulo e composto pelo LaTeX, entao o nome do parametro
## vai como matematica de verdade em vez de "beta_1" em texto corrido.
rot <- c("$\\beta_1$ (L)", "$\\beta_2$ (K)")
g <- data.frame(termo = factor(rot, levels = rev(rot)),
                est = co[2:3, 1], lo = ic[2:3, 1], hi = ic[2:3, 2])
p <- ggplot(g, aes(est, termo)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.12, linewidth = 0.7) +
  geom_point(size = 2.8) +
  ## O porcento vai escapado: com o motor tikz o rotulo e escrito no .tex, e um
  ## "%" cru comentaria o resto da linha.
  labs(x = "Estimativa e intervalo de confiança a 95\\%", y = NULL,
       caption = "A linha tracejada é o zero. O intervalo que a cruza não rejeita a nulidade do coeficiente.") +
  theme_minimal(base_size = 9) +
  theme(plot.caption = element_text(size = 6))
fig_salva("ic_coeficientes_firmas.pdf", p, largura = 5.2, altura = 1.50,
          alt = "Intervalos de confiança dos coeficientes de L e K, com uma linha tracejada no zero.")

source("code/bloco_setup.R"); m <- ctx_firmas()
eq(sprintf("t(\\hat\\beta_1) = %s = %s, \\qquad t(\\hat\\beta_2) = %s = %s.",
           frac(nm(m$b1, 2), nm(m$Sb1, 3)), nm(m$t1, 2),
           frac(nm(m$b2, 2), nm(m$Sb2, 3)), nm(m$t2, 2)))
cat(sprintf("\nO valor crítico é $t_{%s}(%s) = %s$.\n", nm(m$alpha / 2, 3), ni(m$gl), nm(m$tc, 2)))
eq(sprintf("%s < %s \\quad\\text{e}\\quad %s > %s.",
           nm(m$t1, 2), nm(m$tc, 2), nm(m$t2, 2), nm(m$tc, 2)))

source("code/bloco_setup.R"); m <- ctx_firmas()
g <- m$gl; lim <- round(max(abs(m$t2), m$tc) + 0.5, 2)
## constante da densidade t avaliada no R: pgfplots so precisa da forma fechada
cte <- gamma((g + 1) / 2) / (sqrt(g * pi) * gamma(g / 2))
dens <- sprintf("%s*(1+x^2/%s)^(-%s)", signif(cte, 6), g, (g + 1) / 2)
cat("\\centering\n\\begin{tikzpicture}\n")
cat(sprintf("\\begin{axis}[width=0.86\\textwidth, height=4.6cm, domain=%s:%s, samples=200,\n", -lim, lim))
cat(sprintf("  axis lines=middle, ymin=0, ymax=0.42, xlabel={$t$}, ylabel={},\n  xtick={%s,0,%s}, xticklabels={$%s$,,$%s$},\n",
            round(-m$tc, 2), round(m$tc, 2), nm(-m$tc, 2), nm(m$tc, 2)))
cat("  ytick=\\empty, clip=false, x tick label style={font=\\tiny}]\n")
cat(sprintf("  \\addplot[thick, black] {%s};\n", dens))
for (lado in list(c(round(m$tc, 2), lim), c(-lim, round(-m$tc, 2))))
  cat(sprintf("  \\addplot[draw=none, fill=catcolor, fill opacity=0.28, domain=%s:%s] {%s} \\closedcycle;\n",
              lado[1], lado[2], dens))
cat(sprintf("  \\draw[thick, catcolor] (axis cs:%s,0) -- (axis cs:%s,0.30) node[above, font=\\scriptsize] {$t_L = %s$};\n",
            round(m$t1, 3), round(m$t1, 3), nm(m$t1, 2)))
cat(sprintf("  \\draw[thick, black] (axis cs:%s,0) -- (axis cs:%s,0.22) node[above, font=\\scriptsize] {$t_K = %s$};\n",
            round(m$t2, 3), round(m$t2, 3), nm(m$t2, 2)))
cat("\\end{axis}\n\\end{tikzpicture}\n")

source("code/bloco_setup.R"); m <- ctx_firmas()
eq(sprintf("|t(\\hat\\beta_1)| = %s < %s = t_{%s}(%s) \\qquad\\Longrightarrow\\qquad \\text{não se rejeita } H_0: \\beta_1 = 0.",
           nm(m$t1, 2), nm(m$tc, 2), nm(m$alpha / 2, 3), ni(m$gl)))
cat(sprintf("\nO coeficiente do trabalho é $%s$, tem o sinal esperado, e ainda assim não é distinguível de zero a $%s\\%%$.\n",
            nm(m$b1, 2), ni(100 * m$alpha)))

source("code/bloco_setup.R"); m <- ctx_firmas()
eq(sprintf("\\hat\\beta_1 \\pm t_{%s}(%s)\\, S_{\\hat\\beta_1} = %s \\pm %s \\times %s = %s.",
           nm(m$alpha / 2, 3), ni(m$gl), nm(m$b1, 2), nm(m$tc, 2), nm(m$Sb1, 3), iv(m$ic1)))

source("code/bloco_setup.R"); m <- ctx_firmas(); c0 <- 1
eq(sprintf("H_0: \\beta_1 = %s \\qquad t = %s = %s < %s.",
           ni(c0), frac(sprintf("%s - %s", nm(m$b1, 2), ni(c0)), nm(m$Sb1, 3)),
           nm((m$b1 - c0) / m$Sb1, 2), nm(m$tc, 2)))
cat(sprintf("\nTambém não se rejeita. Esta amostra é compatível tanto com $\\beta_1 = 0$ quanto com $\\beta_1 = %s$ --- o que é uma afirmação honesta sobre quão pouco ela informa.\n", ni(c0)))

source("code/bloco_setup.R"); m <- ctx_firmas()
tcu <- qt(1 - m$alpha, m$gl)
cat(sprintf("A estatística é a mesma. Muda o valor crítico: toda a área $\\alpha$ vai para uma cauda só, logo $t_{%s}(%s) = %s$ em vez de $%s$.\n\n",
            nm(m$alpha, 2), ni(m$gl), nm(tcu, 2), nm(m$tc, 2)))
cat(sprintf("Com $t = %s %s %s$, aqui %s.\n", nm(m$t1, 2),
            if (m$t1 > tcu) ">" else "<", nm(tcu, 2),
            if (m$t1 > tcu) "se rejeitaria" else "também não se rejeitaria"))

source("code/bloco_setup.R"); m <- ctx_firmas()
tab(data.frame(Coeficiente = c("$L$", "$K$"),
               `$t$` = paste0("$", nm(c(m$t1, m$t2), 2), "$"),
               `$p$-valor` = paste0("$", nm(c(m$p1, m$p2), 3), "$"),
               check.names = FALSE),
    caption = "Estatísticas $t$ e $p$-valores")
cat(sprintf("\nRejeita-se $H_0$ quando $p < \\alpha$. Com $\\alpha = %s$: $K$ sim, $L$ não.\n",
            nm(m$alpha, 2)))

source("code/bloco_setup.R"); m <- ctx_firmas()
eq(sprintf("F = %s = %s, \\qquad p = %s.", frac("\\text{ESS}/k", "\\text{RSS}/(n-k-1)"),
           nm(m$F, 1), nm(m$pF, 3)))

source("code/bloco_setup.R"); m <- ctx_firmas()
dec <- function(p) if (p < m$alpha) "rejeita" else "não rejeita"
tab(data.frame(
  Teste = c("$H_0: \\beta_1 = 0$", "$H_0: \\beta_2 = 0$", "$H_0: \\beta_1 = \\beta_2 = 0$"),
  Estatística = paste0("$", c(sprintf("t = %s", nm(m$t1, 2)), sprintf("t = %s", nm(m$t2, 2)),
                              sprintf("F = %s", nm(m$F, 1))), "$"),
  `$p$-valor` = paste0("$", nm(c(m$p1, m$p2, m$pF), 3), "$"),
  `Decisão` = sapply(c(m$p1, m$p2, m$pF), dec),
  check.names = FALSE),
    caption = "Os três resultados lado a lado")

source("code/bloco_setup.R"); m <- ctx_firmas()
cat(sprintf("A variância de cada coeficiente é inflada pelo fator $1/(1-r^2)$, que aqui vale $%s$.\n",
            nm(1 / (1 - m$r12^2), 2)))

source("code/bloco_setup.R"); m <- ctx_firmas()
eq(sprintf("\\underbrace{r_{LK} = %s}_{\\text{regressores se sobrepõem}} \\ \\longrightarrow\\ \\underbrace{S_{\\hat\\beta_j} \\text{ grande}}_{\\text{sobra pouca varia\\c{c}\\~ao}} \\ \\longrightarrow\\ \\underbrace{|t| \\text{ pequeno}}_{\\text{não rejeita}}",
           nm(m$r12, 1)))

source("code/bloco_setup.R"); m <- ctx_firmas(); mt <- 3
cat(sprintf("Com $m$ testes independentes ao nível $\\alpha$, a probabilidade de ao menos uma rejeição falsa é $1-(1-\\alpha)^m$ --- com $m = %s$ e $\\alpha = %s$, cerca de $%s\\%%$.\n",
            ni(mt), nm(m$alpha, 2), nm(100 * (1 - (1 - m$alpha)^mt), 0)))

source("code/bloco_setup.R")
library(wooldridge); data("wage1")
modelo <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
co <- summary(modelo)$coefficients; ic <- confint(modelo)
g <- data.frame(
  Termo = rownames(co), est = nm(co[, 1], 4), se = nm(co[, 2], 4),
  t = nm(co[, 3], 2), p = nm(co[, 4], 4),
  ic = sprintf("$[%s;\\ %s]$", nm(ic[, 1], 3), nm(ic[, 2], 3)),
  check.names = FALSE)
names(g) <- c("Termo", "Estimativa", "Erro padrão", "$t$", "$p$", "IC 95\\%")
tab(g, caption = "Resultado do \\texttt{summary()} sobre \\texttt{wage1}")
cat(sprintf("\n$n = %s$, $S_e = %s$, $n-k-1 = %s$, $R^2 = %s$, $F = %s$.\n",
            ni(nobs(modelo)), nm(sigma(modelo), 4), ni(df.residual(modelo)),
            nm(summary(modelo)$r.squared, 3), nm(summary(modelo)$fstatistic[1], 1)))

source("code/bloco_setup.R"); library(ggplot2)
library(wooldridge); data("wage1")
modelo <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
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
          alt = "Intervalos de confiança dos coeficientes de educ, exper e tenure, com uma linha tracejada no zero.")

source("code/bloco_setup.R"); d <- dados("firmas_exercicio.csv"); e <- ctx_firmas_ex()
tab_serie("$L_i$" = d$L, "$K_i$" = d$K, "$Y_i$" = d$Y,
    caption = "Dados do exercício")
cat(sprintf("\nJá se sabe: $\\hat Y = %s %s L %s K$, $S_{LL} = %s$, $S_{KK} = %s$, $S_{LK} = %s$, $\\det = %s$, RSS $= %s$.\n",
            ni(e$b0), ns(e$b1, 0), ns(e$b2, 0), ni(e$S11), ni(e$S22), ni(e$S12), ni(e$det), ni(e$RSS)))

source("code/bloco_setup.R"); e <- ctx_firmas_ex()
cat(sprintf("\\alert{Item 1.} $S_e^2 = %s/%s = %s$, e\n", ni(e$RSS), ni(e$gl), ni(e$Se2)))
eq(sprintf("S_{\\hat\\beta_1} = S_{\\hat\\beta_2} = \\sqrt{%s} = %s.",
           frac(sprintf("%s \\times %s", ni(e$Se2), ni(e$S22)), ni(e$det)), nm(e$Sb1, 3)))
cat(sprintf("\n\\alert{Item 2.} $t(\\hat\\beta_1) = %s$ e $t(\\hat\\beta_2) = %s$, contra $t_{%s}(%s) = %s$. Não se rejeita para $L$; rejeita-se para $K$.\n",
            nm(e$t1, 2), nm(e$t2, 2), nm(e$alpha / 2, 3), ni(e$gl), nm(e$tc, 2)))
cat(sprintf("\n\\alert{Item 3.} $%s \\pm %s \\times %s = %s$, que contém zero --- coerente com não rejeitar.\n",
            ni(e$b1), nm(e$tc, 2), nm(e$Sb1, 3), iv(e$ic1)))
cat(sprintf("\n\\alert{Item 4.} O modelo é conjuntamente significativo ($F = %s$, $p = %s$) com um coeficiente individualmente insignificante. Aqui $r_{LK} = %s$, e o fator de inflação $1/(1-%s) = %s$ é modesto: a causa principal é o tamanho da amostra, com apenas %s graus de liberdade.\n",
            ni(e$F), nm(e$pF, 3), nm(e$r12, 1), nm(e$r12^2, 2),
            nm(1 / (1 - e$r12^2), 2), ni(e$gl)))

source("code/bloco_setup.R"); q <- dados("anpec2021_q15.csv")
cat(sprintf("Estimou-se por OLS, com $n = %s$ imóveis, um modelo em logaritmos do preço contra características do imóvel. Reporta-se $R^2 = %s$, $\\text{TSS} = %s$ e $S_e = %s$, com erros padrão entre parênteses sob cada coeficiente. Julgue:\n\n",
            ni(q$n), nm(q$r2, 2), nm(q$tss, 2), nm(q$se, 2)))
itens <- c(
  sprintf("Um coeficiente cuja razão entre estimativa e erro padrão vale $%s$ é significativo a $%s\\%%$ num teste bilateral.",
          nm(q$razao_avaliada, 1), ni(100 * q$alpha_item)),
  "A soma de quadrados dos resíduos pode ser obtida como $(1-R^2)\\,\\text{TSS}$.",
  "Ao acrescentar um regressor irrelevante, o $R^2$ não cai e o $\\bar R^2$ pode cair.",
  "A estatística $F$ de significância da regressão testa a hipótese de que todos os coeficientes de inclinação são nulos simultaneamente.",
  "Num modelo com $\\log(\\text{preço})$ como dependente, o coeficiente de uma variável binária multiplicado por $100$ aproxima a variação percentual do preço associada à característica.")
cat("\\begin{enumerate}\n")
cat(sprintf("  \\item[(%d)] %s\n", seq_along(itens) - 1, itens), sep = "")
cat("\\end{enumerate}\n")

source("code/bloco_setup.R"); q <- dados("anpec2021_q15.csv")
tc <- qt(1 - q$alpha_item / 2, q$n - q$k - 1)
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
    caption = "Gabarito do Exercício 2")
