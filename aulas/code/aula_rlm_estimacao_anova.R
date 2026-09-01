source("code/bloco_setup.R"); m <- ctx_firmas()
tab_serie("Firma" = m$dados$firma, "$L_i$" = m$x1, "$K_i$" = m$x2, "$Y_i$" = m$y,
    caption = "Dados das cinco firmas")

source("code/bloco_setup.R"); m <- ctx_firmas()
cat(sprintf("Médias: $\\bar L = %s$, $\\bar K = %s$, $\\bar Y = %s$. Somas de desvios:\n",
            ni(m$mx1), ni(m$mx2), ni(m$my)))
eq(sprintf("S_{LL} = %s, \\quad S_{KK} = %s, \\quad S_{LK} = %s, \\quad S_{LY} = %s, \\quad S_{KY} = %s.",
           ni(m$S11), ni(m$S22), ni(m$S12), ni(m$S1y), ni(m$S2y)))
cat(sprintf("\nNote $S_{LK} = %s$: trabalho e capital \\alert{se movem juntos} nesta amostra, com correlação $%s$. Isso reaparece adiante.\n",
            ni(m$S12), nm(m$r12, 1)))

source("code/bloco_setup.R"); m <- ctx_firmas()
eq(sprintf("\\begin{cases} %s\\,\\hat\\beta_1 + %s\\,\\hat\\beta_2 = %s \\\\ %s\\,\\hat\\beta_1 + %s\\,\\hat\\beta_2 = %s \\end{cases}",
           ni(m$S11), ni(m$S12), ni(m$S1y), ni(m$S12), ni(m$S22), ni(m$S2y)))
cat(sprintf("\nDeterminante: $%s \\times %s - %s \\times %s = %s$.\n",
            ni(m$S11), ni(m$S22), ni(m$S12), ni(m$S12), ni(m$det)))
eq(sprintf("\\hat\\beta_1 = %s = %s = %s, \\qquad \\hat\\beta_2 = %s = %s = %s.",
           frac(sprintf("%s(%s) - %s(%s)", ni(m$S22), ni(m$S1y), ni(m$S12), ni(m$S2y)), ni(m$det)),
           frac(ni(m$S22 * m$S1y - m$S12 * m$S2y), ni(m$det)), ni(m$b1),
           frac(sprintf("%s(%s) - %s(%s)", ni(m$S11), ni(m$S2y), ni(m$S12), ni(m$S1y)), ni(m$det)),
           frac(ni(m$S11 * m$S2y - m$S12 * m$S1y), ni(m$det)), ni(m$b2)))
eq(sprintf("\\hat\\beta_0 = \\bar Y - \\hat\\beta_1 \\bar L - \\hat\\beta_2 \\bar K = %s - %s - %s = %s \\qquad\\Longrightarrow\\qquad %s",
           ni(m$my), ni(m$b1 * m$mx1), ni(m$b2 * m$mx2), ni(m$b0),
           cx(sprintf("\\hat Y_i = %s %s L_i %s K_i", ni(m$b0), ns(m$b1, 0), ns(m$b2, 0)))))

source("code/bloco_setup.R"); m <- ctx_firmas()
tab_serie("$L_i$" = m$x1, "$K_i$" = m$x2, "$Y_i$" = m$y,
          "$\\hat Y_i$" = ni(m$aj), "$\\hat u_i$" = ni(m$u),
    caption = "Valores ajustados e resíduos")

source("code/bloco_setup.R"); m <- ctx_firmas()
eq(sprintf("\\text{RSS} = \\sum \\hat u_i^2 = %s, \\qquad S_e^2 = %s = %s = %s.",
           ni(m$RSS), frac("\\text{RSS}", "n-k-1"),
           frac(ni(m$RSS), sprintf("%s-%s-1", ni(m$n), ni(m$k))), ni(m$Se2)))

source("code/bloco_setup.R"); m <- ctx_firmas()
eq(sprintf("\\hat\\beta_1^{\\text{simples}} = %s = %s = %s.",
           frac("S_{LY}", "S_{LL}"), frac(ni(m$S1y), ni(m$S11)), ni(m$b1s)))

source("code/bloco_setup.R"); m <- ctx_firmas()
tab(data.frame(
  Modelo = c("$Y = \\beta_0 + \\beta_1 L + u$", "$Y = \\beta_0 + \\beta_1 L + \\beta_2 K + u$"),
  `Coeficiente de $L$` = paste0("$", ni(c(m$b1s, m$b1)), "$"), check.names = FALSE),
    caption = "Coeficiente de $L$ no modelo simples e no múltiplo")

source("code/bloco_setup.R"); m <- ctx_firmas()
eq(sprintf("L_i = \\gamma_0 + \\gamma_1 K_i + \\tilde{L}_i, \\qquad \\hat\\gamma_1 = %s = %s = %s.",
           frac("S_{LK}", "S_{KK}"), frac(ni(m$S12), ni(m$S22)), nm(m$gamma1, 1)))

source("code/bloco_setup.R"); m <- ctx_firmas()
tab_serie("$\\tilde L_i$" = paste0("$", nm(m$aux, 1), "$"),
    caption = "Resíduos da regressão auxiliar")

source("code/bloco_setup.R"); library(ggplot2)
m <- ctx_firmas(); d <- m$dados
## Residuos de Y contra K: a parte de Y que o capital nao explica.
res_y <- residuals(lm(Y ~ K, data = d))
## Niveis declarados: o facet_wrap ordena alfabeticamente, e hoje a ordem
## alfabetica coincide com a ordem em que o texto abaixo apresenta os paineis.
## A declaracao existe para que uma renomeacao futura nao inverta a figura em
## silencio -- foi o que aconteceu em rls_anamorfose.org em 2026-09-01.
paineis <- c("Bruto: $Y$ contra $L$", "Descontado $K$: resíduo contra resíduo")
painel <- rbind(
  data.frame(x = d$L, y = d$Y, painel = paineis[1]),
  data.frame(x = m$aux, y = res_y, painel = paineis[2]))
painel$painel <- factor(painel$painel, levels = paineis)
p <- ggplot(painel, aes(x, y)) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey40", linewidth = 0.7) +
  geom_point(size = 2.5) +
  facet_wrap(~ painel, scales = "free") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(face = "bold", size = 7))
fig_salva("fwl_parcial_vs_simples.pdf", p, largura = 5.2, altura = 1.85,
          alt = "Dois painéis de dispersão: Y contra L em bruto, e resíduo contra resíduo depois de descontar K.")

source("code/bloco_setup.R"); m <- ctx_firmas()
eq(sprintf("%s = %s = \\hat\\beta_1.",
           frac("\\sum \\tilde L_i Y_i", "\\sum \\tilde L_i^2"),
           cx(ni(sum(m$aux * m$y) / sum(m$aux^2)))))

source("code/bloco_setup.R"); m <- ctx_firmas()
eq(sprintf("%s, \\qquad \\delta = %s = %s.",
           cx("\\hat\\beta_1^{\\text{simples}} = \\beta_1 + \\beta_2 \\cdot \\delta"),
           frac("S_{LK}", "S_{LL}"), nm(m$delta, 1)))
cat(sprintf("\nAqui: $%s + %s \\times %s = %s$. A conta fecha exatamente.\n",
            ni(m$b1), ni(m$b2), nm(m$delta, 1), ni(m$b1s)))

source("code/bloco_setup.R"); m <- ctx_firmas()
cat(sprintf("\\correctwrong{Correto}{``Entre firmas com o mesmo capital, uma unidade adicional de trabalho está associada a %s unidades a mais de produção.''}{Errado}{``Se esta firma contratar mais uma unidade de trabalho, sua produção sobe %s unidades.''}\n",
            ni(m$b1), ni(m$b1)))

source("code/bloco_setup.R"); m <- ctx_firmas()
eq(sprintf("\\hat\\beta_1 = %s = %s = %s.",
           frac("S_{KK} S_{LY} - S_{LK} S_{KY}", "S_{LL}S_{KK} - S_{LK}^2"),
           frac(sprintf("%s(%s) - %s(%s)", ni(m$S22), ni(m$S1y), ni(m$S12), ni(m$S2y)),
                sprintf("%s - %s", ni(m$S11 * m$S22), ni(m$S12^2))),
           ni(m$b1)))

source("code/bloco_setup.R"); m <- ctx_firmas()
tab(data.frame(
  Fonte = c("Regressão", "Resíduo", "Total"),
  `Soma de quadrados` = paste0("$\\text{", c("ESS", "RSS", "TSS"), "} = ",
                               ni(c(m$ESS, m$RSS, m$TSS)), "$"),
  `g.l.` = paste0("$", c(sprintf("k = %s", ni(m$k)),
                         sprintf("n-k-1 = %s", ni(m$gl)),
                         sprintf("n-1 = %s", ni(m$n - 1))), "$"),
  `Quadrado médio` = c(paste0("$", ni(m$ESS / m$k), "$"),
                       paste0("$S_e^2 = ", ni(m$Se2), "$"), ""),
  check.names = FALSE),
    caption = "Tabela ANOVA do exemplo")

source("code/bloco_setup.R"); m <- ctx_firmas()
eq(sprintf("F = %s = %s = %s.", frac("\\text{ESS}/k", "\\text{RSS}/(n-k-1)"),
           frac(ni(m$ESS / m$k), ni(m$Se2)), ni(m$F)))

source("code/bloco_setup.R"); m <- ctx_firmas(); d <- m$dados
av <- anova(lm(Y ~ L + K, data = d))
tab(data.frame(
  `\texttt{anova(m)}` = paste0("\\texttt{", rownames(av), "}"),
  `\texttt{Df}` = ni(av$Df),
  `\texttt{Sum Sq}` = nm(av$`Sum Sq`, 0),
  `\texttt{Mean Sq}` = nm(av$`Mean Sq`, 0),
  `\texttt{F value}` = c(nm(av$`F value`[1:2], 0), ""),
  check.names = FALSE),
    caption = "Decomposição sequencial que o R produz")

source("code/bloco_setup.R"); m <- ctx_firmas(); d <- m$dados
av <- anova(lm(Y ~ L + K, data = d)); sq <- av$`Sum Sq`
eq(sprintf("%s = %s = \\text{ESS}.",
           paste(nm(sq[1:2], 0), collapse = " + "), ni(m$ESS)))
cat(sprintf("\nE o primeiro número não é arbitrário: $%s$ é exatamente a ESS da regressão \\alert{simples} de $Y$ contra $L$, que dá $S_{LY}^2/S_{LL} = %s^2/%s$.\n",
            nm(sq[1], 0), ni(m$S1y), ni(m$S11)))

source("code/bloco_setup.R"); d <- ctx_firmas()$dados
a1 <- anova(lm(Y ~ L + K, data = d)); a2 <- anova(lm(Y ~ K + L, data = d))
tab(data.frame(
  Linha = c("1ª", "2ª", "Res."),
  `\texttt{lm(Y \textasciitilde{} L + K)}` = sprintf("$%s$: $%s$", rownames(a1), nm(a1$`Sum Sq`, 1)),
  `\texttt{lm(Y \textasciitilde{} K + L)}` = sprintf("$%s$: $%s$", rownames(a2), nm(a2$`Sum Sq`, 1)),
  check.names = FALSE),
    caption = "Somas de quadrados sob duas ordens de entrada")

source("code/bloco_setup.R"); m <- ctx_firmas()
R2s <- (m$S1y^2 / m$S11) / m$TSS
eq(sprintf("R^2 = %s = %s = %s.", frac("\\text{ESS}", "\\text{TSS}"),
           frac(ni(m$ESS), ni(m$TSS)), nm(m$R2, 3)))
cat(sprintf("\nContra $R^2 = %s = %s$ no modelo só com $L$.\n",
            frac(ni(m$S1y^2 / m$S11), ni(m$TSS)), nm(R2s, 3)))

source("code/bloco_setup.R"); m <- ctx_firmas(); d <- m$dados
s1 <- summary(lm(Y ~ L, data = d)); s2 <- summary(lm(Y ~ L + K, data = d))
tab(data.frame(
  Modelo = c("só $L$", "$L$ e $K$"),
  `$R^2$` = paste0("$", nm(c(s1$r.squared, s2$r.squared), 3), "$"),
  `$\\bar R^2$` = paste0("$", nm(c(s1$adj.r.squared, s2$adj.r.squared), 3), "$"),
  check.names = FALSE),
    caption = "$R^2$ e $R^2$ ajustado nos dois modelos")

source("code/bloco_setup.R")
library(wooldridge); data("wage1")
simples  <- lm(log(wage) ~ educ, data = wage1)
multipla <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
co <- summary(multipla)$coefficients
tab(data.frame(
  Termo = rownames(co),
  Estimativa = nm(co[, 1], 4),
  `Erro padrão` = nm(co[, 2], 4),
  `$t$` = nm(co[, 3], 2),
  check.names = FALSE),
    caption = "Modelo múltiplo estimado sobre \\texttt{wage1}")
cat(sprintf("\nCoeficiente de \\texttt{educ}: $%s$ no modelo simples, $%s$ no múltiplo. $n = %s$, $\\bar R^2 = %s$.\n",
            nm(coef(simples)[["educ"]], 4), nm(coef(multipla)[["educ"]], 4),
            ni(nobs(multipla)), nm(summary(multipla)$adj.r.squared, 3)))

source("code/bloco_setup.R"); library(ggplot2)
library(wooldridge); data("wage1")
## Mesmo mecanismo do exemplo ficticio, sobre dados reais: descontar dos dois
## lados o que experiencia e tempo de casa explicam, e olhar o que sobra.
rx <- residuals(lm(educ ~ exper + tenure, data = wage1))
ry <- residuals(lm(log(wage) ~ exper + tenure, data = wage1))
p <- ggplot(data.frame(rx, ry), aes(rx, ry)) +
  geom_point(alpha = 0.25, size = 1.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey25", linewidth = 0.8) +
  labs(x = "Escolaridade, descontada experiência e tempo de casa",
       y = "Log do salário, idem",
       caption = "A inclinação da curva ajustada é o coeficiente de \\texttt{educ} no modelo múltiplo.") +
  theme_minimal(base_size = 9) +
  theme(plot.caption = element_text(size = 6))
fig_salva("mincer_parcial_wage1.pdf", p, largura = 5.2, altura = 1.80,
          alt = "Dispersão dos resíduos de escolaridade contra os resíduos do log do salário, com a curva ajustada.")

source("code/bloco_setup.R"); d <- dados("firmas_exercicio.csv")
tab_serie("$L_i$" = d$L, "$K_i$" = d$K, "$Y_i$" = d$Y,
    caption = "Dados do exercício")

source("code/bloco_setup.R"); e <- ctx_firmas_ex()
cat(sprintf("Médias: $\\bar L = %s$, $\\bar K = %s$, $\\bar Y = %s$.\n",
            ni(e$mx1), ni(e$mx2), ni(e$my)))
eq(sprintf("S_{LL} = %s, \\quad S_{KK} = %s, \\quad S_{LK} = %s, \\quad S_{LY} = %s, \\quad S_{KY} = %s.",
           ni(e$S11), ni(e$S22), ni(e$S12), ni(e$S1y), ni(e$S2y)))
cat(sprintf("\n\\alert{Item 1.} $\\hat\\beta_1^{\\text{simples}} = S_{LY}/S_{LL} = %s$. O trabalho parece não ter efeito nenhum.\n",
            ni(e$b1s)))
cat(sprintf("\n\\alert{Item 3.} $\\det = %s(%s) - (%s)^2 = %s$.\n",
            ni(e$S11), ni(e$S22), ni(e$S12), ni(e$det)))
eq(sprintf("\\hat\\beta_1 = %s = %s, \\qquad \\hat\\beta_2 = %s = %s, \\qquad \\hat\\beta_0 = %s.",
           frac(sprintf("%s(%s) - (%s)(%s)", ni(e$S22), ni(e$S1y), ni(e$S12), ni(e$S2y)), ni(e$det)), ni(e$b1),
           frac(sprintf("%s(%s) - (%s)(%s)", ni(e$S11), ni(e$S2y), ni(e$S12), ni(e$S1y)), ni(e$det)), ni(e$b2),
           ni(e$b0)))
cat(sprintf("\n\\alert{Item 4.} $\\delta = S_{LK}/S_{LL} = %s$, logo o viés é $\\beta_2\\delta = %s(%s) = %s$, e $%s %s = %s$.\n",
            nm(e$delta, 1), ni(e$b2), nm(e$delta, 1), ni(e$b2 * e$delta),
            ni(e$b1), ns(e$b2 * e$delta, 0), ni(e$b1s)))

source("code/bloco_setup.R"); m <- ctx_firmas()
cat(sprintf("- $\\hat\\beta_2 = %s$ é grande, ou é ruído amostral?\n", ni(m$b2)))
cat("- Qual o erro padrão de um coeficiente parcial?\n")
cat(sprintf("- O $F = %s$ rejeita o quê, exatamente?\n", ni(m$F)))
