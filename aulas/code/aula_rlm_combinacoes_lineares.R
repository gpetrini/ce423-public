source("code/bloco_setup.R"); m <- ctx_firmas()
dec <- function(p) if (p < m$alpha) "rejeita" else "não rejeita"
cat(sprintf("$\\hat Y = %s %s L %s K$\n\n", ni(m$b0), ns(m$b1, 0), ns(m$b2, 0)))
tab(data.frame(
  `Hipótese` = c("$H_0: \\beta_1 = 0$", "$H_0: \\beta_2 = 0$", "$H_0: \\beta_1 = \\beta_2 = 0$"),
  `Estatística` = paste0("$", c(sprintf("t = %s", nm(m$t1, 2)), sprintf("t = %s", nm(m$t2, 2)),
                                sprintf("F = %s", ni(m$F))), "$"),
  `Decisão` = sapply(c(m$p1, m$p2, m$pF), dec), check.names = FALSE),
    caption = "Estimativas e $p$-valores do exemplo")

source("code/bloco_setup.R"); m <- ctx_firmas()
cat(sprintf("Para o modelo das cinco firmas, com $S_e^2 = %s$ e $\\det = %s$:\n", ni(m$Se2), ni(m$det)))
eq(sprintf("\\Var(\\hat\\beta_1) = %s = %s = %s, \\qquad \\Cov(\\hat\\beta_1,\\hat\\beta_2) = -%s = -%s = %s.",
           frac("S_e^2 S_{KK}", "\\det"), frac(ni(m$Se2 * m$S22), ni(m$det)), nm(m$V[1,1], 3),
           frac("S_e^2 S_{LK}", "\\det"), frac(ni(m$Se2 * m$S12), ni(m$det)), nm(m$cov12, 3)))

source("code/bloco_setup.R"); m <- ctx_firmas()
tab(data.frame(` ` = c("$\\hat\\beta_1$", "$\\hat\\beta_2$"),
               `$\\hat\\beta_1$` = paste0("$", nm(m$V[, 1], 3), "$"),
               `$\\hat\\beta_2$` = paste0("$", nm(m$V[, 2], 3), "$"), check.names = FALSE),
    caption = "Matriz de covariância dos estimadores")

source("code/bloco_setup.R"); m <- ctx_firmas()
cs <- combinacao(m, c(1, 1)); cd <- combinacao(m, c(1, -1))
eqs(sprintf("\\Var(\\hat\\beta_1 + \\hat\\beta_2) &= %s + %s + 2(%s) = %s & S_{\\hat\\theta} &= %s",
            nm(m$V[1,1], 3), nm(m$V[2,2], 3), nm(m$cov12, 3), nm(cs$var, 3), nm(cs$S, 3)),
    sprintf("\\Var(\\hat\\beta_1 - \\hat\\beta_2) &= %s + %s - 2(%s) = %s & S_{\\hat\\theta} &= %s",
            nm(m$V[1,1], 3), nm(m$V[2,2], 3), nm(m$cov12, 3), nm(cd$var, 3), nm(cd$S, 3)))
cat(sprintf("\nOs mesmos dois estimadores, os mesmos dados. A soma tem erro padrão $%s$; a diferença, $%s$ --- cerca de %s vezes maior.\n",
            nm(cs$S, 3), nm(cd$S, 3), nm(cd$S / cs$S, 0)))

source("code/bloco_setup.R"); m <- ctx_firmas(); cs <- combinacao(m, c(1, 1))
eq(sprintf("\\underbrace{S_{\\hat\\beta_1} = %s}_{\\text{um coeficiente}} \\qquad > \\qquad \\underbrace{S_{\\hat\\beta_1 + \\hat\\beta_2} = %s}_{\\text{a soma dos dois}}",
           nm(m$Sb1, 3), nm(cs$S, 3)))

source("code/bloco_setup.R"); m <- ctx_firmas(); cd <- combinacao(m, c(1, -1))
cat("Testando $H_0: \\beta_1 = \\beta_2$, isto é $\\lambda = (1,-1)$ e $c = 0$:\n")
eq(sprintf("\\hat\\theta = %s - %s = %s, \\qquad t = %s = %s, \\qquad |t| %s %s.",
           ni(m$b1), ni(m$b2), ni(cd$theta),
           frac(ni(cd$theta), nm(cd$S, 3)), nm(cd$t, 2),
           if (abs(cd$t) < m$tc) "<" else ">", nm(m$tc, 2)))

source("code/bloco_setup.R"); m <- ctx_firmas(); c0 <- 6; cs <- combinacao(m, c(1, 1), c0)
cat(sprintf("Testando $H_0: \\beta_1 + \\beta_2 = %s$, com $\\lambda = (1,1)$:\n", ni(c0)))
eq(sprintf("\\hat\\theta = %s + %s = %s, \\qquad t = %s = %s, \\qquad |t| %s %s.",
           ni(m$b1), ni(m$b2), ni(cs$theta),
           frac(sprintf("%s - %s", ni(cs$theta), ni(c0)), nm(cs$S, 3)), nm(cs$t, 2),
           if (abs(cs$t) < m$tc) "<" else ">", nm(m$tc, 2)))

source("code/bloco_setup.R"); m <- ctx_firmas(); d <- m$dados
RSSr <- sum(resid(lm(Y ~ I(L + K), data = d))^2)
tab(data.frame(Modelo = c("Irrestrito", "Restrito"),
               RSS = paste0("$", ni(c(m$RSS, RSSr)), "$"),
               `Parâmetros` = paste0("$", ni(c(m$k + 1, m$k)), "$"), check.names = FALSE),
    caption = "RSS do modelo irrestrito e do restrito")
cat(sprintf("\nA restrição custou $%s - %s = %s$ em soma de quadrados dos resíduos.\n",
            ni(RSSr), ni(m$RSS), ni(RSSr - m$RSS)))

source("code/bloco_setup.R"); m <- ctx_firmas(); d <- m$dados
RSSr <- sum(resid(lm(Y ~ I(L + K), data = d))^2)
tf <- teste_F(RSSr, m$RSS, 1, m$gl, m$alpha)
cat(sprintf("No exemplo, $q = %s$:\n", ni(tf$q)))
eq(sprintf("F = %s = %s = %s.",
           frac(sprintf("(%s-%s)/%s", ni(RSSr), ni(m$RSS), ni(tf$q)),
                sprintf("%s/%s", ni(m$RSS), ni(m$gl))),
           frac(ni(RSSr - m$RSS), ni(m$Se2)), ni(tf$F)))
cat(sprintf("\nSob $H_0$, $F$ segue distribuição $F(q,\\ n-k-1)$ --- aqui, $F(%s,%s)$, cujo valor crítico a $%s\\%%$ é $%s$. Não se rejeita.\n",
            ni(tf$q), ni(tf$gl), ni(100 * m$alpha), nm(tf$Fc, 2)))

source("code/bloco_setup.R"); m <- ctx_firmas()
tab(data.frame(`Grau de liberdade` = c("Numerador, $q$", "Denominador, $n-k-1$"),
               `O que conta` = c("quantas restrições foram impostas",
                                 "quanta informação sobra no modelo livre"),
               `No exemplo` = paste0("$", ni(c(1, m$gl)), "$"), check.names = FALSE),
    caption = "Graus de liberdade do teste $F$")

source("code/bloco_setup.R"); m <- ctx_firmas()
d1 <- m$k; d2 <- m$gl; Fc <- qf(1 - m$alpha, d1, d2); lim <- round(1.6 * Fc, 1)
## densidade F(d1,d2) em forma fechada, avaliada ponto a ponto pelo pgfplots
cte <- signif(gamma((d1 + d2) / 2) / (gamma(d1 / 2) * gamma(d2 / 2)) *
              (d1 / d2)^(d1 / 2), 6)
dens <- sprintf("%s*x^(%s)*(1+%s*x)^(%s)", cte, d1 / 2 - 1,
                signif(d1 / d2, 6), -(d1 + d2) / 2)
cat("\\centering\n\\begin{tikzpicture}\n")
cat(sprintf("\\begin{axis}[width=0.86\\textwidth, height=4.4cm, domain=0.01:%s, samples=200,\n", lim))
cat(sprintf("  axis lines=left, ymin=0, ymax=1.05, xlabel={$F$}, ylabel={},\n  xtick={0,%s}, xticklabels={$0$,$%s$},\n",
            round(Fc, 2), nm(Fc, 1)))
cat("  ytick=\\empty, clip=false, x tick label style={font=\\tiny}]\n")
cat(sprintf("  \\addplot[thick, black] {%s};\n", dens))
cat(sprintf("  \\addplot[draw=none, fill=catcolor, fill opacity=0.28, domain=%s:%s] {%s} \\closedcycle;\n",
            round(Fc, 2), lim, dens))
cat(sprintf("  \\draw[dashed, catcolor] (axis cs:%s,0) -- (axis cs:%s,0.16);\n",
            round(Fc, 2), round(Fc, 2)))
cat("\\end{axis}\n\\end{tikzpicture}\n")

source("code/bloco_setup.R"); m <- ctx_firmas()
cat(sprintf("Densidade $F(%s,%s)$, com o crítico a $%s\\%%$ em $%s$. O $F$ global do modelo, $%s$, cai muito à direita do eixo mostrado.\n",
            ni(m$k), ni(m$gl), ni(100 * m$alpha), nm(m$Fc, 1), ni(m$F)))

source("code/bloco_setup.R"); m <- ctx_firmas()
cd <- combinacao(m, c(1, -1)); cs <- combinacao(m, c(1, 1), 6)
cat(sprintf("Verificando com $H_0: \\beta_1 = \\beta_2$: $t = %s$ e $t^2 = %s = F$. E os críticos também correspondem: $t_{%s}(%s)^2 = %s^2 = %s = F_{%s}(1,%s)$.\n\n",
            nm(cd$t, 2), ni(cd$F), nm(m$alpha / 2, 3), ni(m$gl), nm(m$tc, 2),
            nm(m$tc^2, 2), nm(m$alpha, 2), ni(m$gl)))
cat(sprintf("Testando $H_0: \\beta_1 + \\beta_2 = 6$ pelos dois caminhos: $\\text{RSS}_r = %s$, logo $F = (%s-%s)/(%s/%s) = %s$, e $t^2 = %s^2 = %s$.\n",
            ni(cs$RSSr), ni(cs$RSSr), ni(m$RSS), ni(m$RSS), ni(m$gl), ni(cs$F),
            nm(cs$t, 2), ni(cs$F)))

source("code/bloco_setup.R"); m <- ctx_firmas()
cd <- combinacao(m, c(1, -1)); cs <- combinacao(m, c(1, 1), 6)
tab(data.frame(
  `Hipótese` = c("$\\beta_1 = 0$", "$\\beta_1 = \\beta_2$", "$\\beta_1 + \\beta_2 = 6$",
                 "$\\beta_1 = \\beta_2 = 0$"),
  `$q$` = paste0("$", c(1, 1, 1, m$k), "$"),
  Teste = c("$t$, ou $F$ com $q=1$", "$t$ da combinação, ou $F$", "idem", "$F$"),
  `No exemplo` = paste0("$", c(sprintf("t = %s", nm(m$t1, 2)), sprintf("F = %s", ni(cd$F)),
                               sprintf("F = %s", ni(cs$F)), sprintf("F = %s", ni(m$F))), "$"),
  check.names = FALSE),
    caption = "A família de testes $F$, de $q=1$ a $q=k$")

source("code/bloco_setup.R")
library(car)
d <- ctx_firmas()$dados
irrestrito <- lm(Y ~ L + K, data = d)
lh <- linearHypothesis(irrestrito, "L = K")
tab(data.frame(`\texttt{Res.Df}` = ni(lh$Res.Df), `\texttt{RSS}` = ni(lh$RSS),
               `\texttt{Df}` = c("", ni(lh$Df[2])), `\texttt{Sum of Sq}` = c("", ni(lh$`Sum of Sq`[2])),
               `\texttt{F}` = c("", nm(lh$F[2], 2)), `\texttt{Pr(>F)}` = c("", nm(lh$`Pr(>F)`[2], 3)),
               check.names = FALSE),
    caption = "Resultado de \\texttt{linearHypothesis()}")

source("code/bloco_setup.R")
d <- ctx_firmas()$dados
irrestrito <- lm(Y ~ L + K, data = d)
restrito   <- lm(Y ~ I(L + K), data = d)
RSS_u <- sum(resid(irrestrito)^2); RSS_r <- sum(resid(restrito)^2)
gl <- df.residual(irrestrito)
tab(data.frame(`$\\text{RSS}_u$` = ni(RSS_u), `$\\text{RSS}_r$` = ni(RSS_r),
               `$q$` = "1", `$n-k-1$` = ni(gl),
               `$F$` = ni(((RSS_r - RSS_u) / 1) / (RSS_u / gl)), check.names = FALSE),
    caption = "A mesma conta feita à mão")

source("code/bloco_setup.R"); library(ggplot2)
d <- ctx_firmas()$dados
irrestrito <- lm(Y ~ L + K, data = d)
restrito   <- lm(Y ~ I(L + K), data = d)
g <- data.frame(modelo = factor(c("Irrestrito", "Restrito"), levels = c("Restrito", "Irrestrito")),
                rss = c(sum(resid(irrestrito)^2), sum(resid(restrito)^2)))
p <- ggplot(g, aes(rss, modelo)) +
  geom_col(width = 0.45, fill = "grey70") +
  ## O rotulo passa pelo nm(), como todo numero do deck: virgula decimal, e
  ## dentro de $...$ porque quem compoe o texto agora e o LaTeX.
  geom_text(aes(label = sprintf("$%s$", nm(rss, 1))), hjust = -0.15, size = 2.6) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.20))) +
  labs(x = "Soma de quadrados dos resíduos", y = NULL,
       caption = "O $F$ mede este acréscimo, em unidades da variância residual do modelo irrestrito.") +
  theme_minimal(base_size = 9) +
  theme(plot.caption = element_text(size = 6))
fig_salva("custo_da_restricao_rss.pdf", p, largura = 5.2, altura = 1.35,
          alt = "Duas barras horizontais comparando a soma de quadrados dos resíduos do modelo restrito e do irrestrito.")

source("code/bloco_setup.R")
library(wooldridge); data("wage1"); library(car)
modelo <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
lh <- linearHypothesis(modelo, "exper = tenure")
b <- coef(modelo); V <- vcov(modelo)
theta <- b[["exper"]] - b[["tenure"]]
S <- sqrt(V["exper", "exper"] + V["tenure", "tenure"] - 2 * V["exper", "tenure"])
eq(sprintf("H_0: \\beta_{\\text{exper}} - \\beta_{\\text{tenure}} = 0, \\qquad \\hat\\theta = %s, \\qquad S_{\\hat\\theta} = %s.",
           nm(theta, 4), nm(S, 4)))
tab(data.frame(`$\\hat\\theta$` = nm(theta, 4), `$t$` = nm(theta / S, 2),
               `$F$` = nm(lh$F[2], 2), `$p$` = nm(lh$`Pr(>F)`[2], 4),
               check.names = FALSE),
    caption = "Teste de $\\beta_{\\text{exper}} = \\beta_{\\text{tenure}}$ sobre \\texttt{wage1}")

source("code/bloco_setup.R"); library(ggplot2)
library(wooldridge); data("wage1")
modelo <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
b <- coef(modelo); V <- vcov(modelo); gl <- df.residual(modelo)
theta <- b[["exper"]] - b[["tenure"]]
S <- sqrt(V["exper", "exper"] + V["tenure", "tenure"] - 2 * V["exper", "tenure"])
tc <- qt(0.975, gl)
## O sinal de menos vai como matematica. O caractere U+2212, que estava aqui,
## nao existe na fonte de composicao do tikz e sairia vazio ou como caixa.
g <- rbind(
  data.frame(rotulo = "\\texttt{exper}", est = b[["exper"]],  S = sqrt(V["exper", "exper"])),
  data.frame(rotulo = "\\texttt{tenure}", est = b[["tenure"]], S = sqrt(V["tenure", "tenure"])),
  data.frame(rotulo = "\\texttt{exper} $-$ \\texttt{tenure}", est = theta, S = S))
g$rotulo <- factor(g$rotulo, levels = rev(g$rotulo))
p <- ggplot(g, aes(est, rotulo)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = est - tc * S, xmax = est + tc * S), height = 0.12, linewidth = 0.7) +
  geom_point(size = 2.8) +
  labs(x = "Estimativa e intervalo de confiança a 95\\%", y = NULL,
       caption = "A terceira linha é a combinação. Ela tem erro padrão próprio, que não é a soma dos outros dois.") +
  theme_minimal(base_size = 9) +
  theme(plot.caption = element_text(size = 6))
fig_salva("combinacao_exper_tenure.pdf", p, largura = 5.2, altura = 1.15,
          alt = "Intervalos de confiança de exper, de tenure e da diferença entre os dois, com uma linha tracejada no zero.")

source("code/bloco_setup.R"); cb <- ctx_cobb()
cat(sprintf("Uma função de produção Cobb--Douglas foi estimada em log-log com $n = %s$ firmas e %s regressores ($\\log L$ e $\\log K$). Obteve-se:\n\n",
            ni(cb$n), ni(cb$k)))
tab(data.frame(Modelo = c("Irrestrito", "Restrito a $\\beta_1 + \\beta_2 = 1$"),
               RSS = paste0("$", nm(c(cb$rss_irrestrito, cb$rss_restrito), 2), "$"),
               check.names = FALSE),
    caption = "Dados do exercício")

source("code/bloco_setup.R"); cb <- ctx_cobb()
cat(sprintf("\\alert{Item 1.} $q = %s$ restrição. Graus de liberdade: $%s$ no numerador e $n-k-1 = %s-%s-1 = %s$ no denominador.\n",
            ni(cb$q), ni(cb$q), ni(cb$n), ni(cb$k), ni(cb$gl)))
cat("\n\\alert{Item 2.}\n")
eq(sprintf("F = %s = %s = %s.",
           frac(sprintf("(%s - %s)/%s", nm(cb$rss_restrito, 2), nm(cb$rss_irrestrito, 2), ni(cb$q)),
                sprintf("%s/%s", nm(cb$rss_irrestrito, 2), ni(cb$gl))),
           frac(nm(cb$rss_restrito - cb$rss_irrestrito, 2), nm(cb$rss_irrestrito / cb$gl, 4)),
           nm(cb$F, 2)))
cat(sprintf("\n\\alert{Item 3.} $F_{0{,}05}(%s,%s) = %s$, e $%s %s %s$: %s $H_0$ a $5\\%%$ ($p = %s$).\n",
            ni(cb$q), ni(cb$gl), nm(cb$Fc, 2), nm(cb$F, 2),
            if (cb$F > cb$Fc) ">" else "<", nm(cb$Fc, 2),
            if (cb$F > cb$Fc) "rejeita-se" else "não se rejeita", nm(cb$p, 4)))
cat("\n\\alert{Item 4.} Os dados não são compatíveis com retornos constantes de escala nesta amostra de firmas.\n")

source("code/bloco_setup.R"); e <- ctx_enade(); b <- e$b
cat(sprintf("Estimou-se, com $n = %s$ trabalhadores, o modelo\n", ni(e$n)))
eq(sprintf("\\log(S) = %s %s\\,G %s\\,E %s\\,X %s\\,(E \\times G),",
           nm(b[["intercepto"]], 1), ns(b[["G"]], 2), ns(b[["E"]], 2),
           ns(b[["X"]], 2), ns(b[["ExG"]], 3)))
cat("\nem que $S$ é o salário-hora, $G$ é uma binária igual a $1$ para mulheres, $E$ são anos de estudo e $X$ anos de experiência.\n\nAssinale a alternativa correta sobre o diferencial salarial associado a $G$:\n\n")
alt <- c(sprintf("O diferencial depende da escolaridade, e para quem tem %s anos de estudo é de aproximadamente $%s\\%%$.",
                 ni(e$E), nm(100 * e$efeito, 0)),
         sprintf("O diferencial é constante e igual a $%s\\%%$ para toda a amostra.", nm(100 * b[["G"]], 0)),
         sprintf("O diferencial é $%s\\%%$, valor pequeno e economicamente irrelevante.", nm(b[["G"]], 2)),
         "O termo de interação indica que o diferencial desaparece com a escolaridade.",
         "Não é possível avaliar o diferencial sem conhecer os erros padrão.")
cat("\\begin{enumerate}\n")
cat(sprintf("  \\item[(%s)] %s\n", LETTERS[seq_along(alt)], alt), sep = "")
cat("\\end{enumerate}\n")

source("code/bloco_setup.R"); e <- ctx_enade(); b <- e$b
eq(sprintf("\\frac{\\partial \\log S}{\\partial G} = %s %s\\,E \\qquad\\Longrightarrow\\qquad E = %s:\\ %s",
           nm(b[["G"]], 2), ns(b[["ExG"]], 3), ni(e$E),
           cx(sprintf("%s %s = %s", nm(b[["G"]], 2), ns(b[["ExG"]] * e$E, 2), nm(e$efeito, 2)))))

source("code/bloco_setup.R"); e <- ctx_enade(); b <- e$b
tab(data.frame(Alternativa = c("(B)", "(C)", "(D)", "(E)"),
  `Por que está errada` = c(
    "Ignora a interação; o diferencial seria constante só se $\\beta_{EG} = 0$.",
    sprintf("Confunde $%s$ com porcentagem; em modelo log-lin o coeficiente é semi-elasticidade.", nm(b[["G"]], 2)),
    sprintf("O sinal de $\\beta_{EG}$ é %s: o diferencial \\alert{aumenta} em módulo com a escolaridade.",
            if (b[["ExG"]] < 0) "negativo" else "positivo"),
    "O erro padrão é necessário para \\alert{testar}, não para \\alert{avaliar a magnitude}."),
  check.names = FALSE),
    caption = "Gabarito do Exercício 2")
