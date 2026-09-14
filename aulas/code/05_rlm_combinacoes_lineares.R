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
source("code/bloco_setup.R"); m <- ctx_firmas(); d <- m$dados
library(car)
source("code/bloco_setup.R")
library(wooldridge); data("wage1"); library(car)
modelo <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
source("code/bloco_setup.R")
dcb <- dados("cobb_douglas_rss.csv")
cb <- c(as.list(dcb), teste_F(dcb$rss_restrito, dcb$rss_irrestrito, dcb$q,
                              dcb$n - dcb$k - 1))
source("code/bloco_setup.R")
co <- dados("enade_q31.csv"); meta <- dados("enade_q31_meta.csv")
## `be`, e nao `b`: os frames de wage1 definem `b <- coef(modelo)`, e no script
## tangulado, que roda tudo numa sessao, o nome colidiria (CLAUDE.md 5.31).
be <- setNames(co$coeficiente, co$termo)
e <- list(b = be, n = meta$n, E = meta$escolaridade_avaliada,
          efeito = be[["G"]] + be[["ExG"]] * meta$escolaridade_avaliada)
dec <- function(p) if (p < m$alpha) "rejeita" else "não rejeita"
cat(sprintf("$\\widehat{\\log Y} = %s %s \\log L %s \\log K$\n\n", nm(m$b0, 2), ns(m$b1, 2), ns(m$b2, 2)))
tab(data.frame(
  `Hipótese` = c("$H_0: \\beta_1 = 0$", "$H_0: \\beta_2 = 0$", "$H_0: \\beta_1 = \\beta_2 = 0$"),
  `Estatística` = paste0("$", c(sprintf("t = %s", nm(m$t1, 2)), sprintf("t = %s", nm(m$t2, 2)),
                                sprintf("F = %s", nm(m$F, 1))), "$"),
  `Decisão` = sapply(c(m$p1, m$p2, m$pF), dec), check.names = FALSE),
    caption = "Estimativas e $p$-valores do exemplo")
cat(sprintf("Para o modelo das firmas, com $S_e^2 = %s$ e $\\det = %s$:\n", nm(m$Se2, 4), nm(m$det, 1)))
## Sem a substituicao numerica intermediaria: com o exemplo em n = 20 ela tem
## cinco digitos em cada fracao e a linha transborda 48 pt. Os tres valores que
## entram na conta estao na tabela de somas do frame anterior.
eq(sprintf("\\Var(\\hat\\beta_1) = %s = %s, \\qquad \\Cov(\\hat\\beta_1,\\hat\\beta_2) = -%s = %s.",
           frac("S_e^2 S_{KK}", "\\det"), nm(m$V[1,1], 3),
           frac("S_e^2 S_{LK}", "\\det"), nm(m$cov12, 3)))
tab(data.frame(` ` = c("$\\hat\\beta_1$", "$\\hat\\beta_2$"),
               `$\\hat\\beta_1$` = paste0("$", nm(m$V[, 1], 3), "$"),
               `$\\hat\\beta_2$` = paste0("$", nm(m$V[, 2], 3), "$"), check.names = FALSE),
    caption = "Matriz de covariância dos estimadores", tamanho = "small")
cs <- combinacao(m, c(1, 1)); cd <- combinacao(m, c(1, -1))
eqs(sprintf("\\Var(\\hat\\beta_1 + \\hat\\beta_2) &= %s + %s + 2(%s) = %s & S_{\\hat\\theta} &= %s",
            nm(m$V[1,1], 3), nm(m$V[2,2], 3), nm(m$cov12, 3), nm(cs$var, 3), nm(cs$S, 3)),
    sprintf("\\Var(\\hat\\beta_1 - \\hat\\beta_2) &= %s + %s - 2(%s) = %s & S_{\\hat\\theta} &= %s",
            nm(m$V[1,1], 3), nm(m$V[2,2], 3), nm(m$cov12, 3), nm(cd$var, 3), nm(cd$S, 3)))
cat(sprintf("\nOs mesmos dois estimadores, os mesmos dados. A soma tem erro padrão $%s$; a diferença, $%s$ --- cerca de %s vezes maior.\n",
            nm(cs$S, 3), nm(cd$S, 3), nm(cd$S / cs$S, 0)))
cs <- combinacao(m, c(1, 1))
eq(sprintf("\\underbrace{S_{\\hat\\beta_1} = %s}_{\\text{um coeficiente}} \\qquad > \\qquad \\underbrace{S_{\\hat\\beta_1 + \\hat\\beta_2} = %s}_{\\text{a soma dos dois}}",
           nm(m$Sb1, 3), nm(cs$S, 3)))
cd <- combinacao(m, c(1, -1))
cat("Testando $H_0: \\beta_1 = \\beta_2$, isto é $\\lambda = (1,-1)$ e $c = 0$:\n")
eq(sprintf("\\hat\\theta = %s - %s = %s, \\qquad t = %s = %s, \\qquad |t| %s %s.",
           nm(m$b1, 2), nm(m$b2, 2), nm(cd$theta, 2),
           frac(nm(cd$theta, 2), nm(cd$S, 3)), nm(cd$t, 2),
           if (abs(cd$t) < m$tc) "<" else ">", nm(m$tc, 2)))
c0 <- 1; cs <- combinacao(m, c(1, 1), c0)
cat(sprintf("Testando $H_0: \\beta_1 + \\beta_2 = %s$, retornos constantes de escala, com $\\lambda = (1,1)$:\n", ni(c0)))
eq(sprintf("\\hat\\theta = %s + %s = %s, \\qquad t = %s = %s, \\qquad |t| %s %s.",
           nm(m$b1, 2), nm(m$b2, 2), nm(cs$theta, 2),
           frac(sprintf("%s - %s", nm(cs$theta, 2), ni(c0)), nm(cs$S, 3)), nm(cs$t, 2),
           if (abs(cs$t) < m$tc) "<" else ">", nm(m$tc, 2)))
cd <- combinacao(m, c(1, -1)); cs <- combinacao(m, c(1, 1), 1)
## A decisao e DERIVADA dos numeros, e nao afirmada em prosa: e o que impede a
## frase de ficar falsa quando o exemplo muda -- ver MISTAKES.md 26.
cat(sprintf("Nenhuma das duas é rejeitada, e por motivos opostos. Na diferença, a discrepância é de $%s$ contra erro padrão de $%s$. Na soma, a discrepância é de $%s$ contra erro padrão de $%s$.\n",
            nm(abs(cd$theta), 2), nm(cd$S, 2),
            nm(abs(cs$theta - 1), 2), nm(cs$S, 2)))
d <- m$dados
RSSr <- sum(resid(lm(log(Y) ~ I(log(L) + log(K)), data = d))^2)
tab(data.frame(Modelo = c("Irrestrito", "Restrito"),
               RSS = paste0("$", nm(c(m$RSS, RSSr), 3), "$"),
               `Parâmetros` = paste0("$", ni(c(m$k + 1, m$k)), "$"), check.names = FALSE),
    caption = "RSS do modelo irrestrito e do restrito")
cat(sprintf("\nA restrição custou $%s - %s = %s$ em soma de quadrados dos resíduos.\n",
            nm(RSSr, 3), nm(m$RSS, 3), nm(RSSr - m$RSS, 3)))
d <- m$dados
RSSr <- sum(resid(lm(log(Y) ~ I(log(L) + log(K)), data = d))^2)
tf <- teste_F(RSSr, m$RSS, 1, m$gl, m$alpha)
cat(sprintf("No exemplo, $q = %s$:\n", ni(tf$q)))
eq(sprintf("F = %s = %s = %s.",
           frac(sprintf("(%s-%s)/%s", nm(RSSr, 3), nm(m$RSS, 3), ni(tf$q)),
                sprintf("%s/%s", nm(m$RSS, 3), ni(m$gl))),
           frac(nm(RSSr - m$RSS, 3), nm(m$Se2, 4)), nm(tf$F, 2)))
cat(sprintf("\nSob $H_0$, $F$ segue distribuição $F(q,\\ n-k-1)$ --- aqui, $F(%s,%s)$, cujo valor crítico a $%s\\%%$ é $%s$. %s.\n",
            ni(tf$q), ni(tf$gl), ni(100 * m$alpha), nm(tf$Fc, 2), if (tf$F > tf$Fc) "Rejeita-se" else "Não se rejeita"))
tab(data.frame(`Grau de liberdade` = c("Numerador, $q$", "Denominador, $n-k-1$"),
               `O que conta` = c("quantas restrições foram impostas",
                                 "quanta informação sobra no modelo livre"),
               `No exemplo` = paste0("$", ni(c(1, m$gl)), "$"), check.names = FALSE),
    caption = "Graus de liberdade do teste $F$")
d1 <- m$k; d2 <- m$gl; Fc <- qf(1 - m$alpha, d1, d2); lim <- round(1.6 * Fc, 1)
## densidade F(d1,d2) em forma fechada, avaliada ponto a ponto pelo pgfplots
cte <- signif(gamma((d1 + d2) / 2) / (gamma(d1 / 2) * gamma(d2 / 2)) *
              (d1 / d2)^(d1 / 2), 6)
dens <- sprintf("%s*x^(%s)*(1+%s*x)^(%s)", cte, d1 / 2 - 1,
                signif(d1 / d2, 6), -(d1 + d2) / 2)
cat("\\centering\n\\begin{tikzpicture}\n")
cat(sprintf("\\begin{axis}[width=0.86\\textwidth, height=3.0cm, domain=0.01:%s, samples=200,\n", lim))
cat(sprintf("  axis lines=left, ymin=0, ymax=1.05, xlabel={$F$}, ylabel={},\n  xtick={0,%s}, xticklabels={$0$,$%s$},\n",
            round(Fc, 2), nm(Fc, 1)))
cat("  ytick=\\empty, clip=false, x tick label style={font=\\tiny}]\n")
cat(sprintf("  \\addplot[thick, black] {%s};\n", dens))
cat(sprintf("  \\addplot[draw=none, fill=catcolor, fill opacity=0.28, domain=%s:%s] {%s} \\closedcycle;\n",
            round(Fc, 2), lim, dens))
cat(sprintf("  \\draw[dashed, catcolor] (axis cs:%s,0) -- (axis cs:%s,0.16);\n",
            round(Fc, 2), round(Fc, 2)))
cat("\\end{axis}\n\\end{tikzpicture}\n")
cat(sprintf("Densidade $F(%s,%s)$, com o crítico a $%s\\%%$ em $%s$. O $F$ global do modelo, $%s$, cai muito à direita do eixo mostrado.\n",
            ni(m$k), ni(m$gl), ni(100 * m$alpha), nm(m$Fc, 1), nm(m$F, 1)))
cd <- combinacao(m, c(1, -1)); cs <- combinacao(m, c(1, 1), 1)
cat(sprintf("Verificando com $H_0: \\beta_1 = \\beta_2$: $t = %s$ e $t^2 = %s = F$. E os críticos também correspondem: $t_{%s}(%s)^2 = %s^2 = %s = F_{%s}(1,%s)$.\n\n",
            nm(cd$t, 2), nm(cd$F, 2), nm(m$alpha / 2, 3), ni(m$gl), nm(m$tc, 2),
            nm(m$tc^2, 2), nm(m$alpha, 2), ni(m$gl)))
cat(sprintf("Testando $H_0: \\beta_1 + \\beta_2 = 1$ pelos dois caminhos: $\\text{RSS}_r = %s$, logo $F = (%s-%s)/(%s/%s) = %s$, e $t^2 = %s^2 = %s$.\n",
            nm(cs$RSSr, 3), nm(cs$RSSr, 3), nm(m$RSS, 3), nm(m$RSS, 3), ni(m$gl), nm(cs$F, 2),
            nm(cs$t, 2), nm(cs$F, 2)))
cd <- combinacao(m, c(1, -1)); cs <- combinacao(m, c(1, 1), 1)
tab(data.frame(
  `Hipótese` = c("$\\beta_1 = 0$", "$\\beta_1 = \\beta_2$", "$\\beta_1 + \\beta_2 = 1$",
                 "$\\beta_1 = \\beta_2 = 0$"),
  `$q$` = paste0("$", c(1, 1, 1, m$k), "$"),
  Teste = c("$t$, ou $F$ com $q=1$", "$t$ da combinação, ou $F$", "idem", "$F$"),
  `No exemplo` = paste0("$", c(sprintf("t = %s", nm(m$t1, 2)), sprintf("F = %s", nm(cd$F, 2)),
                               sprintf("F = %s", nm(cs$F, 2)), sprintf("F = %s", nm(m$F, 1))), "$"),
  check.names = FALSE),
    caption = "A família de testes $F$, de $q=1$ a $q=k$")
d <- m$dados
irrestrito <- lm(log(Y) ~ log(L) + log(K), data = d)
lh <- linearHypothesis(irrestrito, "log(L) = log(K)")
tab(data.frame(`\texttt{Res.Df}` = ni(lh$Res.Df), `\texttt{RSS}` = nm(lh$RSS, 3),
               `\texttt{Df}` = c("", ni(lh$Df[2])), `\texttt{Sum of Sq}` = c("", nm(lh$`Sum of Sq`[2], 3)),
               `\texttt{F}` = c("", nm(lh$F[2], 2)), `\texttt{Pr(>F)}` = c("", nm(lh$`Pr(>F)`[2], 3)),
               check.names = FALSE),
    caption = "Resultado de \\texttt{linearHypothesis()}")
d <- m$dados
irrestrito <- lm(log(Y) ~ log(L) + log(K), data = d)
restrito   <- lm(log(Y) ~ I(log(L) + log(K)), data = d)
RSSur <- sum(resid(irrestrito)^2); RSSr <- sum(resid(restrito)^2)
gl <- df.residual(irrestrito)
tab(data.frame(`$\\text{RSS}_{ur}$` = nm(RSSur, 2), `$\\text{RSS}_r$` = nm(RSSr, 2),
               `$q$` = "1", `$n-k-1$` = ni(gl),
               `$F$` = nm(((RSSr - RSSur) / 1) / (RSSur / gl), 2), check.names = FALSE),
    caption = "A mesma conta feita à mão")
library(ggplot2)
d <- m$dados
irrestrito <- lm(log(Y) ~ log(L) + log(K), data = d)
restrito   <- lm(log(Y) ~ I(log(L) + log(K)), data = d)
g <- data.frame(modelo = factor(c("Irrestrito", "Restrito"), levels = c("Restrito", "Irrestrito")),
                rss = c(sum(resid(irrestrito)^2), sum(resid(restrito)^2)))
p <- ggplot(g, aes(rss, modelo)) +
  geom_col(width = 0.45, fill = "grey70") +
  ## O rotulo passa pelo nm(), como todo numero do deck: virgula decimal, e
  ## dentro de $...$ porque quem compoe o texto agora e o LaTeX.
  geom_text(aes(label = sprintf("$%s$", nm(rss, 3))), hjust = -0.15, size = 2.6) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.20))) +
  labs(x = "Soma de quadrados dos resíduos", y = NULL,
       caption = "O $F$ mede este acréscimo, em unidades da variância residual do modelo irrestrito.") +
  theme_minimal(base_size = 9) +
  theme(plot.caption = element_text(size = 6))
fig_salva("custo_da_restricao_rss.pdf", p, largura = 5.2, altura = 1.35,
          alt = "Duas barras horizontais comparando a soma de quadrados dos resíduos do modelo restrito e do irrestrito.")
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
library(ggplot2)
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
cat(sprintf("Uma função de produção Cobb--Douglas foi estimada em log-log com $n = %s$ firmas e %s regressores ($\\log L$ e $\\log K$). Obteve-se:\n\n",
            ni(cb$n), ni(cb$k)))
tab(data.frame(Modelo = c("Irrestrito", "Restrito a $\\beta_1 + \\beta_2 = 1$"),
               RSS = paste0("$", nm(c(cb$rss_irrestrito, cb$rss_restrito), 2), "$"),
               check.names = FALSE),
    caption = "Dados do exercício", tamanho = "small")
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
cat(sprintf("\\alert{Item 4.} %s\n",
            if (cb$F > cb$Fc) "Os dados não são compatíveis com retornos constantes de escala nesta amostra de firmas."
            else "Os dados são compatíveis com retornos constantes de escala nesta amostra de firmas."))
cat(sprintf("Estimou-se, com $n = %s$ trabalhadores, o modelo\n", ni(e$n)))
eq(sprintf("\\log(S) = %s %s\\,G %s\\,E %s\\,X %s\\,(E \\times G),",
           nm(be[["intercepto"]], 1), ns(be[["G"]], 2), ns(be[["E"]], 2),
           ns(be[["X"]], 2), ns(be[["ExG"]], 3)))
cat("\nem que $S$ é o salário-hora, $G$ é uma binária igual a $1$ para mulheres, $E$ são anos de estudo e $X$ anos de experiência.\n\nAssinale a alternativa correta sobre o diferencial salarial associado a $G$.\n")
alt <- c(sprintf("O diferencial depende da escolaridade, e para quem tem %s anos de estudo é de aproximadamente $%s\\%%$.",
                 ni(e$E), nm(100 * e$efeito, 0)),
         sprintf("O diferencial é constante e igual a $%s\\%%$ para toda a amostra.", nm(100 * be[["G"]], 0)),
         sprintf("O diferencial é $%s\\%%$, valor pequeno e economicamente irrelevante.", nm(be[["G"]], 2)),
         "O termo de interação indica que o diferencial desaparece com a escolaridade.",
         "Não é possível avaliar o diferencial sem conhecer os erros padrão.")
cat("\\begin{enumerate}\n")
cat(sprintf("  \\item[(%s)] %s\n", LETTERS[seq_along(alt)], alt), sep = "")
cat("\\end{enumerate}\n")
eq(sprintf("\\frac{\\partial \\log S}{\\partial G} = %s %s\\,E \\qquad\\Longrightarrow\\qquad E = %s:\\ %s",
           nm(be[["G"]], 2), ns(be[["ExG"]], 3), ni(e$E),
           cx(sprintf("%s %s = %s", nm(be[["G"]], 2), ns(be[["ExG"]] * e$E, 2), nm(e$efeito, 2)))))
tab(data.frame(Alternativa = c("(B)", "(C)", "(D)", "(E)"),
  `Por que está errada` = c(
    "Ignora a interação; o diferencial seria constante só se $\\beta_{EG} = 0$.",
    sprintf("Confunde $%s$ com porcentagem; em modelo log-lin o coeficiente é semi-elasticidade.", nm(be[["G"]], 2)),
    sprintf("O sinal de $\\beta_{EG}$ é %s: o diferencial \\alert{aumenta} em módulo com a escolaridade.",
            if (be[["ExG"]] < 0) "negativo" else "positivo"),
    "O erro padrão é necessário para \\alert{testar}, não para \\alert{avaliar a magnitude}."),
  check.names = FALSE),
    caption = "Gabarito do Exercício 2")
