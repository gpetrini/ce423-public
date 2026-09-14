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
m <- ctx_firmas()
kL <- contribuicao(m, "x1")   # K depois de L
lK <- contribuicao(m, "x2")   # L depois de K
p  <- parcial(m)
dec <- function(pv) if (pv < m$alpha) "rejeita $H_0$" else "não rejeita $H_0$"
source("code/bloco_setup.R")
library(wooldridge); data("wage1")
irr <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
res <- lm(log(wage) ~ educ, data = wage1)
av  <- anova(irr)
source("code/bloco_setup.R"); e <- ctx_firmas_ex(); d <- e$dados
tab(data.frame(
  `Pergunta` = c("Um coeficiente é nulo?", "Todos são nulos?",
                 "Uma combinação vale $c$?"),
  `Instrumento` = c("$t$ de um coeficiente", "$F$ global",
                    "$t$ da combinação, ou $F$ restrito"),
  `Aula` = c("Teste de hipóteses", "Estimação e ANOVA", "Combinações lineares"), check.names = FALSE),
    caption = "O que o bloco de RLM já respondeu", tamanho = "small")
cat(sprintf("Vinte firmas com função de produção Cobb--Douglas, estimada em logaritmos:\n\n"))
eq(sprintf("\\widehat{\\log Y_i} = %s %s\\,\\log L_i %s\\,\\log K_i", nm(m$b0, 2), ns(m$b1, 2), ns(m$b2, 2)))
cat(sprintf("com $n = %s$, $R^2 = %s$ e $S_e^2 = %s$ sobre $%s$ graus de liberdade.\n",
            ni(m$n), nm(m$R2, 4), nm(m$Se2, 4), ni(m$gl)))
eq(sprintf("\\underbrace{%s}_{\\text{TSS}} = \\underbrace{%s}_{\\text{ESS}} + \\underbrace{%s}_{\\text{RSS}}",
           nm(m$TSS, 2), nm(m$ESS, 2), nm(m$RSS, 2)))
tab(data.frame(
  `Fonte` = c("Regressão restrita", "Contribuição", "Regressão irrestrita"),
  `Modelo` = c("$Y = \\beta_0 + \\beta_1 L + u$", "---",
               "$Y = \\beta_0 + \\beta_1 L + \\beta_2 K + u$"),
  `g.l.` = c("$k - q = 1$", "$q = 1$", "$k = 2$"), check.names = FALSE),
    caption = "Os graus de liberdade se decompõem como as somas de quadrados",
    tamanho = "small")
cat(sprintf("No exemplo, os dois lados valem $%s$:\n\n", nm(kL$cont, 2)))
eqs(sprintf("\\text{ESS}_{ur} - \\text{ESS}_{r} &= %s - %s = %s",
            nm(kL$ESSu, 2), nm(kL$ESSr, 2), nm(kL$cont, 2)),
    sprintf("\\text{RSS}_{r} - \\text{RSS}_{ur} &= %s - %s = %s",
            nm(kL$RSSr, 2), nm(kL$RSSu, 2), nm(kL$cont, 2)))
tab(data.frame(
  `Modelo` = c("$Y \\sim L$ (restrito)", "$Y \\sim L + K$ (irrestrito)", "Acréscimo"),
  `ESS` = nm(c(kL$ESSr, kL$ESSu, kL$cont), 2),
  `RSS` = c(nm(kL$RSSr, 2), nm(kL$RSSu, 2), sprintf("$-%s$", nm(kL$cont, 2))),
  `$R^2$` = c(nm(kL$R2r, 4), nm(kL$R2u, 4), sprintf("$+%s$", nm(kL$R2u - kL$R2r, 4))),
  check.names = FALSE),
    caption = "Contribuição marginal do capital, dado o trabalho", tamanho = "small")
tab(data.frame(
  `Ordem` = c("$L$, depois $K$", "$K$, depois $L$"),
  `Primeiro` = nm(c(kL$ESSr, lK$ESSr), 2),
  `Contribuição` = nm(c(kL$cont, lK$cont), 2),
  `Soma` = nm(c(kL$ESSr + kL$cont, lK$ESSr + lK$cont), 2), check.names = FALSE),
    caption = "Decomposição sequencial da ESS sob as duas ordens", tamanho = "small")
cat(sprintf(paste("O trabalho e o capital estão correlacionados a $r_{LK} = %s$.",
                  "A parte da variação de $Y$ que ambos poderiam explicar é atribuída",
                  "*por inteiro* a quem entra primeiro.\n"), nm(p$r12, 4)))
library(ggplot2)
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
  # Rotulo curto de proposito: o rotulo rotacionado e cortado dentro do
  # tikzpicture quando a figura e baixa (CLAUDE.md 5.27).
  labs(x = NULL, y = "Soma de quadrados") +
  expand_limits(y = 0) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())
fig_salva("contrib_ordem.pdf", p1, largura = 5.0, altura = 1.9,
          alt = "Duas barras verticais comparando a contribuicao marginal do capital dado o trabalho com a do trabalho dado o capital, de alturas muito diferentes.")
tab(data.frame(
  `Fonte` = c("Regressão em $L$", "Contribuição de $K$", "Regressão em $L, K$",
              "Resíduos", "Total"),
  `g.l.` = c("1", "1", "2", ni(m$gl), ni(m$n - 1)),
  `SQ` = nm(c(kL$ESSr, kL$cont, kL$ESSu, kL$RSSu, m$TSS), 2),
  `QM` = c(nm(kL$ESSr, 2), nm(kL$cont, 2), nm(kL$ESSu / 2, 2), nm(m$Se2, 2), ""),
  check.names = FALSE),
    caption = "ANOVA da contribuição marginal do capital", tamanho = "small")
library(ggplot2)
# Dominio comeca DEPOIS do joelho (CLAUDE.md 5.20): a densidade diverge em
# zero, e com ela na figura a cauda de rejeicao fica indistinguivel do eixo.
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
fig_salva("contrib_densidade_F.pdf", p1, largura = 5.0, altura = 1.7,
          alt = "Densidade F com um e dezessete graus de liberdade a partir de 1,2, com a regiao de rejeicao sombreada a direita e a estatistica observada dentro dela.")
eq(sprintf("F = %s = %s = %s",
           frac(sprintf("(%s - %s)/1", nm(kL$ESSu, 2), nm(kL$ESSr, 2)),
                sprintf("%s/%s", nm(kL$RSSu, 2), ni(m$gl))),
           frac(nm(kL$cont, 2), nm(m$Se2, 4)), nm(kL$F, 4)))
cat(sprintf("Com $F_{0{,}05}(1, %s) = %s$ e \\(p\\)-valor de $%s$, %s: a contribuição do capital %s.\n",
            ni(m$gl), nm(kL$Fc, 4), nm(kL$p, 4), dec(kL$p), if (kL$p < m$alpha) "é significativa" else "não é significativa"))
cat(sprintf(paste("Invertendo a ordem, a contribuição de $L$ *depois* de $K$ vale $%s$,",
                  "e produz $F = %s$ com \\(p\\)-valor de $%s$: %s.\n\n"),
            nm(lK$cont, 2), nm(lK$F, 4), nm(lK$p, 4), dec(lK$p)))
tab(data.frame(
  `Contribuição` = c("$K$ depois de $L$", "$L$ depois de $K$"),
  `SQ` = nm(c(kL$cont, lK$cont), 2),
  `$F$` = nm(c(kL$F, lK$F), 4),
  `\\(p\\)` = nm(c(kL$p, lK$p), 4),
  `Decisão` = c(dec(kL$p), dec(lK$p)), check.names = FALSE),
    caption = "As duas contribuições marginais, cada uma dada a outra",
    tamanho = "small")
eqs(sprintf("F(K \\mid L) &= %s = %s = t(\\hat\\beta_2)^2", nm(kL$F, 4), nm(m$t2^2, 4)),
    sprintf("F(L \\mid K) &= %s = %s = t(\\hat\\beta_1)^2", nm(lK$F, 4), nm(m$t1^2, 4)))
cat(sprintf(paste("Aí $\\text{ESS}_r = 0$, a contribuição é a própria ESS,",
                  "e $F = %s$ com $%s$ e $%s$ graus de liberdade --- o $F$ global da aula de estimação e ANOVA.\n"),
            nm(m$F, 2), ni(m$k), ni(m$gl)))
tab(data.frame(
  `Par` = c("$Y$ e $L$", "$Y$ e $K$", "$L$ e $K$"),
  `Correlação simples` = nm(c(p$rY1, p$rY2, p$r12), 4), check.names = FALSE),
    caption = "Correlações simples entre as três variáveis", tamanho = "small")
cat("\nAs três são altas, e a terceira explica por que as duas primeiras dizem pouco: $L$ e $K$ movem-se juntos.\n")
library(ggplot2)
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
  # O painel da esquerda mostra o produto, que e quantidade (CLAUDE.md 5.20).
  # No da direita sao residuos, que ja cruzam o zero: a chamada nao o altera.
  expand_limits(y = 0) +
  theme_minimal(base_size = 8) +
  theme(panel.grid.minor = element_blank())
fig_salva("contrib_parcial_residuos.pdf", p1, largura = 5.0, altura = 1.7,
          alt = "Dois paineis de dispersao: a esquerda produto contra capital, a direita os residuos de cada um descontado o trabalho, com nuvem mais dispersa.")
cat(sprintf(paste("A correlação simples entre produto e capital, em logaritmos, é $%s$.",
                  "Descontado o trabalho de ambos, ela cai para $%s$.\n"),
            nm(p$rY2, 4), nm(p$rY2_1, 4)))
eq(sprintf("r_{YK.L} = r_{\\hat u_{Y.L},\\,\\hat u_{K.L}} = %s", nm(p$rY2_1, 4)))
cat(sprintf("Na outra direção, $r_{YL} = %s$ cai para $r_{YL.K} = %s$.\n",
            nm(p$rY1, 4), nm(p$rY1_2, 4)))
eq(sprintf("r_{YK.L} = %s = %s",
           frac(sprintf("%s - (%s)(%s)", nm(p$rY2, 4), nm(p$rY1, 4), nm(p$r12, 4)),
                sprintf("\\sqrt{(1 - %s^2)(1 - %s^2)}", nm(p$r12, 4), nm(p$rY1, 4))),
           nm(p$rY2_1, 4)))
eq(sprintf("r^2_{YK.L} = %s = %s = %s",
           frac("\\text{Contribuição de } K", "\\text{RSS}_r"),
           frac(nm(kL$cont, 2), nm(kL$RSSr, 2)), nm(kL$r2parcial, 4)))
cat(sprintf("E, pela correlação parcial, $r_{YK.L}^2 = (%s)^2 = %s$.\n",
            nm(p$rY2_1, 4), nm(p$r2Y2_1, 4)))
eq(sprintf("r^2_{YK.L} = %s = %s",
           frac(sprintf("%s - %s", nm(m$R2, 4), nm(p$rY1^2, 4)),
                sprintf("1 - %s", nm(p$rY1^2, 4))), nm(p$via_R2_2, 4)))
eq(sprintf("r^2_{YK.L} = %s = %s",
           frac(sprintf("%s", nm(m$t2^2, 4)),
                sprintf("%s + %s", nm(m$t2^2, 4), ni(m$gl))), nm(p$via_t2, 4)))
cat("Três caminhos, um número. A escolha entre eles é de conveniência de cálculo.\n")
tab(data.frame(
  `Linha` = c("\\texttt{L}", "\\texttt{K}", "\\texttt{Residuals}"),
  `O que é` = c("ESS de $L$ sozinho",
                "contribuição de $K$ depois de $L$",
                "RSS do modelo irrestrito"),
  `SQ` = nm(c(kL$ESSr, kL$cont, kL$RSSu), 2),
  `$F$` = c(nm(kL$ESSr / m$Se2, 2), nm(kL$F, 2), ""), check.names = FALSE),
    caption = "A tabela sequencial que \\texttt{anova()} devolve", tamanho = "small")
tab(data.frame(
  `Fonte` = c("\\texttt{educ}", "\\texttt{exper}", "\\texttt{tenure}", "Resíduos"),
  `g.l.` = ni(av$Df),
  `SQ` = nm(av$`Sum Sq`, 3),
  `$F$` = c(nm(av$`F value`[1:3], 2), ""), check.names = FALSE),
    caption = "Decomposição sequencial em \\texttt{wage1}", tamanho = "small")
tt <- summary(irr)$coefficients["tenure", "t value"]
gl <- df.residual(irr)
cat(sprintf(paste("O tempo de emprego entra por último, então a sua linha testa a contribuição",
                  "dado escolaridade e experiência: $F = %s$, com \\(p\\)-valor abaixo de $0{,}001$.\n\n"),
            nm(av$`F value`[3], 2)))
eq(sprintf("t(\\hat\\beta_3)^2 = %s^2 = %s = F", nm(tt, 4), nm(tt^2, 2)))
cat(sprintf("A determinação parcial vale $%s$: o tempo de emprego explica $%s\\%%$ do que escolaridade e experiência ainda não explicavam.\n",
            nm(tt^2 / (tt^2 + gl), 4), nm(100 * tt^2 / (tt^2 + gl), 1)))
av2 <- anova(lm(log(wage) ~ tenure + exper + educ, data = wage1))
tab(data.frame(
  `Ordem da fórmula` = c("\\texttt{educ + exper + tenure}", "\\texttt{tenure + exper + educ}"),
  `SQ de \\texttt{exper}` = nm(c(av$`Sum Sq`[2], av2$`Sum Sq`[2]), 3),
  `$F$` = nm(c(av$`F value`[2], av2$`F value`[2]), 2),
  `\\(p\\)` = nm(c(av$`Pr(>F)`[2], av2$`Pr(>F)`[2]), 4), check.names = FALSE),
    caption = "A mesma variável, em duas posições da fórmula", tamanho = "small")
cat("\nMesmos dados, mesmo modelo irrestrito, mesma variável --- e decisões opostas a $5\\%$.\n")
eY <- residuals(lm(log(wage) ~ educ + exper, data = wage1))
eT <- residuals(lm(tenure ~ educ + exper, data = wage1))
tab(data.frame(
  `Medida` = c("$r$ simples entre $\\log(\\text{salário})$ e \\texttt{tenure}",
               "$r$ parcial, controlando \\texttt{educ} e \\texttt{exper}"),
  `Valor` = nm(c(cor(log(wage1$wage), wage1$tenure), cor(eY, eT)), 4),
  check.names = FALSE),
    caption = "Associação bruta e associação parcial em \\texttt{wage1}",
    tamanho = "small")
d <- e$dados
tab_serie("$L_i$" = as.character(d$L), "$K_i$" = as.character(d$K),
          "$Y_i$" = as.character(d$Y),
          caption = "Base do exercício ($n = 5$)", tamanho = "small")
cat(sprintf("\nO ajuste múltiplo é $\\hat Y = %s + %s L + %s K$, com $\\text{TSS} = %s$ e $\\text{RSS} = %s$.\n",
            ni(e$b0), ni(e$b1), ni(e$b2), ni(e$TSS), ni(e$RSS)))
k <- contribuicao(e, "x1"); q <- parcial(e)
tab(data.frame(
  `Fonte` = c("Regressão em $L$", "Contribuição de $K$", "Resíduos", "Total"),
  `g.l.` = c("1", "1", ni(e$gl), ni(e$n - 1)),
  `SQ` = ni(c(k$ESSr, k$cont, k$RSSu, e$TSS)),
  `QM` = c(ni(k$ESSr), ni(k$cont), ni(e$Se2), ""), check.names = FALSE),
    caption = "ANOVA da contribuição marginal do capital", tamanho = "small")
cat(sprintf("\n$F = %s/%s = %s$, contra $F_{0{,}05}(1, %s) = %s$: rejeita-se $H_0$.\n",
            ni(k$cont), ni(e$Se2), ni(k$F), ni(e$gl), nm(k$Fc, 2)))
q <- parcial(e)
cat(sprintf("A correlação simples entre produto e trabalho é $r_{YL} = %s$: nula, e $\\text{ESS}(Y \\mid L) = 0$.\n\n",
            nm(q$rY1, 2)))
eq(sprintf("r_{YL.K} = %s = %s",
           frac(sprintf("%s - (%s)(%s)", nm(q$rY1, 2), nm(q$rY2, 4), nm(q$r12, 2)),
                sprintf("\\sqrt{(1 - %s^2)(1 - %s^2)}", nm(q$r12, 2), nm(q$rY2, 4))),
           nm(q$rY1_2, 4)))
cat(sprintf("Mantido o capital constante, a associação entre produto e trabalho é forte, e o coeficiente $\\hat\\beta_1 = %s$ tem $t = %s$.\n",
            ni(e$b1), nm(e$t1, 2)))
tab(data.frame(
  `Notação` = c("$r_{Y1}$", "$r_{Y1.2}$", "$r_{Y1.23}$"),
  `Ordem` = c("zero", "primeira", "segunda"),
  `Controla` = c("nada", "$X_2$", "$X_2$ e $X_3$"), check.names = FALSE),
    caption = "Ordem de uma correlação parcial", tamanho = "small")
tab(data.frame(
  `Ordem` = c("$L$", "$K \\mid L$", "", "$K$", "$L \\mid K$"),
  `SQ` = c(nm(c(kL$ESSr, kL$cont), 2), "", nm(c(lK$ESSr, lK$cont), 2)),
  `$F$` = c(nm(c(kL$ESSr / m$Se2, kL$F), 2), "", nm(c(lK$ESSr / m$Se2, lK$F), 2)),
  `\\(p\\)` = c(nm(c(pf(kL$ESSr / m$Se2, 1, m$gl, lower.tail = FALSE), kL$p), 4), "",
                nm(c(pf(lK$ESSr / m$Se2, 1, m$gl, lower.tail = FALSE), lK$p), 4)),
  check.names = FALSE),
    caption = "As duas decomposições sequenciais do exemplo das firmas",
    tamanho = "small")
eq(sprintf("\\log Y_i = %s + %s\\,\\log L_i + %s\\,\\log K_i + u_i, \\qquad u_i \\sim \\mathcal{N}(0, %s^2)",
           nm(m$dgp$logA, 1), nm(m$dgp$b1, 1), nm(m$dgp$b2, 1), nm(m$dgp$sigma, 2)))
cat(sprintf(paste("Os dois regressores têm coeficiente não nulo no processo gerador.",
                  "Ainda assim, a contribuição de $K$ dado $L$ %s a $5\\%%$:",
                  "com $r_{LK} = %s$, os dados não separam os dois efeitos.\n"),
            if (kL$p < m$alpha) "é significativa" else "não é significativa", nm(p$r12, 4)))
