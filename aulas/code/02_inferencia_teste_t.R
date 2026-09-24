# ---------------------------------------------------------------------------
# CE423 - Econometria I
# 1. Regressao Linear Simples - Inferencia sobre os Parametros
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
source("code/bloco_setup.R")
library(wooldridge); library(ggplot2); library(tidypvals)
d <- dados("salarios_inferencia.csv")
a <- rls(d$educ, d$salario); a$dados <- d
a$dgp <- list(b0 = 6, b1 = 2, sigma = 6)
a$t0  <- a$b0 / a$Sb0
a$ic0 <- a$b0 + c(-1, 1) * a$tc * a$Sb0
a$p0  <- 2 * pt(abs(a$t0), a$gl, lower.tail = FALSE)
a$p1  <- 2 * pt(abs(a$t1), a$gl, lower.tail = FALSE)
a$tc_uni <- qt(1 - a$alpha, a$gl)
a$c_econ <- 1.5
a$econ <- teste_t(a$b1, a$Sb1, a$gl, c0 = a$c_econ, alpha = a$alpha)
a$x0 <- 16
a$prev <- previsao(a, a$x0)
# A simulacao acontece UMA vez, aqui: dois frames a exibem (as 60 retas e o
# histograma) e um terceiro cita a media e o desvio. Reamostrar em cada bloco
# convidava valores desencontrados entre os slides.
sim <- amostras_dgp(a, R = 2000)
# Recorte de uma serie longa, para que a tabela caiba no slide. As somas
# exibidas em qualquer frame sao SEMPRE da amostra inteira.
recorte <- function(v, k = 6, f = 2)
  c(as.character(v)[1:k], "$\\cdots$",
    as.character(v)[(length(v) - f + 1):length(v)])
# A curva ajustada, escrita uma vez. Quatro frames a citam.
ajuste <- sprintf("\\hat Y_i = %s %s\\,X_i", nm(a$b0, 2), ns(a$b1, 2))
# O processo gerador, para o frame que o revela.
dgp <- sprintf("Y_i = %s + %s\\,X_i + u_i, \\qquad u_i \\sim \\mathcal{N}(0, %s^2)",
               ni(a$dgp$b0), ni(a$dgp$b1), ni(a$dgp$sigma))
# Cinco observacoes desenhadas para a conta fechar a mao: S_XX = 10, S_XY = 20,
# beta = (1, 2) exatos e residuos (1, -1, 0, -1, 1), de modo que RSS = 4. O
# X_0 = 6 do segundo exercicio fica fora do dominio amostral de proposito.
de <- dados("rls_inferencia_exercicio.csv")
e <- rls(de$X, de$Y); e$dados <- de
e$t0  <- e$b0 / e$Sb0
e$ic0 <- e$b0 + c(-1, 1) * e$tc * e$Sb0
e$tc_uni <- qt(1 - e$alpha, e$gl)
e$econ <- teste_t(e$b1, e$Sb1, e$gl, c0 = 1, alpha = e$alpha)
e$x0 <- 6
e$prev <- previsao(e, e$x0)
data("wage1")
mincer <- lm(log(wage) ~ educ, data = wage1)
d <- a$dados
d <- data.frame(x = a$x, y = a$y)
p <- ggplot(d, aes(x, y)) +
  geom_abline(intercept = a$b0, slope = a$b1, colour = "grey25", linewidth = 0.7) +
  geom_point(size = 1.7) +
  labs(x = "Anos de estudo", y = "Sal\\'ario-hora (R\\$)") +
  expand_limits(y = 0) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank())
# Sessenta das 2000 amostras simuladas no setup. Os X ficam fixos e so o erro e
# sorteado, como (P2) supoe: cada reta cinza e o ajuste de UMA amostra possivel
# do mesmo processo gerador.
#
# Os pontos da amostra observada saem daqui de proposito. Eles pertencem a uma
# so das sessenta amostras, e desenha-los sob as sessenta retas sugeria que
# todas viessem daqueles mesmos pontos -- que e o oposto do que a figura diz.
curvas <- data.frame(b0 = sim$b0[1:60], b1 = sim$b1[1:60])
# Os dois rotulos nao podem ancorar no mesmo x: na borda direita as duas retas
# quase coincidem e os textos colidiram (verificado na pagina renderizada). O da
# relacao verdadeira fica a direita, o do ajuste desta amostra a esquerda.
x_dir <- max(a$x); x_esq <- min(a$x)
p <- ggplot(data.frame(x = range(a$x), y = range(a$y)), aes(x, y)) +
  geom_blank() +
  geom_abline(data = curvas, aes(intercept = b0, slope = b1),
              colour = "grey78", linewidth = 0.25) +
  geom_abline(intercept = a$b0, slope = a$b1,
              colour = "grey15", linewidth = 0.8) +
  geom_abline(intercept = a$dgp$b0, slope = a$dgp$b1,
              colour = "firebrick", linewidth = 0.7, linetype = "22") +
  annotate("text", x = x_dir, y = a$dgp$b0 + a$dgp$b1 * x_dir,
           label = "rela\\c{c}\\~ao verdadeira", colour = "firebrick",
           hjust = 1, vjust = -1.1, size = 2.6) +
  annotate("text", x = x_esq, y = a$b0 + a$b1 * x_esq,
           label = "ajuste desta amostra", colour = "grey15",
           hjust = 0, vjust = 2.2, size = 2.6) +
  labs(x = "Anos de estudo", y = "Sal\\'ario-hora (R\\$)") +
  expand_limits(y = 0) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank())
# Densidade conceitual, e por isso em pgfplots e nao em R: nao depende dos
# dados, so dos graus de liberdade do exemplo. A constante da t e avaliada
# aqui porque o pgfplots nao tem funcao gama.
# A t com os 18 g.l. do exemplo e visualmente indistinguivel da normal, e a
# figura precisa mostrar o que o titulo afirma: entra tambem a t com 3 g.l.,
# que e o caso em que a diferenca decide o teste.
dens <- function(g) sprintf("%s*(1+x^2/%s)^(-%s)",
                            signif(gamma((g + 1) / 2) /
                                   (sqrt(g * pi) * gamma(g / 2)), 6),
                            g, (g + 1) / 2)
alt <- "Densidade normal padrao, t com 18 graus de liberdade quase colada nela, e t com 3 graus de liberdade mais baixa no centro e mais alta nas caudas."
# O erro padrao de beta_1 contra o tamanho da amostra, para dois regressores de
# dispersao distinta. S_e fica no valor estimado no exemplo, de modo que as
# curvas passam pelo numero que o deck ja mostrou.
rot <- c("$S_X$ pequeno", "$S_X$ grande")
sx <- c(2, 5)
g <- do.call(rbind, lapply(seq_along(sx), function(k)
  data.frame(n = seq(10, 400, by = 2), s = a$Se / (sx[k] * sqrt(seq(10, 400, by = 2) - 1)),
             disp = rot[k])))
g$disp <- factor(g$disp, levels = rot)
np <- c(55, 150)
marc <- do.call(rbind, lapply(seq_along(sx), function(k)
  data.frame(n = np[k], s = a$Se / (sx[k] * sqrt(np[k] - 1)),
             vj = -0.9, disp = rot[k])))
marc$disp <- factor(marc$disp, levels = rot)
p <- ggplot(g, aes(n, s, linetype = disp)) +
  geom_line(colour = "grey15", linewidth = 0.6) +
  geom_text(data = marc, aes(label = disp, vjust = vj), colour = "grey15",
            size = 2.6, hjust = 0, show.legend = FALSE) +
  scale_linetype_manual(values = c("solid", "22"), guide = "none") +
  labs(x = "$n$", y = "$S_{\\hat\\beta_1}$") +
  expand_limits(y = 0) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 1,
                                    margin = margin(r = 6)))
# As mesmas 2000 amostras sorteadas no setup, e as mesmas cujas sessenta
# primeiras retas apareceram no frame das muitas amostras. A curva sobreposta e
# a normal TEORICA, com o sigma do processo gerador -- e o confronto entre o
# resultado algebrico e a simulacao.
#
# A media e o desvio simulados sao desenhados DENTRO da figura, e o paragrafo
# abaixo os cita a partir do mesmo objeto: nao ha como o slide e a figura
# discordarem.
#
# A linha vermelha marca a ESTIMATIVA desta amostra, e nao o parametro: o valor
# do processo gerador so e revelado no ultimo frame da secao. A normal
# sobreposta continua centrada nele, sem que numero algum o nomeie.
d <- data.frame(b1 = sim$b1)
# Duas anotacoes de UMA linha cada, e nao uma de duas: nem o \\ nem o
# \shortstack quebram linha dentro do no que o tikzDevice emite, e o de duas
# linhas ainda sai mal posicionado, porque a medicao devolve a altura de uma so
# (verificado na pagina renderizada). Aqui a separacao e o proprio vjust.
rot_media  <- sprintf("m\\'edia $= %s$", nm(mean(sim$b1), 3))
rot_desvio <- sprintf("desvio $= %s$", nm(sd(sim$b1), 3))
p <- ggplot(d, aes(b1)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = "grey85", colour = "grey55", linewidth = 0.2) +
  stat_function(fun = dnorm, args = list(mean = a$dgp$b1, sd = sim$sd_b1),
                colour = "grey15", linewidth = 0.7) +
  geom_vline(xintercept = a$b1, linetype = "dashed", colour = "firebrick") +
  annotate("text", x = a$b1, y = 0, label = "$\\hat\\beta_1$ desta amostra",
           colour = "firebrick", hjust = -0.1, vjust = -0.4, size = 2.6) +
  annotate("text", x = -Inf, y = Inf, label = rot_media,
           hjust = -0.1, vjust = 1.6, size = 2.6, colour = "grey25") +
  annotate("text", x = -Inf, y = Inf, label = rot_desvio,
           hjust = -0.1, vjust = 3.1, size = 2.6, colour = "grey25") +
  labs(x = "$\\hat\\beta_1$ em 2000 amostras do mesmo processo gerador",
       y = "densidade") +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank())
# A t com gl graus de liberdade, as duas caudas de rejeicao sombreadas e a
# estatistica observada marcada. O dominio vai ate um pouco alem do |t|
# observado, para que a marca nao caia fora da escala.
lim <- max(4, abs(a$t1) * 1.15)
g <- data.frame(x = seq(-lim, lim, length.out = 600))
g$y <- dt(g$x, a$gl)
cauda <- subset(g, abs(x) >= a$tc)
cauda$lado <- factor(ifelse(cauda$x < 0, "esq", "dir"))
p <- ggplot(g, aes(x, y)) +
  geom_area(data = cauda, aes(group = lado), fill = "firebrick", alpha = 0.28) +
  geom_line(colour = "grey20", linewidth = 0.6) +
  geom_vline(xintercept = c(-a$tc, a$tc), linetype = "dashed",
             colour = "firebrick", linewidth = 0.4) +
  geom_vline(xintercept = a$t1, colour = "grey10", linewidth = 0.7) +
  annotate("text", x = a$tc, y = max(g$y) * 0.72,
           label = sprintf("$t_{0{,}025}(%s) = %s$", ni(a$gl), nm(a$tc, 2)),
           colour = "firebrick", hjust = -0.08, vjust = 0, size = 2.6) +
  annotate("text", x = a$t1, y = max(g$y) * 0.30,
           label = sprintf("$t = %s$", nm(a$t1, 2)),
           colour = "grey10", hjust = 1.08, vjust = 0, size = 2.6) +
  labs(x = sprintf("$t_{(%s)}$", ni(a$gl)), y = "densidade") +
  expand_limits(y = 0) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank())
gl_l <- c(10, 18, 20, 30)
al   <- c(0.10, 0.05, 0.01)
m <- outer(gl_l, al, function(g, p) qt(1 - p / 2, g))
tb <- data.frame(gl = ni(gl_l))
for (j in seq_along(al)) tb[[sprintf("$%s$", nm(al[j], 2))]] <- nm(m[, j], 3)
names(tb)[1] <- "g.l."
faixa <- c(0.01, 0.15)
pv <- subset(tidypvals::brodeur2016, pvalue >= faixa[1] & pvalue <= faixa[2])
larg <- 0.005
p <- ggplot(pv, aes(pvalue)) +
  geom_histogram(binwidth = larg, boundary = 0, fill = "grey55",
                 colour = "white", linewidth = 0.2) +
  geom_vline(xintercept = 0.05, colour = "firebrick", linetype = "22",
             linewidth = 0.5) +
  annotate("text", x = 0.05, y = Inf, colour = "firebrick", size = 2.6,
           hjust = -0.12, vjust = 1.8, label = "$\\alpha = 0{,}05$") +
  labs(x = "$p$-valor", y = "Estimativas publicadas") +
  expand_limits(y = 0) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank())
conta <- function(lo) sum(pv$pvalue >= lo & pv$pvalue < lo + larg)
te <- a$econ
te <- a$econ; g <- a$gl
cte <- gamma((g + 1) / 2) / (sqrt(g * pi) * gamma(g / 2))
dens <- sprintf("%s*(1+x^2/%s)^(-%s)", signif(cte, 6), g, (g + 1) / 2)
lim <- 4
alt <- "Densidade t com as caudas do teste bilateral sombreadas, os dois valores criticos assinalados e a estatistica observada entre eles."
te <- a$econ
# A distribuicao amostral de beta_1 chapeu, no eixo do PROPRIO coeficiente:
# uma t com n-2 gl, centrada em beta_1 chapeu e escalada por S. A area
# sombreada vale 1 - alpha, e as suas extremidades, lidas no eixo horizontal,
# sao os limites do intervalo -- e essa projecao que a inversao da desigualdade
# produz algebricamente em seguida.
meia <- 0.85 * diff(a$ic1)
bb <- seq(a$ic1[1] - meia, a$ic1[2] + meia, length.out = 400)
dens <- function(v) dt((v - a$b1) / a$Sb1, a$gl) / a$Sb1
g <- data.frame(b = bb, f = dens(bb))
dentro <- subset(g, b >= a$ic1[1] & b <= a$ic1[2])
esq <- subset(g, b <= a$ic1[1])
dir <- subset(g, b >= a$ic1[2])
alto <- max(g$f)
p <- ggplot(g, aes(b, f)) +
  geom_area(data = esq, fill = "firebrick", alpha = 0.30) +
  geom_area(data = dir, fill = "firebrick", alpha = 0.30) +
  geom_area(data = dentro, fill = "steelblue3", alpha = 0.35) +
  geom_line(colour = "grey10", linewidth = 0.7) +
  geom_segment(data = data.frame(b = a$ic1),
               aes(x = b, xend = b, y = 0, yend = dens(b)),
               colour = "grey20", linetype = "22", linewidth = 0.4,
               inherit.aes = FALSE) +
  annotate("text", x = a$b1, y = 0.42 * alto, size = 2.8,
           label = sprintf("$1 - \\alpha = %s$", nm(1 - a$alpha, 2))) +
  annotate("text", x = a$ic1[1], y = 0.28 * alto, colour = "firebrick",
           size = 2.6, hjust = 1.15, label = "$\\alpha/2$") +
  annotate("text", x = a$ic1[2], y = 0.28 * alto, colour = "firebrick",
           size = 2.6, hjust = -0.15, label = "$\\alpha/2$") +
  scale_x_continuous(breaks = c(a$ic1[1], a$b1, a$ic1[2]),
                     labels = c(sprintf("$%s$", nm(a$ic1[1], 3)),
                                "$\\hat\\beta_1$",
                                sprintf("$%s$", nm(a$ic1[2], 3)))) +
  labs(x = "Valores de $\\beta_1$", y = NULL) +
  expand_limits(y = 0) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank())
rot <- c("$\\beta_0$", "$\\beta_1$")
g <- data.frame(termo = factor(rot, levels = rot),
                est = c(a$b0, a$b1),
                lo = c(a$ic0[1], a$ic1[1]), hi = c(a$ic0[2], a$ic1[2]))
p <- ggplot(g, aes(est, termo)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = a$c_econ, linetype = "22", colour = "firebrick",
             linewidth = 0.5) +
  geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = 0.12, linewidth = 0.7) +
  geom_point(size = 2.6) +
  annotate("text", x = a$c_econ, y = 2.45, colour = "firebrick", size = 2.6,
           hjust = -0.12, label = sprintf("$c = %s$", nm(a$c_econ, 1))) +
  labs(x = "Estimativa e intervalo de confian\\c{c}a a 95\\%", y = NULL) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank())
te <- a$econ
niveis <- c(0.90, 0.95, 0.99)
tc <- qt(1 - (1 - niveis) / 2, a$gl)
lo <- a$b1 - tc * a$Sb1; hi <- a$b1 + tc * a$Sb1
# Os tres intervalos da tabela anterior, desenhados na mesma escala, contra o
# zero. O de 99% e o unico cuja largura se aproxima de cruza-lo.
niveis <- c(0.90, 0.95, 0.99)
tc <- qt(1 - (1 - niveis) / 2, a$gl)
rot <- paste0("$", ni(100 * niveis), "\\%$")
g <- data.frame(nivel = factor(rot, levels = rev(rot)),
                est = a$b1, lo = a$b1 - tc * a$Sb1, hi = a$b1 + tc * a$Sb1)
p <- ggplot(g, aes(est, nivel)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "firebrick") +
  geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = 0.14, linewidth = 0.7) +
  geom_point(size = 2.4) +
  labs(x = "Intervalo para $\\beta_1$", y = "confian\\c{c}a") +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank())
d <- a$dados
mod <- lm(salario ~ educ, data = d)
grade <- data.frame(educ = seq(min(d$educ), max(d$educ), length.out = 120))
cf <- as.data.frame(predict(mod, grade, interval = "confidence"))
pv <- as.data.frame(predict(mod, grade, interval = "prediction"))
rot <- c("intervalo da m\\'edia, $S_{\\hat Y_0}$", "intervalo de previs\\~ao, $S_{\\text{prev}}$")
faixas <- rbind(
  data.frame(educ = grade$educ, lo = cf$lwr, hi = cf$upr, tipo = rot[1]),
  data.frame(educ = grade$educ, lo = pv$lwr, hi = pv$upr, tipo = rot[2]))
faixas$tipo <- factor(faixas$tipo, levels = rot)
# O intervalo da media e area preenchida; o de previsao segue em pontilhado,
# so com as duas bordas. A legenda passa a nomear apenas o de previsao, porque
# a area dispensa chave: ela e a unica regiao colorida da figura.
prev <- subset(faixas, tipo == rot[2])
media <- subset(faixas, tipo == rot[1])
p <- ggplot() +
  geom_ribbon(data = media, aes(educ, ymin = lo, ymax = hi, fill = tipo),
              alpha = 0.30) +
  geom_line(data = prev, aes(educ, lo, linetype = tipo), colour = "grey30",
            linewidth = 0.45) +
  geom_line(data = prev, aes(educ, hi, linetype = tipo), colour = "grey30",
            linewidth = 0.45) +
  geom_line(data = data.frame(educ = grade$educ, y = cf$fit), aes(educ, y),
            colour = "grey10", linewidth = 0.7) +
  geom_point(data = d, aes(educ, salario), size = 1.5) +
  geom_vline(xintercept = a$x0, colour = "firebrick", linetype = "22",
             linewidth = 0.5) +
  annotate("point", x = a$x0, y = a$prev$aj, colour = "firebrick", size = 2) +
  annotate("text", x = a$x0, y = 0, colour = "firebrick", size = 2.6,
           hjust = 1.1, vjust = -0.3,
           label = sprintf("$X_0 = %s$", ni(a$x0))) +
  scale_linetype_manual(values = c("dashed")) +
  scale_fill_manual(values = c("steelblue3")) +
  labs(x = "Anos de estudo", y = "Sal\\'ario-hora (R\\$)",
       linetype = NULL, fill = NULL) +
  expand_limits(y = 0) +
  theme_minimal(base_size = 9) +
  # Legenda VERTICAL, dentro do painel: em linha, as duas entradas e as duas
  # chaves de tracejado colidem na largura disponivel. O canto superior
  # esquerdo esta vazio, porque salario cresce com escolaridade.
  theme(legend.position = c(0.02, 0.98), legend.justification = c(0, 1),
        legend.direction = "vertical",
        legend.background = element_blank(), legend.key = element_blank(),
        legend.key.width = grid::unit(0.9, "cm"),
        legend.text = element_text(size = 7),
        panel.grid.minor = element_blank())
pv <- a$prev
# Figura SIMULADA, a contraparte concreta da anterior: cada painel e uma amostra
# de verdade, ajustada por si. O painel de n = 20 e a amostra observada; os
# outros reamostram educ COM REPOSICAO, para preservar a dispersao de X, e
# sorteiam o erro do mesmo processo gerador.
#
# Os intervalos tremem entre paineis, porque cada um vem de um ajuste diferente.
# E fiel ao que aconteceria, e mistura o efeito de n com o do sorteio -- que e
# justamente o que a figura anterior isola.
semente(20260908)
# NAO chamar este vetor de 'ns': o nome sobrescreve o formatador
# ns() da camada compartilhada, e o script tangulado morre adiante.
tam <- c(20, 100, 1000)
rot <- sprintf("$n = %s$", ni(tam))
sim1 <- function(n) {
  if (n == length(a$x)) {
    x <- a$x; y <- a$y
  } else {
    x <- sample(a$x, n, replace = TRUE)
    y <- a$dgp$b0 + a$dgp$b1 * x + rnorm(n, 0, a$dgp$sigma)
  }
  data.frame(x = x, y = y)
}
amostras <- lapply(tam, sim1)
grade <- seq(min(a$x), max(a$x), length.out = 120)
pontos <- do.call(rbind, lapply(seq_along(tam), function(k)
  transform(amostras[[k]], n = rot[k])))
faixa <- do.call(rbind, lapply(seq_along(tam), function(k) {
  m <- lm(y ~ x, data = amostras[[k]])
  cf <- as.data.frame(predict(m, data.frame(x = grade), interval = "confidence"))
  pv <- as.data.frame(predict(m, data.frame(x = grade), interval = "prediction"))
  data.frame(x = grade, aj = cf$fit, cl = cf$lwr, ch = cf$upr,
             pl = pv$lwr, ph = pv$upr, n = rot[k])
}))
pontos$n <- factor(pontos$n, levels = rot)
faixa$n  <- factor(faixa$n,  levels = rot)
p <- ggplot(faixa, aes(x)) +
  geom_point(data = pontos, aes(x, y), size = 0.4, alpha = 0.18,
             colour = "grey40", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = cl, ymax = ch), fill = "steelblue3", alpha = 0.40) +
  geom_line(aes(y = pl), colour = "grey30", linetype = "22", linewidth = 0.4) +
  geom_line(aes(y = ph), colour = "grey30", linetype = "22", linewidth = 0.4) +
  geom_line(aes(y = aj), colour = "grey10", linewidth = 0.6) +
  facet_wrap(~ n) +
  labs(x = "Anos de estudo", y = "Sal\\'ario-hora (R\\$)") +
  theme_minimal(base_size = 8) +
  theme(panel.grid.minor = element_blank())
# O LEQUE: oitenta das 2000 amostras ja sorteadas no setup, cada uma com a sua
# curva ajustada, sobre um dominio estendido para alem do observado.
#
# Nao mostra QUE a imprecisao e minima em X barra; mostra POR QUE. Com os X
# fixos, o valor ajustado em X barra e a media amostral de Y, que varia pouco,
# e as curvas se cruzam ali. O que difere entre amostras e a INCLINACAO, e o
# efeito dela cresce com a distancia ao centro -- de modo que o leque abre para
# os dois lados. A curva em U de S_{hat Y_0} e a leitura numerica disto.
curvas <- data.frame(b0 = sim$b0[1:80], b1 = sim$b1[1:80])
lim <- c(min(a$x) - 9, max(a$x) + 12)
p <- ggplot(data.frame(x = lim, y = a$b0 + a$b1 * lim), aes(x, y)) +
  geom_blank() +
  annotate("rect", xmin = min(a$x), xmax = max(a$x), ymin = -Inf, ymax = Inf,
           fill = "grey88") +
  geom_abline(data = curvas, aes(intercept = b0, slope = b1),
              colour = "grey55", linewidth = 0.2) +
  geom_abline(intercept = a$b0, slope = a$b1, colour = "grey10",
              linewidth = 0.7) +
  annotate("point", x = a$mx, y = a$my, colour = "firebrick", size = 1.8) +
  annotate("text", x = a$mx, y = a$my, colour = "firebrick", size = 2.6,
           hjust = -0.15, vjust = 2.6, label = "$(\\bar X, \\bar Y)$") +
  labs(x = "Anos de estudo", y = "Sal\\'ario-hora (R\\$)") +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank())
tb <- data.frame(
  Parâmetro = c("$\\beta_0$", "$\\beta_1$", "$\\sigma$"),
  verdadeiro = paste0("$", ni(unlist(a$dgp)), "$"),
  Estimado = paste0("$", nm(c(a$b0, a$b1, a$Se), 2), "$"),
  `Intervalo a 95\\%` = c(iv(a$ic0, 2), iv(a$ic1, 2), "---"),
  check.names = FALSE)
# A aspa tipografica nao passa por nome de argumento do data.frame (o backtick
# nao aninha), entao o rotulo e posto depois.
names(tb)[2] <- "``Verdadeiro''"
co <- summary(mincer)$coefficients
ic <- confint(mincer)
ic <- confint(mincer)["educ", ]
b1 <- coef(mincer)[["educ"]]
grade <- data.frame(educ = seq(min(wage1$educ), max(wage1$educ), length.out = 120))
cf <- as.data.frame(predict(mincer, grade, interval = "confidence"))
p <- ggplot() +
  geom_point(data = wage1, aes(educ, log(wage)), size = 0.7, alpha = 0.35,
             colour = "grey40") +
  geom_ribbon(data = data.frame(educ = grade$educ, lo = cf$lwr, hi = cf$upr),
              aes(educ, ymin = lo, ymax = hi), fill = "grey55", alpha = 0.55) +
  geom_line(data = data.frame(educ = grade$educ, y = cf$fit), aes(educ, y),
            colour = "grey10", linewidth = 0.7) +
  labs(x = "Anos de estudo", y = "$\\log(\\text{sal\\'ario})$") +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank())
# O mesmo ajuste, com e sem intercepto, contra o processo gerador declarado.
sem <- lm(a$y ~ 0 + a$x)
b1_sem <- unname(coef(sem)[1])
xs <- range(a$x)
p <- ggplot(data.frame(x = a$x, y = a$y), aes(x, y)) +
  geom_point(size = 1.4, colour = "grey45") +
  geom_abline(intercept = a$dgp$b0, slope = a$dgp$b1,
              colour = "firebrick", linetype = "22", linewidth = 0.7) +
  geom_abline(intercept = a$b0, slope = a$b1,
              colour = "grey15", linewidth = 0.7) +
  geom_abline(intercept = 0, slope = b1_sem,
              colour = "steelblue4", linetype = "42", linewidth = 0.7) +
# As tres curvas convergem na borda direita, e os rotulos colidiam ali
# (verificado na pagina renderizada). O do ajuste completo passa para a borda
# esquerda, e os dois da direita apontam para lados opostos: o do processo
# gerador para baixo da sua curva, o do ajuste sem intercepto para cima da sua.
  annotate("text", x = xs[2], y = a$dgp$b0 + a$dgp$b1 * xs[2],
           label = "processo gerador", colour = "firebrick",
           hjust = 1, vjust = 1.9, size = 2.5) +
  annotate("text", x = xs[2], y = b1_sem * xs[2],
           label = "OLS sem intercepto", colour = "steelblue4",
           hjust = 1, vjust = -0.9, size = 2.5) +
  annotate("text", x = xs[1], y = a$b0 + a$b1 * xs[1],
           label = "OLS completo", colour = "grey15",
           hjust = 0, vjust = -1.1, size = 2.5) +
  labs(x = "Anos de estudo", y = "Salário-hora (R\\$)") +
  expand_limits(y = 0) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank())
sem <- lm(a$y ~ 0 + a$x)
te <- e$econ
pv <- e$prev
