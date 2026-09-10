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
source("code/bloco_setup.R")
de <- dados("rls_inferencia_exercicio.csv")
e <- rls(de$X, de$Y); e$dados <- de
e$t0  <- e$b0 / e$Sb0
e$ic0 <- e$b0 + c(-1, 1) * e$tc * e$Sb0
e$tc_uni <- qt(1 - e$alpha, e$gl)
e$econ <- teste_t(e$b1, e$Sb1, e$gl, c0 = 1, alpha = e$alpha)
e$x0 <- 6
e$prev <- previsao(e, e$x0)
source("code/bloco_setup.R")
library(wooldridge); data("wage1")
mincer <- lm(log(wage) ~ educ, data = wage1)
d <- a$dados
tab_serie("$X_i$" = recorte(d$educ), "$Y_i$" = recorte(nm(d$salario, 1)),
          caption = sprintf("Base de dados sintética ($n = %s$)", ni(a$n)),
          tamanho = "small")
cat(sprintf("Denotando desvios por minúsculas ($x_i = X_i - \\bar X$), com $\\bar X = %s$ e $\\bar Y = %s$:\n",
            nm(a$mx, 2), nm(a$my, 2)))
eq(sprintf("S_{XX} = \\sum x_i^2 = %s, \\qquad S_{XY} = \\sum x_i y_i = %s, \\qquad S_{YY} = \\sum y_i^2 = %s.",
           nm(a$Sxx, 1), nm(a$Sxy, 1), nm(a$Syy, 1)))
eq(sprintf("\\hat\\beta_1 = %s = %s = %s, \\qquad \\hat\\beta_0 = \\bar Y - \\hat\\beta_1 \\bar X = %s.",
           frac("S_{XY}", "S_{XX}"), frac(nm(a$Sxy, 1), nm(a$Sxx, 1)),
           nm(a$b1, 2), nm(a$b0, 2)))
eq(cx(ajuste))
library(ggplot2)
d <- data.frame(x = a$x, y = a$y)
p <- ggplot(d, aes(x, y)) +
  geom_abline(intercept = a$b0, slope = a$b1, colour = "grey25", linewidth = 0.7) +
  geom_point(size = 1.7) +
  labs(x = "Anos de estudo", y = "Sal\\'ario-hora (R\\$)") +
  expand_limits(y = 0) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank())
fig_salva("rls_inf_dispersao.pdf", p, largura = 5.0, altura = 1.35,
          alt = "Dispersao de salario-hora contra anos de estudo, com a curva ajustada sobreposta.")
library(ggplot2)
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
fig_salva("rls_inf_muitas_amostras.pdf", p, largura = 5.0, altura = 1.9,
          alt = "Sessenta retas cinza, uma por amostra simulada do mesmo processo gerador, com a reta da amostra observada em preto e a relacao verdadeira tracejada em vermelho.")
eq(sprintf("S_e^2 = %s = %s = %s, \\qquad S_e = %s.",
           frac("\\sum \\hat u_i^2", "n-2"), frac(nm(a$RSS, 1), ni(a$gl)),
           nm(a$Se2, 2), nm(a$Se, 2)))
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
cat(sprintf("\\altfig{%s}{%%\n", alt))
cat("\\centering\n\\begin{tikzpicture}\n")
cat("\\begin{axis}[width=0.80\\textwidth, height=4.0cm, domain=-4:4, samples=200,\n")
cat("  axis lines=middle, ymin=0, ymax=0.45, xlabel={$t$}, ylabel={},\n")
cat("  ytick=\\empty, xtick={-2,0,2}, x tick label style={font=\\tiny},\n")
cat("  legend style={font=\\tiny, draw=none, at={(0.02,0.98)}, anchor=north west}]\n")
cat("  \\addplot[thick, black] {0.3989423*exp(-x^2/2)};\n")
cat("  \\addlegendentry{normal padr\\~ao}\n")
cat(sprintf("  \\addplot[thick, catcolor, dashed] {%s};\n", dens(a$gl)))
cat(sprintf("  \\addlegendentry{$t$ com %s g.l.}\n", ni(a$gl)))
cat(sprintf("  \\addplot[thick, gray, dotted] {%s};\n", dens(3)))
cat("  \\addlegendentry{$t$ com 3 g.l.}\n")
cat("\\end{axis}\n\\end{tikzpicture}\n}\n")
library(ggplot2)
# O erro padrao de beta_1 contra o tamanho da amostra, para dois regressores de
# dispersao distinta. S_e fica no valor estimado no exemplo, de modo que as
# curvas passam pelo numero que o deck ja mostrou.
rot <- c("$S_X$ pequeno", "$S_X$ grande")
sx <- c(2, 5)
g <- do.call(rbind, lapply(seq_along(sx), function(k)
  data.frame(n = seq(10, 400, by = 2), s = a$Se / (sx[k] * sqrt(seq(10, 400, by = 2) - 1)),
             disp = rot[k])))
# Niveis declarados, como manda o CLAUDE.md 5.14.
g$disp <- factor(g$disp, levels = rot)
# Rotulos posicionados a mao, e nao por ggrepel (CLAUDE.md 5.21): sao dois, e a
# posicao precisa ser a mesma em toda reexportacao. Ancorados em n DIFERENTES,
# porque as duas curvas se aproximam a direita e num mesmo n os textos colidem
# (verificado na pagina renderizada).
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
  # Titulo do eixo na horizontal: numa figura baixa o rotulo rotacionado encosta
  # nos numeros do eixo (CLAUDE.md 5.27).
  theme(panel.grid.minor = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 1,
                                    margin = margin(r = 6)))
fig_salva("rls_inf_taxa_erro_padrao.pdf", p, largura = 5.0, altura = 1.1,
          alt = "Erro padrao do coeficiente angular contra o tamanho da amostra, em duas curvas decrescentes: a de regressor pouco disperso fica acima da de regressor muito disperso em todo o eixo.")
eq(sprintf("S_{\\hat\\beta_1} = \\sqrt{%s} = %s, \\qquad S_{\\hat\\beta_0} = \\sqrt{%s \\left( %s + %s \\right)} = %s.",
           frac(nm(a$Se2, 2), nm(a$Sxx, 1)), nm(a$Sb1, 4),
           nm(a$Se2, 2), frac("1", ni(a$n)),
           frac(sprintf("%s^2", nm(a$mx, 2)), nm(a$Sxx, 1)), nm(a$Sb0, 4)))
eq(sprintf("\\hat Y_i = \\underset{(%s)}{%s} %s \\underset{(%s)}{%s}\\,X_i",
           nm(a$Sb0, 4), nm(a$b0, 4), ifelse(a$b1 < 0, "-", "+"),
           nm(a$Sb1, 4), nm(abs(a$b1), 4)))
cat(sprintf("\nEntre parênteses, sob cada coeficiente, o seu erro padrão. A unidade de $S_{\\hat\\beta_1}$ é a de $\\hat\\beta_1$: reais por hora, por ano de estudo.\n"))
library(ggplot2)
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
fig_salva("rls_inf_distribuicao_amostral.pdf", p, largura = 5.0, altura = 1.9,
          alt = "Histograma dos coeficientes estimados em duas mil amostras, com a densidade normal teorica sobreposta, uma linha tracejada na estimativa desta amostra e a media e o desvio simulados anotados no canto.")
cat(sprintf("A distribuição está centrada no parâmetro, e não na estimativa desta amostra, que é a linha tracejada. O desvio simulado, $%s$, reproduz $\\sigma/\\sqrt{S_{XX}} = %s$.\n",
            nm(sd(sim$b1), 3), nm(sim$sd_b1, 3)))
eq("H_0: \\beta_1 = 0 \\qquad \\text{contra} \\qquad H_1: \\beta_1 \\neq 0")
eq(sprintf("t(\\hat\\beta_1) = %s = %s = %s, \\qquad t_{0{,}025}(%s) = %s.",
           frac("\\hat\\beta_1 - 0", "S_{\\hat\\beta_1}"),
           frac(nm(a$b1, 4), nm(a$Sb1, 4)), cx(nm(a$t1, 3)),
           ni(a$gl), nm(a$tc, 3)))
cat(sprintf("\nComo $%s > %s$, rejeita-se $H_0$ a $%s\\%%$. O efeito da escolaridade sobre o salário é \\alert{significativo}.\n",
            nm(a$t1, 3), nm(a$tc, 3), ni(100 * a$alpha)))
library(ggplot2)
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
fig_salva("rls_inf_regiao_rejeicao.pdf", p, largura = 5.0, altura = 1.9,
          alt = "Densidade t com as duas caudas de rejeicao sombreadas, os pontos criticos tracejados e a estatistica observada marcada bem a direita da cauda direita.")
gl_l <- c(10, 18, 20, 30)
al   <- c(0.10, 0.05, 0.01)
m <- outer(gl_l, al, function(g, p) qt(1 - p / 2, g))
tb <- data.frame(gl = ni(gl_l))
for (j in seq_along(al)) tb[[sprintf("$%s$", nm(al[j], 2))]] <- nm(m[, j], 3)
names(tb)[1] <- "g.l."
tab(tb, caption = "Pontos críticos da $t$ bilateral, por graus de liberdade e nível de significância", tamanho = "small")
eq(sprintf("t(\\hat\\beta_0) = %s = %s < %s = t_{0{,}025}(%s).",
           frac(nm(a$b0, 4), nm(a$Sb0, 4)), nm(a$t0, 3), nm(a$tc, 3), ni(a$gl)))
cat(sprintf("\nNão se rejeita $H_0: \\beta_0 = 0$ a $%s\\%%$: o intercepto não é distinguível de zero.\n",
            ni(100 * a$alpha)))
tab(data.frame(
  Coeficiente = c("$\\hat\\beta_0$", "$\\hat\\beta_1$"),
  `$t$` = paste0("$", nm(c(a$t0, a$t1), 3), "$"),
  `Valor-$p$` = c(paste0("$", nm(a$p0, 4), "$"), "$< 0{,}0001$"),
  `Decisão a 5\\%` = c("não se rejeita", "rejeita-se"),
  check.names = FALSE),
    caption = "Testes de nulidade dos dois coeficientes", tamanho = "scriptsize")
library(ggplot2)
# tidypvals nao esta no CRAN. Instalacao, uma unica vez:
#   remotes::install_github("jtleek/tidypvals")
# A linha fica comentada de proposito (Lectures/CLAUDE.md 5.27).
library(tidypvals)
# 49.297 p-valores extraidos de AER, JPE e QJE por Brodeur et al. (2016).
#
# O recorte e do DOMINIO, e nao do eixo vertical (CLAUDE.md 5.20): abaixo de
# 0,01 a frequencia e uma ordem de grandeza maior e comprimiria o resto da
# figura. O texto do slide declara o recorte.
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
fig_salva("rls_inf_pvalores_publicados.pdf", p, largura = 5.0, altura = 1.6,
          alt = "Histograma dos p-valores publicados em tres revistas de economia, no intervalo de 0,01 a 0,15: a frequencia cai de forma regular, exceto pela barra imediatamente a esquerda de 0,05, mais alta que as duas vizinhas.")
# As tres contagens saem do MESMO objeto que a figura desenha (CLAUDE.md 5.30).
conta <- function(lo) sum(pv$pvalue >= lo & pv$pvalue < lo + larg)
cat(sprintf(paste("\nRecorte de $0{,}01$ a $0{,}15$, porque os \\(p\\)-valores muito pequenos são numerosos e comprimiriam o restante da figura.",
                  "A barra imediatamente à esquerda de $0{,}05$ reúne %s estimativas, contra %s na faixa anterior e %s na seguinte.\n"),
            ni(conta(0.045)), ni(conta(0.040)), ni(conta(0.050))))
cat(sprintf("Suponha que a literatura sugira retorno de \\textrm{R\\$}\\,%s por hora a cada ano de estudo. Os dados são compatíveis com isso, ou indicam retorno maior?\n",
            nm(a$c_econ, 2)))
te <- a$econ
eq(sprintf("H_0: \\beta_1 = %s \\qquad \\text{contra} \\qquad H_1: \\beta_1 > %s",
           nm(te$c0, 1), nm(te$c0, 1)))
eq(sprintf("t = %s = %s = %s, \\qquad t_{0{,}05}(%s) = %s.",
           frac("\\hat\\beta_1 - c", "S_{\\hat\\beta_1}"),
           frac(sprintf("%s - %s", nm(a$b1, 4), nm(te$c0, 2)), nm(a$Sb1, 4)),
           cx(nm(te$t, 3)), ni(a$gl), nm(te$tc_uni, 3)))
cat(sprintf("\nComo $%s > %s$, rejeita-se $H_0$ a $%s\\%%$: os dados indicam retorno superior ao sugerido pela literatura. Os graus de liberdade e o nível de significância são os do teste de nulidade; o valor crítico muda porque a alternativa é unilateral.\n",
            nm(te$t, 3), nm(te$tc_uni, 3), ni(100 * a$alpha)))
te <- a$econ; g <- a$gl
cte <- gamma((g + 1) / 2) / (sqrt(g * pi) * gamma(g / 2))
dens <- sprintf("%s*(1+x^2/%s)^(-%s)", signif(cte, 6), g, (g + 1) / 2)
lim <- 4
alt <- "Densidade t com as caudas do teste bilateral sombreadas, os dois valores criticos assinalados e a estatistica observada entre eles."
cat(sprintf("\\altfig{%s}{%%\n", alt))
cat("\\centering\n\\begin{tikzpicture}\n")
cat(sprintf("\\begin{axis}[width=0.86\\textwidth, height=4.2cm, domain=%s:%s, samples=200,\n", -lim, lim))
cat("  axis lines=middle, ymin=0, ymax=0.42, xlabel={$t$}, ylabel={},\n")
cat("  xtick={-2,0,2}, ytick=\\empty, clip=false, x tick label style={font=\\tiny}]\n")
cat(sprintf("  \\addplot[thick, black] {%s};\n", dens))
for (lado in list(c(round(te$tc_bi, 3), lim), c(-lim, round(-te$tc_bi, 3))))
  cat(sprintf("  \\addplot[draw=none, fill=catcolor, fill opacity=0.25, domain=%s:%s] {%s} \\closedcycle;\n",
              lado[1], lado[2], dens))
# Rotulos empilhados: os dois criticos distam 0,37 no eixo e se sobreporiam
# lado a lado.
cat(sprintf("  \\draw[gray, dashed] (axis cs:%s,0) -- (axis cs:%s,0.12) node[above, font=\\tiny] {$t_{0{,}05}$};\n",
            round(te$tc_uni, 3), round(te$tc_uni, 3)))
cat(sprintf("  \\draw[gray, dashed] (axis cs:%s,0) -- (axis cs:%s,0.22) node[above, font=\\tiny] {$t_{0{,}025}$};\n",
            round(te$tc_bi, 3), round(te$tc_bi, 3)))
cat(sprintf("  \\draw[thick, catcolor] (axis cs:%s,0) -- (axis cs:%s,0.33) node[above, font=\\scriptsize] {$t = %s$};\n",
            round(te$t, 3), round(te$t, 3), nm(te$t, 2)))
cat("\\end{axis}\n\\end{tikzpicture}\n}\n")
te <- a$econ
cat(sprintf("A mesma estatística, $t = %s$, ultrapassa o valor crítico unilateral $%s$ e não alcança o bilateral $%s$. A região de rejeição é que muda, e não os dados.\n",
            nm(te$t, 3), nm(te$tc_uni, 3), nm(te$tc_bi, 3)))
eq(sprintf("t(\\hat\\beta_1)^2 = %s^2 = %s = F.", nm(a$t1, 3), nm(a$F, 2)))
library(ggplot2)
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
fig_salva("rls_inf_inversao.pdf", p, largura = 5.0, altura = 1.7,
          alt = "Densidade t centrada na estimativa do coeficiente angular, com a regiao central sombreada em azul, de area um menos alfa, e as duas caudas em vermelho, de area alfa sobre dois cada; as fronteiras entre as regioes, lidas no eixo horizontal, sao os limites do intervalo de confianca.")
cat(sprintf("\\correctwrong{Correto}{``Em amostras repetidas, %s\\%% dos intervalos construídos deste modo contêm $\\beta_1$.''}{Errado}{``Há %s\\%% de probabilidade de $\\beta_1$ estar entre %s e %s.''}\n",
            ni(100 * (1 - a$alpha)), ni(100 * (1 - a$alpha)),
            nm(a$ic1[1], 3), nm(a$ic1[2], 3)))
cat(sprintf("Com $t_{0{,}025}(%s) = %s$, a regra $\\hat\\beta_j \\pm t_{\\alpha/2}(n-2)\\,S_{\\hat\\beta_j}$ produz:\n\n",
            ni(a$gl), nm(a$tc, 3)))
eq(sprintf("%s \\pm %s \\times %s = %s \\pm %s \\quad\\Longrightarrow\\quad %s",
           nm(a$b1, 4), nm(a$tc, 3), nm(a$Sb1, 4), nm(a$b1, 3),
           nm(a$tc * a$Sb1, 3), cx(iv(a$ic1, 3))))
eq(sprintf("%s \\pm %s \\times %s = %s",
           nm(a$b0, 4), nm(a$tc, 3), nm(a$Sb0, 4), iv(a$ic0, 3)))
library(ggplot2)
# Niveis declarados, e na ordem em que as equacoes acima citam os coeficientes:
# beta_1 primeiro, logo no alto do eixo discreto (CLAUDE.md 5.14 e 5.16).
rot <- c("$\\beta_0$", "$\\beta_1$")
g <- data.frame(termo = factor(rot, levels = rot),
                est = c(a$b0, a$b1),
                lo = c(a$ic0[1], a$ic1[1]), hi = c(a$ic0[2], a$ic1[2]))
p <- ggplot(g, aes(est, termo)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = a$c_econ, linetype = "22", colour = "firebrick",
             linewidth = 0.5) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.12, linewidth = 0.7) +
  geom_point(size = 2.6) +
  annotate("text", x = a$c_econ, y = 2.45, colour = "firebrick", size = 2.6,
           hjust = -0.12, label = sprintf("$c = %s$", nm(a$c_econ, 1))) +
  labs(x = "Estimativa e intervalo de confian\\c{c}a a 95\\%", y = NULL) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank())
fig_salva("rls_inf_ic_coeficientes.pdf", p, largura = 5.0, altura = 1.35,
          alt = "Intervalos de confianca dos dois coeficientes, com uma linha tracejada no zero, que o do intercepto cruza e o da escolaridade nao, e uma segunda linha no valor de referencia economico, dentro do intervalo da escolaridade.")
te <- a$econ
tab(data.frame(
  `Valor de referência $c$` = c("$0$", sprintf("$%s$", nm(te$c0, 1))),
  `Está em $IC_{95\\%}$?` = c("não", "sim"),
  `Teste bilateral a 5\\%` = c("rejeita-se $H_0$", "não se rejeita $H_0$"),
  check.names = FALSE),
    caption = "Cada valor fora do intervalo é rejeitado, e cada valor dentro não é",
    tamanho = "small")
cat(sprintf("\nO teste unilateral de $\\beta_1 = %s$ rejeitou, e o intervalo bilateral contém $%s$. Não há contradição: são regiões críticas distintas.\n",
            nm(te$c0, 1), nm(te$c0, 1)))
niveis <- c(0.90, 0.95, 0.99)
tc <- qt(1 - (1 - niveis) / 2, a$gl)
lo <- a$b1 - tc * a$Sb1; hi <- a$b1 + tc * a$Sb1
tab(data.frame(
  Confiança = paste0("$", ni(100 * niveis), "\\%$"),
  `$t_{\\alpha/2}$` = paste0("$", nm(tc, 3), "$"),
  Intervalo = sapply(seq_along(niveis), function(i) iv(c(lo[i], hi[i]), 3)),
  Amplitude = paste0("$", nm(hi - lo, 3), "$"),
  check.names = FALSE),
    caption = "O mesmo coeficiente sob três níveis de confiança", tamanho = "small")
library(ggplot2)
# Os tres intervalos da tabela anterior, desenhados na mesma escala, contra o
# zero. O de 99% e o unico cuja largura se aproxima de cruza-lo.
niveis <- c(0.90, 0.95, 0.99)
tc <- qt(1 - (1 - niveis) / 2, a$gl)
rot <- paste0("$", ni(100 * niveis), "\\%$")
g <- data.frame(nivel = factor(rot, levels = rev(rot)),
                est = a$b1, lo = a$b1 - tc * a$Sb1, hi = a$b1 + tc * a$Sb1)
p <- ggplot(g, aes(est, nivel)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "firebrick") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.14, linewidth = 0.7) +
  geom_point(size = 2.4) +
  labs(x = "Intervalo para $\\beta_1$", y = "confian\\c{c}a") +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank())
fig_salva("rls_inf_nivel_largura.pdf", p, largura = 5.0, altura = 1.4,
          alt = "Tres intervalos para o mesmo coeficiente, a 90, 95 e 99 por cento de confianca, empilhados na mesma escala: o de 99 por cento e o mais largo e nenhum alcanca o zero tracejado.")
library(ggplot2)
d <- a$dados
mod <- lm(salario ~ educ, data = d)
grade <- data.frame(educ = seq(min(d$educ), max(d$educ), length.out = 120))
cf <- as.data.frame(predict(mod, grade, interval = "confidence"))
pv <- as.data.frame(predict(mod, grade, interval = "prediction"))
# Niveis declarados, como manda o CLAUDE.md 5.14, mesmo com a ordem alfabetica
# coincidindo com a desejada.
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
fig_salva("rls_inf_bandas.pdf", p, largura = 5.2, altura = 1.9,
          alt = "Curva ajustada com o intervalo da media condicional preenchido em azul e o intervalo de previsao, mais largo, em linhas pontilhadas; uma linha vermelha marca o X0 comentado no texto, e ambos sao mais estreitos na media da escolaridade.")
pv <- a$prev
cat(sprintf("Em $X_0 = %s$ anos de estudo, a previsão pontual é $\\hat Y_0 = %s$. O intervalo da média fica em %s e o de previsão, em %s.\n",
            ni(a$x0), nm(pv$aj, 2), iv(pv$ic_med, 2), iv(pv$ic_prev, 2)))
library(ggplot2)
# Figura SIMULADA, a contraparte concreta da anterior: cada painel e uma amostra
# de verdade, ajustada por si. O painel de n = 20 e a amostra observada; os
# outros reamostram educ COM REPOSICAO, para preservar a dispersao de X, e
# sorteiam o erro do mesmo processo gerador.
#
# Os intervalos tremem entre paineis, porque cada um vem de um ajuste diferente.
# E fiel ao que aconteceria, e mistura o efeito de n com o do sorteio -- que e
# justamente o que a figura anterior isola.
set.seed(20260908)
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
# Niveis declarados nos dois quadros, como manda o CLAUDE.md 5.14.
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
fig_salva("rls_inf_bandas_sim.pdf", p, largura = 5.2, altura = 1.5,
          alt = "Tres amostras de tamanhos crescentes, cada uma com seu proprio ajuste: o intervalo da media condicional encolhe de painel para painel, e o de previsao acompanha a dispersao dos pontos e nao estreita.")
library(ggplot2)
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
fig_salva("rls_inf_variancia_condicional.pdf", p, largura = 5.0, altura = 1.7,
          alt = "Oitenta curvas ajustadas de amostras diferentes, estreitas onde se cruzam, no centroide dos dados, e abertas em leque nas duas pontas, para fora da faixa cinza que marca o dominio observado.")
cat(sprintf("\nAs curvas se cruzam em $(\\bar X, \\bar Y) = (%s;\\, %s)$ e se abrem para os dois lados: o que difere entre amostras é a inclinação, e o efeito dela cresce com a distância ao centro. Fora da faixa sombreada, de $%s$ a $%s$ anos de estudo, o modelo é aplicado onde nada foi observado.\n",
            nm(a$mx, 2), nm(a$my, 2), ni(min(a$x)), ni(max(a$x))))
eq(dgp)
tb <- data.frame(
  Parâmetro = c("$\\beta_0$", "$\\beta_1$", "$\\sigma$"),
  verdadeiro = paste0("$", ni(unlist(a$dgp)), "$"),
  Estimado = paste0("$", nm(c(a$b0, a$b1, a$Se), 2), "$"),
  `Intervalo a 95\\%` = c(iv(a$ic0, 2), iv(a$ic1, 2), "---"),
  check.names = FALSE)
# A aspa tipografica nao passa por nome de argumento do data.frame (o backtick
# nao aninha), entao o rotulo e posto depois.
names(tb)[2] <- "``Verdadeiro''"
tab(tb, caption = "Parâmetros do processo gerador e suas estimativas",
    tamanho = "small")
co <- summary(mincer)$coefficients
ic <- confint(mincer)
eq(sprintf("\\widehat{\\log(\\text{sal\\'ario})} = \\underset{(%s)}{%s} %s \\underset{(%s)}{%s}\\,\\text{educ}",
           nm(co[1, 2], 4), nm(co[1, 1], 4), ifelse(co[2, 1] < 0, "-", "+"),
           nm(co[2, 2], 4), nm(abs(co[2, 1]), 4)))
tab(data.frame(
  Termo = c("Intercepto", "\\texttt{educ}"),
  Estimativa = nm(co[, 1], 4),
  `Erro padrão` = nm(co[, 2], 4),
  `$t$` = nm(co[, 3], 2),
  `$IC_{95\\%}$` = c(iv(ic[1, ], 3), iv(ic[2, ], 3)),
  check.names = FALSE),
    caption = "Equação de Mincer estimada sobre \\texttt{wage1}", tamanho = "small")
cat(sprintf("\n$n = %s$, $S_e = %s$, $R^2 = %s$.\n",
            ni(nobs(mincer)), nm(summary(mincer)$sigma, 3),
            nm(summary(mincer)$r.squared, 3)))
ic <- confint(mincer)["educ", ]
b1 <- coef(mincer)[["educ"]]
cat(sprintf(paste("Um ano a mais de estudo está associado a $%s\\%%$ a mais de salário, com intervalo a $95\\%%$ de $%s\\%%$ a $%s\\%%$.",
                  "O intervalo é estreito porque $n = %s$: o erro padrão cai com a raiz do tamanho da amostra.\n"),
            nm(100 * b1, 1), nm(100 * ic[1], 1), nm(100 * ic[2], 1),
            ni(nobs(mincer))))
library(ggplot2)
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
fig_salva("rls_inf_wage1_banda.pdf", p, largura = 5.0, altura = 1.6,
          alt = "Dispersao do logaritmo do salario contra anos de estudo em wage1, com a curva ajustada e um intervalo de confianca estreito.")
tab_serie("$X_i$" = e$x, "$Y_i$" = e$y,
          caption = "Dados do exercício", tamanho = "small")
cat(sprintf("\n\\begin{center}\\small $n = %s$, $\\bar X = %s$, $\\bar Y = %s$,\n$S_{XX} = %s$, $S_{XY} = %s$, $S_{YY} = %s$.\\end{center}\n",
            ni(e$n), ni(e$mx), ni(e$my), ni(e$Sxx), ni(e$Sxy), ni(e$Syy)))
library(ggplot2)
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
fig_salva("rls_inf_sem_intercepto.pdf", p, largura = 4.6, altura = 1.9,
          alt = "Pontos da amostra com tres retas: o processo gerador tracejado, o ajuste OLS completo e o ajuste forcado a passar pela origem, que sai mais inclinado.")
sem <- lm(a$y ~ 0 + a$x)
cat(sprintf("A inclinação passa de $%s$ para $%s$, contra $\\beta_1 = %s$ do processo gerador: forçada pela origem, ela absorve o intercepto.\n",
            nm(a$b1, 3), nm(unname(coef(sem)[1]), 3), ni(a$dgp$b1)))
eq(sprintf("\\hat\\beta_1 = %s = %s, \\qquad \\hat\\beta_0 = %s - %s \\times %s = %s.",
           frac(ni(e$Sxy), ni(e$Sxx)), ni(e$b1),
           ni(e$my), ni(e$b1), ni(e$mx), ni(e$b0)))
tab_serie("$X_i$" = e$x, "$\\hat Y_i$" = ni(e$aj), "$\\hat u_i$" = ns(e$u, 0),
          caption = "Valores ajustados e resíduos do exercício", tamanho = "small")
eq(sprintf("S_e^2 = %s = %s, \\qquad S_{\\hat\\beta_1} = \\sqrt{%s} = %s, \\qquad S_{\\hat\\beta_0} = %s.",
           frac(ni(e$RSS), ni(e$gl)), nm(e$Se2, 4),
           frac(nm(e$Se2, 4), ni(e$Sxx)), nm(e$Sb1, 4), nm(e$Sb0, 4)))
tab(data.frame(
  Coeficiente = c("$\\hat\\beta_1$", "$\\hat\\beta_0$"),
  Estimativa = paste0("$", ni(c(e$b1, e$b0)), "$"),
  `Erro padrão` = paste0("$", nm(c(e$Sb1, e$Sb0), 4), "$"),
  `$t$` = paste0("$", nm(c(e$t1, e$t0), 3), "$"),
  `Decisão a 5\\%` = c("rejeita-se $H_0$", "não se rejeita $H_0$"),
  check.names = FALSE),
    caption = "Testes de nulidade no exercício", tamanho = "small")
cat(sprintf("\nO valor crítico é $t_{0{,}025}(%s) = %s$. A diferença entre as duas decisões não está na estimativa, e sim no erro padrão: $S_{\\hat\\beta_0}$ é grande porque $\\bar X = %s$ está longe de zero.\n",
            ni(e$gl), nm(e$tc, 3), ni(e$mx)))
te <- e$econ
cat(sprintf("\\alert{Item 5.} $%s \\pm %s \\times %s = %s$. O intervalo não contém o zero, o que reproduz a rejeição do item 3.\n",
            ni(e$b1), nm(e$tc, 3), nm(e$Sb1, 4), iv(e$ic1, 3)))
cat(sprintf("\n\\alert{Item 6.} $t = (%s - 1)/%s = %s > %s = t_{0{,}05}(%s)$: rejeita-se $H_0$ a 5\\%%.\n",
            ni(e$b1), nm(e$Sb1, 4), nm(te$t, 3), nm(te$tc_uni, 3), ni(e$gl)))
pv <- e$prev
eq(sprintf("\\hat Y_0 = %s + %s \\times %s = %s, \\qquad \\frac{1}{%s} + \\frac{(%s - %s)^2}{%s} = %s.",
           ni(e$b0), ni(e$b1), ni(e$x0), ni(pv$aj),
           ni(e$n), ni(e$x0), ni(e$mx), ni(e$Sxx),
           nm(1 / e$n + (e$x0 - e$mx)^2 / e$Sxx, 2)))
eq(sprintf("%s: %s, \\qquad %s: %s.",
           "IC_{95\\%}(\\text{m\\'edia})", iv(pv$ic_med, 3),
           "IC_{95\\%}(\\text{previs\\~ao})", iv(pv$ic_prev, 3)))
cat(sprintf("\nO segundo é mais largo porque acrescenta $S_e^2$, a variância do erro da própria observação. Note ainda que $X_0 = %s$ está fora do domínio amostral, que termina em $%s$.\n",
            ni(e$x0), ni(max(e$x))))
