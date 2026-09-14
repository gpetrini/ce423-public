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
source("code/bloco_setup.R"); m <- ctx_metais()
## O data.frame do exemplo, em NIVEL, com nome proprio: os frames do
## apendice chamam `lm` diretamente, e o codigo exibido no slide tem de ser
## o mesmo que o executor roda -- com o logaritmo a vista, e nao escondido
## numa coluna ja transformada.
metais <- m$dados
## Recorte de uma serie longa, para que a tabela caiba no slide: as primeiras
## `k` observacoes, reticencia, e as ultimas `f`. As somas exibidas em qualquer
## frame sao SEMPRE da amostra inteira -- a tabela e que e truncada.
recorte <- function(v, k = 6, f = 2)
  c(as.character(v)[1:k], "$\\cdots$",
    as.character(v)[(length(v) - f + 1):length(v)])
## A curva ajustada, escrita uma vez. Tres frames a citam.
ajuste <- sprintf("\\widehat{\\log Y_i} = %s %s \\log L_i %s \\log K_i",
                  nm(m$b0, 2), ns(m$b1, 2), ns(m$b2, 2))
## O MESMO ajuste na forma multiplicativa, para o frame que revela o nome da
## forma funcional. Exponenciar os dois lados devolve o fator de escala,
## exp(b0), e converte os coeficientes em expoentes.
cobb <- sprintf("\\hat Y_i = %s \\, L_i^{%s} \\, K_i^{%s}",
                nm(exp(m$b0), 2), nm(m$b1, 2), nm(m$b2, 2))
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
## Os eixos sao os logaritmos, que e o que o modelo do frame regride.
paineis <- c("$\\log Y_i$ contra $\\log L_i$", "$\\log Y_i$ contra $\\log K_i$")
pl <- rbind(data.frame(x = m$x1, y = m$y, painel = paineis[1]),
            data.frame(x = m$x2, y = m$y, painel = paineis[2]))
pl$painel <- factor(pl$painel, levels = paineis)
p <- ggplot(pl, aes(x, y)) +
  geom_point(size = 1.5) +
  facet_wrap(~ painel, scales = "free_x") +
  labs(x = NULL, y = "$\\log Y_i$") +
  ## Sem `expand_limits(y = 0)`: o eixo e um logaritmo, e escala logaritmica e
  ## a excecao que o CLAUDE.md 5.20 declara. Forcar o zero empilharia as 27
  ## observacoes no topo do painel.
  theme_minimal(base_size = 9) +
  ## Figura baixa: o titulo do eixo vertical vai na horizontal, senao encosta
  ## nos numeros do eixo (CLAUDE.md 5.34).
  theme(strip.text = element_text(face = "bold", size = 7),
        axis.title.y = element_text(angle = 0, vjust = 1, margin = margin(r = 9)),
        panel.grid.minor = element_blank())
fig_salva("dispersao_firmas.pdf", p, largura = 5.0, altura = 1.35,
          alt = "Dois paineis de dispersao do logaritmo da producao contra o logaritmo do trabalho e contra o logaritmo do capital, ambos com associacao positiva e quase linear.")
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
  labs(x = "$\\ell_i = \\log L_i - \\bar L$", y = "$\\kappa_i = \\log K_i - \\bar K$", shape = NULL) +
  theme_minimal(base_size = 9) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
fig_salva("produto_cruzado_firmas.pdf", p, largura = 3.0, altura = 1.8,
          alt = "Dispersao dos desvios dos logaritmos de trabalho e de capital, com os quadrantes de produto positivo sombreados e os estados concentrados neles.")
## `:results output latex`, e nao raw: o resultado em cache de um bloco raw que
## e um paragrafo solto nao tem delimitador, o Org nao consegue apaga-lo, e a
## exportacao seguinte emite a frase DUAS vezes -- sem erro. Ver MISTAKES.md.
cat(sprintf("Nos estados, $S_{LK} = %s$ e a correlação entre trabalho e capital é $%s$, de modo que os dois \\alert{variam juntos}.\n",
            nm(m$S12, 1), nm(m$r12, 2)))
eq(sprintf("\\mathbf{X}^\\top\\mathbf{X} = %s, \\qquad \\mathbf{X}^\\top\\mathbf{y} = %s.",
           mat(m$XtX), mat(m$Xty)))
eq(sprintf("(\\mathbf{X}^\\top\\mathbf{X})^{-1} = %s",
           mat(m$XtXinv, fmt = function(z) nm(z, 4))))
## A seta marca a coordenada de cada parametro. A ordem e a das colunas de X,
## e por isso o intercepto vem primeiro.
eq(sprintf("%s = %s \\; \\begin{matrix} \\leftarrow \\hat\\beta_0 \\\\ \\leftarrow \\hat\\beta_1 \\\\ \\leftarrow \\hat\\beta_2 \\end{matrix}",
           "\\hat{\\bm\\beta} = (\\mathbf{X}^\\top\\mathbf{X})^{-1}\\mathbf{X}^\\top\\mathbf{y}",
           cx(mat(m$bvec, fmt = function(z) nm(z, 2)))))
eq(ajuste)
library(ggplot2)
## O subtitulo NAO e escrito a mao: sai dos quatro diagnosticos. Se algum
## rejeitar a 5%, o grafico pede diagnostico; se nenhum rejeitar, ele apenas
## descreve o que se ve. Assim a frase sobrevive a uma reestimacao (5.26).
aj <- lm(log(Y) ~ log(L) + log(K), data = m$dados)
dg <- diagnosticos(aj)
ps <- c(dg$bp$p.value, dg$dw$p.value, dg$sw$p.value, dg$reset$p.value)
## `if ... else` em linhas separadas no nivel superior de um bloco R e erro de
## sintaxe: o parser fecha a atribuicao na primeira linha. Numa linha so, nao.
subt <- if (min(ps) < 0.05) "O padrão dos resíduos pede diagnóstico" else "Sem padrão visível contra o ajustado"
d <- data.frame(aj = m$aj, u = m$u)
p <- ggplot(d, aes(aj, u)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.4) +
  geom_point(size = 1.6) +
  labs(x = "$\\widehat{\\log Y_i}$", y = "$\\hat u_i$", subtitle = subt) +
  theme_minimal(base_size = 9) +
  ## Figura baixa: o titulo do eixo vertical vai na horizontal (5.34).
  theme(axis.title.y = element_text(angle = 0, vjust = 1, margin = margin(r = 8)),
        plot.subtitle = element_text(size = 7, colour = "grey30"),
        panel.grid.minor = element_blank())
fig_salva("residuos_ajustado_firmas.pdf", p, largura = 4.6, altura = 1.45,
          alt = "Dispersao dos residuos contra os valores ajustados, com linha horizontal no zero.")
## Grafico de base, e nao ggplot2: o ggplot nao compoe tres dimensoes. O
## fig_salva aceita uma funcao justamente para este caso.
## Os tres eixos sao logaritmos, e por isso a folga da grade e 0,2 e nao 1: a
## amplitude dos logaritmos e de poucas unidades.
gl <- seq(min(m$x1) - 0.2, max(m$x1) + 0.2, length.out = 26)
gk <- seq(min(m$x2) - 0.2, max(m$x2) + 0.2, length.out = 26)
z  <- outer(gl, gk, function(a, b) m$b0 + m$b1 * a + m$b2 * b)
desenho <- function() {
  par(mar = c(0.2, 0.2, 0.2, 0.2))
  vt <- persp(gl, gk, z, theta = 38, phi = 22, expand = 0.62,
              col = "grey93", border = "grey65", ticktype = "detailed",
              nticks = 4, cex.axis = 0.5, cex.lab = 0.8,
              xlab = "$\\log L$", ylab = "$\\log K$",
              zlab = "$\\widehat{\\log Y}$")
  ## Haste vertical de cada observacao ate o plano: o segmento E o residuo.
  for (i in seq_len(m$n))
    lines(grDevices::trans3d(rep(m$x1[i], 2), rep(m$x2[i], 2),
                             c(m$aj[i], m$y[i]), vt), col = "grey15", lwd = 1.1)
  points(grDevices::trans3d(m$x1, m$x2, m$y, vt), pch = 19, cex = 0.75)
}
fig_salva("plano_ajustado_firmas.pdf", desenho, largura = 3.0, altura = 2.3,
          alt = "Plano ajustado sobre os eixos do logaritmo do trabalho e do logaritmo do capital, com os estados ligados ao plano por segmentos verticais.")
eq(ajuste)
eqs(sprintf("\\frac{\\partial \\widehat{\\log Y}}{\\partial \\log L} &= \\hat\\beta_1 = %s", nm(m$b1, 2)),
    sprintf("\\frac{\\partial \\widehat{\\log Y}}{\\partial \\log K} &= \\hat\\beta_2 = %s", nm(m$b2, 2)))
cat("\nCada derivada parcial mantém a outra variável fixa, e é isso que a palavra \\alert{parcial} nomeia.\n")
cat("\nDerivada de logaritmo contra logaritmo é \\alert{elasticidade}, e não tem unidade.\n")
cat(sprintf("\nO intercepto, $%s$, é o valor previsto de $\\log Y$ quando $\\log L = \\log K = 0$, isto é, para $L = K = 1$.\n",
            nm(m$b0, 2)))
Xs <- cbind(1, m$x1)
bs <- as.vector(solve(t(Xs) %*% Xs) %*% (t(Xs) %*% m$y))
eq(sprintf("\\hat{\\bm\\beta}^{\\text{simples}} = (\\mathbf{X}_s^\\top\\mathbf{X}_s)^{-1}\\mathbf{X}_s^\\top\\mathbf{y} = %s, \\qquad \\hat{\\bm\\beta} = %s.",
           mat(bs, fmt = function(z) nm(z, 2)),
           mat(m$bvec, fmt = function(z) nm(z, 2))))
tab(data.frame(
  Modelo = c("Só $L$", "$L$ e $K$"),
  `$\\hat\\beta_1$` = paste0("$", nm(c(m$b1s, m$b1), 2), "$"),
  `Mantém constante` = c("nada", "o estoque de capital"),
  check.names = FALSE),
    caption = "O coeficiente do trabalho nos dois modelos", tamanho = "scriptsize")
## O `%%` literal vira `\%%` no LaTeX, e dentro de sprintf ele dobra outra vez:
## um `%%` nao escapado comentaria o resto da linha no .tex (CLAUDE.md 5.9).
cat(sprintf("\\correctwrong{Correto}{``Mantendo o estoque de capital constante, $1\\%%$ a mais de trabalho está associado a %s\\%% a mais de produção.''}{Errado}{``Se este estado elevar o trabalho em $1\\%%$, sua produção sobe %s\\%%.''}\n",
            nm(m$b1, 2), nm(m$b1, 2)))
## Cada termo carrega o rotulo de um lado e o valor do outro: onde o texto vai
## em overbrace, o numero vai em underbrace, e vice-versa. Assim a equacao com
## os valores nao precisa ser repetida (pedido do professor, 2026-09-12).
## As chaves nao sao decorativas: sem elas o parser fecha a funcao na primeira
## expressao e o `else` fica orfao no script tangulado (MISTAKES.md 83).
termo <- function(simbolo, rotulo, valor, texto_acima) {
  if (texto_acima)
    sprintf("\\underbrace{\\overbrace{%s}^{\\text{%s}}}_{%s}", simbolo, rotulo, valor)
  else
    sprintf("\\overbrace{\\underbrace{%s}_{\\text{%s}}}^{%s}", simbolo, rotulo, valor)
}
eq(sprintf("%s, \\qquad \\delta = %s = %s.",
           cx(sprintf("%s = %s + %s",
                      termo("\\hat\\beta_1^{\\text{simples}}", "o que a RLS estima", nm(m$b1s, 2), TRUE),
                      termo("\\hat\\beta_1", "efeito parcial", nm(m$b1, 2), FALSE),
                      termo("\\hat\\beta_2 \\cdot \\delta", "vi\\'es de omiss\\~ao",
                            sprintf("%s \\times %s = %s",
                                    nm(m$b2, 2), nm(m$delta, 2), nm(m$b2 * m$delta, 2)),
                            FALSE))),
           frac("S_{LK}", "S_{LL}"), nm(m$delta, 2)))
library(ggplot2)
## A barra decompoe o coeficiente SIMPLES nas duas parcelas da identidade do
## frame anterior. Sem legenda: os dois rotulos ficam SOBRE os segmentos, que e
## mais direto e nao depende da largura da caixa de legenda.
##
## `position_stack(reverse = TRUE)` e obrigatorio: por omissao o ggplot empilha
## na ordem INVERSA dos niveis, de modo que o vies era desenhado a esquerda e as
## anotacoes caiam no segmento errado, com texto claro sobre fundo claro. So a
## renderizacao pega isso (verificado em 2026-09-12).
rot <- c("parcial", "vies")
d <- data.frame(parte = factor(rot, levels = rot),
                valor = c(m$b1, m$b2 * m$delta))
p <- ggplot(d, aes(x = valor, y = 1, fill = parte)) +
  ## `orientation = "y"` diz que as barras correm ao longo de x. Sem ele, com y
  ## continuo, o geom_col desenha barras VERTICAIS e o `limits` de y as recorta
  ## por inteiro: o slide sai com os rotulos e sem barra (verificado em
  ## 2026-09-12). E `position_stack(reverse = TRUE)` mantem a ordem dos niveis
  ## da esquerda para a direita.
  geom_col(width = 0.26, colour = "grey25", linewidth = 0.25,
           orientation = "y", position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = c("grey82", "#8B1A1A"), guide = "none") +
  annotate("text", x = m$b1 / 2, y = 1.28, size = 2.5, colour = "grey15",
           label = sprintf("$\\hat\\beta_1 = %s$", nm(m$b1, 2))) +
  annotate("text", x = m$b1 + (m$b2 * m$delta) / 2, y = 1.28, size = 2.5,
           colour = "#8B1A1A",
           label = sprintf("$\\hat\\beta_2\\delta = %s$", nm(m$b2 * m$delta, 2))) +
  annotate("text", x = m$b1s, y = 0.68, size = 2.5, hjust = 1, colour = "grey15",
           label = sprintf("soma $= %s$", nm(m$b1s, 2))) +
  scale_y_continuous(limits = c(0.6, 1.42), expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())
fig_salva("decomposicao_vies.pdf", p, largura = 2.9, altura = 1.55,
          alt = "Barra horizontal decompondo o coeficiente da regressao simples em efeito parcial, em cinza, e vies de omissao, em vermelho.")
## O sinal e a direcao saem dos numeros, e nao da prosa: com outra base, a
## frase se inverte sozinha (CLAUDE.md 5.26).
v <- m$b2 * m$delta
cat(sprintf("O viés é \\alert{%s}, e vale $%s$.\n\n",
            ifelse(v > 0, "positivo", "negativo"), ns(v, 2)))
cat(sprintf("Por isso a regressão simples \\alert{%s} o efeito do trabalho: ela estima $%s$ onde o efeito parcial é $%s$.\n",
            ifelse(v > 0, "superestima", "subestima"), nm(m$b1s, 2), nm(m$b1, 2)))
library(ggplot2)
## O eixo horizontal e o regressor OMITIDO, e e essa escolha que torna o vies
## visivel: o residuo do ajuste multiplo e ortogonal a log K por construcao, e
## o do ajuste simples carrega o efeito do capital que ficou de fora.
rls <- lm(m$y ~ m$x1)
rot <- c("RLM: $\\log L$ e $\\log K$", "RLS: só $\\log L$")
d <- rbind(data.frame(k = m$x2, u = m$u, modelo = rot[1]),
           data.frame(k = m$x2, u = as.numeric(residuals(rls)), modelo = rot[2]))
d$modelo <- factor(d$modelo, levels = rot)
## Dois paineis lado a lado, e nao um grafico so (pedido do professor): a
## comparacao fica no eixo comum, e cada nuvem e lida por si. O `facet_wrap`
## ordena alfabeticamente, entao os niveis do fator sao DECLARADOS acima --
## sem isso o painel da RLS viria primeiro e a prosa diria o contrario (5.14).
p <- ggplot(d, aes(k, u, colour = modelo)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.4) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.5) +
  geom_point(size = 1.6) +
  facet_wrap(~ modelo) +
  scale_colour_manual(values = c("grey25", "#8B1A1A"), guide = "none") +
  labs(x = "$\\log K_i$", y = "$\\hat u_i$") +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(face = "bold", size = 7),
        axis.title.y = element_text(angle = 0, vjust = 1, margin = margin(r = 8)),
        panel.grid.minor = element_blank())
fig_salva("residuos_rlm_rls.pdf", p, largura = 5.0, altura = 1.55,
          alt = "Dois paineis de residuos contra o logaritmo do capital, o do ajuste multiplo sem inclinacao e o do ajuste simples inclinado, cada um com a sua reta ajustada.")
yty <- sum(m$y^2); bXty <- as.numeric(t(m$bvec) %*% m$Xty); nY2 <- m$n * m$my^2
eq(sprintf("\\mathbf{y}^\\top\\mathbf{y} = %s, \\qquad \\hat{\\bm\\beta}^\\top\\mathbf{X}^\\top\\mathbf{y} = %s, \\qquad n\\bar Y^2 = %s.",
           nm(yty, 1), nm(bXty, 1), nm(nY2, 1)))
eq(sprintf("\\text{RSS} = \\hat{\\mathbf{u}}^\\top\\hat{\\mathbf{u}} = \\sum \\hat u_i^2 = %s, \\qquad S_e^2 = %s = %s = %s.",
           nm(m$RSS, 1), frac("\\text{RSS}", "n-k-1"),
           frac(nm(m$RSS, 1), sprintf("%s-%s-1", ni(m$n), ni(m$k))), nm(m$Se2, 2)))
cat(sprintf("\nCada equação normal impõe uma restrição sobre os resíduos, de modo que os %s parâmetros estimados consomem %s graus de liberdade, e sobram $%s$.\n",
            ni(m$k + 1), ni(m$k + 1), ni(m$gl)))
cat("\nNa regressão simples eram dois, e o denominador era $n-2$.\n")
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
library(ggplot2)
## Densidade F com os graus de liberdade do proprio exemplo. O dominio comeca
## em zero: com dois graus de liberdade no numerador a densidade e finita na
## origem, ao contrario do caso de um grau, que diverge (CLAUDE.md 5.20).
fc <- m$Fc
d <- data.frame(x = seq(0, 8, length.out = 600))
d$y <- df(d$x, m$k, m$gl)
p <- ggplot(d, aes(x, y)) +
  geom_area(data = subset(d, x >= fc), fill = "#8B1A1A") +
  geom_line(linewidth = 0.6) +
  geom_vline(xintercept = fc, linetype = "dashed", colour = "grey35") +
  annotate("text", x = fc, y = max(d$y) * 0.55, hjust = -0.12, size = 2.6,
           label = sprintf("$F_{%s;\\,%s;\\,%s} = %s$",
                           nm(m$alpha, 2), ni(m$k), ni(m$gl), nm(m$Fc, 2))) +
  labs(x = "$F$", y = NULL) +
  theme_minimal(base_size = 9) +
  theme(axis.text.y = element_blank(), panel.grid.minor = element_blank())
fig_salva("densidade_F_rlm.pdf", p, largura = 3.6, altura = 1.7,
          alt = "Densidade da distribuicao F sob a hipotese nula, com a regiao de rejeicao a direita do valor critico sombreada.")
cat(sprintf("\nA área sombreada vale $%s$, e é a probabilidade de rejeitar $H_0$ sendo ela verdadeira.\n",
            nm(m$alpha, 2)))
cat(sprintf("O $F$ do exemplo, $%s$, cai muito à direita do valor crítico, fora da escala do gráfico: $p < 0{,}001$.\n",
            nm(m$F, 1)))
eq(sprintf("R^2 = %s = %s = %s.", frac("\\text{ESS}", "\\text{TSS}"),
           frac(nm(m$ESS, 1), nm(m$TSS, 1)), nm(m$R2, 3)))
## O ajuste e o mesmo do corpo do deck: em logaritmos. Um `lm` em nivel aqui
## faria a tabela discordar da ANOVA dos frames anteriores, sem erro algum.
s1 <- summary(lm(log(Y) ~ log(L), data = metais))
s2 <- summary(lm(log(Y) ~ log(L) + log(K), data = metais))
tab(data.frame(
  Modelo = c("só $L$", "$L$ e $K$"),
  `$R^2$` = paste0("$", nm(c(s1$r.squared, s2$r.squared), 3), "$"),
  `$\\bar R^2$` = paste0("$", nm(c(s1$adj.r.squared, s2$adj.r.squared), 3), "$"),
  check.names = FALSE),
    caption = "$R^2$ e $R^2$ ajustado nos dois modelos", tamanho = "small")
## Erro padrao sob cada coeficiente. O sinal fica FORA do \underset: dentro
## dele o "+" vira atomo comum e o TeX suprime o espaco de operador binario
## (Lectures/CLAUDE.md 5.33).
s0 <- sqrt(m$Se2 * m$XtXinv[1, 1])
sn <- function(b) ifelse(b < 0, "-", "+")
comerro <- sprintf(
  "\\widehat{\\log Y_i} = \\underset{(%s)}{%s} %s \\underset{(%s)}{%s}\\,\\log L_i %s \\underset{(%s)}{%s}\\,\\log K_i",
  nm(s0, 2), nm(m$b0, 2),
  sn(m$b1), nm(m$Sb1, 2), nm(abs(m$b1), 2),
  sn(m$b2), nm(m$Sb2, 2), nm(abs(m$b2), 2))
eqs(comerro, cobb)
tab(data.frame(
  `Objeto estimado` = c("$\\hat\\beta_1$", "$\\hat\\beta_2$",
                        "$\\hat\\beta_1 + \\hat\\beta_2$", "$e^{\\hat\\beta_0}$"),
  Valor = paste0("$", nm(c(m$b1, m$b2, m$b1 + m$b2, exp(m$b0)), 2), "$"),
  `Leitura econômica` = c("elasticidade do produto ao trabalho",
                     "elasticidade do produto ao capital",
                     "retorno de escala",
                     "fator de escala"),
  check.names = FALSE),
    caption = "Leitura econômica dos coeficientes estimados", tamanho = "tiny")
cat("\nEntre parênteses, os \\alert{erros padrão} dos coeficientes --- objeto das próximas aulas.\n")
cw <- coef(multipla)
eq(sprintf("\\widehat{\\log(\\text{sal\\'ario})} = %s %s\\,\\text{educ} %s\\,\\text{exper} %s\\,\\text{tempo}",
           nm(cw[1], 3), ns(cw[2], 3), ns(cw[3], 4), ns(cw[4], 4)))
co <- summary(multipla)$coefficients
tab(data.frame(
  Termo = c("Intercepto", "\\texttt{educ}", "\\texttt{exper}", "\\texttt{tenure}"),
  Estimativa = nm(co[, 1], 4),
  `Erro padrão` = nm(co[, 2], 4),
  `$t$` = nm(co[, 3], 2),
  `$p$-valor` = ifelse(co[, 4] < 0.001, "$< 0{,}001$", paste0("$", nm(co[, 4], 3), "$")),
  check.names = FALSE),
    caption = "Modelo múltiplo estimado sobre \\texttt{wage1}", tamanho = "tiny")
cat(sprintf("\n$n = %s$, $R^2 = %s$, $\\bar R^2 = %s$.",
            ni(nobs(multipla)), nm(summary(multipla)$r.squared, 3),
            nm(summary(multipla)$adj.r.squared, 3)))
cw <- coef(multipla)
## Sem \alert{} aqui: dentro de matematica ele expande para \bfseries, que o
## LaTeX recusa em math mode. O destaque do coeficiente vive na prosa abaixo.
eq(sprintf("\\widehat{\\log(\\text{sal\\'ario})} = %s %s\\,\\text{educ} %s\\,\\text{exper} %s\\,\\text{tempo}",
           nm(cw[1], 3), ns(cw[2], 3), ns(cw[3], 4), ns(cw[4], 4)))
b1s <- coef(simples)[["educ"]]; b1m <- coef(multipla)[["educ"]]
cat(sprintf(paste("O coeficiente de \\texttt{educ} passa de $%s$, no modelo simples, a $%s$ na RLM.",
                  "\\alert{Mantendo constantes} a experiência e o tempo no emprego atual,",
                  "um ano a mais de estudo está associado a $%s\\%%$ a mais de salário.\n"),
            nm(b1s, 4), nm(b1m, 4), nm(100 * b1m, 1)))
## A ANOVA do ajuste multiplo de wage1, na mesma forma da tabela do corpo.
sw <- summary(multipla)
gl2 <- sw$df[2]; kw <- sw$df[1] - 1
RSSw <- sum(residuals(multipla)^2)
TSSw <- sum((log(wage1$wage) - mean(log(wage1$wage)))^2)
ESSw <- TSSw - RSSw
tab(data.frame(
  Fonte = c("Regressão", "Resíduo", "Total"),
  `g.l.` = c(ni(kw), ni(gl2), ni(kw + gl2)),
  `Soma de quadrados` = nm(c(ESSw, RSSw, TSSw), 2),
  `Quadrado médio` = c(nm(ESSw / kw, 2), nm(RSSw / gl2, 4), ""),
  check.names = FALSE),
    caption = "ANOVA do modelo de salários (\\texttt{wage1})", tamanho = "small")
cat(sprintf("\n$F = %s$, com $%s$ e $%s$ graus de liberdade.\n",
            nm(sw$fstatistic[1], 1), ni(kw), ni(gl2)))
library(ggplot2)
sw <- summary(multipla)
kw <- sw$df[1] - 1; gl2 <- sw$df[2]
Fo <- as.numeric(sw$fstatistic[1]); fcw <- qf(0.95, kw, gl2)
pw <- pf(Fo, kw, gl2, lower.tail = FALSE)
d <- data.frame(x = seq(0, 6, length.out = 600))
d$y <- df(d$x, kw, gl2)
p <- ggplot(d, aes(x, y)) +
  geom_area(data = subset(d, x >= fcw), fill = "#8B1A1A") +
  geom_line(linewidth = 0.6) +
  geom_vline(xintercept = fcw, linetype = "dashed", colour = "grey35") +
  annotate("text", x = fcw, y = max(d$y) * 0.55, hjust = -0.12, size = 2.6,
           label = sprintf("$F_{0{,}05;\\,%s;\\,%s} = %s$", ni(kw), ni(gl2), nm(fcw, 2))) +
  labs(x = "$F$", y = NULL) +
  theme_minimal(base_size = 9) +
  theme(axis.text.y = element_blank(), panel.grid.minor = element_blank())
fig_salva("densidade_F_wage.pdf", p, largura = 3.6, altura = 1.6,
          alt = "Densidade da distribuicao F de referencia do modelo de salarios, com a regiao de rejeicao sombreada.")
cat(sprintf("\n$H_0\\colon \\beta_{\\text{educ}} = \\beta_{\\text{exper}} = \\beta_{\\text{tempo}} = 0$, contra $H_1\\colon$ pelo menos um difere de zero. Com $F = %s$ contra o crítico $%s$, \\alert{rejeita-se $H_0$} ($p %s$).\n",
            nm(Fo, 1), nm(fcw, 2),
            if (pw < 0.001) "< 0{,}001" else paste("=", nm(pw, 3))))
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
## refaz a conta. As 27 linhas nao cabem na altura do slide, entao a serie e
## dobrada em TRES blocos de nove, lado a lado.
##
## A tabela traz o NIVEL, que e como o CSV guarda a base; o logaritmo com que o
## deck opera fica a um `log()` de distancia, e a convencao esta declarada no
## frame das somas de desvios. Uma versao com as seis colunas -- nivel e
## logaritmo -- foi medida em 2026-09-12 e descartada: catorze colunas saem
## 52 pt pela direita e 35 pt pelo rodape.
d <- m$dados; blocos <- split(seq_len(nrow(d)), rep(1:3, each = nrow(d) / 3))
corpo <- do.call(cbind, lapply(blocos, function(i)
  data.frame(Estado = ni(d$estado[i]), `$L_i$` = nm(d$L[i], 1),
             `$K_i$` = nm(d$K[i], 1), `$Y_i$` = nm(d$Y[i], 1),
             check.names = FALSE)))
names(corpo) <- rep(c("Estado", "$L_i$", "$K_i$", "$Y_i$"), 3)
tab(corpo, caption = sprintf("Indústria de metais primários (SIC 33) em %s estados, Greene (2003), Tabela F6.1",
                             ni(m$n)),
    align = paste(rep("r", 12), collapse = ""), tamanho = "scriptsize")
eq(sprintf("\\begin{cases} %s\\,\\hat\\beta_1 + %s\\,\\hat\\beta_2 = %s \\\\ %s\\,\\hat\\beta_1 + %s\\,\\hat\\beta_2 = %s \\end{cases} \\quad \\det = %s",
           nm(m$S11, 1), nm(m$S12, 1), nm(m$S1y, 1),
           nm(m$S12, 1), nm(m$S22, 1), nm(m$S2y, 1), nm(m$det, 1)))
eq(sprintf("\\hat\\beta_1 = %s = %s, \\qquad \\hat\\beta_2 = %s = %s.",
           frac("S_{KK} S_{LY} - S_{LK} S_{KY}", "\\det"), nm(m$b1, 2),
           frac("S_{LL} S_{KY} - S_{LK} S_{LY}", "\\det"), nm(m$b2, 2)))
eq(sprintf("\\hat\\beta_0 = \\bar Y - \\hat\\beta_1 \\bar L - \\hat\\beta_2 \\bar K = %s.",
           nm(m$b0, 2)))
sob <- function(termo, rotulo, valor)
  sprintf("\\underbrace{%s}_{\\substack{\\text{%s} \\\\[2pt] = %s}}",
          termo, rotulo, valor)
eq(sprintf("%s = %s + %s",
           sob("\\sum (Y_i - \\bar Y)^2", "TSS", nm(m$TSS, 1)),
           sob("\\sum (\\hat Y_i - \\bar Y)^2", "ESS", nm(m$ESS, 1)),
           sob("\\sum \\hat u_i^2", "RSS", nm(m$RSS, 1))))
av <- anova(lm(log(Y) ~ log(L) + log(K), data = metais))
tab(data.frame(
  ` ` = paste0("\\texttt{", rownames(av), "}"),
  `\\texttt{Df}` = ni(av$Df),
  `\\texttt{Sum Sq}` = nm(av$`Sum Sq`, 1),
  `\\texttt{Mean Sq}` = nm(av$`Mean Sq`, 1),
  `\\texttt{F value}` = c(nm(av$`F value`[1:2], 1), ""),
  check.names = FALSE),
    caption = "Decomposição sequencial que o R produz", tamanho = "small")
av <- anova(lm(log(Y) ~ log(L) + log(K), data = metais)); sq <- av$`Sum Sq`
eq(sprintf("%s = %s = \\text{ESS}.",
           paste(nm(sq[1:2], 1), collapse = " + "), nm(m$ESS, 1)))
cat(sprintf("\nA primeira parcela é a ESS da regressão \\alert{simples} de $Y$ contra $L$, que vale $S_{LY}^2/S_{LL}$.\n"))
fs <- summary(lm(log(Y) ~ log(L) + log(K), data = metais))$fstatistic
cat(sprintf("\\texttt{value} $= %s$, \\texttt{numdf} $= %s$, \\texttt{dendf} $= %s$ --- o $F$ global vive no \\texttt{summary}, e não em \\texttt{anova}.\n",
            nm(fs[["value"]], 1), ni(fs[["numdf"]]), ni(fs[["dendf"]])))
a1 <- anova(lm(log(Y) ~ log(L) + log(K), data = metais))
a2 <- anova(lm(log(Y) ~ log(K) + log(L), data = metais))
tab(data.frame(
  Linha = c("1ª", "2ª", "Res."),
  ## Os rotulos sao nomes de termo do R, e vao em \texttt: em modo matematico
  ## `Residuals` sai como produto de letras italicas, sem espacamento.
  ## O cabecalho traz so a ORDEM de entrada, que e o assunto do frame: a
  ## formula inteira em logaritmos nao cabe na largura de meia tabela.
  `\\texttt{log(L) + log(K)}` = sprintf("\\texttt{%s}: $%s$", rownames(a1), nm(a1$`Sum Sq`, 3)),
  `\\texttt{log(K) + log(L)}` = sprintf("\\texttt{%s}: $%s$", rownames(a2), nm(a2$`Sum Sq`, 3)),
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
eq(sprintf("\\mathbf{X}^\\top\\mathbf{X} = %s, \\qquad \\mathbf{X}^\\top\\mathbf{y} = %s.",
           mat(q$XtX), mat(q$Xty)))
eq(sprintf("\\begin{cases} %s\\,\\hat\\beta_1 + %s\\,\\hat\\beta_2 = %s \\\\ %s\\,\\hat\\beta_1 + %s\\,\\hat\\beta_2 = %s \\end{cases} \\qquad \\det = %s",
           ni(q$S11), ni(q$S12), ni(q$S1y), ni(q$S12), ni(q$S22), ni(q$S2y), ni(q$det)))
eq(sprintf("%s, \\qquad \\hat{\\mathbf{u}} = %s, \\qquad \\mathbf{X}^\\top\\hat{\\mathbf{u}} = %s.",
           cx(sprintf("\\hat{\\bm\\beta} = %s", mat(q$bvec))),
           mat(q$uvec), mat(round(t(q$X) %*% q$uvec))))
