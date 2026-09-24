# ---------------------------------------------------------------------------
# CE423 - Econometria I
# 1. Regressao Linear Simples - Linearidade por Anamorfose
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
library(ggplot2); library(ggforce); library(wooldridge)
d <- dados("rls_curvatura.csv"); a <- rls(d$X, d$Y); a$dados <- d
a$dgp <- list(b0 = 20, b1 = 0, b2 = 1, sigma = 8)
a$quad <- lm(Y ~ X + I(X^2), data = d)
a$diag <- diagnosticos(lm(Y ~ X, data = d))
dp <- dados("rls_potencia.csv")
e <- rls(log2(dp$X), log2(dp$Y)); e$dados <- dp
e$b1_ln <- coef(lm(log(dp$Y) ~ log(dp$X)))[[2]]
k <- 6; f <- 3
## Cabeca e cauda da amostra: as primeiras sustentam a conta a mao e as ultimas
## mostram que a serie continua, o que a tabela truncada sozinha nao diz.
recorte <- function(v) c(ni(v[1:k]), "$\\cdots$", ni(v[(a$n - f + 1):a$n]))
k <- 6; f <- 3
recorte <- function(v) c(ni(v[1:k]), "$\\cdots$", ni(v[(a$n - f + 1):a$n]))
k <- 3
library(ggforce)
R2 <- a$R2

## Dois circulos: o de Y tem raio 1. O raio do de X NAO pode ser arbitrario --
## a area da lente nunca excede a area do menor circulo, entao um rX fixo torna
## o problema insoluvel sempre que R2 for alto. Deriva-se rX do proprio R2, com
## folga sobre o minimo geometrico sqrt(R2)*rY. A distancia entre centros e
## resolvida numericamente para que a AREA da intersecao seja exatamente R2
## vezes a area do circulo de Y.
rY <- 1; rX <- max(0.6, 1.25 * sqrt(R2) * rY)
area_lente <- function(d) {
  if (d >= rY + rX) return(0)
  if (d <= abs(rY - rX)) return(pi * min(rY, rX)^2)
  rY^2 * acos((d^2 + rY^2 - rX^2) / (2 * d * rY)) +
    rX^2 * acos((d^2 + rX^2 - rY^2) / (2 * d * rX)) -
    0.5 * sqrt((-d + rY + rX) * (d + rY - rX) * (d - rY + rX) * (d + rY + rX))
}
dd <- uniroot(function(d) area_lente(d) - R2 * pi * rY^2,
              lower = abs(rY - rX) + 1e-6, upper = rY + rX - 1e-6)$root

## Contorno da lente: arco do circulo de Y entre os dois pontos de intersecao,
## seguido do arco do circulo de X entre os mesmos dois pontos.
xi <- (dd^2 + rY^2 - rX^2) / (2 * dd); yi <- sqrt(rY^2 - xi^2)
al <- acos(xi / rY); be <- atan2(yi, xi - dd)
lente <- data.frame(x = c(rY * cos(seq(-al, al, length.out = 300)),
                          dd + rX * cos(seq(be, 2 * pi - be, length.out = 300))),
                    y = c(rY * sin(seq(-al, al, length.out = 300)),
                          rX * sin(seq(be, 2 * pi - be, length.out = 300))))
## A parte de Y fora da lente e o RSS: o circulo de Y menos a lente, desenhado
## por cima em cor propria e depois recoberto pela lente.
disco_y <- data.frame(x = rY * cos(seq(0, 2 * pi, length.out = 400)),
                      y = rY * sin(seq(0, 2 * pi, length.out = 400)))
circulos <- data.frame(x0 = c(0, dd), y0 = c(0, 0), r = c(rY, rX))

cor_ess <- "#8B1A1A"; cor_rss <- "#BFBFBF"
rotulos <- data.frame(
  x = c(-rY - 0.10, dd + rX + 0.10, (xi + dd - rX) / 2, -rY - 0.95),
  y = c(rY + 0.24, rY + 0.24, 0, -rY - 0.12),
  texto = c("$Y$", "$X$", "ESS", "RSS"))

p <- ggplot() +
  geom_polygon(data = disco_y, aes(x, y), fill = cor_rss, colour = NA) +
  geom_polygon(data = lente, aes(x, y), fill = cor_ess, colour = NA) +
  geom_circle(data = circulos, aes(x0 = x0, y0 = y0, r = r),
              colour = "grey25", linewidth = 0.4, inherit.aes = FALSE) +
  geom_segment(aes(x = -rY - 0.72, y = -rY + 0.02, xend = -rY + 0.10, yend = -0.20),
               colour = "grey40", linewidth = 0.3) +
  geom_text(data = rotulos, aes(x, y, label = texto), size = 3.4,
            colour = c("grey25", "grey25", "white", "grey25")) +
  expand_limits(x = c(-rY - 1.15, dd + rX + 0.55),
                y = c(-rY - 0.55, rY + 0.55)) +
  coord_fixed() +
  theme_void(base_size = 11)
## Densidade F sob H0 com os graus de liberdade do proprio exemplo. Com um
## regressor o numerador tem 1 grau de liberdade, e por isso a curva e
## monotona decrescente em vez do corcovado que se ve nos livros -- desenhar o
## corcovado aqui seria desenhar outro teste.
fc <- qf(1 - a$alpha, 1, a$gl)
dg <- data.frame(x = seq(0.4, 8, length.out = 600))
dg$y <- df(dg$x, 1, a$gl)

## O eixo vertical NAO e cortado. O dominio comeca em 0,4 em vez de 0: com um
## grau de liberdade no numerador a densidade diverge na origem, e incluir a
## vizinhanca de zero levaria o eixo a uma altura em que todo o resto some.
## Comecar depois do joelho e diferente de truncar -- a curva desenhada e
## inteira dentro do dominio mostrado.
pv <- pf(a$F, 1, a$gl, lower.tail = FALSE)
p <- ggplot(dg, aes(x, y)) +
  geom_area(data = subset(dg, x >= fc), fill = "#8B1A1A") +
  geom_line(linewidth = 0.6) +
  geom_vline(xintercept = fc, linetype = "dashed", colour = "grey35") +
  annotate("text", x = fc, y = max(dg$y) * 0.86, hjust = -0.08, size = 2.3,
           label = sprintf("$F_{%s}(1,%s) = %s$", nm(a$alpha, 2), a$gl, nm(fc, 2))) +
  labs(x = "$F$", y = NULL,
       caption = sprintf("$F$ observado $= %s$, fora da escala: $p %s$.",
                         nm(a$F, 2),
                         if (pv < 0.001) "< 0{,}001" else paste("=", nm(pv, 3)))) +
  theme_minimal(base_size = 9) +
  theme(axis.text.y = element_blank(), panel.grid.minor = element_blank(),
        plot.caption = element_text(size = 5.6, hjust = 0.5))
fc <- qf(1 - a$alpha, 1, a$gl)
## A decisao e derivada da comparacao, nunca escrita a mao: se os dados forem
## reestimados e o F mudar de lado, a frase muda junto em vez de mentir.
decisao <- if (a$F > fc) "rejeita-se" else "não se rejeita"
## A amostra inteira, em pe, dobrada em dois blocos lado a lado: vinte linhas
## empilhadas nao cabem na altura do frame, e truncar esconderia justamente a
## troca de sinal nas pontas, que e o ponto do slide.
m <- a$n %/% 2
## Os dois paineis saem da MESMA amostra: a esquerda o ajuste linear, que erra a
## forma; a direita o ajuste quadratico, que e a forma do processo gerador. A
## comparacao so e honesta assim -- trocar de amostra entre os paineis
## confundiria efeito da forma com efeito dos dados.
## Ordem por factor: o facet_wrap ordena alfabeticamente, e o painel discutido
## primeiro no texto cairia a direita.
paineis <- c("Ajuste linear: forma errada", "Alternativa: ajuste quadrático")
df <- rbind(
  data.frame(ajustado = a$aj, residuo = a$u, painel = paineis[1]),
  data.frame(ajustado = fitted(a$quad), residuo = resid(a$quad), painel = paineis[2]))
df$painel <- factor(df$painel, levels = paineis)

p <- ggplot(df, aes(ajustado, residuo)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 1.8) +
  facet_wrap(~ painel, scales = "free") +
  labs(x = "$\\hat Y_i$", y = "$\\hat u_i$") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))
semente(423)
n <- 60; x <- seq(1, 10, length.out = n)
u_curva <- (x - 5.5)^2 / 6 - 2.4 + rnorm(n, sd = 0.35)
u_leque <- rnorm(n, sd = 0.18 * x)
u_serie <- as.numeric(filter(rnorm(n, sd = 0.7), 0.85, method = "recursive"))

df <- rbind(
  data.frame(x = x, u = u_curva, painel = "$U$ ou $\\cap$"),
  data.frame(x = x, u = u_leque, painel = "Leque abrindo"),
  data.frame(x = x, u = u_serie, painel = "Sinais iguais em sequência"))
df$painel <- factor(df$painel, levels = unique(df$painel))

## Tres paineis empilhados em pouca altura: os numeros dos eixos colidiam com o
## painel vizinho. Saem, porque aqui o que se le e a forma da nuvem, nao o
## valor de nenhum ponto.
p <- ggplot(df, aes(x, u)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 0.9) +
  facet_wrap(~ painel, nrow = 3, scales = "free_y") +
  labs(x = "$\\hat Y_i$", y = "$\\hat u_i$") +
  theme_minimal(base_size = 8) +
  theme(strip.text = element_text(face = "bold", size = 6.5),
        axis.text = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0.35, "lines"))
## Os niveis do fator sao declarados: o facet_wrap ordena alfabeticamente, e
## "Alternativa" vinha antes de "Na escala original", pondo o painel
## transformado a esquerda -- o inverso da ordem em que o texto os apresenta.
paineis <- c("Na escala original: $Y$ contra $X$", "Alternativa: $Y$ contra $Z = X^2$")
painel <- rbind(
  data.frame(v = d$X,   Y = d$Y, painel = paineis[1]),
  data.frame(v = d$X^2, Y = d$Y, painel = paineis[2]))
painel$painel <- factor(painel$painel, levels = paineis)
p <- ggplot(painel, aes(v, Y)) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey45", linewidth = 0.7,
              formula = y ~ x) +
  geom_point(size = 2.6) +
  facet_wrap(~ painel, scales = "free_x") +
  labs(x = NULL, y = "$Y$") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"))
## O comportamento da curva vai no rotulo do painel, nao numa frase abaixo do
## grafico: quem olha a figura le a classificacao no mesmo lugar em que ve a
## forma. A linha de baixo e a mesma familia na escala log-log, onde as tres
## viram retas -- e a anamorfose executada, nao descrita.
dg <- curva_forma("log-log", c("$\\beta_1 > 1$: acelera" = 1.7,
                              "$0 < \\beta_1 < 1$: satura" = 0.45,
                              "$\\beta_1 < 0$: decresce" = -0.8))
escalas <- c("Escala original", "Escala log-log")
dg <- rbind(transform(dg, v = x,      w = y,      escala = escalas[1]),
           transform(dg, v = log(x), w = log(y), escala = escalas[2]))
dg$escala <- factor(dg$escala, levels = escalas)

p <- ggplot(dg, aes(v, w)) +
  geom_line(linewidth = 0.8) +
  facet_grid(escala ~ painel, scales = "free") +
  labs(x = NULL, y = NULL,
       caption = "Na escala log-log as três viram retas: é a anamorfose.") +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(face = "bold", size = 6.5),
        axis.text = element_blank(), panel.grid.minor = element_blank())
## Mesma construcao do log-log: em cima a forma original, embaixo a escala em
## que ela vira reta. Aqui so o eixo vertical e transformado.
dg <- curva_forma("log-lin", c("$\\beta_1 > 0$: cresce" = 0.55,
                              "$\\beta_1 < 0$: decai" = -0.55))
escalas <- c("Escala original", "$\\log Y$ contra $X$")
dg <- rbind(transform(dg, w = y,      escala = escalas[1]),
           transform(dg, w = log(y), escala = escalas[2]))
dg$escala <- factor(dg$escala, levels = escalas)

p <- ggplot(dg, aes(x, w)) +
  geom_line(linewidth = 0.8) +
  facet_grid(escala ~ painel, scales = "free") +
  labs(x = NULL, y = NULL,
       caption = "Com $\\log Y$ no eixo vertical, as duas viram retas: é a anamorfose.") +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(face = "bold", size = 6.5),
        axis.text = element_blank(), panel.grid.minor = element_blank())
dg <- curva_forma("lin-log", c("$\\beta_1 > 0$" = 1.4, "$\\beta_1 < 0$" = -1.4))
p <- ggplot(dg, aes(x, y)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ painel, scales = "free_y") +
  labs(x = "$X$", y = "$Y$",
       caption = NULL) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"),
        axis.text = element_blank(), panel.grid.minor = element_blank())
dg <- curva_forma("reciproco", c("$\\beta_1 > 0$" = 1.6, "$\\beta_1 < 0$" = -1.6))
p <- ggplot(dg, aes(x, y)) +
  geom_hline(yintercept = 3, linetype = "dashed", colour = "grey55") +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ painel, scales = "free_y") +
  labs(x = "$X$", y = "$Y$",
       caption = "A linha tracejada é $\\beta_0$: o valor de que $Y$ se aproxima sem alcançar.") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"),
        axis.text = element_blank(), panel.grid.minor = element_blank())
b <- coef(a$quad); xs <- -b[2] / (2 * b[3])
## Dentro ou fora da faixa observada e uma comparacao, nunca uma afirmacao
## escrita a mao: se a amostra for regerada e o ponto critico mudar de lado, a
## frase muda junto.
dentro <- xs >= min(d$X) && xs <= max(d$X)
g <- a$dgp; b <- coef(a$quad)
g <- a$dgp
grade <- data.frame(X = seq(min(d$X), max(d$X), length.out = 200))
grade$dgp <- g$b0 + g$b2 * grade$X^2
grade$lin <- a$b0 + a$b1 * grade$X
grade$qua <- predict(a$quad, newdata = grade)

curvas <- rbind(
  data.frame(X = grade$X, Y = grade$dgp, curva = "Processo gerador"),
  data.frame(X = grade$X, Y = grade$lin, curva = "Ajuste linear"),
  data.frame(X = grade$X, Y = grade$qua, curva = "Ajuste quadrático"))
curvas$curva <- factor(curvas$curva,
                       levels = c("Processo gerador", "Ajuste linear", "Ajuste quadrático"))

p <- ggplot() +
  geom_point(data = d, aes(X, Y), size = 1.6, colour = "grey35") +
  geom_line(data = curvas, aes(X, Y, colour = curva, linetype = curva),
            linewidth = 0.8) +
  scale_colour_manual(values = c("Processo gerador" = "#8B1A1A",
                                 "Ajuste linear" = "grey45",
                                 "Ajuste quadrático" = "#1A4C8B")) +
  scale_linetype_manual(values = c("Processo gerador" = "dashed",
                                   "Ajuste linear" = "solid",
                                   "Ajuste quadrático" = "solid")) +
  labs(x = "$X$", y = "$Y$", colour = NULL, linetype = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", legend.key.width = unit(1.1, "cm"))
data("wage1")
escalas <- c("Salário em nível", "Log do salário")
painel <- rbind(
  data.frame(x = wage1$educ, y = wage1$wage,      escala = escalas[1]),
  data.frame(x = wage1$educ, y = log(wage1$wage), escala = escalas[2]))
painel$escala <- factor(painel$escala, levels = escalas)
p <- ggplot(painel, aes(x, y)) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey25", linewidth = 0.8,
              formula = y ~ x) +
  facet_wrap(~ escala, scales = "free_y") +
  labs(x = "Anos de escolaridade", y = NULL,
       caption = "A transformação muda o que o ajuste linear consegue explicar.") +
  theme_minimal(base_size = 9) +
  ## base_size 12 num grafico de 5,2 polegadas fazia os numeros do eixo
  ## vertical do painel esquerdo colidirem entre si e com o rotulo do eixo
  ## horizontal.
  theme(strip.text = element_text(face = "bold", size = 7.5),
        axis.text = element_text(size = 6),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(size = 6))
data("wage1")
mincer <- lm(log(wage) ~ educ, data = wage1)
s <- summary(mincer)$coefficients
data("wage1")
m <- lm(log(wage) ~ educ, data = wage1)
b1 <- coef(m)[["educ"]]; b0 <- coef(m)[["(Intercept)"]]
dg <- dados("rls_potencia.csv")
dg$rotulo <- sprintf("$(%s,\\,%s)$", ni(dg$X), ni(dg$Y))
## A tabela foi omitida de proposito: com cinco pontos, o rotulo sobre cada um
## carrega o mesmo par ordenado e libera o espaco vertical do frame.
## Colocacao a mao, e nao pelo ggrepel: o rotulo do primeiro ponto vai ABAIXO
## dele, porque acima ele encostava no proprio marcador; os demais ficam acima.
## A mao e deterministico -- o ggrepel reposiciona conforme o tamanho final do
## dispositivo, e a posicao mudava entre exportacoes.
## O primeiro ponto leva o rotulo abaixo; o segundo e o terceiro ficam quase na
## mesma altura e por isso saem um para cada lado, senao os rotulos se tocam.
dg$vjust <- c( 1.9, -0.6, -0.6, -0.9, -0.9)
dg$hjust <- c( 0.5,  1.0,  0.0,  0.5,  0.5)
p <- ggplot(dg, aes(X, Y)) +
  geom_point(size = 2.4) +
  geom_text(aes(label = rotulo, vjust = vjust, hjust = hjust), size = 2.5) +
  scale_y_continuous(expand = expansion(mult = c(0.16, 0.18))) +
  scale_x_continuous(expand = expansion(mult = c(0.14, 0.16))) +
  labs(x = "$X$ (insumo)", y = "$Y$ (produção)") +
  theme_minimal(base_size = 10)
itens <- c(
  sprintf("No ajuste linear do exemplo desta aula, o $R^2$ de $%s$ indica que a forma funcional escolhida é adequada.",
          nm(a$R2, 3)),
  "No modelo $\\log Y = \\beta_0 + \\beta_1 X + u$, a quantidade $100\\,\\hat\\beta_1$ aproxima a variação percentual de $Y$ associada a uma unidade adicional de $X$.",
  "Comparar o $R^2$ de $Y = \\beta_0+\\beta_1 X + u$ com o de $\\log Y = \\beta_0 + \\beta_1 X + u$ permite escolher entre as duas formas funcionais.",
  "O modelo $Y = \\beta_0 + \\beta_1 X + \\beta_2 X^2 + u$ não pode ser estimado por OLS, por não ser linear.",
  "Em $Y = A X^{\\beta_1} e^{u}$, o logaritmo produz um modelo linear nos parâmetros; em $Y = A X^{\\beta_1} + u$, a mesma transformação não produz.")
g <- a$diag
marca <- function(p, limite = 0.05) if (p < limite) "\\tikzxmark" else "\\checkmark"
m <- lm(Y ~ X, data = a$dados)
## RESET rejeita: a curvatura omitida entra como potencia do ajustado.
## Durbin-Watson rejeita, e nao por dependencia temporal -- as observacoes estao
## ordenadas por X, e os residuos de um ajuste curvo ordenados assim ficam
## positivos nas pontas e negativos no meio, o que o DW le como correlacao.
## Breusch-Pagan e Shapiro-Wilk nao rejeitam: o erro do processo gerador e de
## fato homoscedastico e normal, e a ma especificacao nao os destroi.
print(lmtest::resettest(m, power = 2, type = "regressor"))
print(lmtest::bptest(m)); print(lmtest::dwtest(m)); print(shapiro.test(resid(m)))
