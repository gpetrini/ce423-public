source("code/bloco_setup.R")
d <- dados("rls_curvatura.csv"); a <- rls(d$X, d$Y); a$dados <- d
a$dgp <- list(b0 = 20, b1 = 0, b2 = 1, sigma = 8)
a$quad <- lm(Y ~ X + I(X^2), data = d)
a$diag <- diagnosticos(lm(Y ~ X, data = d))
k <- 6; f <- 3
## Cabeca e cauda da amostra: as primeiras sustentam a conta a mao e as ultimas
## mostram que a serie continua, o que a tabela truncada sozinha nao diz.
recorte <- function(v) c(ni(v[1:k]), "$\\cdots$", ni(v[(a$n - f + 1):a$n]))
tab_serie("$X_i$" = recorte(a$x), "$Y_i$" = recorte(a$y),
          caption = sprintf("Base de dados sintética ($n = %s$)", ni(a$n)))

source("code/bloco_setup.R")
d <- dados("rls_curvatura.csv"); a <- rls(d$X, d$Y); a$dados <- d
a$dgp <- list(b0 = 20, b1 = 0, b2 = 1, sigma = 8)
a$quad <- lm(Y ~ X + I(X^2), data = d)
a$diag <- diagnosticos(lm(Y ~ X, data = d))
cat(sprintf("Denotando desvios por minúsculas ($x_i = X_i - \\bar X$), com $\\bar X = %s$ e $\\bar Y = %s$:\n", ni(a$mx), ni(a$my)))
eq(sprintf("S_{XX} = \\sum x_i^2 = %s, \\qquad S_{XY} = \\sum x_i y_i = %s, \\qquad S_{YY} = \\sum y_i^2 = %s.",
           ni(a$Sxx), ni(a$Sxy), ni(a$Syy)))

source("code/bloco_setup.R")
d <- dados("rls_curvatura.csv"); a <- rls(d$X, d$Y); a$dados <- d
a$dgp <- list(b0 = 20, b1 = 0, b2 = 1, sigma = 8)
a$quad <- lm(Y ~ X + I(X^2), data = d)
a$diag <- diagnosticos(lm(Y ~ X, data = d))
eq(sprintf("\\hat\\beta_1 = %s = %s = %s, \\qquad \\hat\\beta_0 = \\bar Y - \\hat\\beta_1 \\bar X = %s - %s = %s.",
           frac("S_{XY}", "S_{XX}"), frac(ni(a$Sxy), ni(a$Sxx)), ni(a$b1),
           ni(a$my), ni(a$b1 * a$mx), ni(a$b0)))
eq(cx(sprintf("\\hat Y_i = %s %s\\,X_i", ni(a$b0), ns(a$b1, 0))))

source("code/bloco_setup.R")
d <- dados("rls_curvatura.csv"); a <- rls(d$X, d$Y); a$dados <- d
a$dgp <- list(b0 = 20, b1 = 0, b2 = 1, sigma = 8)
a$quad <- lm(Y ~ X + I(X^2), data = d)
a$diag <- diagnosticos(lm(Y ~ X, data = d))
k <- 6; f <- 3
recorte <- function(v) c(ni(v[1:k]), "$\\cdots$", ni(v[(a$n - f + 1):a$n]))
tab_serie("$X_i$" = recorte(a$x),
          "$\\hat Y_i$" = recorte(a$aj),
          "$\\hat u_i$" = recorte(a$u),
          caption = "Valores ajustados e resíduos")

source("code/bloco_setup.R")
d <- dados("rls_curvatura.csv"); a <- rls(d$X, d$Y); a$dados <- d
a$dgp <- list(b0 = 20, b1 = 0, b2 = 1, sigma = 8)
a$quad <- lm(Y ~ X + I(X^2), data = d)
a$diag <- diagnosticos(lm(Y ~ X, data = d))
k <- 3
## As vinte parcelas nao cabem na largura do slide. Ficam as tres primeiras e a
## ultima, que bastam para exibir a forma da soma; o total e o da amostra
## inteira.
eq(sprintf("\\sum \\hat u_i^2 = %s + \\cdots + %s = %s.",
           paste(sprintf("(%s)^2", ni(a$u[1:k])), collapse = " + "),
           sprintf("(%s)^2", ni(a$u[a$n])), ni(a$RSS)))

source("code/bloco_setup.R")
d <- dados("rls_curvatura.csv"); a <- rls(d$X, d$Y); a$dados <- d
a$dgp <- list(b0 = 20, b1 = 0, b2 = 1, sigma = 8)
a$quad <- lm(Y ~ X + I(X^2), data = d)
a$diag <- diagnosticos(lm(Y ~ X, data = d))
eq(sprintf("%s \\qquad S_e \\approx %s.",
           cx(sprintf("S_e^2 = %s = %s \\approx %s",
                      frac("\\sum \\hat u_i^2", "n-2"), frac(ni(a$RSS), ni(a$gl)), nm(a$Se2, 2))),
           nm(a$Se, 2)))

source("code/bloco_setup.R")
d <- dados("rls_curvatura.csv"); a <- rls(d$X, d$Y); a$dados <- d
a$dgp <- list(b0 = 20, b1 = 0, b2 = 1, sigma = 8)
a$quad <- lm(Y ~ X + I(X^2), data = d)
a$diag <- diagnosticos(lm(Y ~ X, data = d))
eq(sprintf("S_{\\hat\\beta_1} = \\sqrt{%s} = \\sqrt{%s} = %s",
           frac("S_e^2", "S_{XX}"), frac(nm(a$Se2, 1), ni(a$Sxx)), nm(a$Sb1, 3)))
cat(sprintf("\nCom $t_{0{,}025}(%s) = %s$:\n", ni(a$gl), nm(a$tc, 3)))
eq(cx(sprintf("%s \\pm %s \\times %s = %s",
              nm(a$b1, 2), nm(a$tc, 3), nm(a$Sb1, 3), iv(a$ic1, 2))))

source("code/bloco_setup.R")
d <- dados("rls_curvatura.csv"); a <- rls(d$X, d$Y); a$dados <- d
a$dgp <- list(b0 = 20, b1 = 0, b2 = 1, sigma = 8)
a$quad <- lm(Y ~ X + I(X^2), data = d)
a$diag <- diagnosticos(lm(Y ~ X, data = d))
cat(sprintf("No exemplo, $\\text{RSS} = %s$, e portanto\n", ni(a$RSS)))
eq(sprintf("%s = %s + %s.", ni(a$TSS), ni(a$ESS), ni(a$RSS)))

source("code/bloco_setup.R")
d <- dados("rls_curvatura.csv"); a <- rls(d$X, d$Y); a$dados <- d
a$dgp <- list(b0 = 20, b1 = 0, b2 = 1, sigma = 8)
a$quad <- lm(Y ~ X + I(X^2), data = d)
a$diag <- diagnosticos(lm(Y ~ X, data = d))
library(ggplot2); library(ggforce)
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
              colour = "grey25", size = 0.4, inherit.aes = FALSE) +
  geom_segment(aes(x = -rY - 0.72, y = -rY + 0.02, xend = -rY + 0.10, yend = -0.20),
               colour = "grey40", linewidth = 0.3) +
  geom_text(data = rotulos, aes(x, y, label = texto), size = 3.4,
            colour = c("grey25", "grey25", "white", "grey25")) +
  expand_limits(x = c(-rY - 1.15, dd + rX + 0.55),
                y = c(-rY - 0.55, rY + 0.55)) +
  coord_fixed() +
  theme_void(base_size = 11)
fig_salva("venn_anova.pdf", p, largura = 2.7, altura = 1.55,
          alt = "Dois círculos sobrepostos: o de Y e o de X. A lente escura é a ESS, a parte clara do círculo de Y é a RSS.")

source("code/bloco_setup.R")
d <- dados("rls_curvatura.csv"); a <- rls(d$X, d$Y); a$dados <- d
a$dgp <- list(b0 = 20, b1 = 0, b2 = 1, sigma = 8)
a$quad <- lm(Y ~ X + I(X^2), data = d)
a$diag <- diagnosticos(lm(Y ~ X, data = d))
library(ggplot2)
## Densidade F sob H0 com os graus de liberdade do proprio exemplo. Com um
## regressor o numerador tem 1 grau de liberdade, e por isso a curva e
## monotona decrescente em vez do corcovado que se ve nos livros -- desenhar o
## corcovado aqui seria desenhar outro teste.
fc <- qf(1 - a$alpha, 1, a$gl)
d <- data.frame(x = seq(0.4, 8, length.out = 600))
d$y <- df(d$x, 1, a$gl)

## O eixo vertical NAO e cortado. O dominio comeca em 0,4 em vez de 0: com um
## grau de liberdade no numerador a densidade diverge na origem, e incluir a
## vizinhanca de zero levaria o eixo a uma altura em que todo o resto some.
## Comecar depois do joelho e diferente de truncar -- a curva desenhada e
## inteira dentro do dominio mostrado.
pv <- pf(a$F, 1, a$gl, lower.tail = FALSE)
p <- ggplot(d, aes(x, y)) +
  geom_area(data = subset(d, x >= fc), fill = "#8B1A1A") +
  geom_line(linewidth = 0.6) +
  geom_vline(xintercept = fc, linetype = "dashed", colour = "grey35") +
  annotate("text", x = fc, y = max(d$y) * 0.86, hjust = -0.08, size = 2.3,
           label = sprintf("$F_{%s}(1,%s) = %s$", nm(a$alpha, 2), a$gl, nm(fc, 2))) +
  labs(x = "$F$", y = NULL,
       caption = sprintf("$F$ observado $= %s$, fora da escala: $p %s$.",
                         nm(a$F, 2),
                         if (pv < 0.001) "< 0{,}001" else paste("=", nm(pv, 3)))) +
  theme_minimal(base_size = 9) +
  theme(axis.text.y = element_blank(), panel.grid.minor = element_blank(),
        plot.caption = element_text(size = 5.6, hjust = 0.5))
fig_salva("densidade_F_h0.pdf", p, largura = 2.7, altura = 1.55,
          alt = "Densidade da distribuição F sob a hipótese nula, com a região à direita do valor crítico sombreada.")

source("code/bloco_setup.R")
d <- dados("rls_curvatura.csv"); a <- rls(d$X, d$Y); a$dados <- d
a$dgp <- list(b0 = 20, b1 = 0, b2 = 1, sigma = 8)
a$quad <- lm(Y ~ X + I(X^2), data = d)
a$diag <- diagnosticos(lm(Y ~ X, data = d))
tab(data.frame(
  Fonte = c("Regressão", "Resíduo", "Total"),
  `Soma de quadrados` = paste0("$", c(paste0("\\text{ESS} = ", ni(a$ESS)),
                                      paste0("\\text{RSS} = ", ni(a$RSS)),
                                      paste0("\\text{TSS} = ", ni(a$TSS))), "$"),
  `g.l.` = paste0("$", c("1", paste0("n-2 = ", ni(a$gl)), paste0("n-1 = ", ni(a$n - 1))), "$"),
  `Quadrado médio` = c(paste0("$", ni(a$ESS), "$"),
                       paste0("$S_e^2 = ", nm(a$Se2, 2), "$"), ""),
  check.names = FALSE),
  caption = "Tabela ANOVA do exemplo")

source("code/bloco_setup.R")
d <- dados("rls_curvatura.csv"); a <- rls(d$X, d$Y); a$dados <- d
a$dgp <- list(b0 = 20, b1 = 0, b2 = 1, sigma = 8)
a$quad <- lm(Y ~ X + I(X^2), data = d)
a$diag <- diagnosticos(lm(Y ~ X, data = d))
fc <- qf(1 - a$alpha, 1, a$gl)
## A decisao e derivada da comparacao, nunca escrita a mao: se os dados forem
## reestimados e o F mudar de lado, a frase muda junto em vez de mentir.
decisao <- if (a$F > fc) "rejeita-se" else "não se rejeita"
eq(sprintf("F = %s = %s \\qquad\\text{contra}\\qquad F_{%s}(1, %s) = %s",
           frac("\\text{ESS}/1", "\\text{RSS}/(n-2)"), nm(a$F, 2),
           nm(a$alpha, 2), ni(a$gl), nm(fc, 2)))
cat(sprintf("\nAo nível de significância de $%s\\%%$, %s $H_0$.\n",
            nm(100 * a$alpha, 0), decisao))

source("code/bloco_setup.R")
d <- dados("rls_curvatura.csv"); a <- rls(d$X, d$Y); a$dados <- d
a$dgp <- list(b0 = 20, b1 = 0, b2 = 1, sigma = 8)
a$quad <- lm(Y ~ X + I(X^2), data = d)
a$diag <- diagnosticos(lm(Y ~ X, data = d))
eq(sprintf("t(\\hat\\beta_1)^2 = \\left(%s\\right)^2 = %s \\qquad\\text{e}\\qquad F = %s = %s = %s.",
           frac(ni(a$b1), nm(a$Sb1, 3)), nm(a$t1^2, 2),
           frac("\\text{ESS}/1", "\\text{RSS}/(n-2)"),
           frac(ni(a$ESS), nm(a$Se2, 2)), nm(a$F, 2)))

source("code/bloco_setup.R")
d <- dados("rls_curvatura.csv"); a <- rls(d$X, d$Y); a$dados <- d
a$dgp <- list(b0 = 20, b1 = 0, b2 = 1, sigma = 8)
a$quad <- lm(Y ~ X + I(X^2), data = d)
a$diag <- diagnosticos(lm(Y ~ X, data = d))
eq(sprintf("%s \\qquad\\text{aqui}\\qquad R^2 = %s = %s.",
           cx(sprintf("R^2 = %s = 1 - %s", frac("\\text{ESS}", "\\text{TSS}"),
                      frac("\\text{RSS}", "\\text{TSS}"))),
           frac(ni(a$ESS), ni(a$TSS)), nm(a$R2, 3)))

source("code/bloco_setup.R")
d <- dados("rls_curvatura.csv"); a <- rls(d$X, d$Y); a$dados <- d
a$dgp <- list(b0 = 20, b1 = 0, b2 = 1, sigma = 8)
a$quad <- lm(Y ~ X + I(X^2), data = d)
a$diag <- diagnosticos(lm(Y ~ X, data = d))
## A amostra inteira, em pe, dobrada em dois blocos lado a lado: vinte linhas
## empilhadas nao cabem na altura do frame, e truncar esconderia justamente a
## troca de sinal nas pontas, que e o ponto do slide.
m <- a$n %/% 2
tab(data.frame(
  "$X_i$"  = ni(a$x[1:m]),         "$\\hat u_i$"  = ni(a$u[1:m]),
  "$X_i$ " = ni(a$x[(m + 1):a$n]), "$\\hat u_i$ " = ni(a$u[(m + 1):a$n]),
  check.names = FALSE),
  caption = "Resíduos ao longo de $X$",
  tamanho = "scriptsize")

source("code/bloco_setup.R")
d <- dados("rls_curvatura.csv"); a <- rls(d$X, d$Y); a$dados <- d
a$dgp <- list(b0 = 20, b1 = 0, b2 = 1, sigma = 8)
a$quad <- lm(Y ~ X + I(X^2), data = d)
a$diag <- diagnosticos(lm(Y ~ X, data = d))
eq(cx(sprintf("R^2 = %s = %s = %s", frac("\\text{ESS}", "\\text{TSS}"),
              frac(ni(a$ESS), ni(a$TSS)), nm(a$R2, 3))))

source("code/bloco_setup.R")
d <- dados("rls_curvatura.csv"); a <- rls(d$X, d$Y); a$dados <- d
a$dgp <- list(b0 = 20, b1 = 0, b2 = 1, sigma = 8)
a$quad <- lm(Y ~ X + I(X^2), data = d)
a$diag <- diagnosticos(lm(Y ~ X, data = d))
library(ggplot2)

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
fig_salva("residuos_padrao_u.pdf", p, largura = 5.4, altura = 1.70,
          alt = "Resíduos contra valores ajustados: à esquerda um padrão em U, à direita nuvem sem padrão.")

source("code/bloco_setup.R"); library(ggplot2)
set.seed(423)
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
fig_salva("tres_padroes_residuos.pdf", p, largura = 2.7, altura = 2.10,
          alt = "Três painéis de resíduos: padrão em U, leque abrindo e sinais iguais em sequência.")

source("code/bloco_setup.R")
d <- dados("rls_curvatura.csv"); a <- rls(d$X, d$Y); a$dados <- d
a$dgp <- list(b0 = 20, b1 = 0, b2 = 1, sigma = 8)
a$quad <- lm(Y ~ X + I(X^2), data = d)
a$diag <- diagnosticos(lm(Y ~ X, data = d))
library(ggplot2)
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
fig_salva("redefinicao_regressor.pdf", p, largura = 5.4, altura = 1.25,
          alt = "Os mesmos pontos contra X e contra Z igual a X ao quadrado, onde passam a se alinhar.")

source("code/bloco_setup.R"); library(ggplot2)
## O comportamento da curva vai no rotulo do painel, nao numa frase abaixo do
## grafico: quem olha a figura le a classificacao no mesmo lugar em que ve a
## forma. A linha de baixo e a mesma familia na escala log-log, onde as tres
## viram retas -- e a anamorfose executada, nao descrita.
d <- curva_forma("log-log", c("$\\beta_1 > 1$: acelera" = 1.7,
                              "$0 < \\beta_1 < 1$: satura" = 0.45,
                              "$\\beta_1 < 0$: decresce" = -0.8))
escalas <- c("Escala original", "Escala log-log")
d <- rbind(transform(d, v = x,      w = y,      escala = escalas[1]),
           transform(d, v = log(x), w = log(y), escala = escalas[2]))
d$escala <- factor(d$escala, levels = escalas)

p <- ggplot(d, aes(v, w)) +
  geom_line(linewidth = 0.8) +
  facet_grid(escala ~ painel, scales = "free") +
  labs(x = NULL, y = NULL,
       caption = "Na escala log-log as três viram retas: é a anamorfose.") +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(face = "bold", size = 6.5),
        axis.text = element_blank(), panel.grid.minor = element_blank())
fig_salva("forma_log_log.pdf", p, largura = 5.4, altura = 2.05,
          alt = "Três curvas de potência na escala original e as mesmas três na escala log-log, onde viram retas.")

source("code/bloco_setup.R"); library(ggplot2)
## Mesma construcao do log-log: em cima a forma original, embaixo a escala em
## que ela vira reta. Aqui so o eixo vertical e transformado.
d <- curva_forma("log-lin", c("$\\beta_1 > 0$: cresce" = 0.55,
                              "$\\beta_1 < 0$: decai" = -0.55))
escalas <- c("Escala original", "$\\log Y$ contra $X$")
d <- rbind(transform(d, w = y,      escala = escalas[1]),
           transform(d, w = log(y), escala = escalas[2]))
d$escala <- factor(d$escala, levels = escalas)

p <- ggplot(d, aes(x, w)) +
  geom_line(linewidth = 0.8) +
  facet_grid(escala ~ painel, scales = "free") +
  labs(x = NULL, y = NULL,
       caption = "Com $\\log Y$ no eixo vertical, as duas viram retas: é a anamorfose.") +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(face = "bold", size = 6.5),
        axis.text = element_blank(), panel.grid.minor = element_blank())
fig_salva("forma_log_lin.pdf", p, largura = 4.8, altura = 2.05,
          alt = "Duas curvas exponenciais na escala original e as mesmas duas com log de Y no eixo vertical, onde viram retas.")

source("code/bloco_setup.R"); library(ggplot2)
d <- curva_forma("lin-log", c("$\\beta_1 > 0$" = 1.4, "$\\beta_1 < 0$" = -1.4))
p <- ggplot(d, aes(x, y)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ painel, scales = "free_y") +
  labs(x = "$X$", y = "$Y$",
       caption = NULL) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"),
        axis.text = element_blank(), panel.grid.minor = element_blank())
fig_salva("forma_lin_log.pdf", p, largura = 4.8, altura = 1.00,
          alt = "Curvas logarítmicas crescente e decrescente, com acréscimos cada vez menores.")

source("code/bloco_setup.R"); library(ggplot2)
d <- curva_forma("reciproco", c("$\\beta_1 > 0$" = 1.6, "$\\beta_1 < 0$" = -1.6))
p <- ggplot(d, aes(x, y)) +
  geom_hline(yintercept = 3, linetype = "dashed", colour = "grey55") +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ painel, scales = "free_y") +
  labs(x = "$X$", y = "$Y$",
       caption = "A linha tracejada é $\\beta_0$: o valor de que $Y$ se aproxima sem alcançar.") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"),
        axis.text = element_blank(), panel.grid.minor = element_blank())
fig_salva("forma_reciproco.pdf", p, largura = 4.8, altura = 1.25,
          alt = "Curvas recíprocas aproximando-se de uma assíntota horizontal tracejada, por cima e por baixo.")

source("code/bloco_setup.R")
d <- dados("rls_curvatura.csv"); a <- rls(d$X, d$Y); a$dados <- d
a$dgp <- list(b0 = 20, b1 = 0, b2 = 1, sigma = 8)
a$quad <- lm(Y ~ X + I(X^2), data = d)
a$diag <- diagnosticos(lm(Y ~ X, data = d))
b <- coef(a$quad); xs <- -b[2] / (2 * b[3])
eq(cx(sprintf("\\frac{\\partial \\hat Y}{\\partial X} = %s %s\\,X",
              nm(b[2], 2), ns(2 * b[3], 2))))
## Dentro ou fora da faixa observada e uma comparacao, nunca uma afirmacao
## escrita a mao: se a amostra for regerada e o ponto critico mudar de lado, a
## frase muda junto.
dentro <- xs >= min(d$X) && xs <= max(d$X)
cat(sprintf("\nO ponto crítico cai em $X^{*} = %s$, %s da faixa observada $[%s, %s]$: %s\n",
            nm(xs, 2), if (dentro) "dentro" else "fora",
            ni(min(d$X)), ni(max(d$X)),
            if (dentro) "a curva muda de direção dentro dos dados."
            else "na faixa dos dados a curva é apenas crescente."))
cat(sprintf("\nO $R^2$ passa de %s, no ajuste linear, para %s, e o padrão em $U$ dos resíduos desaparece.\n",
            nm(a$R2, 3), nm(summary(a$quad)$r.squared, 3)))

source("code/bloco_setup.R")
d <- dados("rls_curvatura.csv"); a <- rls(d$X, d$Y); a$dados <- d
a$dgp <- list(b0 = 20, b1 = 0, b2 = 1, sigma = 8)
a$quad <- lm(Y ~ X + I(X^2), data = d)
a$diag <- diagnosticos(lm(Y ~ X, data = d))
g <- a$dgp; b <- coef(a$quad)
eq(sprintf("Y_i = %s + %s\\,X_i^2 + u_i, \\quad u_i \\sim \\mathcal{N}(0, %s^2) \\qquad \\text{(processo gerador)}",
           ni(g$b0), ni(g$b2), ni(g$sigma)))
cat("\nA estimação da forma quadrática sobre os vinte pontos recupera esses valores dentro do erro amostral:\n")
eq(sprintf("\\hat Y_i = %s %s\\,X_i %s\\,X_i^2, \\quad R^2 = %s \\qquad \\text{(forma estimada)}",
           nm(b[1], 1), ns(b[2], 2), ns(b[3], 2), nm(summary(a$quad)$r.squared, 3)))

source("code/bloco_setup.R")
d <- dados("rls_curvatura.csv"); a <- rls(d$X, d$Y); a$dados <- d
a$dgp <- list(b0 = 20, b1 = 0, b2 = 1, sigma = 8)
a$quad <- lm(Y ~ X + I(X^2), data = d)
a$diag <- diagnosticos(lm(Y ~ X, data = d))
library(ggplot2)
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
fig_salva("ajuste_contra_dgp.pdf", p, largura = 5.0, altura = 1.9,
          alt = "Nuvem de pontos com três curvas: processo gerador, ajuste linear e ajuste quadrático.")

source("code/bloco_setup.R"); library(ggplot2)
library(wooldridge); data("wage1")
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
fig_salva("mincer_anamorfose_wage1.pdf", p, largura = 5.2, altura = 1.60,
          alt = "Salário contra escolaridade em nível e em logaritmo, com a curva ajustada linear em cada escala.")

source("code/bloco_setup.R")
library(wooldridge); data("wage1")
mincer <- lm(log(wage) ~ educ, data = wage1)
s <- summary(mincer)$coefficients
tab(data.frame(
  Termo = c("Intercepto", "educ"),
  Estimativa = nm(s[, 1], 4),
  `Erro padrão` = nm(s[, 2], 4),
  `$t$` = nm(s[, 3], 2),
  `$p$` = ifelse(s[, 4] < 0.001, "$< 0{,}001$", nm(s[, 4], 3)),
  check.names = FALSE),
  caption = "Equação de Mincer estimada sobre \\texttt{wage1}")
cat(sprintf("\n$n = %s$, $R^2 = %s$, $S_e = %s$.\n",
            ni(nobs(mincer)), nm(summary(mincer)$r.squared, 3), nm(sigma(mincer), 3)))

source("code/bloco_setup.R")
library(wooldridge); data("wage1")
m <- lm(log(wage) ~ educ, data = wage1)
b1 <- coef(m)[["educ"]]; b0 <- coef(m)[["(Intercept)"]]
cat(sprintf(paste("O coeficiente de \\texttt{educ} é uma \\alert{semi-elasticidade}: $\\hat\\beta_1 = %s$,",
                  "isto é, $%s\\%%$ a mais de salário por ano adicional de estudo.",
                  "O valor exato, $100(e^{\\hat\\beta_1}-1)$, é $%s\\%%$.\n"),
            nm(b1, 4), nm(100 * b1, 1), nm(100 * (exp(b1) - 1), 1)))
cat(sprintf(paste("\n\\medskip\n\nO intercepto:",
                  "$e^{\\hat\\beta_0} = %s$ estima a \\alert{mediana}\\footnote{Transformação monótona preserva quantis, mas não médias: $\\E(e^{u}) = e^{\\sigma^2/2} > 1$.} do salário em escolaridade nula, não a média.\n"),
            nm(exp(b0), 2)))
cat(sprintf("\n\\medskip\n\nO $R^2$ é de $%s$, e isso não invalida as conclusões.\n",
            nm(summary(m)$r.squared, 3)))

source("code/bloco_setup.R"); library(ggplot2)
d <- dados("rls_potencia.csv")
d$rotulo <- sprintf("$(%s,\\,%s)$", ni(d$X), ni(d$Y))
## A tabela foi omitida de proposito: com cinco pontos, o rotulo sobre cada um
## carrega o mesmo par ordenado e libera o espaco vertical do frame.
## Colocacao a mao, e nao pelo ggrepel: o rotulo do primeiro ponto vai ABAIXO
## dele, porque acima ele encostava no proprio marcador; os demais ficam acima.
## A mao e deterministico -- o ggrepel reposiciona conforme o tamanho final do
## dispositivo, e a posicao mudava entre exportacoes.
## O primeiro ponto leva o rotulo abaixo; o segundo e o terceiro ficam quase na
## mesma altura e por isso saem um para cada lado, senao os rotulos se tocam.
d$vjust <- c( 1.9, -0.6, -0.6, -0.9, -0.9)
d$hjust <- c( 0.5,  1.0,  0.0,  0.5,  0.5)
p <- ggplot(d, aes(X, Y)) +
  geom_point(size = 2.4) +
  geom_text(aes(label = rotulo, vjust = vjust, hjust = hjust), size = 2.5) +
  scale_y_continuous(expand = expansion(mult = c(0.16, 0.18))) +
  scale_x_continuous(expand = expansion(mult = c(0.14, 0.16))) +
  labs(x = "$X$ (insumo)", y = "$Y$ (produção)") +
  theme_minimal(base_size = 10)
fig_salva("exercicio_potencia.pdf", p, largura = 2.9, altura = 1.9,
          alt = "Cinco pontos numa curva de potência crescente, cada um rotulado com seu par ordenado.")

source("code/bloco_setup.R")
d <- dados("rls_curvatura.csv"); a <- rls(d$X, d$Y); a$dados <- d
a$dgp <- list(b0 = 20, b1 = 0, b2 = 1, sigma = 8)
a$quad <- lm(Y ~ X + I(X^2), data = d)
a$diag <- diagnosticos(lm(Y ~ X, data = d))
itens <- c(
  sprintf("No ajuste linear do exemplo desta aula, o $R^2$ de $%s$ indica que a forma funcional escolhida é adequada.",
          nm(a$R2, 3)),
  "No modelo $\\log Y = \\beta_0 + \\beta_1 X + u$, a quantidade $100\\,\\hat\\beta_1$ aproxima a variação percentual de $Y$ associada a uma unidade adicional de $X$.",
  "Comparar o $R^2$ de $Y = \\beta_0+\\beta_1 X + u$ com o de $\\log Y = \\beta_0 + \\beta_1 X + u$ permite escolher entre as duas formas funcionais.",
  "O modelo $Y = \\beta_0 + \\beta_1 X + \\beta_2 X^2 + u$ não pode ser estimado por OLS, por não ser linear.",
  "Em $Y = A X^{\\beta_1} e^{u}$, o logaritmo produz um modelo linear nos parâmetros; em $Y = A X^{\\beta_1} + u$, a mesma transformação não produz.")
cat("\\begin{enumerate}\n")
cat(sprintf("  \\item[(%d)] %s\n", seq_along(itens) - 1, itens), sep = "")
cat("\\end{enumerate}\n")

source("code/bloco_setup.R")
dp <- dados("rls_potencia.csv")
e <- rls(log2(dp$X), log2(dp$Y)); e$dados <- dp
e$b1_ln <- coef(lm(log(dp$Y) ~ log(dp$X)))[[2]]
cat(sprintf("Na escala $\\log_2$: $\\log_2 X = %s$ e $\\log_2 Y = %s$.\n",
            paste(ni(e$x), collapse = ", "), paste(ni(e$y), collapse = ", ")))
eq(sprintf("S_{XX} = %s, \\quad S_{XY} = %s, \\qquad %s",
           ni(e$Sxx), ni(e$Sxy),
           cx(sprintf("\\hat\\beta_1 = %s = %s", frac(ni(e$Sxy), ni(e$Sxx)), ni(e$b1)))))
cat(sprintf("\nElasticidade igual a %s: quando o insumo dobra, a produção multiplica por %s.\n",
            ni(e$b1), ni(2^e$b1)))

source("code/bloco_setup.R")
d <- dados("rls_curvatura.csv"); a <- rls(d$X, d$Y); a$dados <- d
a$dgp <- list(b0 = 20, b1 = 0, b2 = 1, sigma = 8)
a$quad <- lm(Y ~ X + I(X^2), data = d)
a$diag <- diagnosticos(lm(Y ~ X, data = d))
tab(data.frame(
  Item = sprintf("(%d)", 0:4),
  Resposta = c("F", "V", "F", "F", "V"),
  `Por quê` = c(
    "Os resíduos formam um $U$: $R^2$ alto convive com forma funcional errada.",
    "É a semi-elasticidade.",
    "As escalas de $Y$ diferem; TSS mede objetos distintos.",
    "É linear nos parâmetros.",
    "A linearização exige erro multiplicativo."),
  check.names = FALSE),
  caption = "Gabarito do Exercício 2")

source("code/bloco_setup.R")
d <- dados("rls_curvatura.csv"); a <- rls(d$X, d$Y); a$dados <- d
a$dgp <- list(b0 = 20, b1 = 0, b2 = 1, sigma = 8)
a$quad <- lm(Y ~ X + I(X^2), data = d)
a$diag <- diagnosticos(lm(Y ~ X, data = d))
g <- a$diag
marca <- function(p, limite = 0.05) if (p < limite) "\\tikzxmark" else "\\checkmark"
tab(data.frame(
  ` ` = c(marca(g$reset$p.value), marca(g$bp$p.value),
          marca(g$dw$p.value), marca(g$sw$p.value)),
  Pressuposto = c("(P2) $\\E(u \\mid X) = 0$", "(P3) Homoscedasticidade",
                  "(P4) $\\Cov(u_i, u_j) = 0$", "(P6) Normalidade"),
  Teste = c("RESET", "Breusch--Pagan", "Durbin--Watson", "Shapiro--Wilk"),
  `$p$` = c(nm(g$reset$p.value, 3), nm(g$bp$p.value, 3),
            nm(g$dw$p.value, 3), nm(g$sw$p.value, 3)),
  check.names = FALSE),
  caption = "Diagnóstico do ajuste linear", tamanho = "small")

source("code/bloco_setup.R")
d <- dados("rls_curvatura.csv"); a <- rls(d$X, d$Y); a$dados <- d
a$dgp <- list(b0 = 20, b1 = 0, b2 = 1, sigma = 8)
a$quad <- lm(Y ~ X + I(X^2), data = d)
a$diag <- diagnosticos(lm(Y ~ X, data = d))
m <- lm(Y ~ X, data = a$dados)
## RESET rejeita: a curvatura omitida entra como potencia do ajustado.
## Durbin-Watson rejeita, e nao por dependencia temporal -- as observacoes estao
## ordenadas por X, e os residuos de um ajuste curvo ordenados assim ficam
## positivos nas pontas e negativos no meio, o que o DW le como correlacao.
## Breusch-Pagan e Shapiro-Wilk nao rejeitam: o erro do processo gerador e de
## fato homoscedastico e normal, e a ma especificacao nao os destroi.
print(lmtest::resettest(m, power = 2, type = "regressor"))
print(lmtest::bptest(m)); print(lmtest::dwtest(m)); print(shapiro.test(resid(m)))
