## Localizador da camada de apresentacao compartilhada. As tres primeiras
## entradas cobrem as profundidades possiveis dentro do monorepo. A quarta
## cobre o ESPELHO PUBLICO, onde templates/ nao existe: la o CI deposita o
## lectures_setup.R ao lado dos demais, em aulas/code/, de modo que o pacote
## que o aluno baixa e autocontido.
local({
  for (p in c("../../../templates/lectures_setup.R",
              "../../templates/lectures_setup.R",
              "../templates/lectures_setup.R",
              "code/lectures_setup.R")) {
    if (file.exists(p)) { source(p, local = globalenv()); return(invisible(NULL)) }
  }
  stop("nao encontrei lectures_setup.R -- tangule Lectures/templates/lectures_setup.org")
})

## Somas de desvios de duas variaveis. Minuscula e desvio: x_i = X_i - media(X).
desvios <- function(x, y) {
  list(n = length(x), mx = mean(x), my = mean(y),
       Sxx = sum((x - mean(x))^2),
       Syy = sum((y - mean(y))^2),
       Sxy = sum((x - mean(x)) * (y - mean(y))))
}

## Tudo o que os slides de 03/09 e 10/09 precisam de um ajuste simples.
rls <- function(x, y, alpha = 0.05) {
  d <- desvios(x, y); n <- d$n
  b1 <- d$Sxy / d$Sxx; b0 <- d$my - b1 * d$mx
  aj <- b0 + b1 * x; u <- y - aj
  RSS <- sum(u^2); ESS <- d$Syy - RSS; gl <- n - 2
  Se2 <- RSS / gl
  Sb1 <- sqrt(Se2 / d$Sxx)
  Sb0 <- sqrt(Se2 * (1 / n + d$mx^2 / d$Sxx))
  tc <- qt(1 - alpha / 2, gl)
  c(d, list(x = x, y = y, b0 = b0, b1 = b1, aj = aj, u = u,
            RSS = RSS, ESS = ESS, TSS = d$Syy, gl = gl, Se2 = Se2, Se = sqrt(Se2),
            R2 = ESS / d$Syy, Sb0 = Sb0, Sb1 = Sb1,
            t1 = b1 / Sb1, F = (ESS / 1) / Se2, tc = tc,
            ic1 = b1 + c(-1, 1) * tc * Sb1, alpha = alpha))
}

## Media condicional e previsao individual em x0.
previsao <- function(a, x0) {
  aj <- a$b0 + a$b1 * x0
  base <- 1 / a$n + (x0 - a$mx)^2 / a$Sxx
  Smed <- a$Se * sqrt(base); Sprev <- a$Se * sqrt(1 + base)
  list(aj = aj, Smed = Smed, Sprev = Sprev,
       ic_med = aj + c(-1, 1) * a$tc * Smed,
       ic_prev = aj + c(-1, 1) * a$tc * Sprev)
}

## Teste t de um coeficiente contra um valor de referencia c0. Devolve os dois
## valores criticos e os dois valores-p, porque a aula compara a decisao
## bilateral com a unilateral no MESMO numero: e ali que se ve que um intervalo
## a 95% pode conter c0 sem contradizer um teste unilateral que rejeita.
teste_t <- function(est, S, gl, c0 = 0, alpha = 0.05) {
  t <- (est - c0) / S
  list(c0 = c0, est = est, S = S, gl = gl, alpha = alpha, t = t,
       tc_bi = qt(1 - alpha / 2, gl), tc_uni = qt(1 - alpha, gl),
       p_bi = 2 * pt(abs(t), gl, lower.tail = FALSE),
       p_dir = pt(t, gl, lower.tail = FALSE),
       ic = est + c(-1, 1) * qt(1 - alpha / 2, gl) * S)
}

## Reamostragem do processo gerador de um contexto de RLS. Os X ficam FIXOS,
## como (P2) supoe: de amostra para amostra muda o erro, nunca o regressor.
## Serve as duas figuras que materializam a distribuicao amostral -- o feixe de
## curvas ajustadas e o histograma de beta_1 contra a normal teorica.
amostras_dgp <- function(a, R = 2000, semente = 20260910) {
  set.seed(semente)
  g <- a$dgp
  sim <- vapply(seq_len(R), function(i) {
    y <- g$b0 + g$b1 * a$x + rnorm(a$n, 0, g$sigma)
    coef(lm(y ~ a$x))
  }, numeric(2))
  list(R = R, b0 = sim[1, ], b1 = sim[2, ],
       ## Desvio padrao TEORICO de beta_1, com o sigma do processo gerador:
       ## e contra ele que o histograma e comparado, e nao contra o estimado.
       sd_b1 = g$sigma / sqrt(a$Sxx))
}

## Tudo o que os slides de 14/09, 17/09 e 24/09 precisam. Nomes x1 e x2 sao os
## rotulos que aparecem nas formulas (ex.: "L" e "K").
rlm2 <- function(x1, x2, y, alpha = 0.05, r1 = "L", r2 = "K") {
  n <- length(y)
  S11 <- sum((x1 - mean(x1))^2); S22 <- sum((x2 - mean(x2))^2)
  S12 <- sum((x1 - mean(x1)) * (x2 - mean(x2)))
  S1y <- sum((x1 - mean(x1)) * (y - mean(y)))
  S2y <- sum((x2 - mean(x2)) * (y - mean(y)))
  det <- S11 * S22 - S12^2
  b1 <- (S22 * S1y - S12 * S2y) / det
  b2 <- (S11 * S2y - S12 * S1y) / det
  b0 <- mean(y) - b1 * mean(x1) - b2 * mean(x2)
  aj <- b0 + b1 * x1 + b2 * x2; u <- y - aj
  RSS <- sum(u^2); TSS <- sum((y - mean(y))^2); ESS <- TSS - RSS
  k <- 2; gl <- n - k - 1; Se2 <- RSS / gl
  V <- Se2 * matrix(c(S22, -S12, -S12, S11), 2, 2) / det
  ## Regressao simples de y contra x1, para o vies de omissao
  b1s <- S1y / S11; delta <- S12 / S11
  list(n = n, k = k, gl = gl, r1 = r1, r2 = r2,
       x1 = x1, x2 = x2, y = y, aj = aj, u = u,
       mx1 = mean(x1), mx2 = mean(x2), my = mean(y),
       S11 = S11, S22 = S22, S12 = S12, S1y = S1y, S2y = S2y, det = det,
       b0 = b0, b1 = b1, b2 = b2, RSS = RSS, ESS = ESS, TSS = TSS,
       Se2 = Se2, Se = sqrt(Se2), R2 = ESS / TSS,
       R2aj = 1 - (RSS / gl) / (TSS / (n - 1)),
       V = V, Sb1 = sqrt(V[1, 1]), Sb2 = sqrt(V[2, 2]), cov12 = V[1, 2],
       t1 = b1 / sqrt(V[1, 1]), t2 = b2 / sqrt(V[2, 2]),
       tc = qt(1 - alpha / 2, gl), alpha = alpha,
       F = (ESS / k) / Se2, Fc = qf(1 - alpha, k, gl),
       pF = pf((ESS / k) / Se2, k, gl, lower.tail = FALSE),
       p1 = 2 * pt(abs(b1 / sqrt(V[1, 1])), gl, lower.tail = FALSE),
       p2 = 2 * pt(abs(b2 / sqrt(V[2, 2])), gl, lower.tail = FALSE),
       ic1 = b1 + c(-1, 1) * qt(1 - alpha / 2, gl) * sqrt(V[1, 1]),
       ic2 = b2 + c(-1, 1) * qt(1 - alpha / 2, gl) * sqrt(V[2, 2]),
       r12 = S12 / sqrt(S11 * S22), b1s = b1s, delta = delta,
       vies = b1s - b1, RSSs = TSS - S1y^2 / S11,
       ## regressao auxiliar de x1 contra x2 (Frisch-Waugh-Lovell)
       aux = x1 - (mean(x1) + (S12 / S22) * (x2 - mean(x2))),
       gamma1 = S12 / S22)
}

## Teste t de lambda1*b1 + lambda2*b2 = c, e o F equivalente por RSS.
combinacao <- function(a, lambda, c0 = 0) {
  theta <- sum(lambda * c(a$b1, a$b2))
  v <- lambda[1]^2 * a$V[1, 1] + lambda[2]^2 * a$V[2, 2] +
       2 * lambda[1] * lambda[2] * a$V[1, 2]
  t <- (theta - c0) / sqrt(v)
  list(theta = theta, var = v, S = sqrt(v), t = t, F = t^2,
       Fc = qf(1 - a$alpha, 1, a$gl),
       RSSr = a$RSS + t^2 * a$Se2)
}

## F de restrito contra irrestrito, a partir dos dois RSS.
teste_F <- function(RSSr, RSSu, q, gl, alpha = 0.05) {
  F <- ((RSSr - RSSu) / q) / (RSSu / gl)
  list(F = F, q = q, gl = gl, Fc = qf(1 - alpha, q, gl),
       p = pf(F, q, gl, lower.tail = FALSE))
}

## Contribuicao marginal de x2 depois de x1, na leitura da ANOVA: o quanto a
## SQReg cresce ao passar do modelo restrito ao irrestrito.
##
## E a MESMA quantidade que RSSr - RSSu, porque TSS nao depende do modelo
## ajustado -- o que a regressao ganha, o residuo perde. A funcao devolve as
## duas leituras para que o deck exiba a identidade em vez de afirma-la.
##
## `ordem` diz qual regressor entra primeiro. Ela importa: a soma de quadrados
## sequencial de um regressor depende do que ja esta no modelo, e so coincide
## entre as duas ordens quando S12 = 0.
contribuicao <- function(m, ordem = c("x1", "x2")) {
  ordem <- match.arg(ordem)
  ## SQReg do modelo restrito, com um regressor so.
  ESSr <- if (ordem == "x1") m$S1y^2 / m$S11 else m$S2y^2 / m$S22
  RSSr <- m$TSS - ESSr
  cont <- m$ESS - ESSr
  ft <- teste_F(RSSr, m$RSS, 1, m$gl, m$alpha)
  c(list(primeiro = if (ordem == "x1") m$r1 else m$r2,
         marginal = if (ordem == "x1") m$r2 else m$r1,
         ESSr = ESSr, ESSu = m$ESS, RSSr = RSSr, RSSu = m$RSS,
         cont = cont, glr = 1, glu = 2,
         QMr = ESSr, QMcont = cont, QMres = m$Se2,
         R2r = ESSr / m$TSS, R2u = m$R2,
         ## Fracao da variabilidade AINDA NAO explicada que o novo regressor
         ## explica. E o coeficiente de determinacao parcial, e o deck usa a
         ## coincidencia para ligar as duas metades da aula.
         r2parcial = cont / RSSr),
    ft)
}

## Correlacoes simples e parciais de um ajuste com dois regressores.
##
## Os vetores de residuos vao junto porque a construcao em tres passos
## (Frisch-Waugh-Lovell) e exibida como figura: a correlacao parcial e a
## correlacao SIMPLES entre o que sobra de Y e o que sobra de X1, uma vez
## descontado X2 de ambos.
parcial <- function(m) {
  rY1 <- m$S1y / sqrt(m$S11 * m$TSS)
  rY2 <- m$S2y / sqrt(m$S22 * m$TSS)
  r12 <- m$r12
  rY1_2 <- (rY1 - rY2 * r12) / sqrt((1 - r12^2) * (1 - rY2^2))
  rY2_1 <- (rY2 - rY1 * r12) / sqrt((1 - r12^2) * (1 - rY1^2))
  list(rY1 = rY1, rY2 = rY2, r12 = r12,
       rY1_2 = rY1_2, rY2_1 = rY2_1,
       r2Y1_2 = rY1_2^2, r2Y2_1 = rY2_1^2,
       ## Residuos dos dois ajustes auxiliares, nas duas direcoes.
       ey2 = m$y - (m$my + (m$S2y / m$S22) * (m$x2 - m$mx2)),
       e12 = m$x1 - (m$mx1 + (m$S12 / m$S22) * (m$x2 - m$mx2)),
       ey1 = m$y - (m$my + (m$S1y / m$S11) * (m$x1 - m$mx1)),
       e21 = m$x2 - (m$mx2 + (m$S12 / m$S11) * (m$x1 - m$mx1)),
       ## A determinacao parcial recuperada do t do coeficiente, e do R^2.
       via_t1 = m$t1^2 / (m$t1^2 + m$gl),
       via_t2 = m$t2^2 / (m$t2^2 + m$gl),
       via_R2_1 = (m$R2 - rY2^2) / (1 - rY2^2),
       via_R2_2 = (m$R2 - rY1^2) / (1 - rY1^2))
}

## Os quatro diagnosticos que a tabela de pressupostos reporta. Cada um testa
## uma pressuposicao distinta; nenhum deles testa (P1), que e escolha do
## analista, nem (P5), que se le da propria coluna de X.
diagnosticos <- function(modelo) {
  list(bp = lmtest::bptest(modelo),
       dw = lmtest::dwtest(modelo),
       sw = shapiro.test(residuals(modelo)),
       reset = lmtest::resettest(modelo, power = 2, type = "regressor"))
}
## Objetos matriciais de um ajuste com dois regressores. Separado de `rlm2`
## porque a forma matricial e a apresentacao oficial do estimador desde
## 2026-09-01 (ADR 0014 desta disciplina), enquanto `rlm2` continua operando em
## somatorios: os dois caminhos precisam coexistir e produzir o mesmo numero.
matricial <- function(m) {
  X <- cbind(1, m$x1, m$x2); y <- m$y
  XtX <- t(X) %*% X; Xty <- t(X) %*% y; XtXinv <- solve(XtX)
  c(m, list(X = X, vy = y, XtX = XtX, Xty = Xty, XtXinv = XtXinv,
            bvec = as.vector(XtXinv %*% Xty),
            uvec = as.vector(y - X %*% (XtXinv %*% Xty))))
}

## 14/09, 17/09, 24/09 -- o exemplo ficticio do bloco de RLM.
##
## Os dados vem de um processo gerador conhecido: Y = 10 + 3L + 5K + u, com
## u ~ N(0, 10^2) e n = 20, gerado por code/gera_firmas_bloco.R com semente fixa.
## O exemplo NAO e deterministico (ADR 0013): beta_hat difere de beta, e a
## comparacao entre os dois fecha a aula de estimacao. Os parametros do DGP
## ficam no proprio contexto porque o slide de revelacao os exibe.
ctx_firmas <- function() {
  f <- dados("firmas_bloco.csv"); m <- rlm2(f$L, f$K, f$Y); m$dados <- f
  m$dgp <- list(b0 = 10, b1 = 3, b2 = 5, sigma = 10)
  matricial(m)
}

## 14/09 e 17/09 -- exercicio de fixacao
ctx_firmas_ex <- function() {
  f <- dados("firmas_exercicio.csv"); m <- rlm2(f$L, f$K, f$Y); m$dados <- f
  matricial(m)
}

## Curvas de uma forma funcional para varios valores de beta1. O argumento
## `betas` e um vetor NOMEADO: o nome vira o rotulo do painel, em LaTeX.
curva_forma <- function(tipo, betas, x = seq(0.3, 4, length.out = 300)) {
  f <- switch(tipo,
    "lin-lin"   = function(b) 2 + b * x,
    "log-log"   = function(b) 2 * x^b,
    "log-lin"   = function(b) exp(0.3 + b * x),
    "lin-log"   = function(b) 3 + b * log(x),
    "reciproco" = function(b) 3 + b / x,
    stop("forma desconhecida: ", tipo))
  d <- do.call(rbind, lapply(seq_along(betas), function(i) {
    data.frame(x = x, y = f(betas[[i]]), painel = names(betas)[i])
  }))
  d$painel <- factor(d$painel, levels = names(betas))
  d
}
