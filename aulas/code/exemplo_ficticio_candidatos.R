## Candidatos de exemplo fictício para o bloco 03/09-17/09 (ticket #9).
## Critério: somas fechando à mão, e a estrutura pedagógica que cada aula exige.

somas <- function(x, y) {
  n <- length(x)
  c(n = n, mx = mean(x), my = mean(y),
    Sxx = sum((x - mean(x))^2), Sxy = sum((x - mean(x)) * (y - mean(y))),
    Syy = sum((y - mean(y))^2),
    b1 = sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2))
}

cat("### A) log-log exato: X dobra, Y quadruplica -> elasticidade = 2\n")
XA <- c(1, 2, 4, 8, 16); YA <- c(1, 4, 16, 64, 256)
print(round(somas(log(XA), log(YA)), 4))
cat("R2 =", summary(lm(log(YA) ~ log(XA)))$r.squared,
    "| elasticidade independe da base do log:",
    coef(lm(log2(YA) ~ log2(XA)))[2], "\n\n")

cat("### B) log-lin exato: X soma 1, Y dobra -> semi-elasticidade = ln 2\n")
XB <- 0:4; YB <- c(1, 2, 4, 8, 16)
print(round(somas(XB, log(YB)), 4))
cat("b1 =", coef(lm(log(YB) ~ XB))[2], "= ln 2 =", log(2), "\n\n")

cat("### C) n=5 com curvatura NAO exata (para diagnostico de residuos)\n")
XC <- 1:5; YC <- c(2, 5, 10, 17, 26)   # Y = X^2 + 1
print(round(somas(XC, YC), 4))
cat("residuos do ajuste linear:", round(resid(lm(YC ~ XC)), 3), "\n\n")

cat("### D) busca: n=5, k=2, somas inteiras e b1 mudando ao entrar x2\n")
x1 <- c(2, 4, 6, 8, 10)
melhores <- list()
for (a in 0:6) for (b in -3:3) for (s in list(c(1,3,2,5,4), c(1,2,4,3,5), c(3,1,4,2,5))) {
  x2 <- s * 2
  y  <- 10 + 2 * x1 + b * x2 + a * c(1, -1, 0, 1, -1)
  m2 <- lm(y ~ x1 + x2); m1 <- lm(y ~ x1)
  cf <- coef(m2)
  if (all(abs(cf - round(cf, 4)) < 1e-9) &&
      abs(coef(m1)[2] - cf[2]) > 0.15 && b != 0) {
    melhores[[length(melhores) + 1]] <-
      list(x1 = x1, x2 = x2, y = y, b_rlm = round(cf, 4),
           b1_rls = round(coef(m1)[2], 4))
  }
}
cat("candidatos encontrados:", length(melhores), "\n")
for (k in head(seq_along(melhores), 3)) {
  z <- melhores[[k]]
  cat("  x1:", z$x1, "| x2:", z$x2, "| y:", z$y, "\n")
  cat("    RLM:", z$b_rlm, " | b1 na RLS:", z$b1_rls, "\n")
}
