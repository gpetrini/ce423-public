## Busca do exemplo fictício com k = 2 para a aula de 10/09 (ticket #9).
##
## Construção: em vez de sortear Y e torcer para os coeficientes fecharem,
## fixa-se beta inteiro e escolhe-se o vetor de resíduos DENTRO do espaço
## ortogonal às colunas de X. Assim beta_hat = beta exatamente, por construção,
## e os resíduos são exatamente o vetor escolhido.

base_ortogonal_inteira <- function(X, limite = 4) {
  n <- nrow(X)
  N <- MASS::Null(X)                       # base do complemento ortogonal
  cand <- list()
  grade <- as.matrix(expand.grid(rep(list(-limite:limite), ncol(N))))
  for (i in seq_len(nrow(grade))) {
    v <- as.numeric(N %*% grade[i, ])
    if (all(abs(v) > 1e-9) || any(abs(v) > 1e-9)) {
      w <- v / min(abs(v[abs(v) > 1e-9]))
      if (all(abs(w - round(w)) < 1e-8) && max(abs(w)) <= 4 && any(w != 0)) {
        cand[[length(cand) + 1]] <- round(w)
      }
    }
  }
  unique(cand)
}

avaliar <- function(x1, x2, b, e) {
  y <- b[1] + b[2] * x1 + b[3] * x2 + e
  if (any(y <= 0)) return(NULL)
  m2 <- lm(y ~ x1 + x2); m1 <- lm(y ~ x1)
  if (max(abs(coef(m2) - b)) > 1e-8) return(NULL)
  RSS <- sum(resid(m2)^2); TSS <- sum((y - mean(y))^2)
  list(x1 = x1, x2 = x2, y = y, e = e, b = b,
       b1_rls = coef(m1)[2], vies = coef(m1)[2] - b[2],
       cor12 = cor(x1, x2), RSS = RSS, ESS = TSS - RSS, TSS = TSS,
       R2 = 1 - RSS / TSS, Se2 = RSS / (length(y) - 3))
}

suppressMessages(library(MASS))
x1 <- c(1, 2, 3, 4, 5)                      # anos de estudo além do mínimo
achados <- list()
for (x2 in list(c(1, 1, 2, 2, 4), c(1, 2, 2, 3, 4), c(2, 2, 4, 4, 6),
                c(1, 1, 3, 3, 5), c(0, 1, 1, 2, 3))) {
  X <- cbind(1, x1, x2)
  if (abs(det(t(X) %*% X)) < 1e-8) next
  for (e in base_ortogonal_inteira(X)) {
    for (b1 in 2:5) for (b2 in c(-4, -3, -2, 2, 3, 4)) for (b0 in c(4, 6, 8, 10)) {
      r <- avaliar(x1, x2, c(b0, b1, b2), e)
      if (!is.null(r) && abs(r$vies) >= 0.5 && r$RSS == round(r$RSS) && r$RSS > 0)
        achados[[length(achados) + 1]] <- r
    }
  }
}
cat("candidatos:", length(achados), "\n\n")
ord <- order(sapply(achados, function(z) -abs(z$vies)))
for (k in head(ord, 4)) {
  z <- achados[[k]]
  cat(sprintf("x1: %s | x2: %s | y: %s\n", paste(z$x1, collapse=" "),
              paste(z$x2, collapse=" "), paste(z$y, collapse=" ")))
  cat(sprintf("  beta = (%s) | residuos = (%s)\n", paste(z$b, collapse=", "),
              paste(z$e, collapse=", ")))
  cat(sprintf("  b1 na RLS = %.4f  -> vies = %+.4f | cor(x1,x2) = %.3f\n",
              z$b1_rls, z$vies, z$cor12))
  cat(sprintf("  TSS = %g | ESS = %g | RSS = %g | R2 = %.4f | Se2 = %g\n\n",
              z$TSS, z$ESS, z$RSS, z$R2, z$Se2))
}
