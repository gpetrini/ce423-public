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
source("code/bloco_setup.R")
library(wooldridge); library(ggplot2)
m <- ctx_metais()
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
e <- ctx_firmas_ex()
dq <- dados("obs_matricial.csv")
q <- matricial(rlm2(dq$X1, dq$X2, dq$Y, r1 = "X_1", r2 = "X_2"))
q$dados <- dq
data("wage1")
simples  <- lm(log(wage) ~ educ, data = wage1)
multipla <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
paineis <- c("$\\log Y_i$ contra $\\log L_i$", "$\\log Y_i$ contra $\\log K_i$")
pl <- rbind(data.frame(x = m$x1, y = m$y, painel = paineis[1]),
            data.frame(x = m$x2, y = m$y, painel = paineis[2]))
pl$painel <- factor(pl$painel, levels = paineis)
p <- ggplot(pl, aes(x, y)) +
  geom_point(size = 1.5) +
  facet_wrap(~ painel, scales = "free_x") +
  labs(x = NULL, y = "$\\log Y_i$") +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(face = "bold", size = 7),
        axis.title.y = element_text(angle = 0, vjust = 1, margin = margin(r = 9)),
        panel.grid.minor = element_blank())
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
trunca <- function(x, dg) {
  ch <- apply(as.matrix(x), 2, function(col) nm(col, dg))
  if (!is.matrix(ch)) ch <- matrix(ch, ncol = 1)
  rbind(head(ch, 6), rep("\\vdots", ncol(ch)), tail(ch, 2))
}
ordem <- function(mtx, rotulo, dim)
  sprintf("\\underbrace{%s}_{%s\\;(%s)}", mtx, rotulo, dim)
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
Xs <- cbind(1, m$x1)
bs <- as.vector(solve(t(Xs) %*% Xs) %*% (t(Xs) %*% m$y))
termo <- function(simbolo, rotulo, valor, texto_acima) {
  if (texto_acima)
    sprintf("\\underbrace{\\overbrace{%s}^{\\text{%s}}}_{%s}", simbolo, rotulo, valor)
  else
    sprintf("\\overbrace{\\underbrace{%s}_{\\text{%s}}}^{%s}", simbolo, rotulo, valor)
}
rot <- c("parcial", "vies")
d <- data.frame(parte = factor(rot, levels = rot),
                valor = c(m$b1, m$b2 * m$delta))
p <- ggplot(d, aes(x = valor, y = 1, fill = parte)) +
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
v <- m$b2 * m$delta
## O eixo horizontal e o regressor OMITIDO, e e essa escolha que torna o vies
## visivel: o residuo do ajuste multiplo e ortogonal a log K por construcao, e
## o do ajuste simples carrega o efeito do capital que ficou de fora.
rls <- lm(m$y ~ m$x1)
rot <- c("RLM: $\\log L$ e $\\log K$", "RLS: só $\\log L$")
d <- rbind(data.frame(k = m$x2, u = m$u, modelo = rot[1]),
           data.frame(k = m$x2, u = as.numeric(residuals(rls)), modelo = rot[2]))
d$modelo <- factor(d$modelo, levels = rot)
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
yty <- sum(m$y^2); bXty <- as.numeric(t(m$bvec) %*% m$Xty); nY2 <- m$n * m$my^2
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
## O ajuste e o mesmo do corpo do deck: em logaritmos. Um `lm` em nivel aqui
## faria a tabela discordar da ANOVA dos frames anteriores, sem erro algum.
s1 <- summary(lm(log(Y) ~ log(L), data = metais))
s2 <- summary(lm(log(Y) ~ log(L) + log(K), data = metais))
s0 <- sqrt(m$Se2 * m$XtXinv[1, 1])
sn <- function(b) ifelse(b < 0, "-", "+")
comerro <- sprintf(
  "\\widehat{\\log Y_i} = \\underset{(%s)}{%s} %s \\underset{(%s)}{%s}\\,\\log L_i %s \\underset{(%s)}{%s}\\,\\log K_i",
  nm(s0, 2), nm(m$b0, 2),
  sn(m$b1), nm(m$Sb1, 2), nm(abs(m$b1), 2),
  sn(m$b2), nm(m$Sb2, 2), nm(abs(m$b2), 2))
cw <- coef(multipla)
co <- summary(multipla)$coefficients
cw <- coef(multipla)
b1s <- coef(simples)[["educ"]]; b1m <- coef(multipla)[["educ"]]
## A ANOVA do ajuste multiplo de wage1, na mesma forma da tabela do corpo.
sw <- summary(multipla)
gl2 <- sw$df[2]; kw <- sw$df[1] - 1
RSSw <- sum(residuals(multipla)^2)
TSSw <- sum((log(wage1$wage) - mean(log(wage1$wage)))^2)
ESSw <- TSSw - RSSw
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
d <- m$dados; blocos <- split(seq_len(nrow(d)), rep(1:3, each = nrow(d) / 3))
corpo <- do.call(cbind, lapply(blocos, function(i)
  data.frame(Estado = ni(d$estado[i]), `$L_i$` = nm(d$L[i], 1),
             `$K_i$` = nm(d$K[i], 1), `$Y_i$` = nm(d$Y[i], 1),
             check.names = FALSE)))
names(corpo) <- rep(c("Estado", "$L_i$", "$K_i$", "$Y_i$"), 3)
sob <- function(termo, rotulo, valor)
  sprintf("\\underbrace{%s}_{\\substack{\\text{%s} \\\\[2pt] = %s}}",
          termo, rotulo, valor)
av <- anova(lm(log(Y) ~ log(L) + log(K), data = metais))
av <- anova(lm(log(Y) ~ log(L) + log(K), data = metais)); sq <- av$`Sum Sq`
fs <- summary(lm(log(Y) ~ log(L) + log(K), data = metais))$fstatistic
a1 <- anova(lm(log(Y) ~ log(L) + log(K), data = metais))
a2 <- anova(lm(log(Y) ~ log(K) + log(L), data = metais))
