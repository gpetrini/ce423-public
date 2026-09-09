## Numero em modo matematico: 1{,}23
##
## O sub() final elimina o zero negativo. Uma quantidade que arredonda para zero
## por baixo sai do formatC como "-0" ou "-0{,}00", e o sinal ali afirma algo que
## o numero nao diz: sugere quantidade negativa onde ha apenas arredondamento.
nm <- function(x, d = 2) {
  s <- formatC(x, format = "f", digits = d, big.mark = "")
  s <- sub("\\.", "{,}", s)
  sub("^-(0(\\{,\\}0*)?)$", "\\1", s)
}

## Inteiro. O "+ 0" converte o zero negativo do IEEE em zero positivo.
ni <- function(x) formatC(round(x) + 0, format = "d")

## Numero com sinal explicito, para vies e diferencas. O sinal e decidido DEPOIS
## do arredondamento, senao -0,001 com d = 2 sairia como "-0{,}00".
##
## Vetorizado, como `nm` e `ni`. A versao anterior usava `if`, que so olha o
## primeiro elemento: chamada com um vetor de residuos ela abortava o bloco, e
## a tabela sumia do slide sem erro de compilacao. Corrigido em 2026-09-01, na
## primeira vez em que um exemplo passou a ter n grande o bastante para que a
## coluna de residuos fosse formatada de uma vez.
ns <- function(x, d = 2) {
  paste0(ifelse(round(x, d) < 0, "-", "+"), nm(abs(x), d))
}

## Intervalo no formato [a; b]
iv <- function(v, d = 2) sprintf("[\\,%s\\,;\\ %s\\,]", nm(v[1], d), nm(v[2], d))

## Procura data/ a partir do diretorio corrente e dos niveis acima, para
## funcionar tanto exportando de aulas/ quanto da raiz da disciplina.
caminho_dado <- function(arquivo) {
  for (p in c("data", "aulas/data", "../data", "../aulas/data")) {
    alvo <- file.path(p, arquivo)
    if (file.exists(alvo)) return(alvo)
  }
  stop("nao encontrei ", arquivo)
}

dados <- function(arquivo) read.csv(caminho_dado(arquivo))

## Equacao sem numero, com quebra de linha correta para o ox-latex.
eq <- function(...) cat("\\begin{equation*}\n", paste0(...), "\n\\end{equation*}\n")

## Varias linhas alinhadas.
eqs <- function(...) cat("\\begin{align*}\n", paste(c(...), collapse = " \\\\\n"),
                         "\n\\end{align*}\n")

## Caminho de figura. Sempre figs/ ao lado do .org da aula, criado se faltar.
## O link [[file:...]] tem de ser relativo ao .org para o LaTeX achar o PDF.
fig <- function(nome) {
  dir.create("figs", showWarnings = FALSE, recursive = TRUE)
  file.path("figs", nome)
}

frac <- function(a, b) sprintf("\\frac{%s}{%s}", a, b)
cx   <- function(x) sprintf("\\boxed{%s}", x)

## Matriz em LaTeX, para os decks que operam em notacao matricial. Aceita
## matriz, vetor ou data.frame; conteudo numerico passa por `fmt` (por omissao
## `ni`, inteiro) e conteudo de texto vai intacto, de modo que a mesma funcao
## serve tanto para a matriz de dados quanto para uma matriz de simbolos.
##
## O ambiente e `pmatrix` por omissao. Vetor sem dimensao vira COLUNA: e o que
## y, u e beta sao em y = X beta + u, e a alternativa (linha) estaria errada
## em toda ocorrencia desta disciplina.
mat <- function(x, fmt = ni, env = "pmatrix") {
  if (is.null(dim(x))) x <- matrix(x, ncol = 1) else x <- as.matrix(x)
  celulas <- if (is.numeric(x)) matrix(fmt(x), nrow(x)) else x
  linhas <- apply(celulas, 1, paste, collapse = " & ")
  sprintf("\\begin{%s}%s\\end{%s}", env,
          paste0(" ", paste(linhas, collapse = " \\\\ "), " "), env)
}

## Salva um ggplot em figs/ e emite a inclusao para o Org, numa chamada so.
##
## O motor "tikz" e o padrao: o R desenha os tracos e o LaTeX compoe todo o
## texto, de modo que os rotulos saem na fonte do deck e a matematica dos eixos
## e matematica de verdade ($\hat u_i$), nao a aproximacao do plotmath do R.
## O rotulo, ai, e string LaTeX -- expression() nao e interpretado pelo tikz.
##
## O motor "pdf" fica como escape para figuras ainda nao convertidas. Ele USA
## cairo_pdf, NUNCA pdf(). O dispositivo pdf() do R escreve texto em Latin-1 e
## precisa converter a string antes de medi-la. Dentro do ob-R essa conversao
## falha -- "unknown encoding 'latin1' in 'mbcsToSbcs'" -- e o bloco morre
## calado: a figura ate chega a ser gravada, mas o cat() nunca roda, o Org
## recebe resultado vazio e o slide sai sem figura, sem erro de compilacao.
##
## Cada motor emite o que o bloco que o chama espera:
##   tikz -> \input{...}    com  :results output latex
##   pdf  -> [[file:...]]  com  :results output raw
## Transliteracao para ASCII do alt-text. Nao e preciosismo: a string /Alt de um
## PDF sem BOM e lida como PDFDocEncoding, entao um "a" com til gravado em UTF-8
## chega ao leitor de tela como dois caracteres errados. As alternativas do
## accsupp foram testadas e nenhuma funciona por pdflatex -- ver o comentario em
## templates/preambule.tex. Perder o acento e o menor dos danos.
alt_ascii <- function(x) {
  x <- chartr("áàâãäéèêëíìîïóòôõöúùûüçÁÀÂÃÄÉÈÊËÍÌÎÏÓÒÔÕÖÚÙÛÜÇ",
              "aaaaaeeeeiiiiooooouuuucAAAAAEEEEIIIIOOOOOUUUUC", x)
  ## Travessao, aspas curvas e reticencias sobrevivem ao chartr por nao terem
  ## letra correspondente; viram ASCII aqui. O que restar fora da faixa
  ## imprimivel cai fora, e chave e barra invertida tambem: o alt-text e
  ## argumento de macro, e um deles sozinho quebra a compilacao.
  x <- gsub("—|–", "-", x)
  x <- gsub("“|”|‘|’", "'", x)
  x <- gsub("…", "...", x)
  x <- gsub("[{}\\\\]", "", x)
  gsub("[^ -~]", "", x)
}

fig_salva <- function(nome, plot, largura = 6, altura = 3.2,
                      motor = c("tikz", "pdf"), alt = NULL) {
  motor <- match.arg(motor)
  if (motor == "pdf") {
    caminho <- fig(nome)
    if (is.function(plot)) {
      ## Grafico de base tambem no motor pdf. O `ggsave` recebe um objeto e o
      ## imprime; uma FUNCAO de zero argumentos desenha por efeito colateral e
      ## nao produz objeto, e o `ggsave` morre com "no applicable method for
      ## 'grid.draw'". Verificado em 2026-09-02: o ramo `is.function` existia so
      ## no motor tikz, e a assimetria nao estava registrada em lugar nenhum.
      grDevices::cairo_pdf(caminho, width = largura, height = altura)
      plot()
      invisible(grDevices::dev.off())
    } else {
      suppressMessages(
        ggplot2::ggsave(caminho, plot, width = largura, height = altura,
                        device = grDevices::cairo_pdf))
    }
    ## O motor pdf emite um link do Org, que so vira \includegraphics na
    ## exportacao -- longe daqui, e sem lugar onde encaixar o \altfig. Alt-text
    ## exige o motor tikz, que e o padrao desde o ADR Lectures 0011.
    if (!is.null(alt)) warning("alt-text exige motor = 'tikz'; ignorado em 'pdf'")
    cat(sprintf("[[file:%s]]\n", caminho))
  } else {
    caminho <- fig(sub("\\.pdf$", ".tex", nome))
    ## O tikzDevice mede a largura de cada string rodando LaTeX com um preambulo
    ## proprio, que por omissao nao carrega inputenc nem a fonte do deck. Sem
    ## estes dois pacotes o R avisa "Attempting to calculate the width of a
    ## Unicode string" a cada rotulo acentuado e posiciona o texto por metricas
    ## da Computer Modern, deslocando rotulo e eixo no slide final.
    ## Dicionario de metricas em disco. E a diferenca entre uma exportacao de
    ## quatro minutos e uma de poucos segundos: para posicionar cada rotulo o
    ## tikzDevice mede a largura da string rodando LaTeX com o preambulo abaixo,
    ## que carrega a ebgaramond inteira. Sem dicionario persistente essa medicao
    ## e refeita a cada processo R -- ou seja, a cada exportacao.
    ##
    ## Medido em 2026-09-01, processo R novo, mesma figura: 20,7 s sem
    ## dicionario contra 0,58 s com ele. O ganho aparece a partir da segunda
    ## exportacao; a primeira, e qualquer rotulo inedito, pagam a medicao.
    ##
    ## Fica no cache do usuario, fora do repositorio: e derivado e refazivel.
    ## A chave do dicionario inclui o preambulo de medicao, entao trocar a fonte
    ## invalida as entradas antigas sozinho, sem intervencao.
    cache <- file.path(Sys.getenv("XDG_CACHE_HOME", path.expand("~/.cache")),
                       "tikzDevice")
    dir.create(cache, recursive = TRUE, showWarnings = FALSE)
    options(tikzMetricsDictionary = file.path(cache, "metricas"),
            tikzDefaultEngine = "pdftex",
            ## amsmath entra porque o rotulo de um grafico legitimamente usa
            ## \\text{} dentro de matematica. Sem ele o tikzDevice nao consegue
            ## MEDIR a string, o bloco morre, e a exportacao deixa em figs/ um
            ## .tex de cabecalho vazio -- o deck compila, sem figura e sem erro.
            ## Verificado em 2026-09-01 no painel de wage1.
            tikzMetricPackages = c("\\usepackage[T1]{fontenc}",
                                   "\\usepackage[utf8]{inputenc}",
                                   "\\usepackage{amsmath}",
                                   "\\usepackage[lining,tabular]{ebgaramond}",
                                   "\\usetikzlibrary{calc}"))
    tikzDevice::tikz(caminho, width = largura, height = altura,
                     standAlone = FALSE, sanitize = FALSE,
                     verbose = FALSE, engine = "pdftex")
    ## O aviso "Attempting to calculate the width of a Unicode string" e emitido
    ## para todo rotulo acentuado, mesmo quando a medicao funciona -- e o que os
    ## pacotes de metrica acima garantem. Silenciado por mensagem, nao em bloco,
    ## para que avisos de verdade do ggplot continuem chegando.
    ## `plot` costuma ser um ggplot, e entao basta imprimi-lo. Quando e uma
    ## FUNCAO de zero argumentos, ela e chamada aqui: e o unico caminho para os
    ## graficos de base, que desenham por efeito colateral no dispositivo e nao
    ## produzem objeto para imprimir. Existe por causa do plano ajustado em tres
    ## dimensoes (`persp`), que o ggplot2 nao faz.
    withCallingHandlers(
      if (is.function(plot)) plot() else print(plot),
      warning = function(w) {
        if (grepl("Unicode string", conditionMessage(w), fixed = TRUE))
          invokeRestart("muffleWarning")
      })
    invisible(grDevices::dev.off())
    ## O \input{} vai envolvido em \altfig quando ha alt-text, de modo que o
    ## desenho carregue a entrada /Alt do PDF. Sem alt, emite-se o \input{} nu.
    if (is.null(alt)) {
      cat(sprintf("\\input{%s}\n", caminho))
    } else {
      cat(sprintf("\\altfig{%s}{\\input{%s}}\n", alt_ascii(alt), caminho))
    }
  }
  invisible(caminho)
}

## Tabela LaTeX a partir de um data.frame ja formatado como texto.
## O comando de tamanho entra DENTRO do float. Envolver o \begin{table} num
## grupo TeX -- {\small \begin{table}...} -- faz a tabela sumir do PDF sem erro
## de compilacao; ver Lectures/CLAUDE.md 5.9.
encolhe <- function(x, tamanho) {
  if (is.null(tamanho)) return(x)
  sub("\\begin{table}", sprintf("\\begin{table}\n\\%s", tamanho), x, fixed = TRUE)
}

tab <- function(df, caption = NULL, align = NULL, tamanho = NULL) {
  cat(encolhe(knitr::kable(df, format = "latex", booktabs = TRUE, escape = FALSE,
                           caption = caption, align = align, linesep = "",
                           row.names = FALSE),
              tamanho),
      "\n")
}

## Tabela de uma serie: cada argumento nomeado vira uma LINHA, com o nome na
## primeira coluna. Sem cabecalho -- as colunas sao observacoes, nao variaveis.
tab_serie <- function(..., caption = NULL, tamanho = NULL) {
  args <- list(...)
  corpo <- do.call(rbind, lapply(args, as.character))
  df <- data.frame(rotulo = names(args), corpo, check.names = FALSE,
                   stringsAsFactors = FALSE)
  cat(encolhe(knitr::kable(df, format = "latex", booktabs = TRUE, escape = FALSE,
                           col.names = NULL, caption = caption, linesep = "",
                           row.names = FALSE),
              tamanho),
      "\n")
}
