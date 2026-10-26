## Numero em modo matematico: 1{,}23
##
## O sub() final elimina o zero negativo. Uma quantidade que arredonda para zero
## por baixo sai do formatC como "-0" ou "-0{,}00", e o sinal ali afirma algo que
## o numero nao diz: sugere quantidade negativa onde ha apenas arredondamento.
## O separador decimal e a virgula, "{,}", por omissao. Um deck que prefira o
## ponto declara `options(lectures_decimal = ".")` no seu <<setup>> (decisao
## do professor em 2026-09-15, para RLM: Teste de Hipoteses); a opcao vale
## para todos os formatadores, e os numeros escritos a mao no .org precisam
## seguir a mesma escolha.
nm <- function(x, d = 2) {
  sep <- getOption("lectures_decimal", "{,}")
  s <- formatC(x, format = "f", digits = d, big.mark = "")
  s <- sub("\\.", sep, s, fixed = FALSE)
  sub(paste0("^-(0(", gsub("([{}.])", "\\\\\\1", sep), "0*)?)$"), "\\1", s)
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
                      motor = c("tikz", "pdf"), alt = NULL, centro = TRUE) {
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
    ## Centralizada por omissao (decisao do professor, 2026-10-07): uma figura
    ## de exemplo encostada a esquerda do frame foi corrigida a mao mais de uma
    ## vez. `centro = FALSE` para a figura que divide coluna com outro objeto.
    if (centro) cat("\\begin{center}\n")
    if (is.null(alt)) {
      cat(sprintf("\\input{%s}\n", caminho))
    } else {
      cat(sprintf("\\altfig{%s}{\\input{%s}}\n", alt_ascii(alt), caminho))
    }
    if (centro) cat("\\end{center}\n")
  }
  invisible(caminho)
}

## Prosa com numeros embutidos. `{{ }}` delimita uma expressao R, avaliada no
## ambiente de quem chama, de modo que `pr("erro padrao ${{nm(cs$S, 3)}}$")`
## le o objeto do bloco. Sao os delimitadores que o banco de questoes ja usa
## em `bq_tpl`, com a mesma armadilha: chave do LaTeX nao encosta em marcador.
## `\frac{{{a}}}{b}` aborta com "Expecting '}}'"; escrever `\frac{ {{a}} }{b}`,
## com o espaco que o TeX ignora em modo matematico. `}}` fora de marcador
## (`b_{1}}`) e literal, e `{{` literal se escreve `{{{{`.
##
## Varios argumentos sao concatenados sem separador: uma frase por argumento,
## cada uma terminando em "\n", preserva a regra de uma frase por linha no
## .org e no .tex. `.trim = FALSE` porque o glue, por omissao, apaga a quebra
## inicial e a final.
pr <- function(..., env = parent.frame()) {
  txt <- paste(c(...), collapse = "")
  cat(as.character(glue::glue(txt, .open = "{{", .close = "}}",
                              .envir = env, .trim = FALSE)), "\n", sep = "")
}

## Semente com os tres geradores fixados. O padrao do R e este desde a 3.6.0,
## mas e um padrao, e o R ja o trocou uma vez (sample() em 3.6.0). Fixa-lo
## aqui torna a sequencia a mesma em qualquer R >= 3.6, em qualquer maquina,
## seja qual for o padrao da versao. O que este mecanismo NAO cobre: sorteio
## feito por pacote com gerador proprio, e mudanca no NUMERO de chamadas ao
## gerador entre duas versoes do codigo -- essa muda o fluxo, e nenhuma
## semente a compensa.
semente <- function(s) {
  set.seed(s, kind = "Mersenne-Twister", normal.kind = "Inversion",
           sample.kind = "Rejection")
}

## Paletas por categoria. "pos" segue o tema whale: azul-marinho em tres
## intensidades, ocre, cinza e terracota.
paletas_aula <- list(
  pos = list(qualitativa = c("#1f3864", "#4a6fa5", "#8fb0d9", "#c9a227", "#7f7f7f", "#b85c38"),
             pontos      = c("#1f3864", "#b85c38", "#4a6fa5", "#7f7f7f"),
             divergente  = c(baixo = "#b85c38", alto = "#1f3864")))

paleta_aula <- function() {
  p <- getOption("lectures_paleta")
  if (is.null(p) || is.null(paletas_aula[[p]]))
    stop("declare options(lectures_paleta = ...) no setup da disciplina; paletas: ",
         paste(names(paletas_aula), collapse = ", "))
  paletas_aula[[p]]
}

tema_aula <- function(base_size = 9) ggplot2::theme_minimal(base_size = base_size)

## Tabela LaTeX a partir de um data.frame ja formatado como texto.
## O comando de tamanho entra DENTRO do float. Envolver o \begin{table} num
## grupo TeX -- {\small \begin{table}...} -- faz a tabela sumir do PDF sem erro
## de compilacao; ver Lectures/CLAUDE.md 5.9.
encolhe <- function(x, tamanho) {
  if (is.null(tamanho)) return(x)
  sub("\\begin{table}", sprintf("\\begin{table}\n\\%s", tamanho), x, fixed = TRUE)
}

## Coluna numerica sai alinhada a DIREITA por omissao (decisao do professor,
## 2026-09-15; Lectures/CLAUDE.md 5.9): com o mesmo numero de casas em toda a
## coluna, o separador decimal fica alinhado e um sinal negativo nao desloca
## os algarismos, o que a coluna alinhada a esquerda fazia. Uma coluna e
## numerica quando todas as celulas nao vazias sao um numero, com ou sem
## cifroes, sinal, "<" ou ">" (o p-valor "< 0{,}001") e estrelas.
coluna_numerica <- function(x) {
  x <- trimws(as.character(x)); x <- x[nzchar(x)]
  length(x) > 0 &&
    all(grepl("^\\$?\\s*[<>]?\\s*-?[0-9]+([{,}.]+[0-9]+)?(\\^\\{\\*+\\})?\\s*\\$?$", x))
}

tab <- function(df, caption = NULL, align = NULL, tamanho = NULL) {
  if (is.null(align)) align <- ifelse(vapply(df, coluna_numerica, logical(1)), "r", "l")
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

## Registro global de exercicios. Criado uma vez: cada familia do banco
## re-sourcia esta camada, e um registro recriado a cada source apagaria o que
## as familias anteriores registraram.
if (!exists("EXERCICIOS_REGISTRO", envir = globalenv(), inherits = FALSE))
  assign("EXERCICIOS_REGISTRO", new.env(parent = emptyenv()), envir = globalenv())

## Um exercicio e uma lista com id, tema, titulo, ctx, chave, e tres funcoes:
## dados(ctx, chave); enunciado(dados, ctx), que devolve list(cabeca, itens);
## e resolucao(dados, ctx), que devolve um vetor com uma parte por item. As
## partes sao LaTeX. Partir por item e o que deixa o deck emitir cada
## resolucao num frame, sem `shrink` (CLAUDE.md 26b).
exercicio_registra <- function(x) {
  faltam <- setdiff(c("id", "tema", "titulo", "ctx", "chave", "dados",
                      "enunciado", "resolucao"), names(x))
  if (length(faltam)) stop("exercicio sem ", paste(faltam, collapse = ", "))
  assign(x$id, x, envir = EXERCICIOS_REGISTRO)
  invisible(x$id)
}

exercicio_busca <- function(id) {
  if (!exists(id, envir = EXERCICIOS_REGISTRO, inherits = FALSE))
    stop("exercicio '", id, "' nao registrado -- a familia do banco foi carregada?")
  get(id, envir = EXERCICIOS_REGISTRO)
}

## Rotulo de item: (a), (b), ... em paragrafo, e nao `enumerate`, para que um
## item avulso emitido num frame do deck traga o mesmo rotulo da lista.
exercicio_rotulo <- function(k, texto)
  sprintf("\\par\\noindent\\textbf{(%s)}~%s\n", letters[k], texto)

## Texto de um exercicio. `ctx` e `chave` trocam o contexto e a amostra sem
## editar o banco.
exercicio_texto <- function(id, ctx = NULL, chave = NULL) {
  x <- exercicio_busca(id)
  ctx <- ctx %||% x$ctx; chave <- chave %||% x$chave
  d <- x$dados(ctx, chave)
  en <- x$enunciado(d, ctx); rs <- x$resolucao(d, ctx)
  if (length(rs) != length(en$itens))
    stop("exercicio '", id, "': ", length(en$itens), " itens e ",
         length(rs), " partes de resolucao")
  list(id = id, titulo = x$titulo, tema = x$tema, ctx = ctx$nome, chave = chave,
       cabeca = en$cabeca, itens = en$itens, resolucao = rs)
}

## Emissao no deck. `parte = "enunciado"` emite a cabeca e os itens;
## `parte = "resolucao"` emite as partes da resolucao, ou so a do `item`.
exercicio_emite <- function(id, parte = c("enunciado", "resolucao"), item = NULL,
                            ctx = NULL, chave = NULL) {
  tx <- exercicio_texto(id, ctx, chave)
  k <- item %||% seq_along(tx$itens)
  out <- if (match.arg(parte) == "enunciado")
    c(tx$cabeca, exercicio_rotulo(k, tx$itens[k]))
  else exercicio_rotulo(k, tx$resolucao[k])
  cat(out, sep = "\n")
  invisible(tx)
}

## Ano, semestre, turma e disciplina, lidos do primeiro heading do
## CRONOGRAMA.org ("* Turma C (Noturno) — 2026/2"), e nao escritos a mao.
lista_edicao <- function(arquivo = NULL) {
  if (is.null(arquivo)) {
    cand <- file.path(c(".", "..", "../.."), "CRONOGRAMA.org")
    arquivo <- cand[file.exists(cand)][1]
    if (is.na(arquivo)) stop("CRONOGRAMA.org nao encontrado")
  }
  h <- grep("^\\* Turma", readLines(arquivo, encoding = "UTF-8"), value = TRUE)[1]
  m <- regmatches(h, regexec("Turma (\\S+).*?(\\d{4})/(\\d)", h))[[1]]
  if (length(m) != 4) stop("cabecalho de turma ilegivel em ", arquivo, ": ", h)
  list(disciplina = basename(dirname(normalizePath(arquivo))),
       ano = as.integer(m[3]), semestre = as.integer(m[4]), turma = m[2])
}

LISTA_AVISO <- paste(
  "Na prova, itens deste tipo podem aparecer como múltipla escolha, com",
  "alternativas construídas a partir de erros frequentes de cálculo e de",
  "leitura.")

## Lista de exercicios a partir de IDs. Emite LaTeX e grava o snapshot da
## edicao em `snapshots/<lista>.tsv`, substituindo as linhas da mesma edicao:
## exportar de novo nao acumula duplicatas.
lista_monta <- function(ids, lista, resolucao = FALSE, edicao = lista_edicao(),
                        aviso = LISTA_AVISO, snapshot = TRUE) {
  tx <- lapply(ids, exercicio_texto)
  if (nzchar(aviso)) cat("\\noindent ", aviso, "\n\n", sep = "")
  for (k in seq_along(tx)) {
    t <- tx[[k]]; j <- seq_along(t$itens)
    cat(sprintf("\\subsection*{Exercício %d. %s}\n\n", k, t$titulo))
    cat(t$cabeca, exercicio_rotulo(j, t$itens), sep = "\n")
    if (resolucao)
      cat("\\subsubsection*{Resolução}\n", exercicio_rotulo(j, t$resolucao), sep = "\n")
  }
  if (snapshot) {
    novo <- data.frame(
      disciplina = edicao$disciplina, ano = edicao$ano,
      semestre = edicao$semestre, turma = edicao$turma, lista = lista,
      posicao = seq_along(tx), id = ids,
      contexto = vapply(tx, `[[`, "", "ctx"),
      chave = vapply(tx, `[[`, "", "chave"),
      sha256 = vapply(tx, function(t)
        digest::digest(paste(c(t$cabeca, t$itens), collapse = "\n"),
                       algo = "sha256", serialize = FALSE), ""))
    arq <- file.path("snapshots", paste0(lista, ".tsv"))
    dir.create("snapshots", showWarnings = FALSE)
    velho <- if (file.exists(arq))
      utils::read.delim(arq, colClasses = "character") else novo[0, ]
    mesma <- velho$disciplina == edicao$disciplina & velho$ano == edicao$ano &
             velho$semestre == edicao$semestre & velho$turma == edicao$turma &
             velho$lista == lista
    utils::write.table(rbind(velho[!mesma, , drop = FALSE], novo), arq,
                       sep = "\t", quote = FALSE, row.names = FALSE)
  }
  invisible(tx)
}

## As cinco alternativas da asercao-razao, na ordem canonica. Copia das de
## moodle_setup (MDL_ALT_RAZAO): esta camada nao depende daquela.
FX_ALT_RAZAO <- c(
  "As duas asserções são verdadeiras, e a segunda justifica a primeira.",
  "As duas asserções são verdadeiras, mas a segunda não justifica a primeira.",
  "A primeira asserção é verdadeira e a segunda é falsa.",
  "A primeira asserção é falsa e a segunda é verdadeira.",
  "As duas asserções são falsas.")

## Avalia {{ }} em todo texto de uma definicao; funcao e numero passam intactos.
fx_glue <- function(x, env) {
  if (is.list(x)) return(lapply(x, fx_glue, env = env))
  if (is.character(x))
    return(vapply(x, function(s) as.character(glue::glue(s, .open = "{{", .close = "}}",
                                                          .envir = env, .trim = FALSE)),
                  "", USE.NAMES = FALSE))
  x
}

fx_questao <- function(..., env = parent.frame()) fx_glue(list(...), env)

fx_passo <- function(rotulo, afirma, porque = character(0))
  list(rotulo = rotulo, afirma = afirma, porque = porque)

fx_passos <- function(passos) {
  cat("\\begin{itemize}\n")
  for (p in passos) {
    ## A afirmacao abre frase depois do rotulo: primeira letra em maiuscula.
    af <- sub("^(\\p{Ll})", "\\U\\1", p$afirma, perl = TRUE)
    cat("\\item \\textbf{", p$rotulo, ".} ", af, "\n", sep = "")
    if (length(p$porque))
      cat("\\begin{itemize}\n", paste0("\\item ", p$porque, "\n"), "\\end{itemize}\n", sep = "")
  }
  cat("\\end{itemize}\n")
}

fx_formulario <- function(f) {
  if (!length(f)) return(invisible())
  cat("\\begin{formulario}\n"); do.call(eqs, as.list(f)); cat("\\end{formulario}\n\n")
}

## No deck (pasta aulas/), o enunciado leva roteiro e formulario; na folha
## impressa de fixacao.org, nao.
fx_no_deck <- function() getOption("fx_slide", basename(getwd()) == "aulas")

## Formulario do slide: sem o ambiente da folha, que o deck nao define.
fx_formulario_slide <- function(f) {
  if (!length(f)) return(invisible())
  cat("\\tiny\\textbf{Fórmulas}\n"); do.call(eqs, as.list(f))
}

## Roteiro em caixa, separado dos itens por um espaco (objetiva e
## assercao-razao); no calculo, que ja divide o slide com tabela e formulario,
## uma linha corrida abaixo dos itens.
fx_roteiro <- function(etapas, sep = " $\\to$ ", caixa = TRUE) {
  if (!length(etapas)) return(invisible())
  if (caixa) cat("\\vspace{0.6em}\n\\begin{intuicaobox}[Roteiro]\n", paste(etapas, collapse = sep), "\n\\end{intuicaobox}\n\n", sep = "")
  else cat("\\textbf{Roteiro.} ", paste(etapas, collapse = sep), "\n\n", sep = "")
}

## Com formulario no deck, a tabela divide a largura com ele.
fx_cabeca <- function(q, formulario = character(0)) {
  if (length(q$apresentacao)) cat(q$apresentacao, "\n\n", sep = "")
  if (is.function(q$tabela) && length(formulario)) {
    ## onlytextwidth e o \centering anulado: a tabela comeca na margem do enunciado.
    cat("\\begin{columns}[t,onlytextwidth]\\begin{column}{0.52\\textwidth}\\let\\centering\\raggedright\n"); q$tabela()
    cat("\\end{column}\\begin{column}{0.46\\textwidth}\n"); fx_formulario_slide(formulario)
    cat("\\end{column}\\end{columns}\n\n")
  } else if (is.function(q$tabela)) { q$tabela(); cat("\n") }
  else if (length(formulario)) fx_formulario_slide(formulario)
}

fx_alternativas <- function(opcoes)
  cat(paste0("(", LETTERS[seq_along(opcoes)], ") ", opcoes, "\n\n"), sep = "")

## Gabarito curto, sem o procedimento: a resposta de cada item numa linha. O
## rotulo \\gabaritorotulo{<id>} e definido pelo documento que o emite: no deck
## (preambule.tex) vira "Exercicio N", com N do contador de exercicios; no
## banco (fixacao_setup.org), so "Gabarito".
fx_gabarito <- function(id, respostas) {
  cat("\\textbf{\\gabaritorotulo{", id, "}.} ",
      paste(respostas, collapse = "; "), "\n\n", sep = "")
}

## Calculo: q$itens e uma lista de list(pergunta, passos, gabarito); q$roteiro,
## opcional, e uma funcao que desenha o procedimento antes do formulario.
fx_calculo <- function(q, parte = c("enunciado", "resolucao", "gabarito"), id = "") {
  parte <- match.arg(parte)
  if (parte == "gabarito") {
    fx_gabarito(id, vapply(seq_along(q$itens), function(k)
      sprintf("(%s) %s", letters[k], q$itens[[k]]$gabarito), ""))
  } else if (parte == "enunciado") {
    deck <- fx_no_deck()
    fx_cabeca(q, if (deck) q$formulario else character(0))
    for (k in seq_along(q$itens))
      cat("(", letters[k], ") ", q$itens[[k]]$pergunta, "\n\n", sep = "")
    if (deck) fx_roteiro(vapply(seq_along(q$itens), function(k)
      sprintf("(%s) %s", letters[k], paste(vapply(q$itens[[k]]$passos, `[[`, "", "rotulo"), collapse = ", ")), ""), caixa = FALSE)
  } else {
    if (is.function(q$roteiro)) { q$roteiro(); cat("\n") }
    fx_formulario(q$formulario)
    for (k in seq_along(q$itens)) {
      cat("\\textbf{(", letters[k], ")}\n", sep = "")
      fx_passos(q$itens[[k]]$passos)
    }
  }
}

## Objetiva: q$opcoes, q$correta (indice) e q$porques (um por alternativa).
## A resolucao nomeia cada alternativa pela mesma letra do enunciado, porque as
## duas partes leem a mesma lista.
fx_objetiva <- function(q, parte = c("enunciado", "resolucao", "gabarito"), id = "") {
  parte <- match.arg(parte)
  if (parte == "gabarito") {
    fx_gabarito(id, sprintf("(%s)", LETTERS[q$correta]))
  } else if (parte == "enunciado") {
    fx_cabeca(q); cat(q$pergunta, "\n\n", sep = ""); fx_alternativas(q$opcoes)
    if (fx_no_deck()) fx_roteiro(q$dica)
  } else {
    fx_formulario(q$formulario)
    cat("\\textbf{Resposta: (", LETTERS[q$correta], ")}\n", sep = "")
    alt <- lapply(seq_along(q$opcoes), function(j)
      fx_passo(sprintf("(%s) %s", LETTERS[j], if (j == q$correta) "é correta" else "é falsa"),
               q$porques[j]))
    fx_passos(c(q$passos, alt))
  }
}

## Assercao-razao: q$I, q$II, q$gabarito (1 a 5, na ordem de FX_ALT_RAZAO).
fx_razao <- function(q, parte = c("enunciado", "resolucao", "gabarito"), id = "") {
  parte <- match.arg(parte)
  if (parte == "gabarito") {
    fx_gabarito(id, sprintf("(%s)", LETTERS[q$gabarito]))
  } else if (parte == "enunciado") {
    fx_cabeca(q)
    cat("\\textbf{I.} ", q$I, "\n\n\\textbf{PORQUE}\n\n\\textbf{II.} ", q$II, "\n\n", sep = "")
    fx_alternativas(FX_ALT_RAZAO)
    if (fx_no_deck()) fx_roteiro(q$dica)
  } else {
    fx_formulario(q$formulario)
    cat("\\textbf{Resposta: (", LETTERS[q$gabarito], ")}\n", sep = "")
    fx_passos(q$passos)
  }
}
