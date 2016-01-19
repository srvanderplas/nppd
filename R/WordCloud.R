#' @useDynLib nppd
#' @exportPattern "^[[:alpha:]]+"
#' @importFrom Rcpp sourceCpp
NULL

#' .overlap
#'
#' C function to calculate whether text overlaps
#'
#' @noRd
.overlap <- function(x11,y11,sw11,sh11,boxes1){
  .Call("is_overlap",x11,y11,sw11,sh11,boxes1, package = "nppd")
}

#' Modify stem completion to go for the shortest word
#' @param x vector of strings
#' @param dict dictionary
#' @param type Defaults to "shortest"
#' @return PlainTextDocument
#' @export
stemCompletion_mod <- function(x, dict, type = "shortest") {
  x %>% as.character() %>%
    strsplit(split = " ") %>%
    paste() %>%
    tm::stemCompletion(dictionary = dict, type = type) %>%
    paste(sep = "", collapse = " ") %>%
    tm::stripWhitespace() %>%
    tm::PlainTextDocument
}

#' Create a word frequency table from a list of words or phrases
#' @param wordlist vector of strings
#' @param stem T/F - stem?
#' @param rm.stopwords T/F - remove english stopwords?
#' @param stopword.list List of stopwords to remove in addition to english stopwords (only matters if rm.stopwords is T)
#' @return word frequency list
#' @export
MakeWordFreq <- function(wordlist, stem = T, rm.stopwords = T,
                         stopword.list = NULL){

  # Function to make a word frequency table
  wordlist %<>%
    str_replace_all("\\n|\\r|\\\"|/", " ") %>%
    str_replace_all("[\\W“”≤]", " ") %>%
    iconv(to = "ASCII", sub = " ") %>%
    str_replace_all("[\\d]", " ") %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_replace_all("[^[A-z] ]", " ") %>%
    str_replace_all("[\\s]{1,}", " ") %>%
    str_trim() %>%
    str_to_lower() %>%
    paste(., collapse = " ", sep = " ") %>%
    str_split(boundary("word")) %>%
    str_trim() %>%
    unlist()

  if (rm.stopwords) {
    wordlist <- wordlist[!wordlist %in% c(tm::stopwords("en"), stopword.list)]
  } else if (length(stopword.list) > 0) {
    wordlist <- wordlist[!wordlist %in% stopword.list]
  }

  wordlist %<>%
    table() %>%
    as.data.frame(stringsAsFactors = F) %>%
    magrittr::set_names(c("word", "freq"))

  if (stem) {
    wordlist %<>%
      dplyr::mutate(stems = tm::stemDocument(word))
  } else {
    wordlist %<>%
      dplyr::mutate(stems = word)
  }

  wordlist %<>%
    group_by(stems) %>%
    summarize(word = word[which.max(freq)], freq = sum(freq)) %>%
    ungroup() %>%
    select(-stems)

  if (rm.stopwords) {
    wordlist %<>%
      subset(!word %in% c(tm::stopwords("en"), stopword.list))
  } else if (length(stopword.list) > 0) {
    wordlist <- wordlist[!wordlist %in% stopword.list]
  }

  wordlist %<>%
    dplyr::arrange(desc(freq))
}

#' Function to make a wordcloud
#' @param x word frequency data frame (from MakeWordFreq), with columns "word" and "freq"
#' @param color.set vector of colors
#' @param max.words maximum words
#' @param rot.per percentage of words to rotate
#' @param random.order plot words in random order?
#' @param random.color color words randomly?
#' @param ... arguments to wordcloud()
#' @return wordcloud
#' @export
#' @examples
#' readLines("./data/compileText.txt") %>% str_replace_all("[[^[A-z] _]\\\\`]", " ") %>% str_split(" ") %>% unlist %>%  str_trim() %>% table() %>% as.data.frame(stringsAsFactors = F) %>% set_names(c("word", "freq")) %>% filter(nchar(word) > 0) -> tmp
#' tmp %>% MakeWordcloud()
MakeWordcloud <- function(x, color.set = RColorBrewer::brewer.pal(6, "Dark2"), colors = color.set, max.words = 50, rot.per = .3, random.order = F, random.color = T, ...){
  stopifnot(sum(c("word", "freq") %in% names(x)) == 2)
  args <- list(...)
  if ("word" %in% names(x)) {
    args$words <- x$word
  }

  if ("freq" %in% names(x)) {
    args$freq <- x$freq
  }

  args$colors <- colors
  args$random.color <- random.color
  args$max.words <- max.words
  args$random.order <- random.order
  args$rot.per <- rot.per

  do.call(wordcloud, args)
}

#' Wordcloud function, from the wordcloud package.
#' @param words the words
#' @param freq their frequencies
#' @param scale A vector of length 2 indicating the range of the size of the words.
#' @param min.freq words with frequency below min.freq will not be plotted
#' @param max.words Maximum number of words to be plotted. least frequent terms dropped
#' @param random.order plot words in random order. If false, they will be plotted in decreasing frequency
#' @param random.color choose colors randomly from the colors. If false, the color is chosen based on the frequency
#' @param rot.per proportion words with 90 degree rotation
#' @param colors color words from least to most frequent
#' @param ordered.colors if true, then colors are assigned to words in order
#' @param use.r.layout if false, then c++ code is used for collision detection, otherwise R is used
#' @param fixed.asp if TRUE the aspect ratio is fixed. Variable aspect ratio only supported if rot.per == 0
#' @param asp aspect ratio of plot (if fixed aspect ratio)
#' @param ... Additional parameters to be passed to text (and strheight, strwidth).
#' @export
wordcloud <- function(
  words, freq, scale = c(4, 0.5), min.freq = 3, max.words = Inf,
  random.order = TRUE, random.color = FALSE, rot.per = 0.1,
  colors = "black", ordered.colors = FALSE, use.r.layout = FALSE,
  fixed.asp = TRUE, asp = 1, ...)
{
  if (!fixed.asp && rot.per > 0)
    stop("Variable aspect ratio not supported for rotated words. Set rot.per = 0.")

  if (asp == 0 | is.infinite(asp)) {
    stop("Aspect ratio must not be 0 or infinite")
  } else {
    xlen = 1/asp
    ylen = 1
  }

  tails <- "g|j|p|q|y"
  last <- 1
  nc <- length(colors)
  if (missing(freq)) {
    if (!require("tm"))
      stop("freq must either be non - missing, or the tm package must be available")
    if (is.character(words) || is.factor(words)) {
      corpus <- Corpus(VectorSource(words))
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, function(x) removeWords(x,
                                                       stopwords()))
    }
    else corpus <- words
    tdm <- TermDocumentMatrix(corpus)
    freq <- slam::row_sums(tdm)
    words <- names(freq)
  }
  if (ordered.colors) {
    if (length(colors) != 1 && length(colors) != length(words)) {
      stop(paste("Length of colors does not match length of words",
                 "vector"))
    }
  }
  if (min.freq > max(freq))
    min.freq <- 0
  overlap <- function(x1, y1, sw1, sh1) {
    if (!use.r.layout)
      return(.overlap(x1, y1, sw1, sh1, boxes))
    s <- 0
    if (length(boxes) == 0)
      return(FALSE)
    for (i in c(last, 1:length(boxes))) {
      bnds <- boxes[[i]]
      x2 <- bnds[1]
      y2 <- bnds[2]
      sw2 <- bnds[3]
      sh2 <- bnds[4]
      if (x1 < x2)
        overlap <- x1 + sw1 > x2 - s
      else overlap <- x2 + sw2 > x1 - s
      if (y1 < y2)
        overlap <- overlap && (y1 + sh1 > y2 - s)
      else overlap <- overlap && (y2 + sh2 > y1 - s)
      if (overlap) {
        last <<- i
        return(TRUE)
      }
    }
    FALSE
  }
  ord <- rank( -freq, ties.method = "random")
  words <- words[ord <= max.words]
  freq <- freq[ord <= max.words]
  if (ordered.colors) {
    colors <- colors[ord <= max.words]
  }
  if (random.order)
    ord <- sample.int(length(words))
  else ord <- order(freq, decreasing = TRUE)
  words <- words[ord]
  freq <- freq[ord]
  words <- words[freq >= min.freq]
  freq <- freq[freq >= min.freq]
  if (ordered.colors) {
    colors <- colors[ord][freq >= min.freq]
  }
  thetaStep <- 0.1
  rStep <- 0.05
  plot.new()
  op <- par("mar")
  par(mar = c(0, 0, 0, 0))
  if (fixed.asp)
    plot.window(c(0, xlen), c(0, ylen), asp = asp)
  else plot.window(c(0, 1), c(0, 1))
  normedFreq <- freq/max(freq)
  size <- (scale[1] - scale[2]) * normedFreq + scale[2]
  boxes <- list()
  for (i in 1:length(words)) {
    rotWord <- runif(1) < rot.per
    r <- 0
    theta <- runif(1, 0, 2 * pi)
    x1 <- xlen/2
    y1 <- ylen/2
    wid <- strwidth(words[i], cex = size[i], ...)
    ht <- strheight(words[i], cex = size[i], ...)
    if (grepl(tails, words[i]))
      ht <- ht + ht * 0.2
    if (rotWord) {
      tmp <- ht
      ht <- wid
      wid <- tmp
    }
    isOverlaped <- TRUE
    while (isOverlaped) {
      if (!overlap(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, ht) &&
          x1 - 0.5 * wid > 0 &&
          y1 - 0.5 * ht > 0 &&
          x1 + 0.5 * wid < xlen &&
          y1 + 0.5 * ht < ylen) {
        if (!random.color) {
          if (ordered.colors) {
            cc <- colors[i]
          }
          else {
            cc <- ceiling(nc * normedFreq[i])
            cc <- colors[cc]
          }
        }
        else {
          cc <- colors[sample(1:nc, 1)]
        }
        text(x1, y1, words[i], cex = size[i], offset = 0,
             srt = rotWord * 90, col = cc, ...)
        boxes[[length(boxes) + 1]] <- c(x1 - 0.5 * wid,
                                        y1 - 0.5 * ht, wid, ht)
        isOverlaped <- FALSE
      }
      else {
        if (r > sqrt((xlen + ylen)/4)) {
          warning(paste(words[i], "could not be fit on page. It will not be plotted."))
          isOverlaped <- FALSE
        }
        theta <- theta + thetaStep
        r <- r + rStep * thetaStep/(2 * pi)
        x1 <- xlen/2 + r * cos(theta)
        y1 <- ylen/2 + r * sin(theta)
      }
    }
  }
  par(mar = op)
  invisible()
}

#' A word cloud showing the common words among documents
#' @param term.matrix A term frequency matrix whose rows represent words and whose columns represent documents.
#' @param commonality.measure A function taking a vector of frequencies for a single term, and returning a common frequency
#' @param max.words Maximum number of words to be plotted. least frequent terms dropped
#' @param ... Additional parameters to be passed to wordcloud.
#' @export
#' @examples
#' if (require(tm)) {
#'   data(SOTU)
#'   corp <- SOTU
#'   corp <- tm_map(corp, removePunctuation)
#'   corp <- tm_map(corp, content_transformer(tolower))
#'   corp <- tm_map(corp, removeNumbers)
#'   corp <- tm_map(corp, function(x)removeWords(x, stopwords()))
#'
#'   term.matrix <- TermDocumentMatrix(corp)
#'   term.matrix <- as.matrix(term.matrix)
#'   colnames(term.matrix) <- c("SOTU 2010", "SOTU 2011")
#'   comparison.cloud(term.matrix, max.words = 40, random.order = FALSE)
#'   commonality.cloud(term.matrix, max.words = 40, random.order = FALSE)
#' }
commonality.cloud <- function(term.matrix, comonality.measure = min, max.words = 300, ...){
  ndoc <- ncol(term.matrix)
  for (i in 1:ndoc) {
    term.matrix[, i] <- term.matrix[, i] / sum(term.matrix[, i])
  }
  freq <- apply(term.matrix, 1, function(x) comonality.measure(x))
  freq <- freq + min(freq)
  wordcloud(rownames(term.matrix)[freq > 0], freq[freq > 0], min.freq = 0, max.words = max.words, ...)
}


#' Plot a cloud comparing the frequencies of words across documents.
#' @param term.matrix A term frequency matrix whose rows represent words and whose columns represent documents.
#' @param scale A vector of length 2 indicating the range of the size of the words.
#' @param max.words Maximum number of words to be plotted. least frequent terms dropped
#' @param random.order plot words in random order. If false, they will be plotted in decreasing frequency
#' @param rot.per proportion words with 90 degree rotation
#' @param colors color words from least to most frequent
#' @param use.r.layout if false, then c++ code is used for collision detection, otherwise R is used
#' @param title.size Size of document titles
#' @param ... Additional parameters to be passed to text (and strheight, strwidth).
#' @export
comparison.cloud <- function(
  term.matrix, scale = c(4, .5), max.words = 300, random.order = FALSE,
  rot.per = .1, colors = RColorBrewer::brewer.pal(ncol(term.matrix), "Dark2"),
  use.r.layout = FALSE, title.size = 3, ...) {

  ndoc <- ncol(term.matrix)
  thetaBins <- seq(from = 0, to = 2*pi, length = ndoc + 1)
  for (i in 1:ndoc) {
    term.matrix[, i] <- term.matrix[, i] / sum(term.matrix[, i])
  }
  mean.rates <- rowMeans(term.matrix)
  for (i in 1:ndoc) {
    term.matrix[, i] <- term.matrix[, i] - mean.rates
  }

  group <- apply(term.matrix, 1, function(x) which.max(x))
  words <- rownames(term.matrix)
  freq <- apply(term.matrix, 1, function(x) max(x))

  tails <- "g|j|p|q|y"
  last <- 1
  nc <- length(colors)

  overlap <- function(x1, y1, sw1, sh1) {
    if (!use.r.layout)
      return(.overlap(x1, y1, sw1, sh1, boxes))
    s <- 0
    if (length(boxes) == 0)
      return(FALSE)
    for (i in c(last, 1:length(boxes))) {
      bnds <- boxes[[i]]
      x2 <- bnds[1]
      y2 <- bnds[2]
      sw2 <- bnds[3]
      sh2 <- bnds[4]
      if (x1 < x2)
        overlap <- x1 + sw1 > x2 - s
      else
        overlap <- x2 + sw2 > x1 - s

      if (y1 < y2)
        overlap <- overlap && (y1 + sh1 > y2 - s)
      else
        overlap <- overlap && (y2 + sh2 > y1 - s)
      if (overlap) {
        last <<- i
        return(TRUE)
      }
    }
    FALSE
  }

  ord <- rank( -freq, ties.method = "random")
  words <- words[ord <= max.words]
  freq <- freq[ord <= max.words]
  group <- group[ord <= max.words]
  if (random.order) {
    ord <- sample.int(length(words))
  } else {
    ord <- order(freq , decreasing = TRUE)
  }
  words <- words[ord]
  freq <- freq[ord]
  group <- group[ord]
  thetaStep <- .05
  rStep <- .05
  plot.new()
  op <- par("mar")
  par(mar = c(0, 0, 0, 0))
  plot.window(c(0, 1), c(0, 1), asp = 1)
  normedFreq <- freq/max(freq)
  size <- (scale[1] - scale[2])*normedFreq + scale[2]
  boxes <- list()

  #add titles
  docnames <- colnames(term.matrix)
  for (i in 1:ncol(term.matrix)) {
    th <- mean(thetaBins[i:(i + 1)])
    word <- docnames[i]
    wid <- strwidth(word, cex = title.size)*1.2
    ht <- strheight(word, cex = title.size)*1.2
    x1 <- .5 + .45*cos(th)
    y1 <- .5 + .45*sin(th)
    rect(x1 - .5*wid,
         y1 - .5*ht,
         x1 + .5*wid,
         y1 + .5*ht,
         col = "grey90",
         border = "transparent")
    text(x1, y1, word, cex = title.size)
    boxes[[length(boxes) + 1]] <- c(x1 - .5*wid, y1 - .5*ht, wid, ht)
  }

  for (i in 1:length(words)) {
    rotWord <- runif(1) < rot.per
    r <- 0
    theta <- runif(1, 0, 2*pi)
    x1 <- .5
    y1 <- .5
    wid <- strwidth(words[i], cex = size[i], ...)
    ht <- strheight(words[i], cex = size[i], ...)
    #mind your ps and qs
    if (grepl(tails, words[i]))
      ht <- ht + ht*.2
    if (rotWord) {
      tmp <- ht
      ht <- wid
      wid <- tmp
    }
    isOverlaped <- TRUE
    while (isOverlaped) {
      inCorrectRegion <- theta > thetaBins[group[i]] && theta < thetaBins[group[i] + 1]
      if (inCorrectRegion && !overlap(x1 - .5*wid, y1 - .5*ht, wid, ht) &&
         x1 - .5*wid > 0 && y1 - .5*ht > 0 &&
         x1 + .5*wid < 1 && y1 + .5*ht < 1){
        text(x1, y1, words[i], cex = size[i], offset = 0, srt = rotWord*90,
             col = colors[group[i]], ...)
        #rect(x1 - .5*wid, y1 - .5*ht, x1 + .5*wid, y1 + .5*ht)
        boxes[[length(boxes) + 1]] <- c(x1 - .5*wid, y1 - .5*ht, wid, ht)
        isOverlaped <- FALSE
      }else{
        if (r > sqrt(.5)) {
          warning(paste(words[i],
                        "could not be fit on page. It will not be plotted."))
          isOverlaped <- FALSE
        }
        theta <- theta + thetaStep
        if (theta > 2*pi) theta <- theta - 2*pi
        r <- r + rStep*thetaStep/(2*pi)
        x1 <- .5 + r*cos(theta)
        y1 <- .5 + r*sin(theta)
      }
    }
  }
  par(mar = op)
  invisible()
}


wordlayout <- function(x, y, words, cex = 1, rotate90 = FALSE,
                       xlim = c( -Inf, Inf), ylim = c( -Inf, Inf), tstep = .1, rstep = .1, ...){
  tails <- "g|j|p|q|y"
  n <- length(words)
  sdx <- sd(x, na.rm = TRUE)
  sdy <- sd(y, na.rm = TRUE)
  if (sdx == 0)
    sdx <- 1
  if (sdy == 0)
    sdy <- 1
  if (length(cex) == 1)
    cex <- rep(cex, n)
  if (length(rotate90) == 1)
    rotate90 <- rep(rotate90, n)


  boxes <- list()
  for (i in 1:length(words)) {
    rotWord <- rotate90[i]
    r <- 0
    theta <- runif(1, 0, 2*pi)
    x1 <- xo <- x[i]
    y1 <- yo <- y[i]
    wid <- strwidth(words[i], cex = cex[i], ...)
    ht <- strheight(words[i], cex = cex[i], ...)
    #mind your ps and qs
    if (grepl(tails, words[i]))
      ht <- ht + ht*.2
    if (rotWord) {
      tmp <- ht
      ht <- wid
      wid <- tmp
    }
    isOverlaped <- TRUE
    while (isOverlaped) {
      if (!.overlap(x1 - .5*wid, y1 - .5*ht, wid, ht, boxes) &&
         x1 - .5*wid > xlim[1] && y1 - .5*ht > ylim[1] &&
         x1 + .5*wid < xlim[2] && y1 + .5*ht < ylim[2]){
        boxes[[length(boxes) + 1]] <- c(x1 - .5*wid, y1 - .5*ht, wid, ht)
        isOverlaped <- FALSE
      } else {
        theta <- theta + tstep
        r <- r + rstep*tstep/(2*pi)
        x1 <- xo + sdx*r*cos(theta)
        y1 <- yo + sdy*r*sin(theta)
      }
    }
  }
  result <- do.call(rbind, boxes)
  colnames(result) <- c("x", "y", "width", "ht")
  rownames(result) <- words
  result
}

textplot <- function(x, y, words, cex = 1, new = TRUE, show.lines = TRUE, ...){
  if (new)
    plot(x, y, type = "n", ...)
  lay <- wordlayout(x, y, words, cex, ...)
  if (show.lines) {
    for (i in 1:length(x)) {
      xl <- lay[i, 1]
      yl <- lay[i, 2]
      w <- lay[i, 3]
      h <- lay[i, 4]
      if(x[i] < xl || x[i] > xl + w ||
         y[i] < yl || y[i] > yl + h){
        points(x[i], y[i], pch = 16, col = "red", cex = .5)
        nx <- xl + .5*w
        ny <- yl + .5*h
        lines(c(x[i], nx), c(y[i], ny), col = "grey")
      }
    }
  }
  text(lay[, 1] + .5*lay[, 3], lay[, 2] + .5*lay[, 4], words, cex = cex, ...)
}

