essai <- function(group, x) {
  st <- spearman2(group, x)
  list(P = st["P"], stat = st["F"], df = st[c("df1", "df2")], 
       testname = if (st["df1"] == 1) "Wilcoxon" else "Kruskal-Wallis", 
       statname = "F", latexstat = "F_{df}", plotmathstat = "F[df]")
}
essai(esoph$tobgp, esoph$ncases)
kruskal.test(esoph$ncases, esoph$tobgp)


function(x, ...) {
    results <- NULL
    for (i in 1:n) {
      func <- match.fun(fs[[i]])
      args <- formals(func)
      if (all(names(args) != "..."))
        formals(func) <- c(args, alist(...=))
      tmp <- func(x, ...)
      names(tmp) <- paste(fnames[i], names(tmp))
      results <- c(results, tmp)
    }
    results
  }


function(x, ...) {
    results <- NULL
    args <- list(...)
    namesargs <- names(args)
    for (i in 1:n) {
      func <- match.fun(fs[[i]])
      forms <- formals(func)
      namesforms <- names(forms)
      if (all(namesforms != "...")) {
        finalargs <- c(x, args[namesargs %in% namesforms])
      } else {
        finalargs <- c(x, args)
      }
      tmp <- do.call(func, finalargs)
      names(tmp) <- paste(fnames[i], names(tmp))
      results <- c(results, tmp)
    }
    results
  }


toto <- function(x, ...) {
    results <- NULL
    args <- list(...)
    namesargs <- names(args)
    for (i in 1:n) {
      func <- match.fun(fs[[i]])
      forms <- formals(func)
      namesforms <- names(forms)
      if (all(namesforms != "...")) {
        finalargs <- c(list(x = x), args[namesargs %in% namesforms])
      } else {
        finalargs <- c(list(x = x), args)
      }
      tmp <- do.call(func, finalargs)
      names(tmp) <- paste(fnames[i], names(tmp))
      results <- c(results, tmp)
    }
    results
  }
