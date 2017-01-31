#' Convert \code{ROCR::performance} objects to \code{data.frame}
#'
#' @param model \code{performance} instances
#' @inheritParams fortify_base
#' @return data.frame
#' @export
fortify.performance <- function(model, data = NULL, ...) {
  if (is(model, 'performance')) {

    ## Get metadata

    # number of repetitions (bootstrapped, CV, etc)
    n_rep <- length(model@y.values)
    # repetition names
    rep_names <- paste0('Rep', seq(n_rep))
    # length of each repetition
    rep_lengths <- vapply(model@y.values, length, integer(1))

    ## Extract values

    # unlist in long form
    df <- data.frame(y = unlist(model@y.values))
    # label each data point with its repetition name
    df$repn <- as.factor(rep(rep_names, rep_lengths))
    # add x and alpha, which may or may not be there
    df$x <- unlist(model@x.values)
    df$alpha <- unlist(model@alpha.values)

    # Rename/reorder cols
    renames <- c(repn = 'Repetition.Number',
                 x = make.names(model@x.name),
                 y = make.names(model@y.name),
                 alpha = make.names(model@alpha.name))

    names(df) <- renames[names(df)]
    renames <- intersect(renames, names(df))
    df <- df[, renames]

  } else {
    stop(paste0('Unsupported class for fortify.performance: ', class(model)))
  }

  ggfortify:::post_fortify(df, klass = model)
}




