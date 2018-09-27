#' @export
plot_list <- function(l, plt.ind = NULL, facet_title_size = 9, tick_label_size = 7,
                      nrow = NULL, ncol = NULL) {
  code_as_int <- function(x){
    level.class <- class(levels(x))
    if (is.character(level.class)) {
      val <- as.integer(x)
    } else if (is.integer(level.class)) {
      val <- as.integer(as.character(x))
    }
  }
  if (is.null(plt.ind)) {
    plt.ind <- 1:length(l)
  }
  is.fac <- l %>%
    map_lgl(is.factor)
  l[is.fac] <- map(l[is.fac], code_as_int)
  l <- l[plt.ind]
  plt <- data.frame(val = double(), key = character())
  for (i in seq_len(length(l))) {
    plt <- rbind(plt,
                 data.frame(val = l[[i]], key = names(l)[i]))
  }
  plt$key <- factor(plt$key, levels = names(l))
  plt %>%
    ggplot(aes(x = val, y = ..count..)) +
    geom_histogram(fill = "skyblue", colour = "black") +
    theme(strip.text = element_text(size = facet_title_size),
          axis.text = element_text(size = tick_label_size),
          axis.title = element_blank(),
          axis.line = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank(),
          strip.background = element_blank()) +
    facet_wrap(~key, scales = 'free', nrow = nrow, ncol = ncol)
}
