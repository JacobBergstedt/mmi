#' @export
plot_list <- function(l, plt_ind = NULL, save_path = NULL, title_sz = 12,
                      width = 16, height = 10, ncol = NULL, nrow = NULL) {
  library(tidyverse)
  code_as_int <- function(x){
    level.class <- class(levels(x))
    if (is.character(level.class)) {
      val <- as.integer(x)
    } else if (is.integer(level.class)) {
      val <- as.integer(as.character(x))
    }
  }
  if (is.null(plt_ind)) {
    plt_ind <- 1:length(l)
  }
  is.fac <- l %>%
    map_lgl(is.factor)
  l[is.fac] <- map(l[is.fac], code_as_int)
  l <- l[plt_ind]
  plt <- data.frame(val = double(), key = character())
  for (i in seq_len(length(l))) {
    plt <- rbind(plt,
                 data.frame(val = l[[i]], key = names(l)[i]))
  }
  plt$key <- factor(plt$key, levels = names(l))
  plt <- plt %>%
    ggplot(aes(x = val, y = ..count..)) +
    geom_histogram(fill = "skyblue", colour = "black") +
    theme(axis.title = element_blank(),
          axis.line = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size = title_sz)) +
    facet_wrap(~key, scales = 'free', ncol = ncol, nrow = nrow)
  if (is.null(save_path)) {
    plt
  } else {
    ggsave(save_path, plt, width = width, height = height)
  }
}
