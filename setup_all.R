my_palette = c('#ba0c00', rep.int(c('#000000', '#925E9F'), 5))

signif_size = 10

#' Publication theme
#' 
#' @param base_size The base font size.
theme_Publication <- function(base_size=14) {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(size = 28),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
}

# Orange: color of anger
# Blue: color of sadness
orange = '#ed000099'
blue = '#00468b99'

#' Weighted standard error of the proportion
#' 
#' @description 
#' Computes the weighted standard error of the proportion by normalizing weights and
#' then aggregating using weights. 
#' 
#' @seealso \url{https://stats.stackexchange.com/a/159220}
#' 
#' @return A numeric standard error estimate
weighted_se = function(x, w, na.rm = FALSE) {
  if (length(x) != length(w)) {
    stop(paste0('Length of x (', length(x), ') != length of w (', length(w), ')'))
  }
  w = w / sum(w)
  df = data.frame(x = x, w = w)
  if (na.rm) df = df %>% drop_na()
  sqrt(mean(df$x) * (1 - mean(df$x)) * sum(df$w^2))
}