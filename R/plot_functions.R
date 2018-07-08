#' Plot bargraph 
#'
#' This function will plot means in a bargraph for for 1 or 2 categorical variables.
#' @param data A dataframe
#' @param x The independent variable to be plotted on the x axis. Must be categorical. 
#' @param y The dependent variable to be plotted on the y axis. 
#' @param se The standard error variable to create error bars.
#' @param group A second grouping variable.
#' @param xlab A title for the x-axis. If not defined, will return name of x variable.
#' @param ylab A title for the y-axis. If not defined, will return name of y variable.
#' @keywords barplot
#' @export
#' @import ggplot2
#' @examples
#' aa <- data.frame(condition = c("neutral","positive","negative"),
#'                 accuracy = c(.6, .74, .8),
#'                 se_accuracy = c(.04, .08, .09))
#' bplot(datset = aa, x = "condition", y = "accuracy", e = "se_accuracy",
#'         xlab = "Experimental Condition", ylab = "Mean Accuracy")

bplot <- function(data, x, y, group = NULL, e, xlab = NULL, ylab = NULL) {
  
  x1 <- eval(substitute(x), data, parent.frame())
  y1 <- eval(substitute(y), data, parent.frame())
  e1 <- eval(substitute(e), data, parent.frame())
  g1 <- eval(substitute(group), data, parent.frame())
  
  if(missing(group)){
    
    ggplot2::ggplot(data = data, aes(x = x1, y = y1, fill = x1)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      geom_linerange(aes(ymin = y1 - e1, ymax = y1 + e1)) +
      theme_classic() +
      ggthemes::scale_fill_hc() +
      theme(axis.text = element_text(size = 12), 
            axis.title = element_text(size = 14)) +
      labs(x = ifelse(missing(xlab), deparse(substitute(x)), xlab),
           y = ifelse(missing(ylab), deparse(substitute(y)), ylab))
    
  } else {
    
    ggplot2::ggplot(data = data, aes(x = x1, y = y1, fill = g1)) +
      geom_bar(stat = "identity", show.legend = FALSE, position = "dodge") +
      geom_linerange(aes(ymin = y1 - e1, ymax = y1 + e1), 
                     position = position_dodge(width =  .9)) +
      theme_classic() +
      ggthemes::scale_fill_hc() +
      theme(axis.text = element_text(size = 12), 
            axis.title = element_text(size = 14)) +
      labs(x = ifelse(missing(xlab), deparse(substitute(x)), xlab),
           y = ifelse(missing(ylab), deparse(substitute(y)), ylab))
  }
}

#' Plot Corr-Val
#' 
#' The function will plot a correlation with an annotation of the cross-validated pearson coefficient.
#' 
#' @param x A single vector.
#' @param y A single vector.
#' @return A plot of the correlation between x and y, with the cross-validated correlation coefficient annotated within the graph.
#' @keywords cross-validated correlation
#' @export
#' @import ggplot2
#' @examples
#' x <- rnorm(n = 25, mean = 0, sd = 1)
#' y <- rnorm(n = 25, mean = 25, sd = 5)
#' corr_cv(x, y)

corr_cvplot <- function(x, y, xlab = NULL, ylab = NULL) {
  
  datset <- data.frame(cbind(x, y))
  
  datset <- dplyr::filter(datset, !is.na(x) & !is.na(y))
  
  r <- corr_cv(x, y)
  p <- psych::r.test(n = nrow(datset), r)$p

  ggplot2::ggplot(data = datset, aes(x = datset[[1]], y = datset[[2]])) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    theme_classic() +
    theme(axis.text = element_text(size = 12), 
          axis.title = element_text(size = 14)) +
    labs(x = ifelse(missing(xlab), deparse(substitute(x)), xlab),
         y = ifelse(missing(ylab), deparse(substitute(y)), ylab)) +
    annotate(geom = "text", x = max(datset[1]), y = max(datset[2]), 
             label = sprintf("r = %.3f \n p = %.3f", r, p))
    
}

