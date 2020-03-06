#' @export
scoreBarchart_criteria <- function(estimatesByCriterion,
                                   estimateCol,
                                   fill = "black",
                                   strokeColor = "black",
                                   strokeSize = .1,
                                   title = "DMCDA criteria bar chart",
                                   xLab = "Criteria",
                                   yLab = estimateCol,
                                   criteriaOrder = NULL,
                                   criteriaLabels = NULL,
                                   theme = ggplot2::theme_minimal(base_size = dmcda::opts$get("ggBaseSize"))) {

  if (!is.null(criteriaOrder)) {
    row.names(estimatesByCriterion) <-
      estimatesByCriterion$criterion_id;
    estimatesByCriterion <- estimatesByCriterion[criteriaOrder, ];
    estimatesByCriterion$criterion_id <-
      factor(estimatesByCriterion$criterion_id,
             levels = criteriaOrder,
             ordered = TRUE);
  } else {
    tmpDf <- estimatesByCriterion;
  }

  res <-
    ggplot2::ggplot(data = estimatesByCriterion,
                    mapping = ggplot2::aes_string(x = "criterion_id",
                                                  y = estimateCol)) +
    ggplot2::geom_col(fill = fill,
                      color = strokeColor,
                      size = strokeSize) +
    theme +
    ggplot2::guides(color = ggplot2::guide_legend(ncol = 1)) +
    ggplot2::theme(axis.text.x.bottom = ggplot2::element_text(angle = 45,
                                                              hjust = 1,
                                                              vjust = 1)) +
    ggplot2::labs(title = title,
                  x = xLab,
                  y = yLab) +
    NULL;

  if (!is.null(criteriaLabels)) {
    res <- res +
      ggplot2::scale_x_discrete(labels = criteriaLabels);
  }

  return(res);

}
