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
                                   criteriaLabelCol = NULL,
                                   theme = ggplot2::theme_minimal(base_size = dmcda::opts$get("ggBaseSize")),
                                   guides = ggplot2::guide_legend(ncol = 2),
                                   legend.position = "bottom",
                                   legend.box.margin = ggplot2::margin(.5, .5, .5, .5, "cm")) {



  if (is.null(criteriaLabelCol)) {
    criteriaLabelCol <- "criteriaLabelCol";
  }

  if (is.null(criteriaLabels)) {
    estimatesByCriterion[, criteriaLabelCol] <-
      estimatesByCriterion$criterion_id;
  } else {
    estimatesByCriterion[, criteriaLabelCol] <-
      criteriaLabels;
  }

  if (!is.null(criteriaOrder)) {
    if (is.character(criteriaOrder) &&
        (length(criteriaOrder) == 1) &&
        ((tolower(criteriaOrder) == "decreasing") ||
         (tolower(criteriaOrder) == "increasing"))) {
      criteriaOrder <-
        estimatesByCriterion[order(estimatesByCriterion[, estimateCol],
                                   decreasing = (tolower(criteriaOrder) == "decreasing")),
                            "decision_id"];
    }
    row.names(estimatesByCriterion) <-
      estimatesByCriterion$criterion_id;
    estimatesByCriterion <- estimatesByCriterion[criteriaOrder, ];
  } else {
    criteriaOrder <- seq_along(1:nrow(estimatesByCriterion));
  }

  estimatesByCriterion$criterion_id <-
    factor(estimatesByCriterion$criterion_id,
           levels = criteriaOrder,
           labels = estimatesByCriterion[, criteriaLabelCol],
           ordered = TRUE);

  res <-
    ggplot2::ggplot(data = estimatesByCriterion,
                    mapping = ggplot2::aes_string(x = "criterion_id",
                                                  y = estimateCol)) +
    ggplot2::geom_col(fill = fill,
                      color = strokeColor,
                      size = strokeSize) +
    theme +
    ggplot2::guides(fill = guides,
                    color = guides) +
    ggplot2::theme(axis.text.x.bottom = ggplot2::element_text(angle = 90,
                                                              hjust = 1,
                                                              vjust = 0.5),
                   plot.title.position = "plot",
                   legend.position = legend.position,
                   legend.box.margin = legend.box.margin) +
    ggplot2::labs(title = title,
                  x = xLab,
                  y = yLab) +
    NULL;

  return(res);

}
