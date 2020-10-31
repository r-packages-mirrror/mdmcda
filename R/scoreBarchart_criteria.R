#' @export
scoreBarchart_criteria <- function(estimatesByCriterion,
                                   estimateCol,
                                   fill = "black",
                                   strokeColor = "black",
                                   strokeSize = .1,
                                   title = "MDMCDA criteria bar chart",
                                   xLab = "Criteria",
                                   yLab = estimateCol,
                                   criterionOrder = NULL,
                                   criterionLabels = NULL,
                                   parentCriterionOrder = NULL,
                                   parentCriterionIds_by_childId = NULL,
                                   xLabelRotationAngle = 45,
                                   verticalPlot = FALSE,
                                   theme = ggplot2::theme_minimal(base_size = mdmcda::opts$get("ggBaseSize")),
                                   guides = ggplot2::guide_legend(nrow = 2),
                                   legend.position = "top",
                                   legend.box.margin = ggplot2::margin(.5, .5, .5, .5, "cm")) {

  criterionId_col <- mdmcda::opts$get("criterionId_col");

  if (is.null(criterionOrder)) {
    criterionOrder <-
      estimatesByCriterion[, criterionId_col];
  } else {
    if (is.character(criterionOrder) &&
        (length(criterionOrder) == 1)) {
      if ((tolower(criterionOrder) == "decreasing") ||
          (tolower(criterionOrder) == "increasing")) {

        criterionOrder <-
          estimatesByCriterion[order(estimatesByCriterion[, estimateCol],
                                     decreasing = (tolower(criterionOrder) == "decreasing")),
                               criterionId_col];

      } else {
        stop("If `criterionOrder` is not 'increasing' or 'decreasing', it ",
             "must be a character vector with the criterion identifier values ",
             "in the desired order!");
      }
    }
  }

  if (verticalPlot) {
    criterionOrder <- rev(criterionOrder);
  }

  estimatesByCriterion$criteria <-
    factor(estimatesByCriterion[, criterionId_col],
           levels = criterionOrder,
           labels = criterionLabels[criterionOrder],
           ordered = TRUE);

  if (!is.null(parentCriterionIds_by_childId)) {

    estimatesByCriterion$clusters <-
      factor(
        parentCriterionIds_by_childId[estimatesByCriterion[, criterionId_col]],
        levels = parentCriterionOrder,
        labels = criterionLabels[parentCriterionOrder],
        ordered = TRUE
      );

    res <-
      ggplot2::ggplot(data = estimatesByCriterion,
                      mapping = ggplot2::aes_string(x = "criteria",
                                                    y = estimateCol,
                                                    fill = "clusters")) +
      ggplot2::geom_col(color = strokeColor,
                        size = strokeSize);

  } else {
    res <-
      ggplot2::ggplot(data = estimatesByCriterion,
                      mapping = ggplot2::aes_string(x = "criteria",
                                                    y = estimateCol)) +
      ggplot2::geom_col(fill = fill,
                        color = strokeColor,
                        size = strokeSize);
  }

  res <-
    res +
    theme +
    ggplot2::scale_fill_viridis_d(end=.9, name="Criteria Clusters") +
    ggplot2::theme_minimal() +
    ggplot2::guides(fill = guides,
                    color = guides) +
    ggplot2::theme(axis.text.x.bottom = ggplot2::element_text(angle = xLabelRotationAngle,
                                                              hjust = 1,
                                                              vjust = 0.6),
                   plot.title.position = "plot",
                   legend.position = legend.position,
                   legend.box.margin = legend.box.margin) +
    ggplot2::labs(title = title,
                  x = xLab,
                  y = yLab) +
    NULL;

  if (verticalPlot) {
    res <- res +
      ggplot2::coord_flip();
  }

  return(res);

}
