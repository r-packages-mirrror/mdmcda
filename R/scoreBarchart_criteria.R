#' Create a bar chart with scores per criterion
#'
#' @param estimatesByCriterion An `estimatesByCriterion` object as created by
#' [aggregate_estimates_by_criterion()].
#' @param estimateCol The column name with the estimates to use.
#' @param fill The color to use to fill the bars, if not by criterion cluster.
#' @param strokeColor,strokeSize The color and pen width of the stroke.
#' @param title,xLab,yLab The title and x and y axis labels.
#' @param criterionOrder The order of the criteria, a vector
#' with criterion identifiers.
#' @param criterionLabels The criterion labels, a names vector where every
#' element if a label and the corresponding name the criterion identifier.
#' @param parentCriterionOrder The order of the parent criteria (the clusters),
#' a vector with criterion identifiers.
#' @param parentCriterionIds_by_childId A names vector to easily find the
#' parent criterion identifier of each criterion identifier; the names should
#' be the criterion identifiers in `criterionOrder`, and the values the
#' corresponding parent criterion identifiers.
#' @param xLabelRotationAngle THe angel to rotate the x axis labels.
#' @param verticalPlot Whether to plot vertically.
#' @param theme The `ggplot2` theme to use.
#' @param guides A guides argument to tweak the legend.
#' @param legend.position,legend.box.margin The position and spacing for the
#' legend.
#'
#' @return A `ggplot2` plot.
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
