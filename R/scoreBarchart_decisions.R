#' Create a bar chart with scores per decision
#'
#' @param estimatesByDecision An `estimatesByDecision` object as created by
#' [aggregate_estimates_by_decision()].
#' @param estimateCol The column name with the estimates to use.
#' @param fill The color to use to fill the bars, if not by criterion cluster.
#' @param strokeColor,strokeSize The color and pen width of the stroke.
#' @param title,xLab,yLab The title and x and y axis labels.
#' @param decisionOrder The order of the decisions, a vector
#' with decision identifiers.
#' @param decisionLabels The decision labels, a names vector where every
#' element if a label and the corresponding name the decision identifier.
#' @param xLabelRotationAngle THe angel to rotate the x axis labels.
#' @param verticalPlot Whether to plot vertically.
#' @param theme The `ggplot2` theme to use.
#' @param guides A guides argument to tweak the legend.
#' @param legend.position,legend.box.margin The position and spacing for the
#' legend.
#'
#' @return A `ggplot2` plot.
#' @export
scoreBarchart_decisions <- function(estimatesByDecision,
                                    estimateCol,
                                    fill = "black",
                                    strokeColor = "black",
                                    strokeSize = .1,
                                    title = "MDMCDA criteria bar chart",
                                    xLab = "Criteria",
                                    yLab = estimateCol,
                                    decisionOrder = NULL,
                                    decisionLabels = NULL,
                                    xLabelRotationAngle = 45,
                                    verticalPlot = FALSE,
                                    theme = ggplot2::theme_minimal(base_size = mdmcda::opts$get("ggBaseSize")),
                                    guides = ggplot2::guide_legend(nrow = 2),
                                    legend.position = "top",
                                    legend.box.margin = ggplot2::margin(.5, .5, .5, .5, "cm")) {

  decisionId_col <- mdmcda::opts$get("decisionId_col");

  if (is.null(decisionOrder)) {
    decisionOrder <-
      estimatesByDecision[, decisionId_col];
  } else {
    if (is.character(decisionOrder) &&
        (length(decisionOrder) == 1)) {
      if ((tolower(decisionOrder) == "decreasing") ||
          (tolower(decisionOrder) == "increasing")) {

        decisionOrder <-
          estimatesByDecision[order(estimatesByDecision[, estimateCol],
                                     decreasing = (tolower(decisionOrder) == "decreasing")),
                              decisionId_col];

      } else {
        stop("If `decisionOrder` is not 'increasing' or 'decreasing', it ",
             "must be a character vector with the decision identifier values ",
             "in the desired order!");
      }
    }
  }

  if (verticalPlot) {
    decisionOrder <- rev(decisionOrder);
  }

  if (is.null(decisionLabels)) {
    decisionLabels <-
      stats::setNames(decisionOrder,
                      nm = decisionOrder);
  }

  estimatesByDecision$decision <-
    factor(estimatesByDecision[, decisionId_col],
           levels = decisionOrder,
           labels = decisionLabels[decisionOrder],
           ordered = TRUE);

  res <-
    ggplot2::ggplot(data = estimatesByDecision,
                    mapping = ggplot2::aes_string(x = "decision",
                                                  y = estimateCol)) +
    ggplot2::geom_col(fill = fill,
                      color = strokeColor,
                      size = strokeSize) +
    theme +
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



#' #' @export
#' scoreBarchart_decisions <- function(estimatesByDecision,
#'                                     estimateCol,
#'                                     fill = "black",
#'                                     strokeColor = "black",
#'                                     strokeSize = .1,
#'                                     title = "DMCDA decision bar chart",
#'                                     xLab = "Decisions",
#'                                     yLab = estimateCol,
#'                                     decisionOrder = NULL,
#'                                     decisionLabels = NULL,
#'                                     decisionLabelCol = NULL,
#'                                     theme = ggplot2::theme_minimal(base_size = mdmcda::opts$get("ggBaseSize")),
#'                                     guides = ggplot2::guide_legend(ncol = 2),
#'                                     legend.position = "bottom",
#'                                     legend.box.margin = ggplot2::margin(.5, .5, .5, .5, "cm")) {
#'
#'   if (is.null(decisionLabelCol)) {
#'     decisionLabelCol <- "decisionLabelCol";
#'
#'     if (is.null(decisionLabels)) {
#'       estimatesByDecision[, decisionLabelCol] <-
#'         estimatesByDecision$decision_id;
#'     } else {
#'       estimatesByDecision[, decisionLabelCol] <-
#'         decisionLabels;
#'     }
#'   }
#'
#'   if (is.null(decisionOrder)) {
#'     decisionOrder <- estimatesByDecision$decision_id;
#'     row.names(estimatesByDecision) <-
#'       estimatesByDecision$decision_id;
#'   } else {
#'     if (is.character(decisionOrder) &&
#'         (length(decisionOrder) == 1)) {
#'       if ((tolower(decisionOrder) == "decreasing") ||
#'           (tolower(decisionOrder) == "increasing")) {
#'         decisionOrder <-
#'           estimatesByDecision[order(estimatesByDecision[, estimateCol],
#'                                     decreasing = (tolower(decisionOrder) == "decreasing")),
#'                               "decision_id"];
#'         row.names(estimatesByDecision) <-
#'           estimatesByDecision$decision_id;
#'       } else {
#'         stop("If `criterionOrder` is not 'increasing' or 'decreasing', it ",
#'              "must be a character vector with the criterion_id value ",
#'              "in the desired order!");
#'       }
#'       estimatesByDecision <- estimatesByDecision[decisionOrder, ];
#'     }
#'   }
#'
#'   estimatesByDecision$decision_id <-
#'     factor(estimatesByDecision$decision_id,
#'            levels = decisionOrder,
#'            labels = estimatesByDecision[, decisionLabelCol],
#'            ordered = TRUE);
#'
#'   res <-
#'     ggplot2::ggplot(data = estimatesByDecision,
#'                     mapping = ggplot2::aes_string(x = "decision_id",
#'                                                   y = estimateCol)) +
#'     ggplot2::geom_col(fill = fill,
#'                       color = strokeColor,
#'                       size = strokeSize) +
#'     theme +
#'     ggplot2::guides(fill = guides,
#'                     color = guides) +
#'     ggplot2::theme(axis.text.x.bottom = ggplot2::element_text(angle = 90,
#'                                                               hjust = 1,
#'                                                               vjust = 0.5),
#'                    plot.title.position = "plot",
#'                    legend.position = legend.position,
#'                    legend.box.margin = legend.box.margin) +
#'     ggplot2::labs(title = title,
#'                   x = xLab,
#'                   y = yLab) +
#'     NULL;
#'
#'   return(res);
#'
#' }
