#' @export
scoreBarchart_decisions <- function(estimatesByDecision,
                                    estimateCol,
                                    fill = "black",
                                    strokeColor = "black",
                                    strokeSize = .1,
                                    title = "DMCDA decision bar chart",
                                    xLab = "Decisions",
                                    yLab = estimateCol,
                                    decisionOrder = NULL,
                                    decisionLabels = NULL,
                                    decisionLabelCol = NULL,
                                    theme = ggplot2::theme_minimal(base_size = mdmcda::opts$get("ggBaseSize")),
                                    guides = ggplot2::guide_legend(ncol = 2),
                                    legend.position = "bottom",
                                    legend.box.margin = ggplot2::margin(.5, .5, .5, .5, "cm")) {

  if (is.null(decisionLabelCol)) {
    decisionLabelCol <- "decisionLabelCol";

    if (is.null(decisionLabels)) {
      estimatesByDecision[, decisionLabelCol] <-
        estimatesByDecision$decision_id;
    } else {
      estimatesByDecision[, decisionLabelCol] <-
        decisionLabels;
    }
  }

  if (is.null(decisionOrder)) {
    decisionOrder <- estimatesByDecision$decision_id;
    row.names(estimatesByDecision) <-
      estimatesByDecision$decision_id;
  } else {
    if (is.character(decisionOrder) &&
        (length(decisionOrder) == 1)) {
      if ((tolower(decisionOrder) == "decreasing") ||
          (tolower(decisionOrder) == "increasing")) {
        decisionOrder <-
          estimatesByDecision[order(estimatesByDecision[, estimateCol],
                                    decreasing = (tolower(decisionOrder) == "decreasing")),
                              "decision_id"];
        row.names(estimatesByDecision) <-
          estimatesByDecision$decision_id;
      } else {
        stop("If `criteriaOrder` is not 'increasing' or 'decreasing', it ",
             "must be a character vector with the criterion_id value ",
             "in the desired order!");
      }
      estimatesByDecision <- estimatesByDecision[decisionOrder, ];
    }
  }

  estimatesByDecision$decision_id <-
    factor(estimatesByDecision$decision_id,
           levels = decisionOrder,
           labels = estimatesByDecision[, decisionLabelCol],
           ordered = TRUE);

  res <-
    ggplot2::ggplot(data = estimatesByDecision,
                    mapping = ggplot2::aes_string(x = "decision_id",
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
