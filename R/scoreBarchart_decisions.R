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
                                    theme = ggplot2::theme_minimal(base_size = dmcda::opts$get("ggBaseSize")),
                                    guides = ggplot2::guide_legend(ncol = 2),
                                    legend.position = "bottom",
                                    legend.box.margin = ggplot2::margin(.5, .5, .5, .5, "cm")) {

  if (is.null(decisionLabelCol)) {
    decisionLabelCol <- "decisionLabelCol";
  }

  if (!is.null(decisionLabels)) {
    estimatesByDecision[, decisionLabelCol] <-
      decisionLabels;
  } else {
    estimatesByDecision[, decisionLabelCol] <-
      estimatesByDecision$decision_id;
  }

  if (!is.null(decisionOrder)) {
    if (is.character(decisionOrder) &&
        (length(decisionOrder) == 1) &&
        ((tolower(decisionOrder) == "decreasing")) ||
         (tolower(decisionOrder) == "increasing")) {
      decisionOrder <-
        estimatesByDecision[order(estimatesByDecision[, estimateCol],
                                  decreasing = (tolower(decisionOrder) == "decreasing")),
                            "decision_id"];
    }
    row.names(estimatesByDecision) <-
      estimatesByDecision$decision_id;
    estimatesByDecision <- estimatesByDecision[decisionOrder, ];
  } else {
    decisionOrder <- seq_along(1:nrow(estimatesByDecision));
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
