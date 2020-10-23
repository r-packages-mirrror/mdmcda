#' Produce a DiagrammeR plot and show it and/or convert to SVG
#'
#' @param criteria The criteria object as produced by a call to, for
#' example, [load_criteria_from_xl()]. The object should contain the
#' criteria tree in `$criteriaTree`.
#' @param labels Optionally, labels to apply to the criteria nodes.
#' @param wrapLabels If applying labels, the number of characters to wrap
#' them to.
#' @param renderGraph Whether to show the graph.
#' @param returnSVG Whether to return SVG or not. Valid values are `FALSE`,
#' to return the `DiagrammeR` object; `TRUE`, to return the SVG;
#' and anything that's not logical to return both in a list.
#'
#' @return Depending on the value of `returnSVG`, a `DiagrammeR` object;,
#' a character vector with SVG, or both in a list.
#' @export
plot_criteria <- function(criteria,
                          labels = NULL,
                          wrapLabels = 60,
                          renderGraph = TRUE,
                          returnSVG = FALSE) {

  graph <-
    data.tree::ToDiagrammeRGraph(
      criteria$criteriaTree
    );

  if (!is.null(labels)) {
    if (is.numeric(wrapLabels)) {
      labels <-
        unlist(
          lapply(
            lapply(
              labels,
              strwrap,
              width = wrapLabels
            ),
            paste0,
            collapse="\n"
          )
        );
    }
    graph <-
      DiagrammeR::set_node_attrs(
        graph,
        "label", labels[DiagrammeR::get_node_df(graph)$label],
        DiagrammeR::get_node_df(graph)$id);
  }

  graph <-
    DiagrammeR::add_global_graph_attrs(graph,
                                       "shape", "box",
                                       "node");
  graph <-
    DiagrammeR::add_global_graph_attrs(graph,
                                       "layout", "dot",
                                       "graph");
  graph <-
    DiagrammeR::add_global_graph_attrs(graph,
                                       "rankdir", "LR",
                                       "graph");
  graph <-
    DiagrammeR::add_global_graph_attrs(graph,
                                       "outputorder", "nodesfirst",
                                       "graph");

  ### Render graph
  if (renderGraph) {
    DiagrammeR::render_graph(graph);
  }

  if (is.logical(returnSVG) && (!returnSVG)) {
    return(invisible(graph));
  }

  ### Produce SVG
  dot_code <- DiagrammeR::generate_dot(graph);
  graphSvg <-
    DiagrammeRsvg::export_svg(DiagrammeR::grViz(dot_code));
  graphSvg <-
    sub(".*\n<svg ", "<svg ", graphSvg);
  graphSvg <- gsub('<svg width=\"[0-9]+pt\" height=\"[0-9]+pt\"\n viewBox=',
                   '<svg viewBox=',
                   graphSvg);

  if (is.logical(returnSVG) && (returnSVG)) {
    return(graphSvg);
  } else {
    return(list(graph = graph,
                graphSvg = graphSvg));
  }
}
