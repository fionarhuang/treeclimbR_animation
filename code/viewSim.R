viewSim <- function(obj = lse, zoom_scale = 2, legend_position = "left",
                    show_leaf = FALSE, ref_value = 1, ...){

  md <- metadata(obj)
  # tree
  tree <- rowTree(obj)

  # branch
  branch <- c(md$branch$A, md$branch$B)
  if (!is.numeric(branch)) {
    branch <- transNode(tree = tree, node = branch,
                        use.alias = TRUE, message = FALSE)
  }

  # scenario
  sc <- md$scenario

  # fc
  fc <- md$FC

  # leaves that change abundance
  high <- names(fc)[fc > ref_value]
  low <- names(fc)[fc < ref_value]
  cls <- list(increase = high, decrease = low)
  cls <- lapply(cls, FUN = function(x){
    xx <- signalNode(tree = tree, node = x)
    xs <- findOS(tree = tree, node = xx, only.leaf = FALSE,
                 self.include = TRUE)
    unlist(xs)
  })

  # figure
  df_edge <- data.frame(node = showNode(tree = tree, only.leaf = FALSE)) %>%
    mutate((!!sc) := ifelse(node %in% cls$increase, "increase",
                            ifelse(node %in% cls$decrease, "decrease", "same")))
  head(df_edge)

  p1 <- ggtree(tree, ...) %<+% df_edge +
    aes_string(color = sc) +
    scale_color_manual(values = c(increase = "orange",
                                  decrease = "blue", "same" = "grey"),
                       labels = c(increase = "increase",
                                  decease = "decrease", "same" = "other")) +
    theme(legend.position = legend_position,
          legend.background = element_rect(fill="transparent")) +
    guides(color = guide_legend(title = sc))

  p2 <- scaleClade(p1, node = branch[1], scale = zoom_scale)
  p3 <- scaleClade(p2, node = branch[2], scale = zoom_scale)
  if (show_leaf) {
    p3 <- p3 + geom_tiplab(aes(label = label))
  }

  fc_data <- data.frame(node = transNode(tree = tree, node = names(fc)),
                        fc = fc)
  fc_data <- fc_data[fc_data$fc != ref_value, ]
  if (sc == "US") {
    p3 <- p3 %<+% fc_data + geom_point2(aes(size = fc), alpha = 0.3)
  }
  p3

}

