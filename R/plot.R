
violins = function(plotdat, var) {
  ggplot(plotdat,
         aes_string(x = "Timepoint", y = "Count", fill = var)) +
  geom_violin(alpha = 0.5,
              position = position_dodge(width = 0.8)) +
  geom_point(show.legend = FALSE,
             position = position_jitterdodge(seed = 1,
                                             dodge.width = 0.8,
                                             jitter.width = 0.2,
                                             jitter.height = 0.2))
}


spaghetti = function(plotdat, var) {
  varsym = rlang::sym(var)

  ggplot(plotdat, aes(x = Timepoint, y = Count)) +
    geom_line(aes(group = ID, col = !!varsym), size = 0.5, alpha = 0.5) +
    geom_point(show.legend = FALSE) +
    stat_summary(aes(group = !!varsym, color = !!varsym),
                 fun = "mean", geom = "line", size = 3)
}
