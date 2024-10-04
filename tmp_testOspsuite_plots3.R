
data = rbind(data.table(xValues = 1:10,
                        yValues = 1:10,
                        yMin = seq(1,10)*0.9,
                        yMax = seq(1,10)*1.1,
                        group = NA,
                        colorIndex = c(rep('A',5),rep('B',5)),
                        dataType = 'simulated',
                        xUnit = 'h'),
             data.table(xValues = 1:10,
                        yValues = 1:10+0.1,
                        yMin = seq(1,10)*0.9,
                        yMax = seq(1,10)*1.1,
                        group = 'IV',
                        colorIndex = c(rep('A',5),rep('B',5)),
                        dataType = 'observed',
                        xUnit = 'h'))




ggplot(data) +
  geom_point(aes(x = xValues, y = yValues, shape = 'observed data', color = colorIndex)) +
  geom_errorbar(aes(x = xValues, ymin = yMin, ymax = yMax, color = colorIndex)) +
  geom_line(aes(x = xValues, y = yValues, color = colorIndex), size = 1, linetype = 'solid', show.legend = TRUE) +
  geom_hline(aes(yintercept = 3, linetype = 'LLOQ',color = colorIndex), size = 1,  show.legend = TRUE) +
  guides(
    colour = guide_legend(
      title = 'color',
      title.position = 'top',
      order = 1,
      override.aes = list(shape = NA)
    ),
    shape = guide_legend(
      title = 'shape',
      title.position = 'top',
      order = 2,
      override.aes = list(alpha = 0.4,
                          linetype = 'solid',
                          color = 'black',
                          fill = 'black')
    ),
    linetype = guide_legend(
      title = 'LLOQ',
      order = 3,
      override.aes = list(color = 'black', linewidth = 0.5,size = 0.5,linetype = 'dashed')
    )
  ) +
  scale_linetype_manual(values = c("LLOQ" = "dashed"))
