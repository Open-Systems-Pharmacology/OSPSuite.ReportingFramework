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




ospsuite_plotTimeProfile(
  plotData = data,
  mapping = aes(groupby = colorIndex,
                shape = 'Observed data',
                color = colorIndex),
  groupAesthetics = c("colour", "fill" )) +
  scale_color_manual(values = c('A' = 'blue','B' = 'darkred')) +
  scale_fill_manual(values = c('A' = 'lightblue','B' = 'red')) +
  guides(
    color = guide_legend(
      title = '',
      title.position = 'top',
      order = 1,
      override.aes = list(shape = NA)
    ),
    fill = guide_legend(
      title = '',
      title.position = 'top',
      order = 1
    ),
    shape = guide_legend(
      title = '',
      title.position = 'top',
      order = 2,
      override.aes = list(alpha = 0.4,
                          color = 'black',
                          fill = 'black'),
    ),
    linetype = guide_legend(
      title = '',
      title.position = 'top',
      order = 3
    )
  )
