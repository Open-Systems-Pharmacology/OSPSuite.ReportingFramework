data = rbind(data.table(xValues = 1:10,
                  yValues = 1:10,
                  yMin = seq(1,10)*0.9,
                  yMax = seq(1,10)*1.1,
                  group = NA,
                  colorIndex = 'simulated time profile',
                  dataType = 'simulated',
                  xUnit = 'h'),
             data.table(xValues = 1:10,
                        yValues = 1:10+0.1,
                        yMin = seq(1,10)*0.9,
                        yMax = seq(1,10)*1.1,
                        group = 'IV',
                        lloq = 3,
                        colorIndex = 'simulated time profile',
                        dataType = 'observed',
                        xUnit = 'h'),
             fill = TRUE)



ospsuite_plotTimeProfile(
  plotData = data,
  mapping = aes(groupby = colorIndex,
              shape = 'Observed data',
              color = colorIndex),
  groupAesthetics = c("colour", "fill" ),
  geomLineAttributes = list(linetype = 'solid', show.legend = TRUE)) +
  scale_color_manual(values = c('simulated time profile' = 'blue')) +
  scale_fill_manual(values = c('simulated time profile' = 'lightblue')) +
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
                          linetype = 'solid',
                          color = 'black',
                          fill = 'black'),
    ),
  linetype = guide_legend(
    title = '',
    order = 3,
    override.aes = list(color = 'black', linewidth = 0.5,linetype = 'dashed')
  )
)
