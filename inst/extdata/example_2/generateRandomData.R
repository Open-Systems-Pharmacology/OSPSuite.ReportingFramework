dataFolder <- file.path(projectConfiguration$configurationsFolder,'..','..','Data')
if (!dir.exists(dataFolder)) dir.create(dataFolder)

set.seed(1234)

dt = data.table(STUD = 1234,
                Parameter = 'C_max',
                Group = c('adults','toddler','children','school_children','adolescents'),
                N = c(20,12,14,16,18),
                Geomean = round(rnorm(5,mean = 0.25,sd = 0.03),3),
                Geosd = round(rnorm(5,mean = 0.25,sd = 0.03),3))
dt[, CI90_LowerLimit := Geomean - 0.02]
dt[, CI90_UpperLimit:= Geomean + 0.02]


fwrite(dt,file.path(dataFolder,'Cmax_ratio_Case2.csv'))
