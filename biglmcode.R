#Loading biglm library
library(biglm)

#big lm modeling
big.model = biglm(Temp ~ Wind + Solar.R + Ozone, data = airquality)
summary(big.model)

#lm modeling
model = lm(Temp ~
             Wind + Solar.R + Ozone, data = airquality)
summary(model)[4]
