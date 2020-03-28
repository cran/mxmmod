## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
eval_semplots <- F

## ---- message=F, warning=F-----------------------------------------------
library(tidyverse)
library(OpenMx)
library(mxmmod)

## ------------------------------------------------------------------------
data(nlsy97depression)
summary(nlsy97depression)

## ---- fig.width=6, fig.height=6, fig.align='center'----------------------
set.seed(1000)
subset <- sample(unique(nlsy97depression$pid), 9)

nlsy97depression %>%
  filter(pid %in% subset) %>%
  gather(measure, val, -pid, -occasion) %>%
  ggplot(aes(x=occasion, group=measure, color=measure, y=val)) +
  geom_line(position=position_jitter(w=0.1, h=0.1)) +
  facet_wrap(~pid)

## ---- fig.width=6, fig.height=4, fig.align='center'----------------------
nlsy97depression %>%
  gather(measure, val, -occasion, -pid) %>%
  na.omit() %>%
  ggplot(aes(x=occasion, color=measure, y=val)) +
  stat_summary(fun.y = mean, geom='line') +
  stat_summary(fun.y = mean, geom='point') +
  stat_summary(fun.data = mean_se, geom='errorbar', width=0.2)

## ------------------------------------------------------------------------
structure <- list(
  F1 = c('nervous', 'down', 'depressed', 'calm', 'happy')
)
mmod_model <- mxMmodModel(data=nlsy97depression,
                          modelName='1 Factor MMOD',
                          idvar='pid', timevar='occasion', structure=structure, fiml=F)
mmod_fit <- mxRun(mmod_model)
(mmod_summary <- summary(mmod_fit))

## ---- eval=eval_semplots, fig.width=6, fig.height=4, fig.align='center'----
#  # Note: This can take a while to draw...
#  semPlot::semPaths(mmod_fit, 'est')

## ------------------------------------------------------------------------
structure2 <- list(
  F1 = c('nervous', 'down', 'depressed'),
  F2 = c('happy', 'calm')
)
mmod_model2 <- mxMmodModel(data=nlsy97depression,
                          modelName='2 Factor MMOD',
                          idvar='pid', timevar='occasion', structure=structure2)
mmod_fit2 <- mxRun(mmod_model2)
(mmod_summary2 <- summary(mmod_fit2))

## ---- eval=eval_semplots, fig.width=6, fig.height=4, fig.align='center'----
#  # Note: This can take a while to draw...
#  semPlot::semPaths(mmod_fit2, 'est')

## ------------------------------------------------------------------------
fits <- list(mmod_summary, mmod_summary2)

(compare_models <- tibble(
    name=map_chr(fits, 'modelName'),
    chisq=map_dbl(fits, 'Chi'),
    dof=map_dbl(fits, 'ChiDoF'),
    `-2ll`=map_dbl(fits, 'Minus2LogLikelihood'),
    aic=map_dbl(fits, 'AIC.Mx'),
    bic=map_dbl(fits, 'BIC.Mx'),
    rmsea=map_dbl(fits, 'RMSEA'),
    cfi=map_dbl(fits, 'CFI'),
    tli=map_dbl(fits, 'TLI')  
))

