require(tidyverse)
require(xlsx)

se <- function(
  p = 0.5,
  z = 1.96,
  Deff = 1.5,
  n = 6e3,
  pf = 0.6
){
  se_unadj <- (p * (1-p)) / n
  N <- n / pf
  fpc = (N-n) / (N-1)
  return(sqrt(se_unadj*(Deff)*fpc))
}

# se_2 <- function(
#     e,
#     n,
#     pf,
#     p
# ){
#   b <- e^2 * (n/pf) - e^2 * p * (1-p) * 2 / e^2
#   o <- 7.68 * (n/pf) - 7.68
#   return(sqrt(b/o))
# }

data <- expand.grid(
  p = seq(0.1, .9, .1),
  n = c(30, 50, 1e2, 1e3, 1e4),
  N = 0,
  pf = seq(0.1, .9, .1),
  se_exp = NA,
  bi = NA
  )
for(i in 1:nrow(data)){
  data$N[i] <- data$n[i] / data$pf[i]
  data$se_exp[i] <- se(
    p = data$p[i],
    pf = data$pf[i],
    n = data$n[i]
    )
  data$bi[i] <- 2 * 1.96 * data$se_exp[i]
}
data$se_exp_rank = percent_rank(data$se_exp)
#write.xlsx(data, file = "I:/h.broekhuizen/tmp/vopg_stat_table.xlsx")

# deadline 20 april
# breedte ci
#sowieso de min. aantallen uit de uitvraag wg stat: 50, 70, 75, 80, 500