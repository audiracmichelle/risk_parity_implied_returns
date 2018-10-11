pkgs <- c('xts', 'quantmod', 'nloptr', 'tidyverse', 'mltools', 'Rcpp')

# para instalar paquetes usar:
#install.packages(pkgs,
#                 dependencies = TRUE,
#                 repos = "http://cran.itam.mx")

# para cargar paquetes usar:
lapply(pkgs, require, character.only = TRUE)

require(plotly)

#' @title plot_xts
#' @description Plots time series from xts
#' @param x xts

plot_xts <- function(x, ...){
  x %>%
    as.data.frame() %>%
    tibble::rownames_to_column("date") %>%
    mutate(date = as.Date(date)) %>%
    gather(key, value, -date) %>%
    ggplot(aes(x = date, y = value, color = key)) +
    geom_line() +
    labs(x = "", y = "", color = "", ...)
}

#' @title plotly_xts
#' @description Plots time series from xts
#' @param x xts

plotly_xts <- function(x, ...){
  x %<>%
    as.data.frame() %>%
    tibble::rownames_to_column("date") %>%
    mutate(date = as.Date(date)) %>%
    gather(key, value, -date)

  p <- x %>%
    plot_ly(x = ~date,
            y = ~value,
            color = ~key, ...) %>%
    add_lines()

  p
}

#' @title get_prices_yahoo
#' @description Builds an xts containing time series downloaded from Yahoo! Finance.
#' @param yahoo_id character array containining yahoo ids.
#' @param from start date. Retrieve data no earlier than this date. (2007-01-01).
#' @param to end date. (Sys.Date())
#' @param column indicating which column should be returned: "Open", "High", "Low", "Close", "Volume", ("Adjusted")
#' @param ... additional parameters
#' @param periodicity periodicity of data to query and return. Must be one of "daily", "weekly", "monthly", ("daily")

get_prices_yahoo <- function(
  yahoo_id,
  column = 'Adjusted',
  from = "2007-01-01",
  to = Sys.Date(),
  ...,
  periodicity = "daily"
) {
  series <- list()
  i <- 0

  for(id in yahoo_id) {
    col <- paste(toupper(id), '.', column, sep = '')

    ss <- getSymbols.yahoo(id, auto.assign = FALSE,
                           from = from, to = to,
                           periodicity = periodicity, ...)[, col]

    i <- i + 1
    series[[i]] <- ss
  }

  series <- do.call(cbind, series)
  series <- na.locf(series, na.rm = FALSE, fromLast = TRUE)
  series
}

get_dlyChg_from_price <- function(
  price
) {
  # calculamos cambios relativos diarios
  dlyChg <- price / lag.xts(price)
  # por conveniencia asignamos 1's al primer renglon de `dlyChg`
  dlyChg[1, ] <- 1
  dlyChg
}

get_cumRet_from_dlyChg <- function(
  dlyChg
) {
  cumRet <- xts(order.by=index(dlyChg))
  for (c in 1:ncol(dlyChg)) {
    dlyChg_col <- log(dlyChg[ ,c])
    dlyChg_col[is.na(dlyChg_col)] <- 0
    cumRet <- cbind(cumRet, exp(cumsum(dlyChg_col)))
  }
  cumRet
}

get_rebalance_t2 <- function(
  dlyChg_t2,
  portWeightOpen_t2,
  portValue_t1
) {
  portSumTerm_t2 <- dlyChg_t2 * portWeightOpen_t2
  portChg_t2 <- sum(portSumTerm_t2)

  portWeightClose_t2 <- portSumTerm_t2 / portChg_t2
  portValue_t2 <- portValue_t1 * portChg_t2

  list(portWeight = portWeightClose_t2,
       portValue = portValue_t2)
}

get_rebalance_ <- function(
  dlyChg,
  rebWeight
) {
  stopifnot(index(rebWeight)[1] == index(dlyChg)[1])

  dates <- as.character(index(dlyChg))
  reb_dates <- as.character(index(rebWeight))

  portWeightOpen <- list()
  portWeight <- list()
  portValue <- list()

  portWeight[[dates[1]]] <- as.numeric(rebWeight[dates[1]])
  portValue[[dates[1]]] <- 1.0

  for(i in 2:length(dates)) {
    if(dates[i-1] %in% reb_dates) {
      portWeightOpen[[dates[i]]] <- as.numeric(rebWeight[dates[i-1]])
    } else {
      portWeightOpen[[dates[i]]] <- portWeight[[dates[i-1]]]
    }
    reb <- get_rebalance_t2(as.numeric(dlyChg[dates[i]]),
                            portWeightOpen[[dates[i]]],
                            portValue[[dates[i-1]]])
    portWeight[[dates[i]]] <- reb$portWeight
    portValue[[dates[i]]] <- reb$portValue
  }
  portWeight <- do.call(rbind, portWeight)
  portValue <- do.call(rbind, portValue)
  portWeight <- xts(portWeight, order.by = index(dlyChg))
  portValue <- xts(portValue, order.by = index(dlyChg))
  names(portWeight) <- names(dlyChg)
  names(portValue) <- 'port'

  list(portWeight = portWeight,
       portValue = portValue)
}

# ÚLTIMA MODIFICACIÓN DE REBALANCE EN LAS CONTRIBUCIONES
get_rebalance <- function(
  dlyChg,
  rebWeight
) {
  rebalance <- get_rebalance_(dlyChg, rebWeight)

  portContrib <- list()
  for(c in 1:ncol(dlyChg)) {
    dlyChg_c <- dlyChg[, c]
    dlyChg_c$fix <- 1
    rebWeight_c <- rebWeight[, c]
    rebWeight_c$fix <- 1 - rebWeight_c[, 1]
    portContrib[[c]] <- get_rebalance_(dlyChg_c, rebWeight_c)$portValue
  }
  portContrib <- do.call(cbind, portContrib)
  names(portContrib) <- names(dlyChg)
  rebalance[['portContrib']] <- portContrib
  rebalance
}

#' @title get_w_with_geomTruncDecay
get_w_with_geomTruncDecay <- function(
  T,
  halflife,
  interval = c(0.000001, 0.999999)
) {
  median_p <- function(p) (1-(1-p)^halflife) / (1-(1-p)^T) - 0.5
  s <- uniroot(median_p, interval)
  p <- s$root
  k <- T:1
  w <- p * (1-p)^{k-1} / (1-(1-p)^T)
  w
}

#' @title weighted.sd
#' @description Compute a weighted standard deviation.
#' @param x a vector or dataframe containing the values
#' whose weighted standard deviation is to be computed.
#' @param w a vector of weights.
#' @param na.rm remove NA values before processing,
#' the default value is FALSE.
#' @details See radiant.data::weighted.sd and
#' stats::weighted.mean.default.
#' @export
weighted.sd <- function (
  x,
  w,
  na.rm = TRUE
){
  if (na.rm) {
    i <- !is.na(x)
    w <- w[i]
    x <- x[i]
  }
  w <- w / sum(w)
  wm <- weighted.mean(x, w)
  n <- length(x)
  sqrt(sum(w * (x - wm)^2) * n / (n - 1))
}

# Con el parámetro `method` decidimos si la función debe regresar el promedio geométrico o aritmético. -->
# El valor `halflife=NULL` indicará que la función debe regresar el rendimiento promedio sin half-life. Cuando se incluya un half-life este tiene que ser un entero $m$ entre $0$ y $T$, es decir $0<m<T$. -->

#' @title get_meanRet_from_dlyChg
get_meanRet_from_dlyChg <- function(
  dlyChg,
  method = "geometric",
  halflife = NULL,
  interval = c(0.000001, 0.999999),
  annualization_factor = 1
) {
  dlyChg <- dlyChg[-1, ] #pensar como manejar lo del renglon con unos
  dlyChg <- na.trim(dlyChg)
  T <- nrow(dlyChg)

  if(is.null(halflife)) {
    w <- 1 / T
  } else {
    w <- get_w_with_geomTruncDecay(T = T, halflife = halflife, interval = interval)
  }

  if(method == "arithmetic") meanRet <- annualization_factor * apply(w * dlyChg, 2, sum, na.rm = TRUE)
  if(method == "geometric") meanRet <- exp(annualization_factor * apply(w * log(dlyChg), 2, sum, na.rm = TRUE))

  meanRet
}

#' @title get_rollChg_from_dlyChg
get_rollChg_from_dlyChg <- function(
  dlyChg,
  method = "geometric",
  roll = 30,
  halflife = NULL,
  trim = FALSE,
  interval = c(0.000001, 0.999999)
) {
  dlyChg <- dlyChg[-1, ] #pensar como manejar lo del renglon con unos

  if(is.null(halflife)) {
    w <- rep(1/roll, roll)
  } else {
    w <- get_w_with_geomTruncDecay(T = roll, halflife = halflife, interval = interval)
  }

  if(method == "arithmetic") rollChg <- rollapply(zoo(dlyChg),
                                                  width = roll,
                                                  align = 'right',
                                                  FUN = weighted.mean,
                                                  w = w,
                                                  na.rm = TRUE,
                                                  fill = NA)

  if(method == "geometric") rollChg <- exp(rollapply(zoo(log(dlyChg)),
                                                     width = roll,
                                                     align = 'right',
                                                     FUN = weighted.mean,
                                                     w = w,
                                                     na.rm = TRUE,
                                                     fill = NA))

  rollChg <- reclass(rollChg, dlyChg)
  na.trim(rollChg)
}



#' @title get_sdRet_from_dlyChg
get_sdRet_from_dlyChg <- function(
  dlyChg,
  method = "arithmetic",
  halflife = NULL,
  interval = c(0.000001, 0.999999),
  annualization_factor = 1
) {
  dlyChg <- dlyChg[-1, ] #pensar como manejar lo del renglon con unos
  dlyChg <- na.trim(dlyChg)
  T <- nrow(dlyChg)

  if(is.null(halflife)) {
    w <- rep(1 / T, T)
  } else {
    w <- get_w_with_geomTruncDecay(T = T, halflife = halflife, interval = interval)
  }

  if(method == "arithmetic") sdRet <- apply(sqrt(annualization_factor) * dlyChg,
                                            2,
                                            weighted.sd,
                                            w = w,
                                            na.rm = TRUE)
  if(method == "geometric") sdRet <- exp(apply(sqrt(annualization_factor) * log(dlyChg),
                                               2,
                                               weighted.sd,
                                               w = w,
                                               na.rm = TRUE))

  sdRet
}

plot_riskReward_from_dlyChg <- function(
  dlyChg,
  roll,
  roll_halflife = NULL) {
  rollChg <- get_rollChg_from_dlyChg(dlyChg = dlyChg,
                                     roll = roll,
                                     halflife = roll_halflife,
                                     trim = TRUE) ^ roll

  reward <- get_meanRet_from_dlyChg(rollChg)

  risk <- get_sdRet_from_dlyChg(rollChg) #* sqrt(nrow(dlyChg))

  plot_data <- data.frame(
    Instrumento =  names(risk),
    Rendimiento = 100 * (as.numeric(reward) - 1),
    Riesgo = 100 * (as.numeric(risk)),
    check.names = FALSE,
    row.names = NULL
  )
  slopes <- data.frame(
    intercept = 0,
    slope = (reward - 1)/risk
  )

  ggplot(data = plot_data, aes(x = Riesgo, y = Rendimiento, color = Instrumento)) +
    geom_point(size = 4, alpha = 0.4) +
    geom_abline(data = slopes, aes(intercept = intercept ,slope = slope), colour = "red", linetype = 2) +
    scale_y_continuous(
      labels = function(x) paste0(x, "%"),
      limits = c(min(0, min(plot_data$Rendimiento)), max(plot_data$Rendimiento))
    ) +
    scale_x_continuous(
      labels = function(x) paste0(x, "%"),
      limits = c(min(0, min(plot_data$Riesgo)), max(plot_data$Riesgo))
    ) +
    theme_bw() +
    theme(legend.title = element_blank())
}

get_covRet_from_dlyChg <- function(
  dlyChg,
  halflife = NULL,
  ...
) {
  dlyChg <- dlyChg[-1, ] #pensar como manejar lo del renglon con unos
  dlyChg <- na.omit(dlyChg)
  T <- nrow(dlyChg)
  if(!is.null(halflife)) {
    w <- get_w_with_geomTruncDecay(T, halflife, ...)
    covRet <- cov.wt(dlyChg, w)$cov
  } else {
    covRet <- cov.wt(dlyChg)$cov
  }

  covRet
}

