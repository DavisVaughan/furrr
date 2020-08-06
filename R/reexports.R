# ------------------------------------------------------------------------------
# Utilities

#' @importFrom future plan
#' @export
future::plan

#' @importFrom future tweak
#' @export
future::tweak

#' @importFrom future makeClusterPSOCK
#' @export
future::makeClusterPSOCK

# ------------------------------------------------------------------------------
# Strategies

#' @importFrom future cluster
#' @export
future::cluster

#' @importFrom future multicore
#' @export
future::multicore

#' @importFrom future multisession
#' @export
future::multisession

#' @importFrom future multiprocess
#' @export
future::multiprocess

#' @importFrom future remote
#' @export
future::remote

#' @importFrom future sequential
#' @export
future::sequential

#' @importFrom future transparent
#' @export
future::transparent
