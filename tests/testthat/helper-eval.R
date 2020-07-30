globally <- function(expr, env = caller_env()) {
  expr <- enexpr(expr)
  call <- expr(local(!!expr, envir = globalenv()))
  eval_bare(call, env = env)
}
