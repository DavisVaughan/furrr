globally <- function(expr, env = caller_env()) {
  expr <- enexpr(expr)
  call <- expr_no_srcref(local(!!expr, envir = globalenv()))
  eval_bare(call, env = env)
}
