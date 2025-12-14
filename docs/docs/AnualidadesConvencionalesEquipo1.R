# ============================================================
# Funciones de anualidades convencionales (vencidas y anticipadas)
# Autor: Diego Alberto Montes Ramírez
# Archivo: anualidades.R
# ============================================================

#' Valida cuántos argumentos "faltan" (NA o no especificados) en una lista
.count_missing <- function(...) {
  args <- list(...)
  sum(vapply(args, function(x) {
    (is.null(x)) || (length(x) == 0) || (isTRUE(missing(x))) || (isTRUE(is.na(x)))
  }, logical(1)))
}

#' Convierte un valor a lógico (TRUE/FALSE) con manejo simple
.as_bool <- function(x) {
  if (isTRUE(x)) return(TRUE)
  if (identical(x, FALSE)) return(FALSE)
  if (is.character(x)) {
    x2 <- tolower(trimws(x))
    if (x2 %in% c("true","t","1","si","sí","yes","y")) return(TRUE)
    if (x2 %in% c("false","f","0","no","n")) return(FALSE)
  }
  FALSE
}

# ============================================================
# 1) VALOR FUTURO DE ANUALIDADES
# ============================================================

#' valorFuturoAnualidades
#'
#' Calcula el valor futuro (VF), el pago, la tasa r o el número de periodos T,
#' dependiendo de cuál argumento dejes como NA (o sin especificar).
#'
#' @param r tasa por periodo (ej. 0.02)
#' @param VF valor futuro (monto acumulado al final)
#' @param T número de periodos de pago
#' @param pago monto del pago de la anualidad
#' @param anticipada TRUE si es anticipada, FALSE si es vencida
#' @return lista con los datos y el resultado
valorFuturoAnualidades <- function(r = NA, VF = NA, T = NA, pago = NA, anticipada = FALSE) {
  anticipada <- .as_bool(anticipada)

  # contar cuántos faltan (debe faltar exactamente 1)
  miss <- sum(is.na(c(r, VF, T, pago)))
  if (miss != 1) {
    stop("Debe dejar EXACTAMENTE un argumento como NA (o sin especificar): r, VF, T o pago.")
  }

  # helper: factor para FV
  fv_factor <- function(r, T, anticipada) {
    if (r == 0) {
      # si r=0, FV = pago*T (vencida); anticipada igual, porque no hay interés
      return(T)
    }
    base <- ((1 + r)^T - 1) / r
    if (anticipada) base <- base * (1 + r)
    base
  }

  # 1) calcular VF
  if (is.na(VF)) {
    if (any(is.na(c(r, T, pago)))) stop("Para calcular VF, necesitas r, T y pago.")
    VF_calc <- pago * fv_factor(r, T, anticipada)
    return(list(operacion = "Calcular VF", r = r, T = T, pago = pago, anticipada = anticipada, VF = VF_calc))
  }

  # 2) calcular pago
  if (is.na(pago)) {
    if (any(is.na(c(r, T, VF)))) stop("Para calcular pago, necesitas r, T y VF.")
    fac <- fv_factor(r, T, anticipada)
    pago_calc <- VF / fac
    return(list(operacion = "Calcular pago", r = r, T = T, VF = VF, anticipada = anticipada, pago = pago_calc))
  }

  # 3) calcular T (fórmula cerrada cuando r != 0)
  if (is.na(T)) {
    if (any(is.na(c(r, VF, pago)))) stop("Para calcular T, necesitas r, VF y pago.")
    if (r == 0) {
      # VF = pago*T
      T_calc <- VF / pago
      return(list(operacion = "Calcular T (r=0)", r = r, VF = VF, pago = pago, anticipada = anticipada, T = T_calc))
    }
    denom_pago <- if (anticipada) (pago * (1 + r)) else pago
    # VF = denom_pago * ((1+r)^T - 1)/r
    inside <- (VF * r / denom_pago) + 1
    if (inside <= 0) stop("Con esos datos no se puede calcular T (revisa signos y magnitudes).")
    T_calc <- log(inside) / log(1 + r)
    return(list(operacion = "Calcular T", r = r, VF = VF, pago = pago, anticipada = anticipada, T = T_calc))
  }

  # 4) calcular r (numérico con uniroot)
  if (is.na(r)) {
    if (any(is.na(c(VF, T, pago)))) stop("Para calcular r, necesitas VF, T y pago.")
    f <- function(rr) pago * fv_factor(rr, T, anticipada) - VF

    # intervalos típicos: rr > -1
    lower <- -0.999999
    upper <- 10
    # asegurar cambio de signo; si no, ampliar upper
    val_l <- f(lower + 1e-6)
    val_u <- f(upper)

    if (is.nan(val_l) || is.nan(val_u) || is.infinite(val_l) || is.infinite(val_u)) {
      stop("No se pudo evaluar la función para encontrar r. Revisa los datos.")
    }

    tries <- 0
    while (val_l * val_u > 0 && tries < 10) {
      upper <- upper * 2
      val_u <- f(upper)
      tries <- tries + 1
    }
    if (val_l * val_u > 0) stop("No se pudo acotar una raíz para r con los datos proporcionados.")

    r_calc <- uniroot(f, lower = lower + 1e-6, upper = upper)$root
    return(list(operacion = "Calcular r (numérico)", VF = VF, T = T, pago = pago, anticipada = anticipada, r = r_calc))
  }

  stop("Caso no contemplado.")
}

# ============================================================
# 2) VALOR ACTUAL DE ANUALIDADES
# ============================================================

#' valorActualAnualidades
#'
#' Calcula el valor actual (VA), el pago, la tasa r o el número de periodos T,
#' dependiendo de cuál argumento dejes como NA (o sin especificar).
#'
#' @param r tasa por periodo
#' @param VA valor actual (presente)
#' @param T número de periodos de pago
#' @param pago monto del pago de la anualidad
#' @param anticipada TRUE si es anticipada, FALSE si es vencida
#' @return lista con los datos y el resultado
valorActualAnualidades <- function(r = NA, VA = NA, T = NA, pago = NA, anticipada = FALSE) {
  anticipada <- .as_bool(anticipada)

  miss <- sum(is.na(c(r, VA, T, pago)))
  if (miss != 1) {
    stop("Debe dejar EXACTAMENTE un argumento como NA (o sin especificar): r, VA, T o pago.")
  }

  # helper: factor para PV
  pv_factor <- function(r, T, anticipada) {
    if (r == 0) {
      # si r=0, PV = pago*T (vencida); anticipada igual, porque no hay interés
      return(T)
    }
    base <- (1 - (1 + r)^(-T)) / r
    if (anticipada) base <- base * (1 + r)
    base
  }

  # 1) calcular VA
  if (is.na(VA)) {
    if (any(is.na(c(r, T, pago)))) stop("Para calcular VA, necesitas r, T y pago.")
    VA_calc <- pago * pv_factor(r, T, anticipada)
    return(list(operacion = "Calcular VA", r = r, T = T, pago = pago, anticipada = anticipada, VA = VA_calc))
  }

  # 2) calcular pago
  if (is.na(pago)) {
    if (any(is.na(c(r, T, VA)))) stop("Para calcular pago, necesitas r, T y VA.")
    fac <- pv_factor(r, T, anticipada)
    pago_calc <- VA / fac
    return(list(operacion = "Calcular pago", r = r, T = T, VA = VA, anticipada = anticipada, pago = pago_calc))
  }

  # 3) calcular T (fórmula cerrada cuando r != 0)
  if (is.na(T)) {
    if (any(is.na(c(r, VA, pago)))) stop("Para calcular T, necesitas r, VA y pago.")
    if (r == 0) {
      T_calc <- VA / pago
      return(list(operacion = "Calcular T (r=0)", r = r, VA = VA, pago = pago, anticipada = anticipada, T = T_calc))
    }
    denom_pago <- if (anticipada) (pago * (1 + r)) else pago
    # VA = denom_pago * (1 - (1+r)^(-T))/r
    inside <- 1 - (VA * r / denom_pago)
    if (inside <= 0) stop("Con esos datos no se puede calcular T (revisa signos y magnitudes).")
    T_calc <- -log(inside) / log(1 + r)
    return(list(operacion = "Calcular T", r = r, VA = VA, pago = pago, anticipada = anticipada, T = T_calc))
  }

  # 4) calcular r (numérico con uniroot)
  if (is.na(r)) {
    if (any(is.na(c(VA, T, pago)))) stop("Para calcular r, necesitas VA, T y pago.")
    f <- function(rr) pago * pv_factor(rr, T, anticipada) - VA

    lower <- -0.999999
    upper <- 10
    val_l <- f(lower + 1e-6)
    val_u <- f(upper)

    if (is.nan(val_l) || is.nan(val_u) || is.infinite(val_l) || is.infinite(val_u)) {
      stop("No se pudo evaluar la función para encontrar r. Revisa los datos.")
    }

    tries <- 0
    while (val_l * val_u > 0 && tries < 10) {
      upper <- upper * 2
      val_u <- f(upper)
      tries <- tries + 1
    }
    if (val_l * val_u > 0) stop("No se pudo acotar una raíz para r con los datos proporcionados.")

    r_calc <- uniroot(f, lower = lower + 1e-6, upper = upper)$root
    return(list(operacion = "Calcular r (numérico)", VA = VA, T = T, pago = pago, anticipada = anticipada, r = r_calc))
  }

  stop("Caso no contemplado.")
}

# ============================================================
# Ejemplos rápidos 
# ============================================================
# # Ejemplo 1: calcular pago a partir de VF
# # valorFuturoAnualidades(r=0.001923, VF=1000000, T=1820, pago=NA, anticipada=FALSE)
#
# # Ejemplo 2: calcular VF a partir de pago
# # valorFuturoAnualidades(r=0.02, pago=1500, T=360, VF=NA, anticipada=TRUE)
#
# # Ejemplo 3: calcular pago a partir de VA
# # valorActualAnualidades(r=0.00125, VA=23000, T=48, pago=NA, anticipada=TRUE)
