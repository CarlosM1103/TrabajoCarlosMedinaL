def integracion(a: Double, b: Double, f: Double => Double): Double = {
  val xMedio = (a + b) / 2.0
  (b - a) * (f(a) + 4 * f(xMedio) + f(b)) / 6.0
}

def errorAprox(valorEsperado: Double, valorObtenido: Double): Double =
  Math.abs(valorEsperado - valorObtenido)


// Funciones
def f1(x: Double): Double = -x * x + 8 * x - 12
def f2(x: Double): Double = 3 * x * x
def f3(x: Double): Double = x + 2 * x * x - x * x * x + 5 * Math.pow(x, 4)
def f4(x: Double): Double = (2 * x + 1) / (x * x + x)
def f5(x: Double): Double = Math.exp(x)
def f6(x: Double): Double = 1.0 / (x - 1)
def f7(x: Double): Double = 1.0 / (1 + x * x)

// Integral 1
val obtenido1 = integracion(3, 5, f1)
val esperado1 = 7.33
val error1    = errorAprox(esperado1, obtenido1)

// Integral 2
val obtenido2 = integracion(0, 2, f2)
val esperado2 = 8.0
val error2    = errorAprox(esperado2, obtenido2)

// Integral 3
val obtenido3 = integracion(-1, 1, f3)
val esperado3 = 3.333
val error3    = errorAprox(esperado3, obtenido3)

// Integral 4
val obtenido4 = integracion(1, 2, f4)
val esperado4 = 1.09861
val error4    = errorAprox(esperado4, obtenido4)

// Integral 5
val obtenido5 = integracion(0, 1, f5)
val esperado5 = 1.71828
val error5    = errorAprox(esperado5, obtenido5)

// Integral 6
val obtenido6 = integracion(2, 3, f6)
val esperado6 = 0.828427
val error6    = errorAprox(esperado6, obtenido6)

// Integral 7
val obtenido7 = integracion(0, 1, f7)
val esperado7 = 0.785398
val error7    = errorAprox(esperado7, obtenido7)