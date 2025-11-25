def simpson(a: Double, b: Double, func: Double => Double): Double = {
  val puntoMedio = (a + b) / 2.0
  val base = b - a
  base * (func(a) + 4 * func(puntoMedio) + func(b)) / 6.0
}

def calcularError(real: Double, estimado: Double): Double =
  math.abs(real - estimado)

// Definición de funciones
def g1(x: Double): Double = -(x * x) + 8 * x - 12
def g2(x: Double): Double = 3 * math.pow(x, 2)
def g3(x: Double): Double = x + 2 * x * x - math.pow(x, 3) + 5 * math.pow(x, 4)
def g4(x: Double): Double = (2 * x + 1) / (x * x + x)
def g5(x: Double): Double = math.exp(x)
def g6(x: Double): Double = 1.0 / (x - 1)
def g7(x: Double): Double = 1.0 / (1 + x * x)

// Integral 1
val res1 = simpson(3, 5, g1)
val ref1 = 7.33
val err1 = calcularError(ref1, res1)

// Integral 2
val res2 = simpson(0, 2, g2)
val ref2 = 8.0
val err2 = calcularError(ref2, res2)

// Integral 3
val res3 = simpson(-1, 1, g3)
val ref3 = 3.333
val err3 = calcularError(ref3, res3)

// Integral 4
val res4 = simpson(1, 2, g4)
val ref4 = 1.09861
val err4 = calcularError(ref4, res4)

// Integral 5
val res5 = simpson(0, 1, g5)
val ref5 = 1.71828
val err5 = calcularError(ref5, res5)

// Integral 6
val res6 = simpson(2, 3, g6)
val ref6 = 0.828427
val err6 = calcularError(ref6, res6)

// Integral 7
val res7 = simpson(0, 1, g7)
val ref7 = 0.785398
val err7 = calcularError(ref7, res7)
