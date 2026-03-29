module Practica03 where

-- Sintaxis de la logica proposicional
data Prop
  = Var String
  | Cons Bool
  | Not Prop
  | And Prop Prop
  | Or Prop Prop
  | Impl Prop Prop
  | Syss Prop Prop
  deriving (Eq)

instance Show Prop where
  show (Cons True) = "⊤"
  show (Cons False) = "⊥"
  show (Var p) = p
  show (Not p) = "¬" ++ show p
  show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
  show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
  show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
  show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

w = Var "w"

v = Var "v"

{-
FORMAS NORMALES
-}

-- Ejercicio 1
fnn :: Prop -> Prop
fnn (Cons b) = Cons b
fnn (Var p) = Var p
fnn (Not (Cons True)) = Cons False
fnn (Not (Cons False)) = Cons True
fnn (Not (Var p)) = Not (Var p)
fnn (Not (Not p)) = fnn p
fnn (Not (And p q)) = Or (fnn (Not p)) (fnn (Not q))
fnn (Not (Or p q)) = And (fnn (Not p)) (fnn (Not q))
fnn (Not (Impl p q)) = fnn (And p (Not q))
fnn (Not (Syss p q)) = fnn (Or (Not (Impl p q)) (Not (Impl q p)))
fnn (And p q) = And (fnn p) (fnn q)
fnn (Or p q) = Or (fnn p) (fnn q)
fnn (Impl p q) = fnn (Or (Not p) q)
fnn (Syss p q) = fnn (And (Impl p q) (Impl q p))

-- Ejercicio 2
fnc :: Prop -> Prop
fnc p = auxFnc (fnn p)
  where
    auxFnc (And p1 p2) = And (auxFnc p1) (auxFnc p2)
    auxFnc (Or p1 p2) = dist (auxFnc p1) (auxFnc p2)
    auxFnc p1 = p1

    dist p (And q1 q2) = And (dist p q1) (dist p q2)
    dist (And p1 p2) q = And (dist p1 q) (dist p2 q)
    dist p q = Or p q

{-
RESOLUCION BINARIA
-}

-- Sinonimos a usar
type Literal = Prop

type Clausula = [Literal]

-- Ejercicio 1
clausulas :: Prop -> [Clausula]
clausulas p = [quitarRepetidos c | c <- auxClausulas (fnc p)]
  where
    auxClausulas (And p1 p2) = auxClausulas p1 ++ auxClausulas p2
    auxClausulas p1 = [obtenerLiterales p1]

    obtenerLiterales (Or p1 p2) = obtenerLiterales p1 ++ obtenerLiterales p2
    obtenerLiterales l = [l]

    quitarRepetidos [] = []
    quitarRepetidos (x : xs) = x : quitarRepetidos (filtrar (/= x) xs)

-- Ejercicio 2
resolucion :: Clausula -> Clausula -> Clausula
resolucion c1 c2 =
  let pares = [(l1, l2) | l1 <- c1, l2 <- c2, esComplemento l1 l2]
   in if esVacia pares
        then quitarRepetidos (c1 ++ c2)
        else
          let (comp1, comp2) = cabeza pares
           in quitarRepetidos (quitar comp1 c1 ++ quitar comp2 c2)
  where
    esComplemento (Var p) (Not (Var q)) = p == q
    esComplemento (Not (Var p)) (Var q) = p == q
    esComplemento _ _ = False

    quitar _ [] = []
    quitar l (x : xs) = if l == x then xs else x : quitar l xs

    quitarRepetidos [] = []
    quitarRepetidos (x : xs) = x : quitarRepetidos (filtrar (/= x) xs)

{-
ALGORITMO DE SATURACION
-}

-- Ejercicio 1
hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente c1 c2 = c1 /= c2 && alguno (\l1 -> alguno (esComplemento l1) c2) c1
  where
    esComplemento (Var p) (Not (Var q)) = p == q
    esComplemento (Not (Var p)) (Var q) = p == q
    esComplemento _ _ = False

-- Ejercicio 2
saturacion :: Prop -> Bool
saturacion p =
  let clausulasOriginales = [quitar c | c <- clausulas p]
   in satura clausulasOriginales
  where
    satura :: [Clausula] -> Bool
    satura actuales
      | [] `pertenece` actuales = False
      | actuales == union = True
      | otherwise = satura union
      where
        nuevas = [resolucion c1 c2 | c1 <- actuales, c2 <- actuales, hayResolvente c1 c2]
        union = actuales ++ filtrar (`noPertenece` actuales) nuevas

    quitar [] = []
    quitar (x : xs) = x : quitar (filtrar (/= x) xs)

-- Funciones auxiliares
filtrar :: (a -> Bool) -> [a] -> [a]
filtrar _ [] = []
filtrar p (x:xs)
  | p x       = x : filtrar p xs
  | otherwise = filtrar p xs

alguno :: (a -> Bool) -> [a] -> Bool
alguno _ [] = False
alguno p (x:xs) = p x || alguno p xs

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y:ys) = x == y || pertenece x ys

noPertenece :: Eq a => a -> [a] -> Bool
noPertenece x xs = not (pertenece x xs)

cabeza :: [a] -> a
cabeza (x:_) = x
cabeza []    = error "Lista vacia"

esVacia :: [a] -> Bool
esVacia [] = True
esVacia _  = False
