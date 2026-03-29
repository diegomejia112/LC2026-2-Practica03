module Practica03 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
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

--Ejercicio 1
fnn :: Prop -> Prop
fnn = undefined


--Ejercicio 2
fnc :: Prop -> Prop
fnc = undefined

{-
RESOLUCION BINARIA
-}

--Sinonimos a usar
type Literal = Prop
type Clausula = [Literal]

--Ejercicio 1
clausulas :: Prop -> [Clausula]
clausulas = undefined

--Ejercicio 2
resolucion :: Clausula -> Clausula -> Clausula
resolucion = undefined

{-
ALGORITMO DE SATURACION
-}

--Ejercicio 1
hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente c1 c2 = c1 /= c2 && alguno (\l1 -> alguno (esComplemento l1) c2) c1
  where
    esComplemento (Var p) (Not (Var q)) = p == q
    esComplemento (Not (Var p)) (Var q) = p == q
    esComplemento _ _ = False


--Ejercicio 2
--Funcion principal que pasa la formula proposicional a fnc e invoca a res con las clausulas de la formula.
saturacion :: Prop -> Bool
saturacion p =
  let clausulasOriginales = [quitar c | c <- clausulas p]
   in satura clausulasOriginales
  where
    satura :: [Clausula] -> Bool
    satura actuales
      | [] pertenece actuales = False
      | actuales == union = True
      | otherwise = satura union
      where
        nuevas = [resolucion c1 c2 | c1 <- actuales, c2 <- actuales, hayResolvente c1 c2]
        union = actuales ++ filtrar (noPertenece actuales) nuevas

    quitar [] = []
    quitar (x : xs) = x : quitar (filtrar (/= x) xs)


-- Funciones auxiliares 
alguno :: (a -> Bool) -> [a] -> Bool
alguno _ [] = False
alguno p (x:xs) = p x || alguno p xs

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y:ys) = x == y || pertenece x ys

noPertenece :: Eq a => a -> [a] -> Bool
noPertenece x xs = not (pertenece x xs)

filtrar :: (a -> Bool) -> [a] -> [a]
filtrar _ [] = []
filtrar p (x:xs)
  | p x       = x : filtrar p xs
  | otherwise = filtrar p xs