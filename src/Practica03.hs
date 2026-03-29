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

--Ejercicio 2
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
hayResolvente = undefined

--Ejercicio 2
--Funcion principal que pasa la formula proposicional a fnc e invoca a res con las clausulas de la formula.
saturacion :: Prop -> Bool
saturacion = undefined