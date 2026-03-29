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
clausulas p = [quitarRepetidos c | c <- auxClausulas (fnc p)]
  where
    auxClausulas (And p1 p2) = auxClausulas p1 ++ auxClausulas p2
    auxClausulas p1 = [obtenerLiterales p1]

--Ejercicio 2
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

--Ejercicio 1
hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente = undefined

--Ejercicio 2
--Funcion principal que pasa la formula proposicional a fnc e invoca a res con las clausulas de la formula.
saturacion :: Prop -> Bool
saturacion = undefined


--Funciones auxiliares 
obtenerLiterales (Or p1 p2) = obtenerLiterales p1 ++ obtenerLiterales p2
obtenerLiterales l = [l]

filtrar :: (a -> Bool) -> [a] -> [a]
filtrar _ [] = []
filtrar p (x:xs)
  | p x       = x : filtrar p xs
  | otherwise = filtrar p xs

cabeza :: [a] -> a
cabeza (x:_) = x
cabeza []    = error "Lista vacia"

esVacia :: [a] -> Bool
esVacia [] = True
esVacia _  = False
