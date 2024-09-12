import Test.HUnit

{-- Tipos --}

import Data.Either
import Data.List

data Dirección = Norte | Sur | Este | Oeste
  deriving (Eq, Show)
type Posición = (Float, Float)

data Personaje = Personaje Posición String  -- posición inicial, nombre
  | Mueve Personaje Dirección               -- personaje que se mueve, dirección en la que se mueve
  | Muere Personaje                         -- personaje que muere
  deriving (Eq, Show)
data Objeto = Objeto Posición String        -- posición inicial, nombre
  | Tomado Objeto Personaje                 -- objeto que es tomado, personaje que lo tomó
  | EsDestruido Objeto                      -- objeto que es destruido
  deriving (Eq, Show)
type Universo = [Either Personaje Objeto]

{-- Observadores y funciones básicas de los tipos --}

siguiente_posición :: Posición -> Dirección -> Posición
siguiente_posición p Norte = (fst p, snd p + 1)
siguiente_posición p Sur = (fst p, snd p - 1)
siguiente_posición p Este = (fst p + 1, snd p)
siguiente_posición p Oeste = (fst p - 1, snd p)

posición :: Either Personaje Objeto -> Posición
posición (Left p) = posición_personaje p
posición (Right o) = posición_objeto o

posición_objeto :: Objeto -> Posición
posición_objeto = foldObjeto const (const posición_personaje) id

nombre :: Either Personaje Objeto -> String
nombre (Left p) = nombre_personaje p
nombre (Right o) = nombre_objeto o

nombre_personaje :: Personaje -> String
nombre_personaje = foldPersonaje (const id) const id

está_vivo :: Personaje -> Bool
está_vivo = foldPersonaje (const (const True)) (const (const True)) (const False)

fue_destruido :: Objeto -> Bool
fue_destruido = foldObjeto (const (const False)) const (const True)

universo_con :: [Personaje] -> [Objeto] -> [Either Personaje Objeto]
universo_con ps os = map Left ps ++ map Right os

es_un_objeto :: Either Personaje Objeto -> Bool
es_un_objeto (Left o) = False
es_un_objeto (Right p) = True

es_un_personaje :: Either Personaje Objeto -> Bool
es_un_personaje (Left o) = True
es_un_personaje (Right p) = False

-- Asume que es un personaje
personaje_de :: Either Personaje Objeto -> Personaje
personaje_de (Left p) = p

-- Asume que es un objeto
objeto_de :: Either Personaje Objeto -> Objeto
objeto_de (Right o) = o

en_posesión_de :: String -> Objeto -> Bool
en_posesión_de n = foldObjeto (const (const False)) (\ r p -> nombre_personaje p == n) (const False)

objeto_libre :: Objeto -> Bool
objeto_libre = foldObjeto (const (const True)) (const (const False)) (const False)

norma2 :: (Float, Float) -> (Float, Float) -> Float
norma2 p1 p2 = sqrt ((fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2)

cantidad_de_objetos :: Universo -> Int
cantidad_de_objetos = length . objetos_en

cantidad_de_personajes :: Universo -> Int
cantidad_de_personajes = length . personajes_en

distancia :: (Either Personaje Objeto) -> (Either Personaje Objeto) -> Float
distancia e1 e2 = norma2 (posición e1) (posición e2)

objetos_libres_en :: Universo -> [Objeto]
objetos_libres_en u = filter objeto_libre (objetos_en u)

está_el_personaje :: String -> Universo -> Bool
está_el_personaje n = foldr (\x r -> es_un_personaje x && nombre x == n && (está_vivo $ personaje_de x) || r) False

está_el_objeto :: String -> Universo -> Bool
está_el_objeto n = foldr (\x r -> es_un_objeto x && nombre x == n && not (fue_destruido $ objeto_de x) || r) False

-- Asume que el personaje está
personaje_de_nombre :: String -> Universo -> Personaje
personaje_de_nombre n u = foldr1 (\x1 x2 -> if nombre_personaje x1 == n then x1 else x2) (personajes_en u)

-- Asume que el objeto está
objeto_de_nombre :: String -> Universo -> Objeto
objeto_de_nombre n u = foldr1 (\x1 x2 -> if nombre_objeto x1 == n then x1 else x2) (objetos_en u)

es_una_gema :: Objeto -> Bool
es_una_gema o = isPrefixOf "Gema de" (nombre_objeto o)

{-Ejercicio 1-}

foldPersonaje :: (Posición -> String -> c) -> (c -> Dirección -> c) -> (c-> c) -> Personaje -> c
foldPersonaje fPersonaje fMueve fMuere personaje = case personaje of 
                                            (Personaje pos nombre) -> fPersonaje pos nombre
                                            (Mueve pj dir) -> fMueve (r pj) dir
                                            (Muere pj) -> fMuere (r pj)
                                            where r = foldPersonaje fPersonaje fMueve fMuere
        
        
foldObjeto   :: (Posición -> String -> c) -> (c -> Personaje -> c) -> (c -> c) -> Objeto -> c
foldObjeto fObjeto fTomado fDestruido objeto = case objeto of
                                               (Objeto pos nombre) -> fObjeto pos nombre
                                               (Tomado obj pj) -> fTomado (r obj) pj
                                               (EsDestruido obj) -> fDestruido (r obj)

                                               where r = foldObjeto fObjeto fTomado fDestruido  

{-Ejercicio 2-}

posición_personaje :: Personaje -> Posición
posición_personaje = foldPersonaje (\pos _ -> pos) (\r dir -> siguiente_posición r dir ) (\pos -> pos) 

nombre_objeto :: Objeto -> String
nombre_objeto = foldObjeto (\_ nombre -> nombre) (\r _ -> r) (\r -> r)

-- {-Ejercicio 3-}

objetos_en :: Universo -> [Objeto]
objetos_en u = map (\(Right x) -> x) (filter es_un_objeto u)


personajes_en :: Universo -> [Personaje]
personajes_en u = map (\(Left x) -> x) (filter es_un_personaje u)

-- {-Ejercicio 4-}
objetos_en_posesión_de :: Universo -> String -> [Objeto]
objetos_en_posesión_de u pj = filter (en_posesión_de pj) (objetos_en u )  

-- {-Ejercicio 5-}

-- -- Asume que hay al menos un objeto
objeto_libre_mas_cercano :: Universo -> Personaje -> Objeto
objeto_libre_mas_cercano u pj = 
  foldr1 (\obj acc -> 
    if (distancia (Left pj) (Right obj)) < (distancia (Left pj) (Right acc)) 
    then obj 
    else acc) (objetos_libres_en u)

-- {-Ejercicio 6-}

tiene_thanos_todas_las_gemas :: Universo -> Bool
tiene_thanos_todas_las_gemas u = (foldr (\x acc -> if (es_una_gema x) then (acc+1) else acc) 0 (objetos_en_posesión_de u "Thanos")) == 6

-- {-Ejercicio 7-}

estaYVivePorNombre :: String -> Universo -> Bool
estaYVivePorNombre nombre u = (está_el_personaje nombre u) && (está_vivo (personaje_de_nombre nombre u))

-- Aclaración: No ponemos la condición de que thanos tiene que estar en el universo ni que tiene que estar vivo porque 
--la consigna no aclara eso explícitamente, como si lo hace para los casos de thor, wanda y vision.
-- Si fuere necesario sería tan simple como agregar otra condicion más estaYVivePorNombre "Thanos"
podemos_ganarle_a_thanos :: Universo -> Bool
podemos_ganarle_a_thanos u = (not (tiene_thanos_todas_las_gemas u)) &&
                             (((estaYVivePorNombre "Thor" u) && (está_el_objeto "StormBreaker" u) ) ||
                             ((estaYVivePorNombre "Wanda" u) && (estaYVivePorNombre "Visión" u) && ((está_el_objeto "Gema de la Mente" u)&& (en_posesión_de "Visión" (objeto_de_nombre "Gema de la Mente" u)))))


-- {-Tests-}

main :: IO Counts
main = do runTestTT allTests

allTests = test [ -- Reemplazar los tests de prueba por tests propios
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7
  ]

phil = Personaje (0,0) "Phil"
mjölnir = Objeto (2,2) "Mjölnir"
-- Personajes 
ironMan = Personaje (0,0) "IronMan"
carlos = Personaje (1,0) "Carlos"
thor = Personaje (2,0) "Thor"
wanda = Personaje (5,0) "Wanda"
visión = Personaje (6,0) "Visión"
loki = Personaje (3,0) "loki"
capitanAmerica = Personaje (4,0) "CapitanAmerica"
spiderman = Personaje (8,0) "Spiderman"
thanos = Personaje (10,0) "Thanos"

-- Objetos 
escudo = Objeto (4,4) "Escudo Del Capitan America"
sartén = Objeto (9,9) "Sartén"
lanza = Objeto (10,10) "Lanza"
stormBreaker = Objeto (2,1) "StormBreaker"
gemaMente = Objeto (5,5) "Gema de la Mente"
gemaRealidad = Objeto (1,1) "Gema de la Realidad"
gemaTiempo = Objeto (3,3) "Gema del Tiempo"
gemaEspacio = Objeto (9,0) "Gema del Espacio"
gemaPoder = Objeto (7,7) "Gema del Poder"
gemaAlma = Objeto (8,8) "Gema del Alma"

--universos

universo_no_tiene_todas_thanos_esta_Thor_Mjolnir = universo_con [thor,thanos] [escudo,stormBreaker,(Tomado gemaMente capitanAmerica),gemaRealidad,(Tomado gemaTiempo thanos),gemaEspacio,gemaPoder,(Tomado gemaAlma thanos)]
universo_no_tiene_todas_thanos = universo_con [wanda,visión,thor,thanos] [escudo,(Tomado gemaMente capitanAmerica),gemaRealidad,(Tomado gemaTiempo thanos),gemaEspacio,gemaPoder,(Tomado gemaAlma thanos)]
universo_tiene_todas_thanos = universo_con [carlos,thanos] [escudo,(Tomado gemaMente thanos),(Tomado gemaRealidad thanos),(Tomado gemaTiempo thanos),(Tomado gemaEspacio thanos),(Tomado gemaPoder thanos),(Tomado gemaAlma thanos)]
universo_tiene_todas_thanos_y_estan_los_que_ganan = universo_con [wanda,visión,thor,thanos] [stormBreaker,(Tomado gemaMente thanos),(Tomado gemaRealidad thanos),(Tomado gemaTiempo thanos),(Tomado gemaEspacio thanos),(Tomado gemaPoder thanos),(Tomado gemaAlma thanos)]
universo_tiene_todas_thanos_y_estan_wanda_vision_con_gema = universo_con [wanda,visión,thanos] [stormBreaker,(Tomado gemaMente visión),(Tomado gemaRealidad thanos),(Tomado gemaTiempo thanos),(Tomado gemaEspacio thanos),(Tomado gemaPoder thanos),(Tomado gemaAlma thanos)]
universo_sin_thanos = universo_con [phil] [mjölnir]
-- universo



testsEj1 = test [ -- Casos de test para el ejercicio 1
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) phil ~=? 0,
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) (Muere thanos) ~=? 1,                                                               
  foldPersonaje (\p s -> 1) (\r d -> r+1) (\r -> r+1) (Muere thanos) ~=? 2,                                                               
  foldPersonaje (\p s -> 1) (\r d -> r+1) (\r -> r+1) (Mueve (Muere thanos) Norte) ~=? 3,                                                               
  foldPersonaje (\p s -> p) (\r _ -> r) (\r -> r) (Muere thor) ~=? (2,0),

  foldObjeto (\p s -> 0) (\r pj -> r+1) (\r -> r) escudo ~=? 0,
  foldObjeto (\p s -> s) (\r _ -> r) (\r -> r) escudo ~=? "Escudo Del Capitan America",
  foldObjeto (\p s -> 1) (\r pj -> r+1) (\r -> r) (Tomado escudo thanos) ~=? 2,
  foldObjeto (\p s -> 1) (\r pj -> r + (fst (foldPersonaje (\p s -> p) (\r _ -> r) (\r -> r) (Muere pj)))) (\r -> r) (Tomado escudo thor) ~=? 3,
  foldObjeto (\p s -> "el " ++ s) (\r pj -> r ++ " lo tiene " ++ (nombre_personaje pj)) (\r -> r) (Tomado escudo thanos) ~=? "el Escudo Del Capitan America lo tiene Thanos"
  ]


testsEj2 = test [ -- Casos de test para el ejercicio 2
  posición_personaje phil ~=? (0,0),
  siguiente_posición (posición_personaje phil) Norte ~=? (0,1),
  posición_personaje wanda ~=? (5,0),
  nombre_objeto escudo ~=? "Escudo Del Capitan America"
  ]

testsEj3 = test [ -- Casos de test para el ejercicio 3
  objetos_en []   ~=? [],
  objetos_en universo_no_tiene_todas_thanos_esta_Thor_Mjolnir ~=? [escudo,stormBreaker,(Tomado gemaMente capitanAmerica),gemaRealidad,(Tomado gemaTiempo thanos),gemaEspacio,gemaPoder,(Tomado gemaAlma thanos)], 
  personajes_en [] ~=? [],
  personajes_en universo_no_tiene_todas_thanos_esta_Thor_Mjolnir ~=? [thor,thanos]  
  ]

testsEj4 = test [ -- Casos de test para el ejercicio 4
  objetos_en_posesión_de [] "Phil" ~=? [],
  objetos_en_posesión_de universo_no_tiene_todas_thanos_esta_Thor_Mjolnir "Thor" ~=? [],
  objetos_en_posesión_de universo_tiene_todas_thanos_y_estan_wanda_vision_con_gema "Visión" ~=? [Tomado gemaMente visión],
  objetos_en_posesión_de universo_no_tiene_todas_thanos "Thanos" ~=? [Tomado gemaTiempo thanos,Tomado gemaAlma thanos]                         
  ]

testsEj5 = test [ -- Casos de test para el ejercicio 5
  objeto_libre_mas_cercano [Right mjölnir] phil ~=? mjölnir,
  objeto_libre_mas_cercano universo_tiene_todas_thanos thanos ~=? escudo,
  objeto_libre_mas_cercano universo_no_tiene_todas_thanos_esta_Thor_Mjolnir thor ~=? stormBreaker,
  objeto_libre_mas_cercano universo_no_tiene_todas_thanos_esta_Thor_Mjolnir thanos ~=? gemaEspacio
  ]

testsEj6 = test [ -- Casos de test para el ejercicio 6
  tiene_thanos_todas_las_gemas universo_sin_thanos ~=? False,
  tiene_thanos_todas_las_gemas universo_tiene_todas_thanos ~=? True,
  tiene_thanos_todas_las_gemas universo_no_tiene_todas_thanos ~=? False                                      
  ]

testsEj7 = test [ -- Casos de test para el ejercicio 7
  podemos_ganarle_a_thanos universo_sin_thanos ~=? False,
  podemos_ganarle_a_thanos universo_tiene_todas_thanos ~=? False,
  podemos_ganarle_a_thanos universo_no_tiene_todas_thanos ~=? False,
  podemos_ganarle_a_thanos universo_no_tiene_todas_thanos_esta_Thor_Mjolnir ~=? True,
  podemos_ganarle_a_thanos universo_tiene_todas_thanos_y_estan_wanda_vision_con_gema ~=? True,
  podemos_ganarle_a_thanos universo_tiene_todas_thanos_y_estan_los_que_ganan ~=? False
  ]

