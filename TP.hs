type Nombre = [Char]
type Valor = Integer
type Tesoro = (Nombre ,Valor)
type Botin = [ Tesoro ]
type Pirata = ( Nombre , Botin )

type Isla = (Nombre,Tesoro) --Mismo tesoro para todos los piratas
type Ciudad = (Nombre,Botin)
type Tripulacion = [ Pirata ]
type Saqueo = (Tesoro -> Bool)
type Barco = (Nombre,Saqueo,Tripulacion)

--type Pirata = [ ([Char] , [([Char] ,Int)]) ]
--type Botin = [ ([Char] ,Int) ]
--type Tesoro = ([Char],Int)

--EJEMPLOS--
ejemploBotin = [("Frasco de Arena",5) , ("Brujula",10000)]

ejemploPirata1 = ("Jack Sparrow" , [("Frasco de arena",0) , ("Brujula",10000)])
ejemploPirata2 = ("David Jones" , [("Cajita musical",1)])
ejemploPirata3 = ("Anne Bonny", [("Doblones",100) , ("Frasco de arena",1)])
ejemploPirata4 = ("Elizabeth Swann", [("Moneda de cofre muerto",100),("Espada de hierro",50)])
ejemploPirata5 = ("Will Turner", [("Cuchillo",5)])

ejemploBarco1 = ("Perla Negra",saqueoValioso,[ejemploPirata1,ejemploPirata3])
ejemploBarco2 = ("Holandes Errante",saqueoConCorazon,[ejemploPirata2])

ejemploIsla1 = ("Isla Tortuga",("Frasco de arena",1))
ejemploIsla2 = ("Isla del Ron",("Ron",25))

ejemploCiudad2 = ("Carmen de Patagones",[("Perla",400),("Reloj",6)])
ejemploCiudad1 = ("Port Royal",[("Diamante",1000),("Espada",100),("Pintura",150),("Reloj de oro",350),("Ron",25)])

--TESOROS PIRATAS--
--Funcion de cantidad de tesoros
cantidadDeTesoros :: Pirata -> Int --Cantidad de tesoros que tiene el botin de un pirata
cantidadDeTesoros (_,botin) = length botin

--Funciones de Pirata Afortunado
valorDelTesoro :: Tesoro -> Valor --Obtiene el valor de un tesoro
valorDelTesoro (_,valor) = valor

valoresDelBotin :: Botin -> [Valor] --Arma una lista solo con los valores de cada tesoro del botin
valoresDelBotin botin = map valorDelTesoro botin

valorTotalDelBotin :: Botin -> Valor --Valor total al sumar todos los botines
valorTotalDelBotin botin = sum (valoresDelBotin botin)

esAfortunado :: Pirata -> Bool --Chequea si el pirata es o no afortunado (botin total > 10000)
esAfortunado (_,botin) = valorTotalDelBotin botin > 10000

--Funcion de dos piratas mismo tesoro y distinto valor
esMismoTesoroConValorDistinto :: Tesoro -> Tesoro -> Bool --Compara los nombres de dos tesoros y los valores
esMismoTesoroConValorDistinto (nombre1,valor1) (nombre2,valor2) = (nombre1 == nombre2) && (valor1 /= valor2)

loTieneOtroPirataConDistintoValor :: Pirata -> Tesoro -> Bool
loTieneOtroPirataConDistintoValor (_,botin) unTesoro = any (esMismoTesoroConValorDistinto unTesoro) botin

tienenElMismoTesoroConValorDiferente :: Pirata -> Pirata -> Bool
tienenElMismoTesoroConValorDiferente  (_,botin) otroPirata = any (loTieneOtroPirataConDistintoValor otroPirata) botin

--Funcion de perder tesoro con nombre
esMismoNombre :: Tesoro -> Tesoro -> Bool --Compara el nombre de un tesoro con otro el de tesoro
esMismoNombre (nombre1,_) (nombre2,_) = nombre1 == nombre2

perderTesoro :: Pirata -> Nombre -> Pirata
perderTesoro (nombre,botin) nombretesoro = (nombre,filter (not.esMismoNombre (nombretesoro,0)) botin)

--Funcion de Tesoro mas valioso
masValiosoTesoro :: Pirata -> Valor --Valor mas alto entre todos los tesoros del botin de un pirata
masValiosoTesoro (_,botin) = maximum (valoresDelBotin botin)

--Funciones de ganar o perder tesoros
nuevoTesoro :: Pirata -> Tesoro -> Pirata --Agrega un tesoro al botin
nuevoTesoro (nombre,botin) tesoro = (nombre, botin ++ [tesoro])

noEsTesoroValioso :: Tesoro -> Bool --Determina si un tesoro NO es valioso (valioso implica que: valor > 100)
noEsTesoroValioso (_,valor) = valor <= 100

esTesoroValioso :: Tesoro -> Bool
esTesoroValioso = not.noEsTesoroValioso

perderTesorosValiosos :: Pirata -> Pirata --Muestra al pirata sin los tesoros valiosos
perderTesorosValiosos (nombre,botin) = (nombre, filter noEsTesoroValioso botin)


--TEMPORADA DE SAQUEOS--
saqueoValioso :: Tesoro -> Bool
saqueoValioso tesoro = esTesoroValioso tesoro

saqueoConCorazon :: Tesoro-> Bool
saqueoConCorazon tesoro = False

saqueoEspecifico :: Nombre -> Tesoro -> Bool     --tiene prototipo distinto pero como la voy a aplicar parcialmente para que quede Tesoro -> Bool
saqueoEspecifico palabraclave (nombre,valor) = (==palabraclave) nombre
 
saqueoComplejo :: Nombre -> Tesoro -> Bool     -- idem saqueo específico
saqueoComplejo palabraclave tesoro = saqueoValioso tesoro || saqueoEspecifico palabraclave tesoro

saquear :: Saqueo -> Tesoro -> Pirata -> Pirata
saquear formaDeSaqueo tesoro (nombre,botin) = (nombre, botin ++ (filter formaDeSaqueo [tesoro]))


--NAVEGANDO LOS SIETE MARES--
tripulacionDelBarco :: Barco -> Tripulacion --Para probar ejemplos unicamente
tripulacionDelBarco (_,_,x) = x

--Funciones de un pirata se incorpora o abandona un barco
pirataSeIncorpora :: Barco -> Pirata -> Barco
pirataSeIncorpora (nombre,saqueo,tripulacion) pirata = (nombre,saqueo,tripulacion ++ [pirata])

pirataAbandona :: Barco -> Pirata -> Barco
pirataAbandona (nombre,saqueo,tripulacion) pirata = (nombre,saqueo,filter (/= pirata) tripulacion)

--Funciones de barco navegando
anclarEnIslaDeshabitada :: Barco -> Isla -> Barco
anclarEnIslaDeshabitada (nombre,saqueo,tripulacion) (_,tesoro) = (nombre,saqueo,map (saquear saqueo tesoro) tripulacion)

atacarCiudad :: Barco -> Ciudad -> Barco
atacarCiudad (nombre,saqueo,tripulacion) (_,botin) = (nombre,saqueo,zipWith (saquear saqueo ) botin tripulacion)


esBarcoConMasPiratas :: Barco -> Barco -> Barco -> Bool
esBarcoConMasPiratas (_,_,tripulacion1) (_,_,tripulacion2) (_,_,tripulacion) = tripulacion >= tripulacion1 && tripulacion >= tripulacion2

barcoGanador :: Barco -> Barco -> Barco --Devuelve al barco con mas tripulacion
barcoGanador barco1 barco2 = head (filter (esBarcoConMasPiratas barco1 barco2) [barco1,barco2])

barcoPerdedor :: Barco -> Barco -> Barco --Devuelve el barco con menos tripulacion
barcoPerdedor barco1 barco2 = head (filter (not.esBarcoConMasPiratas barco1 barco2) [barco1,barco2])

secuestrarTripulacion :: Barco -> Barco -> Barco --Devuelve el barco ganador con la tripulacion del barco perdedor
secuestrarTripulacion (n1,s1,t1) (_,_,t2) = (n1,s1,t1++t2)

abordarBarcoEnAltaMar :: Barco -> Barco -> Barco --El barco con mas piratas secuestra a la tripulacion del otro quedandose con sus tesoros
abordarBarcoEnAltaMar barco1 barco2 = secuestrarTripulacion (barcoGanador barco1 barco2) (barcoPerdedor barco1 barco2)

--HACETE TODA LA PELICULA
--hacerTodaLaPelicula => 
--hacerTodaLaPelicula  = (pirataAbandona(ejemploBarco1,ejemploPirata3))
