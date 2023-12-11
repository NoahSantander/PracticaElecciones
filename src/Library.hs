module Library where
import PdePreludat

-- Defino mis alias
type Tema = String
type EsBeneficiario = Persona -> Bool
type Costo = Number 
type Nombre = String
type Partido = String
type Promesas = [Promesa]
type EstaDesocupado = Bool
type Actividad = String
type Actividades = [Actividad]
type Candidatos = [Candidato]
type Voto = Persona -> Candidatos -> Candidato
type Importe = Number
type Ingresos = Number

-- Defino mis tipos
data Promesa = UnaPromesa {
    tema:: Tema,
    esBeneficiario:: EsBeneficiario,
    costo:: Costo
} deriving Show

data Candidato = UnCandidato {
    nombre:: Nombre,
    partido:: Partido,
    promesas:: Promesas
} deriving Show

data Persona = UnaPersona {
    desocupado :: EstaDesocupado,
    actividades :: Actividades,
    ingresos :: Ingresos
} deriving Show

-- Defino a que candidato votaria cada persona
importe :: Partido -> Number
importe "Jovenes por el Clima" = 1000
importe "Urgencia por la Pelota" = 5000
importe _ = 0

esRealizable :: Partido -> Promesa -> Bool
esRealizable partido promesa = (< importe partido) (costo promesa) -- Uso aplicación parcial

promesasRealizables :: Candidato -> Promesas
promesasRealizables candidato = filter (esRealizable (partido candidato)) (promesas candidato)

loBeneficia :: Persona -> Promesa -> Bool
loBeneficia persona promesa = (esBeneficiario promesa) persona 

promesasBeneficiosas :: Persona -> Promesas -> Promesas
promesasBeneficiosas persona = filter (loBeneficia persona) 

tieneMejorPropuesta :: Persona -> Candidato -> Candidato -> Candidato
tieneMejorPropuesta persona candidato1 candidato2 
    | (length.promesasBeneficiosas persona) (promesasRealizables candidato1) > (length.promesasBeneficiosas persona) (promesasRealizables candidato2) = candidato1 -- Uso composición de funciones y aplicación parcial
    | otherwise = candidato2

votaA :: Voto
votaA persona candidatos = foldl1 (tieneMejorPropuesta persona) candidatos

-- Inicializo algunas personas
manuel = UnaPersona False ["Actividad Informatica", "Actividad Culinaria"] 10000
pepe = UnaPersona False ["Zapatero"] 100
moni = UnaPersona True ["Se rasca el culo", "Mira tele"] 0

-- Inicializo algunas promesas
realiza :: Actividad -> EsBeneficiario
realiza actividad persona = elem actividad (actividades persona)
estaSinTrabajo :: EsBeneficiario
estaSinTrabajo = desocupado
paraPobres :: EsBeneficiario
paraPobres = (==0).ingresos -- Uso composición de funciones y aplicación parcial
paraCualquiera :: EsBeneficiario
paraCualquiera _ = True
paraMantenidos :: EsBeneficiario
paraMantenidos persona = estaSinTrabajo persona  && (not.paraPobres) persona
paraPepe :: EsBeneficiario
paraPepe = realiza "Zapatero"

promesaParaPobres = UnaPromesa "" paraPobres 0
promesaParaCualquiera = UnaPromesa "" paraCualquiera 700
promesaParaMantenidos = UnaPromesa "" paraMantenidos 500
promesaParaDesocupados = UnaPromesa "" estaSinTrabajo 6000
promesaParaInformaticos = UnaPromesa "" (realiza "ActividadInformatica") 1
promesaParaNoInformaticos = UnaPromesa "" (not.(realiza "ActividadInformatica")) 10 -- Uso composición de funciones
promesaParaPepe = UnaPromesa "" paraPepe 100
 
-- Inicializo algunos candidatos
jovenDelClima = UnCandidato "Candidato del Clima" "Jovenes por el Clima" [promesaParaCualquiera, promesaParaInformaticos, promesaParaPepe]
jovenPelotero = UnCandidato "Candidato por la pelota" "Urgencia por la Pelota" [promesaParaPepe, promesaParaPepe, promesaParaPepe,promesaParaPepe]
candidatoConMuchasPromesas = UnCandidato "Candidato prometedor" "Partido de las Promesas" [promesaParaCualquiera, promesaParaDesocupados, promesaParaInformaticos, promesaParaMantenidos, promesaParaPepe, promesaParaPobres, promesaParaNoInformaticos]

candidatos = [jovenDelClima, jovenPelotero]