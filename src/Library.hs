module Library where
import PdePreludat
import GHC.Base (BCO)

doble :: Number -> Number
doble numero = numero + numero


data Invitade = UnInvitade{
    nivelDeCansancio :: Number,
    nivelDeFelicidad :: Number,
    cancionFavorita :: String
}deriving (Show, Eq)

juan :: Invitade
juan = UnInvitade{nivelDeCansancio = 40, nivelDeFelicidad = 50, cancionFavorita = "Believer"}

--1

estaCansada :: Invitade -> Bool
estaCansada invitade = nivelDeCansancio invitade > 80

cansarse :: Invitade -> Invitade
cansarse invitade = invitade{nivelDeCansancio = nivelDeCansancio invitade + 10}

disfrutar :: Invitade -> Invitade
disfrutar invitade = invitade{nivelDeFelicidad = nivelDeFelicidad invitade + 100 - nivelDeCansancio invitade}

--2

type Plancito = Invitade -> Invitade

charlitaDeFulbo :: Plancito
charlitaDeFulbo = disfrutar

bailar :: Plancito
bailar = cansarse . disfrutar

comerMesaDulce :: Plancito
comerMesaDulce = disfrutar . cansarse

tieneBuenGusto :: Invitade -> Bool
tieneBuenGusto = even . length . cancionFavorita

leVaADarFiaca :: Plancito -> Invitade -> Bool
leVaADarFiaca plancito = estaCansada . plancito 

--3

type Bandurria = [Invitade]

playlist :: Bandurria -> [String]
playlist = map cancionFavorita

lesQueLaSiguen :: Bandurria -> Bandurria
lesQueLaSiguen = filter (not . estaCansada)

hacerPlancitoEnBandurria :: Plancito -> Bandurria -> Bandurria
hacerPlancitoEnBandurria plancito = map plancito

laRompe :: Bandurria -> Bool
laRompe = all (not . estaCansada)

vaASonarUnHitazo :: Bandurria -> Bool
vaASonarUnHitazo = any tieneBuenGusto

vaAArmarUnFieston :: Bandurria -> Bool
vaAArmarUnFieston = sumanNivelDeFelicidadAlto . lesQueLaSiguen

sumanNivelDeFelicidadAlto :: Bandurria -> Bool
sumanNivelDeFelicidadAlto = (>300) . sum . map (nivelDeFelicidad)

seLaSube :: Plancito -> Bandurria -> Bool
seLaSube plancito = vaAArmarUnFieston . hacerPlancitoEnBandurria plancito