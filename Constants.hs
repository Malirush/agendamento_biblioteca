module Constants 
( dias,
  diasAbrev,
  turnos,
  tiposSala,
  assocDias,
  assocTurnos,
  ajuda
) where

import DataTypes

dias :: [Dia]
dias = [Segunda .. Domingo]

diasAbrev :: [String]
diasAbrev = ["Sg","T","Qa","Qi","Sx","Sb","D"]

tiposSala :: [String]
tiposSala = ["Individual", "Grupo"]

turnos :: [String]
turnos = ["Manha", "Tarde", "Noite"]

assocDias :: [(String, Dia)]
assocDias = [("Segunda",Segunda),("Terca",Terca),("Quarta",Quarta),("Quinta",Quinta),("Sexta",Sexta),("Sabado",Sabado),("Domingo",Domingo),
             ("Sg",Segunda),("T",Terca),("Qa",Quarta),("Qi",Quinta),("Sx",Sexta),("Sb",Sabado),("D",Domingo)]

assocTurnos :: [(String, Turno)]
assocTurnos = [("Manha",Manha),("Tarde",Tarde),("Noite",Noite)]

ajuda :: [[String]]
ajuda = [diasAbrev, turnos, tiposSala]