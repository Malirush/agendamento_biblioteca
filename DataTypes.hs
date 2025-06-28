module DataTypes(
  Dia(..),
  Turno(..),
  TipoSala(..),
  Sala(..),
  Biblioteca(..),
  Pedido(..)
) where

data Dia = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Turno = Manha | Tarde | Noite deriving (Eq, Show, Read)

data TipoSala = Individual | Grupo deriving (Show, Read, Eq)

type Agendamento = [(Dia, Turno)]

data Sala = Sala {
  salaId         :: Int,
  tipoSala       :: TipoSala,
  recursosSala   :: String,
  agendamentos   :: Agendamento
} | SalaVazia deriving (Show, Read, Eq)

data Biblioteca = Biblioteca {
  nomeBiblioteca :: String,
  salasTotais    :: Int,
  historicoAgend :: [(Dia, Turno, Int)]
} deriving (Show, Read)

data Pedido = Pedido {
  tipoDesejado   :: Maybe TipoSala,
  diasTurnos     :: [(Dia, Turno)]
} | PedidoVazio deriving (Show, Read)