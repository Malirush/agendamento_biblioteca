module TypesHandle where

import DataTypes
import Constants
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe, fromJust)
import Data.List (intercalate)

transformaArquivo :: String -> Either String ([Sala], Biblioteca)
transformaArquivo conteudo = case parseArquivo $ lines conteudo of
  Left err -> Left err
  Right (nome, linhasSalas) -> if SalaVazia `elem` salas then Left "Erro no arquivo." else Right (salas, inicializaBiblioteca nome salas)
    where salas = inicializaSalas linhasSalas

parseArquivo :: [String] -> Either String (String, [String])
parseArquivo [] = Left "Erro: Arquivo vazio."
parseArquivo [_] = Left "Erro: Lista de salas vazia."
parseArquivo (nome:resto) = Right (nome, resto)

inicializaBiblioteca :: String -> [Sala] -> Biblioteca
inicializaBiblioteca nome salas = Biblioteca nome (length salas) (historico salas)

inicializaSalas :: [String] -> [Sala]
inicializaSalas = map stringParaSala

historico :: [Sala] -> [(Dia, Turno, Int)]
historico salas = foldr adicionar [] salas

adicionar :: Sala -> [(Dia, Turno, Int)] -> [(Dia, Turno, Int)]
adicionar (Sala sid _ _ ags) acc = foldr (\(d,t) acc2 -> (d,t,sid):acc2) acc ags
adicionar SalaVazia acc = acc

stringParaSala :: String -> Sala
stringParaSala linha = case words linha of
  (i:t:r:resto) -> case (readMaybe i :: Maybe Int, readMaybe t :: Maybe TipoSala) of
    (Just sid, Just tp) -> Sala sid tp r (transformaAgendamentos resto)
    _ -> SalaVazia
  _ -> SalaVazia

transformaAgendamentos :: [String] -> [(Dia, Turno)]
transformaAgendamentos = mapMaybe interpretarAgendamento

interpretarAgendamento :: String -> Maybe (Dia, Turno)
interpretarAgendamento entrada = case break (=='-') entrada of
  (d, '-':t) -> do
    dia <- lookup d assocDias
    turno <- lookup t assocTurnos
    return (dia, turno)
  _ -> Nothing

salaParaString :: Sala -> String
salaParaString SalaVazia = ""
salaParaString (Sala i t r ags) = unwords [show i, show t, r] ++ " " ++ unwords (map agendamentoParaTexto ags) ++ "\n"

agendamentoParaTexto :: (Dia, Turno) -> String
agendamentoParaTexto (d,t) = abreviarDia d ++ "-" ++ show t

abreviarDia :: Dia -> String
abreviarDia d = case d of
  Segunda -> "Sg"; Terca -> "T"; Quarta -> "Qa"; Quinta -> "Qi"
  Sexta -> "Sx"; Sabado -> "Sb"; Domingo -> "D"

atualizaSala :: Sala -> Pedido -> Sala
atualizaSala (Sala i t r ags) (Pedido _ novos) = Sala i t r (ags ++ novos)
atualizaSala _ _ = SalaVazia

atualizaSalas :: [Sala] -> Sala -> [Sala]
atualizaSalas [] _ = []
atualizaSalas (s1@(Sala i _ _ _):ss) s2@(Sala j _ _ _) | i == j    = s2 : ss
                                                       | otherwise = s1 : atualizaSalas ss s2
atualizaSalas _ _ = []

atualizaBiblioteca :: Biblioteca -> Sala -> Biblioteca
atualizaBiblioteca (Biblioteca n total hist) (Sala sid _ _ ags) = Biblioteca n total (hist ++ map (\(d,t) -> (d,t,sid)) ags)
atualizaBiblioteca b _ = b

salasDisponiveis :: Pedido -> [Sala] -> [Sala]
salasDisponiveis PedidoVazio _ = []
salasDisponiveis _ [] = []
salasDisponiveis (Pedido Nothing ags) (s@(Sala _ _ _ ags2):ss)
  | not (conflita ags ags2) = s : salasDisponiveis (Pedido Nothing ags) ss
  | otherwise = salasDisponiveis (Pedido Nothing ags) ss
salasDisponiveis (Pedido (Just t1) ags) (s@(Sala _ t2 _ ags2):ss)
  | t1 == t2 && not (conflita ags ags2) = s : salasDisponiveis (Pedido (Just t1) ags) ss
  | otherwise = salasDisponiveis (Pedido (Just t1) ags) ss
salasDisponiveis _ _ = []

conflita :: [(Dia, Turno)] -> [(Dia, Turno)] -> Bool
conflita novos existentes = any (`elem` existentes) novos

mostrarSala :: Sala -> String
mostrarSala (Sala sid tp rc _) = show tp ++ " sala #" ++ show sid ++ " com recurso: " ++ rc
mostrarSala SalaVazia = ""

mostrarSalas :: [Sala] -> String
mostrarSalas = unlines . map mostrarSala

mostrarHistorico :: [(Dia, Turno, Int)] -> String
mostrarHistorico = intercalate "\n" . map formatar
  where formatar (d,t,i) = show d ++ " " ++ show t ++ ": Sala " ++ show i