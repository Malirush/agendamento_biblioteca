module Book (menuPrincipal, transformaArquivo, Biblioteca(..)) where
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Data.Maybe (fromJust)
import Data.List (find)
import DataTypes
import Constants (ajuda, tiposSala)
import TypesHandle
import System.IO (hFlush, stdout)

menuPrincipal :: [Sala] -> Biblioteca -> String -> IO ()
menuPrincipal salas biblioteca nomeArquivo = do
  putStr "\nComando -> " >> hFlush stdout
  cmd <- getLine
  case cmd of
    "/start" -> do
      pedido <- solicitarPedido salas
      case pedido of
        PedidoVazio -> do
          putStrLn "Agendamento cancelado."
          menuPrincipal salas biblioteca nomeArquivo
        _ -> do
          salaEscolhida <- agendarSala salas pedido
          case salaEscolhida of
            SalaVazia -> do
              putStrLn "Agendamento cancelado."
              menuPrincipal salas biblioteca nomeArquivo
            _ -> do
              let novasSalas = atualizaSalas salas salaEscolhida
                  novaBib = atualizaBiblioteca biblioteca salaEscolhida
              menuPrincipal novasSalas novaBib nomeArquivo

    "/admin" -> do
      putStrLn "==== Histórico de agendamentos ===="
      putStrLn $ mostrarHistorico (historicoAgend biblioteca)
      putStrLn "=================================="
      menuPrincipal salas biblioteca nomeArquivo

    "/exit" -> do
      let conteudo = concatMap salaParaString salas
      writeFile nomeArquivo (nomeBiblioteca biblioteca ++ "\n" ++ conteudo)
      putStrLn "Até logo!"

    _ -> do
      putStrLn "Comando não reconhecido. Tente novamente."
      menuPrincipal salas biblioteca nomeArquivo

solicitarPedido :: [Sala] -> IO Pedido
solicitarPedido salas = do
  putStrLn "\n> Informe seu pedido no formato: Dia-Turno Dia-Turno (opcional: TipoSala)"
  putStrLn "  Ex: Sg-Manha Qi-Tarde ou Sg-Manha Tarde Individual"
  putStr "Entrada -> " >> hFlush stdout
  entrada <- getLine
  case entrada of
    "/cancel" -> return PedidoVazio
    _ ->
      let partes = words entrada
          ags = mapMaybe interpretarAgendamento (takeWhile (\x -> not (x `elem` tiposSala)) partes)
          tipo = case filter (`elem` tiposSala) partes of
                   [t] -> readMaybe t :: Maybe TipoSala
                   _   -> Nothing
      in if null ags then do
           putStrLn "> Pedido inválido. Tente novamente."
           solicitarPedido salas
         else return (Pedido tipo ags)

agendarSala :: [Sala] -> Pedido -> IO Sala
agendarSala salas pedido = do
  let disponiveis = salasDisponiveis pedido salas
  if null disponiveis then do
    putStrLn "> Não há salas disponíveis para este pedido."
    return SalaVazia
  else do
    putStrLn "==== Salas Disponíveis ===="
    putStr $ mostrarSalas disponiveis
    putStrLn "> Para cancelar, digite /cancel. Para agendar, digite o ID da sala."
    putStr "ID -> " >> hFlush stdout
    resposta <- getLine
    case resposta of
      "/cancel" -> return SalaVazia
      _ -> case readMaybe resposta :: Maybe Int of
        Nothing -> do
          putStrLn "> Entrada inválida."
          agendarSala salas pedido
        Just n -> case find (\(Sala i _ _ _) -> i == n) disponiveis of
          Just s -> do
            let novaSala = atualizaSala s pedido
            putStrLn "> Sala agendada com sucesso:"
            putStrLn $ mostrarSala novaSala
            return novaSala
          _ -> do
            putStrLn "> Sala não encontrada."
            agendarSala salas pedido