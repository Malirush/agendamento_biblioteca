module Main (main) where

import Book
import System.IO (hFlush, stdout)
import Control.Exception (IOException, handle)

lerArquivoSeguro :: FilePath -> IO (Maybe String)
lerArquivoSeguro nome = handle (\(_ :: IOException) -> return Nothing) $ do
  conteudo <- readFile nome
  return (Just conteudo)

main :: IO ()
main = do
  putStrLn "\n> Digite o nome do arquivo com as salas da biblioteca (ex: biblioteca.txt):"
  putStr "Arquivo -> " >> hFlush stdout
  nome <- getLine
  conteudo <- lerArquivoSeguro nome
  case conteudo of
    Nothing -> do
      putStrLn "Erro ao ler o arquivo. Tente novamente."
      main
    Just dados -> case transformaArquivo dados of
      Left err -> putStrLn err
      Right (salas, biblioteca) -> do
        putStrLn "********************************"
        putStrLn $ "> Bem-vindo à biblioteca: " ++ nomeBiblioteca biblioteca ++ "!"
        putStrLn "> Comandos disponíveis:\n/start - iniciar agendamento\n/admin - ver histórico\n/exit - sair e salvar"
        putStrLn "********************************"
        menuPrincipal salas biblioteca nome