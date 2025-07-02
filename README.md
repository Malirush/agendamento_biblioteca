# agendamento\_biblioteca


O **agendamento\_biblioteca** é um sistema simples em Haskell para agendar salas de estudo da biblioteca por dia da semana e turno (Manhã, Tarde, Noite). Seu propósito é oferecer uma interface de linha de comando onde estudantes podem reservar um horário de estudo e o bibliotecário (ou administrador) pode gerenciar esses agendamentos. Para armazenar os dados de forma persistente, utilizei arquivos de texto (`.txt`), facilitando a leitura e escrita sem depender de bancos de dados complexos. Decidi usar **tipos algébricos** para representar conceitos como dia da semana e turno, e assim garantir que apenas valores válidos sejam aceitos. Os comandos `/start`, `/admin` e `/exit` organizam o fluxo de uso: um usuário comum inicia o processo com `/start`, um administrador entra no modo administrativo com `/admin` para verificar reservas, e `/exit` encerra o programa. 

## Descrição Técnica

* **Funcionalidades principais:** O sistema permite listar turnos disponíveis, fazer um agendamento informando dia, turno e nome do estudante, e exibir todos os agendamentos existentes.
* **Comandos:**

  * `/start`: inicia o processo de agendamento para um usuário comum; o sistema pede dia, turno e nome e salva no arquivo.
  * `/admin`: acessa uma função administrativa que mostra todos os agendamentos gravados no sistema.
  * `/exit`: encerra o programa de forma limpa, garantindo que o arquivo de agendamentos seja fechado corretamente.
* **Estrutura de dados:** Utilizei tipos algébricos personalizados para representar dias e turnos, por exemplo:

```haskell
data Dia   = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo deriving (Show, Read, Eq, Enum)
data Turno = Manha   | Tarde | Noite deriving (Show, Read, Eq, Enum)
type Agendamento = (Dia, Turno, String)
```

Internamente, os agendamentos são armazenados em uma lista.

* **Regras de negócio:** Cada par (dia, turno) pode ter no máximo um agendamento, evitando reservas duplicadas no mesmo horário. Antes de adicionar uma nova reserva, o sistema verifica se já existe uma entrada com o mesmo dia e turno. Se houver conflito, retorna mensagem de erro ao usuário. A validação de entrada (como garantir que o dia e turno são válidos) é feita usando conversão segura (`readMaybe`), tratando casos inválidos com mensagens de erro amigáveis.

## Como executar o sistema

1. **Compile o programa:**

```bash
ghc --make agendamento_biblioteca.hs -o agendar
```

2. **Execute o executável:**

```bash
./agendar
```

3. **Use os comandos no terminal:** Digite `/start` para iniciar o agendamento de salas, `/admin` para entrar no modo administrador (visualizar reservas) ou `/exit` para sair do programa.

4. **Preencha as informações solicitadas:** O sistema pedirá o dia (por exemplo, `Segunda`), o turno (`Manha`, `Tarde` ou `Noite`) e o nome completo do estudante. As reservas serão salvas automaticamente em um arquivo de texto.

## Uso de IA no desenvolvimento

Durante o desenvolvimento, utilizei um assistente de IA em várias situações para esclarecer dúvidas e resolver problemas complexos. Seguem alguns exemplos de como a IA auxiliou:

### 1. Conversão de Strings para tipos algébricos

Tive dificuldade em converter as entradas de texto (como "Segunda" e "Manha") para os tipos `Dia` e `Turno`. A IA sugeriu usar `readMaybe` do módulo `Text.Read` para tratar conversões seguras:

```haskell
import Text.Read (readMaybe)

parseDia :: String -> Maybe Dia
parseDia s = readMaybe s
```

Essa abordagem facilitou o manuseio de dias/turnos inválidos sem travar o programa.

### 2. Leitura e parsing seguro de arquivos

Ao ler o arquivo de agendamentos, precisei tratar casos de linhas mal formatadas. A IA sugeriu usar o tipo `Either` com mensagens de erro específicas:

```haskell
parseLinha :: String -> Either String Agendamento
parseLinha linha =
  case splitOn "," linha of
    [dStr, tStr, nome] ->
      case (readMaybe dStr, readMaybe tStr) of
        (Just d, Just t) -> Right (d, t, nome)
        (Nothing, _)     -> Left "Dia inválido"
        (_, Nothing)     -> Left "Turno inválido"
    _ -> Left "Linha de arquivo com formato incorreto"
```

### 3. Uso de `mapMaybe` para filtrar listas

Para processar listas com possíveis entradas inválidas, a IA indicou o uso de `mapMaybe`:

```haskell
import Data.Maybe (mapMaybe)

parseInts :: [String] -> [Int]
parseInts xs = mapMaybe readMaybe xs
```

Esse padrão foi aplicado para converter strings em tipos válidos e ignorar entradas incorretas.

### 4. Atualização imutável de dados

Como as estruturas em Haskell são imutáveis, pedi à IA um exemplo para atualizar uma lista de agendamentos sem duplicar horários:

```haskell
atualizaAgendamentos :: [Agendamento] -> Agendamento -> [Agendamento]
atualizaAgendamentos ags novo@(dNew, tNew, _) =
  novo : [ a | a@(d, t, _) <- ags, (d, t) /= (dNew, tNew) ]
```

A função substitui qualquer reserva anterior para o mesmo horário, respeitando as regras do sistema.

## Recursos utilizados

* **Tipos algébricos personalizados** (`data`) para representar dias, turnos e tipos de sala.
* **Funções de ordem superior** como `map`, `filter`, `mapMaybe`, e **compreensões de listas**.
* **Conversão segura com `readMaybe`** para evitar exceções com entradas inválidas.
* **Concatenação e manipulação de strings** com `intercalate` e `unwords`.
* **Tratamento de erro com `Either`**, garantindo robustez na leitura de dados externos.
* **Entrada e saída de arquivos** usando `readFile`, `writeFile`, e interação via terminal com `getLine`, `putStrLn` e `hFlush`.



**Projeto desenvolvido para fins acadêmicos — 1ª VA 2025.1**
