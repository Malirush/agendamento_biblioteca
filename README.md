# agendamento_biblioteca

## Descrição
Este sistema permite o agendamento de **salas de estudo em uma biblioteca** por **dia da semana e turno fixo** (manhã, tarde ou noite). O sistema é executado via terminal e utiliza arquivos `.txt` para armazenar e recuperar os dados.

---

##Funcionalidades

### Comandos do sistema
- `/start`: inicia o processo de agendamento
- `/admin`: mostra histórico de salas agendadas
- `/exit`: encerra o sistema e salva os dados

### Processo de agendamento
1. Usuário digita dias e turnos desejados (ex: `Sg-Manha Qi-Noite`)
2. Opcionalmente, pode filtrar tipo da sala (ex: `Grupo` ou `Individual`)
3. O sistema lista salas disponíveis
4. Usuário escolhe a sala pelo número
5. Sistema confirma e registra o agendamento

### Persistência
- Ao encerrar com `/exit`, os dados são salvos no arquivo original.

### Regras de negócio
- Uma sala não pode ser agendada duas vezes no mesmo turno/dia.
- O sistema exibe uma mensagem se não houver salas disponíveis.

---

## 🏛️ Estrutura do arquivo de entrada (`biblioteca.txt`)
```
Biblioteca Central
101 Individual Projetor Sg-Manha T-Noite
102 Grupo TV Qi-Tarde Sx-Noite
```

Formato:
```
<ID> <TipoSala> <Recursos> <Dia-Turno> ...
```

Tipos de sala válidos: `Individual`, `Grupo`
Turnos válidos: `Manha`, `Tarde`, `Noite`
Dias (abreviações): `Sg`, `T`, `Qa`, `Qi`, `Sx`, `Sb`, `D`

---

## Como executar

### 1. Compilar com GHC:
```
ghc -o biblioteca Main.hs
```

### 2. Executar:
```
./biblioteca
```

Digite o nome do arquivo de dados quando solicitado (ex: `biblioteca.txt`).

---

## Recursos utilizados
- Tipos algébricos personalizados (`Dia`, `Turno`, `Sala`)
- Listas, `mapMaybe`, `zipWith`, `intercalate`
- Funções lambda e compreensão de listas
- Manipulação de arquivos

---

## Uso de IA
- Auxilio em diversas partes do codigo devido a erros de implementação
---



---

** Desenvolvido para a 1ª VA — 2025.1 **
