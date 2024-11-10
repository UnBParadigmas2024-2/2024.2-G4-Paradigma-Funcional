# Campo Minado - Documentação Técnica Completa

 O jogo de campo minado (minesweeper) consiste em revelar células de um tabuleiro sem cair em bombas. O tabuleiro é composto por células, algumas das quais contêm bombas, e outras que indicam o número de bombas adjacentes. O objetivo é abrir todas as células seguras sem explodir uma bomba.

## Estrutura e Módulos
## 1. Visão Geral da Arquitetura

O sistema é composto por módulos independentes que trabalham em conjunto para criar uma experiência de jogo completa. A arquitetura segue um padrão modular com Paradigma funcional com separação clara de responsabilidades.

### 1.1. Estrutura de Módulos
```
App(CampoMinado)/
├── Node/           # Definição básica das células
├── Grid/          # Gerenciamento do tabuleiro
├── BFS/           # Lógica de expansão
├── Game/          # Controle do estado do jogo
└── Main/          # Interface gráfica e loop principal
└── MainBot/       # Interface para rodar Bot

```

## 2. Módulos do Sistema

### 2.1. Node (Células)
#### 2.1.1. Estrutura Principal
```haskell
data Node = Node { 
    dataNode :: Int,    -- Conteúdo da célula (-1 para bomba, 0-8 para contagem)
    visited :: Bool,    -- Status de visitação
    hasFlag :: Bool     -- Indica se há bandeira
} deriving Show
```

#### 2.1.2. Tipos e Constantes
```haskell
type Coord = (Int, Int)  -- Coordenadas no tabuleiro
bomba :: Int = -1        -- Valor que representa uma bomba
```

#### 2.1.3. Estados Possíveis de uma Célula
- Não visitada sem bandeira
- Não visitada com bandeira
- Visitada vazia (0 bombas adjacentes)
- Visitada com número (1-8 bombas adjacentes)
- Visitada com bomba

### 2.2. Grid (Tabuleiro)
#### 2.2.1. Tipo Principal
```haskell
type Grid = [[Node]]  -- Matriz de nodos
```

#### 2.2.2. Funções Principais
```haskell
generateGrid :: Int -> Int -> IO (Grid, Int)
-- Gera tabuleiro com tamanho e número de bombas específicos
-- Retorna o grid e o número total de bombas

countAdjacentBombs :: Grid -> Int -> Grid
-- Calcula e atualiza o número de bombas adjacentes para cada célula

check :: Grid -> Int -> Coord -> Bool
-- Valida se uma coordenada é válida e pode ser revelada

updateGrid :: Grid -> Int -> Int -> Grid
-- Marca uma célula como visitada

showFlag :: Grid -> Int -> Int -> Grid
-- Coloca/remove uma bandeira em uma célula

revealBombs :: Grid -> Grid
-- Revela todas as bombas (usado ao perder o jogo)
```

#### 2.2.3. Processo de Geração do Tabuleiro
1. Seleção aleatória de posições para bombas
2. Criação da matriz inicial
3. Cálculo de bombas adjacentes
4. Remoção de duplicatas para garantir distribuição única

### 2.3. BFS (Busca em Largura)
#### 2.3.1. Função Principal
```haskell
bfs :: Grid -> Int -> Coord -> Int -> Int -> Bool -> IO (Bool, Int, Grid)
-- Parâmetros:
--   Grid atual
--   Tamanho do tabuleiro
--   Coordenada selecionada
--   Contador atual
--   Meta para vitória
--   Flag de marcação
-- Retorno:
--   Status do jogo
--   Novo contador
--   Grid atualizado
```

#### 2.3.2. Lógica de Expansão
1. Verifica se a célula atual é bomba
2. Se for célula vazia (0), expande para vizinhos
3. Se for número, apenas revela
4. Atualiza contador de células reveladas
5. Verifica condições de vitória

#### 2.3.3. Algoritmo de Expansão
```haskell
expandQueue :: Grid -> Int -> Coord -> [Coord] -> [Coord]
-- Expande a busca para células adjacentes válidas
-- Direções de expansão: Norte, Sul, Leste, Oeste
```

### 2.4. Game (Controle do Jogo)
#### 2.4.1. Estado do Jogo
```haskell
data GameState = GameState {
    state'grid :: Grid,              -- Tabuleiro atual
    state'size :: Int,               -- Dimensões
    state'cnt :: Int,                -- Células reveladas
    state'win :: Int,                -- Meta para vitória
    state'remainingBombs :: Int,     -- Bombas restantes
    state'finished :: Bool,          -- Jogo finalizado
    state'lose :: Bool               -- Status de derrota
}
```

#### 2.4.2. Funções de Controle
```haskell
gameInit :: Int -> String -> IO GameState
-- Inicializa novo jogo com tamanho e dificuldade

gameUpdate :: GameState -> Int -> Int -> Bool -> IO GameState
-- Processa ações do jogador e atualiza estado

getBombChance :: String -> Int -> Int
-- Calcula número de bombas baseado na dificuldade
```

#### 2.4.3. Níveis de Dificuldade
- **Fácil:**
  - 10% de bombas
  - Fórmula: `max 1 (size * size / 10)`
- **Normal:**
  - 20% de bombas
  - Fórmula: `max 2 (size * size / 5)`
- **Difícil:**
  - 33% de bombas
  - Fórmula: `max 3 (size * size / 3)`

## 3. Interface Gráfica (Main)

### 3.1. Configuração da Janela
```haskell
initWindow :: Int -> Int -> String -> Int -> IO ()
-- Largura: 1200 pixels (600*2)
-- Altura: 450 pixels
-- Título: "Campo Minado"
-- FPS: 60
```

### 3.2. Sistema de Renderização
#### 3.2.1. Assets e Sprites
```haskell
-- Definições de sprites
spriteVisited    = Rectangle (32) (83) 16 16
spriteNotVisited = Rectangle (50) (83) 16 16
spriteBomb       = Rectangle (16*2) (21+16*2) 16 16
-- ... outros sprites
```

#### 3.2.2. Características dos Sprites
- Tamanho base: 16x16 pixels
- Escala: 2x (32x32 pixels finais)
- Offset da grade: 100 pixels
- Carregamento via arquivo GIF

### 3.3. Sistema de Input
#### 3.3.1. Controles do Mouse
```haskell
MouseButtonLeft  -- Revela célula
MouseButtonRight -- Coloca/remove bandeira
```

#### 3.3.2. Processamento de Coordenadas
```haskell
-- Conversão de coordenadas do mouse para grid
col = (mouseX - gridOffset) div spriteBombSize
row = (mouseY - gridOffset) div spriteBombSize
```

## 4. Loop Principal do Jogo

### 4.1. Estrutura do Loop
```haskell
whileWindowOpen_ :: (GameState -> IO GameState) -> GameState -> IO ()
```

### 4.2. Fases do Loop
1. **Input Processing**
   ```haskell
   -- Captura de eventos
   leftButtonClicked  <- isMouseButtonPressed MouseButtonLeft
   rightButtonClicked <- isMouseButtonPressed MouseButtonRight
   (mouseX, mouseY)   <- getMousePosition
   ```

2. **Game Update**
   ```haskell
   newState <- if validClick
               then gameUpdate state row col rightButtonClicked
               else return state
   ```

3. **Rendering**
   ```haskell
   -- Renderização do tabuleiro visível
   clearBackground black
   forM_ (zip [0..] grid) $ \(rowIndex, rowList) -> 
       forM_ (zip [0..] rowList) $ \(colIndex, _) ->
           drawCell rowIndex colIndex
   ```

## 5. Mecânicas de Jogo

### 5.1. Sistema de Pontuação
- Células reveladas
- Bombas restantes

### 5.2. Condições de Vitória/Derrota
- **Vitória:** `estado 'cnt == state'win`
- **Derrota:** Revelar uma bomba

### 5.3. Sistema de Bandeiras
- Marcação de possíveis bombas
- Limite de bandeiras igual ao número de bombas
- Não impede revelação da célula

## 6. Debug e Desenvolvimento

### 6.1. Grade de Debug
- Mostra posição real das bombas
- Exibe números de todas as células
- Facilita testes e desenvolvimento

### 6.2. Logging
```haskell
putStrLn $ "Remaining bombs: " ++ show (state'remainingBombs newState)
putStrLn $ "       Finished: " ++ show (state'finished newState)
putStrLn $ "           Lose: " ++ show (state'lose newState)
putStrLn $ "            Cnt: " ++ show (state'cnt newState)
```

### 7. Bot do Mineswepper
### 7.1. Estrutura Principal

- `data CellKnowledge`: Tipo de dado que armazena o conhecimento de cada célula do tabuleiro.
```haskell
        data CellKnowledge = CellKnowledge {
            isBomb :: Bool,       -- Se a célula contém uma bomba
            probability :: Float, -- Probabilidade de ser uma bomba
            isRevealed :: Bool   -- Se a célula foi revelada
        } deriving Show
```

- `initKnowledge :: Int -> [[CellKnowledge]]`:
  Função que inicializa o conhecimento do **Bot** com uma probabilidade padrão (50%) de cada célula ser uma bomba e marca todas as células como não reveladas.

- `updateKnowledge :: Grid -> [[CellKnowledge]] -> [[CellKnowledge]]`:
  Atualiza o conhecimento das células com base no estado atual do tabuleiro. Se uma célula foi revelada ou tem uma bandeira, o conhecimento é ajustado.

- `calculateBombProbability :: Grid -> [[CellKnowledge]] -> Coord -> Float`:
  Calcula a probabilidade de uma célula ser uma bomba com base nas células adjacentes reveladas.

- `getAdjacentCells :: Grid -> Coord -> [Coord]`:
  Retorna as coordenadas das células adjacentes válidas de uma célula dada.

- `chooseMove :: GameState -> [[CellKnowledge]] -> IO (Int, Int, Bool)`:
  Escolhe a próxima jogada do **Bot**. O **Bot** prioriza movimentos seguros (probabilidade de bomba menor que 30%).

- `playBot :: GameState -> IO GameState`:
  Função principal que faz o **Bot** jogar. Ela chama a função **`playBotTurn`** recursivamente até que o jogo seja finalizado.

- `playBotTurn :: GameState -> [[CellKnowledge]] -> IO GameState`:
  Executa o turno do **Bot**. Ela escolhe o próximo movimento, atualiza o estado do jogo e continua jogando até que o jogo termine.



### 9.3. Estrutura de Arquivos
```
.
├── app/
│   ├── Node.hs
│   ├── Grid.hs
│   ├── BFS.hs
│   ├── Game.hs
│   └── Main.hs
│   └── MainBot.hs
├── assets/
│   └── sprite.gif
└── README.md
└── Guia.md
└── LICENSE

```