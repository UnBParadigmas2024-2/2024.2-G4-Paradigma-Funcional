## Especificação de Entradas e Saídas



## Estrutura e Módulos

### 1. Módulo Tabuleiro

1. **Tabuleiro** (`Grid`)
   - Geração do campo minado com bombas aleatórias.
   - Indicação do número de minas adjacentes a cada célula.
   - Controle do estado de cada célula (bomba, número ou vazia).
   - Controle do tabuleiro visível e do tabuleiro solução.
   - Atualização de células conforme interações do usuário ou do Bot.

2. **Lógica de Busca** (`BFS`)
   - Implementação da lógica de expansão (busca em largura) para abrir células vazias.
   - Verificação de vitórias e derrotas.
   - Expansão de células adjacentes de forma eficiente.

3. **Nodo** (`Node`)
   - Estrutura de dados para cada célula, com informação sobre conteúdo (bomba ou número) e estado (visitado ou não).

---


### Entradas e Saídas por Módulo

1. **Grid (Tabuleiro)**
   - **Entrada:**
     - `Int`: Tamanho do tabuleiro.
     - `Coord`: Coordenadas da célula a ser revelada.
   - **Saída:**
     - `Grid`: Matriz com o estado atualizado das células.
     - `Int`: Número de bombas no tabuleiro.
   - **Funções Chave:**
     - `generateGrid :: Int -> IO (Grid, Int)`: Gera um campo minado aleatório.
     - `countAdjacentBombs :: Grid -> Int -> Grid`: Conta bombas adjacentes a cada célula.
     - `revealBombs :: Grid -> Grid`: Revela todas as bombas ao perder o jogo.

2. **BFS (Lógica de Expansão)**
   - **Entrada:**
     - `Grid`: Estado atual do tabuleiro.
     - `Int`: Tamanho do tabuleiro.
     - `Coord`: Coordenada inicial da célula a ser explorada.
     - `Int`: Contagem atual de células abertas.
     - `Int`: Contagem necessária para vitória.
   - **Saída:**
     - `(Bool, Int, Grid)`: Booleano indicando se o jogo continua, nova contagem de células abertas e o tabuleiro atualizado.
   - **Funções Chave:**
     - `bfs :: Grid -> Int -> Coord -> Int -> Int -> IO (Bool, Int, Grid)`: Lógica de busca em largura para revelar células e verificar o estado do jogo.

3. **Node (Células)**
   - **Estrutura:**
     - `data Node = Node { dataNode :: Int, visited :: Bool }`: Cada célula possui um conteúdo (bomba ou número) e um estado (visitado ou não).
   - **Constantes:**
     - `bomba :: Int`: Constante que representa uma bomba (`-1`).

---

## Parâmetros para Interações

1. **Tamanho do Tabuleiro**
   - O tamanho do tabuleiro é definido pelo usuário no início do jogo. Exemplo: `size :: Int`

2. **Movimentos do Usuário**
   - O usuário fornece as coordenadas da célula que deseja revelar. Exemplo: `input :: (Int, Int)`

3. **Interações do Bot**
   - O Bot gera movimentos aleatórios ou otimizados, dependendo do nível de dificuldade.
   - Diferentes estratégias de input para simular a jogabilidade do usuário.

---

## Exemplo de Uso

1. **Jogo no Modo Usuário**
   - **Entrada:** `generateGrid 10` (gera um tabuleiro 10x10)
   - **Saída:** Tabuleiro com células ocultas e uma contagem de bombas.

2. **Jogo no Modo Bot**
   - **Entrada:** Coordenadas geradas aleatoriamente ou com lógica de IA.
   - **Saída:** Tabuleiro atualizado conforme os movimentos do Bot.

3. **Input de Usuário/Bot**
   - Formato esperado: `Linha Coluna` (Exemplo: `3 4` para a célula na linha 3 e coluna 4)
   - Mensagens de feedback: "Vitória", "Derrota" ou "Continua o jogo"

### Entradas e Saídas por Módulo

1. **Grid (Tabuleiro)**
   - **Entrada:**
     - `Int`: Tamanho do tabuleiro.
     - `Coord`: Coordenadas da célula a ser revelada.
   - **Saída:**
     - `Grid`: Matriz com o estado atualizado das células.
     - `Int`: Número de bombas no tabuleiro.
   - **Funções Chave:**
     - `generateGrid :: Int -> IO (Grid, Int)`: Gera um campo minado aleatório.
     - `countAdjacentBombs :: Grid -> Int -> Grid`: Conta bombas adjacentes a cada célula.
     - `revealBombs :: Grid -> Grid`: Revela todas as bombas ao perder o jogo.
  
   **Exemplos de Uso:**
   - **Geração de Tabuleiro**
     ```haskell
     main :: IO ()
     main = do
         (grid, countBombs) <- generateGrid 10
         putStrLn "Tabuleiro Gerado:"
         printGrid grid
         putStrLn $ "Número de Bombas: " ++ show countBombs
     ```
   - **Revelação de Bombas ao Perder**
     ```haskell
     let finalGrid = revealBombs grid
     printGrid finalGrid
     ```

2. **BFS (Lógica de Expansão)**
   - **Entrada:**
     - `Grid`: Estado atual do tabuleiro.
     - `Int`: Tamanho do tabuleiro.
     - `Coord`: Coordenada inicial da célula a ser explorada.
     - `Int`: Contagem atual de células abertas.
     - `Int`: Contagem necessária para vitória.
   - **Saída:**
     - `(Bool, Int, Grid)`: Booleano indicando se o jogo continua, nova contagem de células abertas e o tabuleiro atualizado.
   - **Funções Chave:**
     - `bfs :: Grid -> Int -> Coord -> Int -> Int -> IO (Bool, Int, Grid)`: Lógica de busca em largura para revelar células e verificar o estado do jogo.

   **Exemplos de Uso:**
   - **Revelar Célula e Verificar Jogo**
     ```haskell
     main :: IO ()
     main = do
         (grid, countBombs) <- generateGrid 10
         let win = 100 - countBombs
         (result, newCount, updatedGrid) <- bfs grid 10 (5, 5) 0 win
         if result
             then putStrLn "O jogo continua!"
             else putStrLn "Você perdeu!"
         printGrid updatedGrid
     ```

3. **Node (Células)**
   - **Estrutura:**
     - `data Node = Node { dataNode :: Int, visited :: Bool }`: Cada célula possui um conteúdo (bomba ou número) e um estado (visitado ou não).
   - **Constantes:**
     - `bomba :: Int`: Constante que representa uma bomba (`-1`).

   **Exemplos de Uso:**
   - **Criação de um Nodo de Bomba**
     ```haskell
     let bombaNode = Node bomba False
     print bombaNode -- Saída: Node { dataNode = -1, visited = False }
     ```

---