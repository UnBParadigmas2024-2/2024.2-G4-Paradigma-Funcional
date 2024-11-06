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
     - `Int`: Contagem necessária para vencer o jogo.
   - **Saída:**
     - `(Bool, Int, Grid)`: Booleano indicando se o jogo continua (`True` ou `False`), a nova contagem de células abertas e o tabuleiro atualizado.
   - **Função Chave:**
     - `bfs :: Grid -> Int -> Coord -> Int -> Int -> IO (Bool, Int, Grid)`: Inicia a busca em largura a partir da célula fornecida, explorando as células vizinhas e atualizando o estado do jogo. Retorna o estado do jogo (continua ou termina), a nova contagem de células abertas e o tabuleiro atualizado.
     - **Processo:**
       - Se a célula atual for uma bomba, o jogo termina com derrota.
       - Se o número de células abertas atingir o limite necessário para vencer, o jogo termina com vitória.
       - Se a célula for um número (diferente de zero), a busca não expande para células adjacentes.
       - Se a célula for vazia, a fila de busca é expandida para incluir as células adjacentes não visitadas.

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
     - `Int`: Contagem necessária para vencer o jogo.
   - **Saída:**
     - `(Bool, Int, Grid)`: Booleano indicando se o jogo continua (`True` ou `False`), a nova contagem de células abertas e o tabuleiro atualizado.
   - **Função Chave:**
     - `bfs :: Grid -> Int -> Coord -> Int -> Int -> IO (Bool, Int, Grid)`: Inicia a busca em largura a partir da célula fornecida, explorando as células vizinhas e atualizando o estado do jogo. Retorna o estado do jogo (continua ou termina), a nova contagem de células abertas e o tabuleiro atualizado.

   - **Descrição Detalhada:**
     A função `bfs` implementa a lógica de busca em largura para explorar o tabuleiro de campo minado. A partir de uma célula inicial fornecida pelo usuário ou pelo bot, a função explora as células vizinhas (não visitadas e não bombas), revelando-as e verificando se o jogo termina em vitória ou derrota.

     - **Processo:**
       - Se a célula atual for uma bomba, o jogo termina com derrota.
       - Se o número de células abertas atingir o limite necessário para vencer, o jogo termina com vitória.
       - Se a célula for um número (diferente de zero), a busca não expande para células adjacentes.
       - Se a célula for vazia, a fila de busca é expandida para incluir as células adjacentes não visitadas.

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

     **Descrição do Exemplo:**
     - Neste exemplo, o tabuleiro é gerado com `generateGrid` e as bombas são aleatoriamente distribuídas.
     - A célula (5, 5) é a célula inicial explorada pela função `bfs`.
     - O contador de células abertas começa em `0`, e o número necessário para vitória é calculado como `100 - countBombs` (onde 100 representa o número total de células do tabuleiro, considerando o exemplo de tamanho 10x10).
     - O resultado da busca é avaliado: se `result` for `True`, significa que o jogo continua; caso contrário, o jogo foi perdido por ter atingido uma bomba.
     - Após a execução, o tabuleiro atualizado é impresso.

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