# Campo Minado

**Disciplina**: FGA0210 - PARADIGMAS DE PROGRAMAÇÃO - T01 <br>
**Nro do Grupo**: 4<br>
**Paradigma**: Funcional<br>

## Alunos
|Matrícula | Aluno |
| :--: | :--: |
| 20/2045624 | [Abdul hannan](https://github.com/hannanhunny01)	|
| 21/1029147 | [Arthur de Melo Viana](https://github.com/arthurmlv)	|
| 19/0026758 | [Deivid Carvalho](https://github.com/deivid-a1) |	
| 21/1062867 | [Felipe de Jesus Rodrigues](https://github.com/felipeJRdev) |
| 17/0108341 | [Levi de Oliveira Queiroz]() |	
| 17/0111059 | [Matheus Fonseca Sousa](https://github.com/gatotabaco) |	
| 19/0093331 | [Matheus Costa Gomes](https://github.com/mc-gomes) |	
| 21/2005444 | [Pedro fonseca Cruz]()	|
| 21/1029559 | [Rafael Brito Bosi Rodrigues](https://github.com/StrangeUnit28) |	
| 16/0149410 | [Yudi Yamane de Azevedo](https://github.com/yudi-azvd)	| 


## Sobre 
Descreva o seu projeto em linhas gerais. 
Use referências, links, que permitam conhecer um pouco mais sobre o projeto.
Capriche nessa seção, pois ela é a primeira a ser lida pelos interessados no projeto.


## Screenshots
Adicione 2 ou mais screenshots do projeto em termos de interface e/ou funcionamento.


## Execução
**Linguagens**: [Haskell](https://www.haskell.org/downloads/)

**Tecnologias**: [Cabal](https://www.haskell.org/cabal/), [Raylib](https://www.raylib.com/)

### Pré-requisitos

Clone este repositório em seu ambiente local:
```bash
git clone <URL_DO_REPOSITORIO>
cd 2024.2_G4_Funcional_CampoMinado
```

Certifique-se de ter o Haskell instalado. Para verificar, execute:
```bash
ghc --version
```

O Cabal geralmente é incluído com a instalação do Haskell, mas caso não esteja disponível, instale-o conforme as instruções no site do Haskell: [Haskell.org](https://www.haskell.org/).

### Dependências
- Cabal 3.10.3
- GHC 9.6.6
- GHCUP 0.1.30.0 (opcional)
- Raylib (através do pacote h-raylib)
- base 4.18 && < 4.19

### h-raylib
Para instalação das dependências do h-raylib, consulte a documentção 
[Platform specific requirements](https://hackage.haskell.org/package/h-raylib-5.5.2.1#platform-specific-requirements).

### Executando o Projeto no Windows

Instale as dependências e compile o projeto usando:

```bash
cabal build
```

Após a construção do projeto, execute o jogo com o comando:
```bash
cabal run
```

### Executando o Projeto no Linux e macOS

Execute o comando a seguir para instalar Haskell e Cabal:

```bash
sudo apt-get install haskell-platform  # Para Linux (Debian/Ubuntu)
brew install ghc cabal-install          # Para macOS com Homebrew
```

Compile o projeto e instale as dependências com:
```bash
cabal build
```

Após o build, inicie o jogo com:
```bash
cabal run
```

### Problemas Comuns
- Caso enfrente erros de compilação, verifique se as versões do GHC e Cabal estão atualizadas.
- Consulte a documentação oficial para resolver dependências específicas de pacotes Haskell.

Com essas instruções, você deve estar pronto para compilar e jogar Campo Minado no Haskell!


## Uso 
Explique como usar seu projeto.
Procure ilustrar em passos, com apoio de telas do software, seja com base na interface gráfica, seja com base no terminal.
Nessa seção, deve-se revelar de forma clara sobre o funcionamento do software.


## Vídeo
Adicione 1 ou mais vídeos com a execução do projeto.
Procure: 
(i) Introduzir o projeto;
(ii) Mostrar passo a passo o código, explicando-o, e deixando claro o que é de terceiros, e o que é contribuição real da equipe;
(iii) Apresentar particularidades do Paradigma, da Linguagem, e das Tecnologias, e
(iV) Apresentar lições aprendidas, contribuições, pendências, e ideias para trabalhos futuros.
OBS: TODOS DEVEM PARTICIPAR, CONFERINDO PONTOS DE VISTA.
TEMPO: +/- 15min


## Participações
Apresente, brevemente, como cada membro do grupo contribuiu para o projeto.

|       Nome do Membro        | Contribuição                                                                 | Significância da Contribuição para o Projeto (Excelente/Boa/Regular/Ruim/Nula) | Comprobatórios (ex. links para commits) |
| :-------------------------: | ---------------------------------------------------------------------------- | :----------------------------------------------------------------------------: | :-------------------------------------: |
|           Fulano            | Programação dos Fatos da Base de Conhecimento Lógica                         |                                      Boa                                       |          Commit tal (com link)          |
| Abdul hannan |    Criação do documento Guia.md para detalhar e explicar melhor todos os módulos e funções do projeto e criacao do bot de toma decisões basedo nas probabilidades, escolhendo células com menor risco (<0.3) e usa BFS para explorar células| Excelente   |   |
|    Arthur de Melo Viana     |                                                                              |                                                                                |                                         |
|       Deivid Carvalho       |                                                                              |                                                                                |                                         |
|  Felipe de Jesus Rodrigues  |                                                                              |                                                                                |                                         |
|  Levi de Oliveira Queiroz   |                                                                              |                                                                                |                                         |
|    Matheus Fonseca Sousa    |    Implementação das interações com mouse na interface do projeto utilizando algumas funções da biblioteca Raylib  | Boa |   [fc55305](https://github.com/UnBParadigmas2024-2/2024.2_G4_Funcional_CampoMinado/commit/fc55305cf02c9b3078a1ebd669e9830a0f04d7e8) |                                                       |                                         |
|     Matheus Costa Gomes     | Implementação da funcionalidade de inserir bandeira e organização de código  |  Excelente  | [PR #5](https://github.com/UnBParadigmas2024-2/2024.2_G4_Funcional_CampoMinado/pull/5) <br> [PR #11](https://github.com/UnBParadigmas2024-2/2024.2_G4_Funcional_CampoMinado/pull/11)  |	
| Pedro fonseca Cruz          |                                                                              |                                                                                |                                         |
| Rafael Brito Bosi Rodrigues | Implementação da DFS para busca dos nós por meio de uma estrutura de pilha, implementação das telas de seleção de estrutura e tamanho de grid | Excelente |  [2dc4047](https://github.com/UnBParadigmas2024-2/2024.2_G4_Funcional_CampoMinado/commit/2dc404706ade9381073bbcaab5b31ec6b4dcccd1)<br>[6adef38](https://github.com/UnBParadigmas2024-2/2024.2_G4_Funcional_CampoMinado/commit/6adef3884914a3cd77b3cd1539e486ee11b9b00b)<br>[74e39b3](https://github.com/UnBParadigmas2024-2/2024.2_G4_Funcional_CampoMinado/commit/74e39b3b9ba5a51f1d1e8d2266cfd01e52ced112)|
|   Yudi Yamane de Azevedo    | Setup projeto com Raylib, renderização da grade de acordo com o estado do nó |                                      Excelente                                       |            [1ccffce](https://github.com/UnBParadigmas2024-2/2024.2_G4_Funcional_CampoMinado/commit/1ccffceb1186b3d9fbcff0c66a690cc7074c153f), [b292060](https://github.com/UnBParadigmas2024-2/2024.2_G4_Funcional_CampoMinado/commit/b292060a043fd68fa13040a5df1e4f9bb7767a93)             |



## Outros 

### Projeto Campo Minado em Haskell

Este projeto, desenvolvido em Haskell com a biblioteca gráfica raylib, implementa o jogo de Campo Minado explorando o paradigma funcional. Durante o desenvolvimento, aprofundamos nossa compreensão sobre as vantagens e desafios da programação funcional, especialmente no contexto de manipulação de estados e recursão, características centrais no Haskell.

### Lições Aprendidas
Ao longo do projeto, aprendemos a aproveitar o poder das funções puras e da imutabilidade, características que tornam o código mais previsível e seguro, apesar da complexidade que essa abordagem pode trazer em jogos interativos. Compreendemos melhor o uso de funções de alta ordem e como a ausência de efeitos colaterais facilita o teste e a verificação de nosso código.

### Percepções
Notamos que a implementação de algoritmos como DFS (usando pilhas) e BFS (usando filas) em um ambiente funcional pode exigir uma abordagem diferente daquela que faríamos em paradigmas imperativos, o que tornou o desenvolvimento desafiador e instigante. Haskell nos incentivou a pensar de forma diferente, buscando resolver problemas com elegância funcional.

### Contribuições e Fragilidades
Cada membro contribuiu significativamente para a conclusão do projeto. Contudo, tivemos dificuldades em alinhar os horários para reuniões, o que impactou na integração dos códigos desenvolvidos individualmente e no entendimento conjunto de algumas partes da lógica. Esse desafio acabou gerando algum retrabalho, mas também reforçou a importância da comunicação e da coordenação em equipes de desenvolvimento.

### Trabalhos Futuros
Para o futuro, planejamos otimizar os algoritmos de busca e explorar novas abordagens para a resolução do jogo, podendo incluir a implementação de um tipo diferente de busca. Além disso, estamos estudando formas de melhorar a performance geral do projeto e a interação entre os diferentes módulos funcionais.

## Fontes

- [Exemplo de imagem h-raylib](https://github.com/Anut-py/h-raylib/tree/master/examples/basic-images)
- [Assets do Campo Minado](https://github.com/BrandonDusseau/minesweeper-classic)
- [Asset do Botão](https://pauboeg.itch.io/ui-starter-pack?download)
