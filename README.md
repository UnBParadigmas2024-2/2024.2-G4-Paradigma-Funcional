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
|    Matheus Fonseca Sousa    |                                                                              |                                                                                |                                         |
|     Matheus Costa Gomes     | Implementação da funcionalidade de inserir bandeira e organização de código  |  Excelente  | [PR #5](https://github.com/UnBParadigmas2024-2/2024.2_G4_Funcional_CampoMinado/pull/5) <br> [PR #11](https://github.com/UnBParadigmas2024-2/2024.2_G4_Funcional_CampoMinado/pull/11)  |	
| Pedro fonseca Cruz          |                                                                              |                                                                                |                                         |
| Rafael Brito Bosi Rodrigues |                                                                              |                                                                                |                                         |
|   Yudi Yamane de Azevedo    | Setup projeto com Raylib, renderização da grade de acordo com o estado do nó |                                      Boa                                       |            [1ccffce](https://github.com/UnBParadigmas2024-2/2024.2_G4_Funcional_CampoMinado/commit/1ccffceb1186b3d9fbcff0c66a690cc7074c153f), [b292060](https://github.com/UnBParadigmas2024-2/2024.2_G4_Funcional_CampoMinado/commit/b292060a043fd68fa13040a5df1e4f9bb7767a93)             |



## Outros 
Quaisquer outras informações sobre o projeto podem ser descritas aqui. Não esqueça, entretanto, de informar sobre:
(i) Lições Aprendidas;
(ii) Percepções;
(iii) Contribuições e Fragilidades, e
(iV) Trabalhos Futuros.


## Fontes
Referencie, adequadamente, as referências utilizadas.
Indique ainda sobre fontes de leitura complementares.

- [Exemplo de imagem h-raylib](https://github.com/Anut-py/h-raylib/tree/master/examples/basic-images)
