# fp-checkers

## Running Tests

- **Run all the tests and exit**  
  >stack test

- **Run single tests**  
  >stack ghci fp-checkers:fp-checkers-test  
  - Examples  
    - MoveSpec.main
    - JumpMoveSpec.main   
  - In ghci need to load
  - >:set -package hspec
  - >:set -package QuickCheck
  - >:set -package hspec -package QuickCheck
  
- **Development and tests**
  - > stack ghci
  - > :add test/**NameOfSpec**
  - > **NameOfSpec**.main
  