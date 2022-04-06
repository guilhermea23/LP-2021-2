-- Intruções:
-- -  Não importe nenhuma biblioteca EXCETO se na descrição do exercício estiver explícito.
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Trab1.Tr where
import Data.List(sortOn)
-- 1) (Valor da questão: 1,0 ponto)
-- Defina uma função que retorne o maior entre quatro inteiros.
maior4 :: Int -> Int -> Int -> Int -> Int
maior4 a b c d
  | a >= b && a >= c && a >= d = a
  | b >= a && b >= c && b >= d = b
  | c >= a && c >= b && c >= d = c
  | otherwise = d

-- 2) (Valor da questão: 1,0 ponto)
-- Defina uma função que receba uma nota e retorne a menção do aluno.
-- Não se preocupe com a validação do input. A nota sempre será um Número entre 0.0 (inclusive) e 10.0 (inclusive).
-- Considere a seguinte tabela para tradução da menção:
-- De 9 a 10 -> "SS"
-- De 7 a 8.9 -> "MS"
-- De 5 a 6.9 -> "MM"
-- De 3 a 4.9 -> "MI"
-- De 0.1 a 2.9 -> "II"
-- De 0 -> "SR"
converterNotaParaMencao :: Float -> String
converterNotaParaMencao nota
  | nota == 0.0 = "SR"
  | nota >= 0.1 && nota < 3.0 = "SR"
  | nota >= 3.0 && nota < 5.0 = "MI"
  | nota >= 5.0 && nota < 7.0 = "MM"
  | nota >= 7.0 && nota < 9.0 = "MS"
  | otherwise = "SS"

-- 3) (Valor da questão: 1,0 ponto)
-- defina uma função que retorna um booleano indicando se uma lista de inteiros é decrescente ou não:

isDecrescente :: [Int] -> Bool
isDecrescente [] = True
isDecrescente [lst] = True
isDecrescente (lst:lst2)
 | lst <= head lst2 = False
 | null lst2 = True
 | otherwise = isDecrescente lst2

-- 4) (Valor da questão: 2,0 pontos)
-- defina uma função que recebe uma lista de strings como entrada e computa uma lista de pares
-- de (String, Int) representando o histograma de seus elementos:
histograma :: [String] -> [(String, Int)]
histograma = undefined

-- 5)(Valor da questão: 1,5 ponto)
-- Defina a função myZipWith que tem como parâmetros uma função binária (que tem dois parâmetros) e duas listas,
-- retornando uma lista de valores resultantes da aplicação dessa função nos elementos correspondentes dessas
-- listas:
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith func [] _ = []
myZipWith func _ [] = []
myZipWith func (a:as) (b:bs) = func a b : myZipWith func as bs

-- 6) (Valor da questão: 2,0 ponto)
-- Resolva em Haskell o seguinte problema: a partir de duas notas das provas de cada aluno,
-- determinar a lista dos alunos aprovados, com suas respectivas médias. O resultado deve estar
-- ordenado crescentemente pela média aritmética das notas. A aprovação ocorre se, e somente se, tal
-- média é maior ou igual a cinco.
-- OBSERVAÇÃO: especificamente para este exercício, você pode importar as funções de ordenaçao de listas (como 'sort' ou 'sortBy') se achar necessário.

aprovadosMedia :: [(String, Float, Float)] -> [(String, Float)]
aprovadosMedia [] = []
aprovadosMedia ((name,nota1,nota2):restante)
 | media >=5 = (name,media) : aprovadosMedia restante
 | otherwise = aprovadosMedia restante
 where 
  media = (nota1 +nota2)/2


aprovadosOrdemDeMedia media = reverse (sortOn snd (aprovadosMedia media))

-- 7) (Valor da questão: 1,5 ponto, sendo 0.5 ponto para cada letra)
-- Considere a representação de matrizes como lista de listas em que cada elemento da lista é uma lista
-- que representa uma linha da matriz. Com base nisso, determine as seguintes funções:
--  a) some duas matrizes
--  b) compute a transposta de duas matrizes
--  c) compute a multiplicação de duas matrizes
-- OBSERVAÇÃO: considere que os inputs são válidos (ou seja, as matrizes são válidas e as suas dimensões são compatíveis para soma e multiplicação)
somaMatricial :: Num u => [[u]] -> [[u]] -> [[u]]
somaMatricial = undefined

matrizTransposta :: Num u => [[u]] -> [[u]]
matrizTransposta = undefined

multiplicacaoMatricial :: Num u => [[u]] -> [[u]] -> [[u]]
multiplicacaoMatricial = undefined