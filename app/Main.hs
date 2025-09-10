module Main where

import Graphics.Gloss
import Types
import Tempo
import Events
import Draw
import EstadoInicial

main :: IO ()
main = play window background 80 estadoInicial desenha reageEventos reageTempo