module Tempo where

import Types
import Movimento

reageTempo :: Float -> World -> World
reageTempo t w = atualiza t w