module EstadoInicial where

import Types

estadoInicial :: World
estadoInicial = World {player1 = p1, player2 = p2, mapa = mp}
    where
        mp = Mapa {
            paredeEsq = -5
            , chao = 0
            , paredeDir = 5
            }
        p1 = Fighter{
        fighterPos = (-6, 0)
        , fighterVida = 100
        , fighterStance = Standing
        , fighterDir = Direita
        , fighterVelX = 0.05
        , fighterVelY = 0
        , keyLeft = False
        , keyRight = False
        , keyDown = False
        }

        p2 = Fighter{
        fighterPos = (5, 0)
        , fighterVida = 100
        , fighterStance = Standing
        , fighterDir = Esquerda
        , fighterVelX = 0.05
        , fighterVelY = 0
        , keyLeft = False
        , keyRight = False
        , keyDown = False
        } 
        