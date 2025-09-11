module EstadoInicial where

import Types

estadoInicial :: World
estadoInicial = World {player1 = p1, player2 = p2, mapa = mp}
    where
        mp = Mapa {
            paredeEsq = -870
            , chao = 0
            , paredeDir = 870
            }
        p1 = Fighter { fighterPos = (-600, 0)
             , fighterVida = 100
             , fighterTamanho = 300
             , fighterStance = Standing
             , fighterDir = Direita
             , fighterVelX = 10
             , fighterVelY = 0
             , keyLeft = False
             , keyRight = False
             , keyDown = False
             , normalAttack = Nothing
             }

        p2 = Fighter { fighterPos = (600, 0)
             , fighterVida = 100
             , fighterTamanho = 300
             , fighterStance = Standing
             , fighterDir = Esquerda
             , fighterVelX = 10
             , fighterVelY = 0
             , keyLeft = False
             , keyRight = False
             , keyDown = False
             , normalAttack = Nothing
             }
        