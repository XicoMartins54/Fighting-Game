module EstadoInicial where

import Types
import Types (Peso(MuitoPesado, MuitoLeve))

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
                     , fighterTamanho = 200
                     , fighterPeso = MuitoLeve
                     , fighterStance = Standing
                     , fighterDir = Direita
                     , fighterVelY = 0
                     , keyLeft = False
                     , keyRight = False
                     , keyDown = False
                     , keyUp = False
                     , normalAttack = Nothing
                     , isInvincible = False
                     , invincibleTimer = 0
                     }

        p2 = Fighter { fighterPos = (600, 0)
                     , fighterVida = 100
                     , fighterTamanho = 300
                     , fighterPeso = Pesado
                     , fighterStance = Standing
                     , fighterDir = Direita
                     , fighterVelY = 0
                     , keyLeft = False
                     , keyRight = False
                     , keyDown = False
                     , keyUp = False
                     , normalAttack = Nothing
                     , isInvincible = False
                    , invincibleTimer = 0
                     }
             
        