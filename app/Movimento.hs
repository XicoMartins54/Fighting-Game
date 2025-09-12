module Movimento where

import Types
import Types (Fighter(normalAttack, fighterTamanho, fighterPeso))
import Data.Maybe (Maybe(Nothing))

gravityForPeso :: Peso -> Float
gravityForPeso MuitoLeve   = -4500
gravityForPeso Leve        = -6000
gravityForPeso Medio       = -7500
gravityForPeso Pesado      = -9000
gravityForPeso MuitoPesado = -10500

jumpInitialVel :: Float
jumpInitialVel = 2250

velocidadeForPeso :: Peso -> Float
velocidadeForPeso MuitoLeve   = 13
velocidadeForPeso Leve        = 11
velocidadeForPeso Medio       = 9
velocidadeForPeso Pesado      = 7
velocidadeForPeso MuitoPesado = 5


atualiza :: Float -> World -> World
atualiza dt w@(World { player1 = p1, player2 = p2, mapa = mp }) =
  w { player1 = atualizaP dt p1 p2 mp, player2 = atualizaP dt p2 p1 mp}

-- actualiza um fighter dado o delta-time e o mapa
-- actualiza um fighter dado o delta-time e o mapa
atualizaP :: Float -> Fighter -> Fighter -> Mapa -> Fighter
atualizaP dt f1@(Fighter { fighterPos = (x,y)
                        , fighterTamanho = tam
                        , fighterPeso = peso
                        , fighterVelY = vy
                        , fighterStance = stance
                        , fighterDir = dir
                        , keyLeft = kl
                        , keyRight = kr
                        , keyDown = kd})
            f2@(Fighter { fighterPos = (z,w)})
           (Mapa {paredeEsq = pe, chao = ch, paredeDir = pd}) =

  let
    -- avança ataque normal
    normalAttack' = stepNormalAttack dt f1 (normalAttack f1)
    -- atualizamos f1 já com a instância de ataque avançada
    f1' = f1 { normalAttack = normalAttack' }

    -- movimento horizontal (vx é velocidade em px/s -> multiplicamos por dt)
    dx
      | kl && not kr = - vx
      | kr && not kl =   vx
      | otherwise    = 0
    x'
      | x + dx <= pe = pe
      | x + dx >= pd - tam / 2 = pd - tam / 2
      | otherwise = x + dx

    g = gravityForPeso peso

    vx = velocidadeForPeso peso

    -- movimento vertical (mantive a tua lógica)
    (y', vy', stance') = case stance of
      Jumping ->
        let vy0 = if vy == 0 then jumpInitialVel else vy
            vy1 = vy0 + g * dt
            y1  = y + vy1 * dt
        in if vy1 <= 0
           then (y1, vy1, Falling)
           else (y1, vy1, Jumping)

      Falling ->
        let vy1 = vy + g * dt
            y1  = y + vy1 * dt
        in if y1 <= ch
           then (ch, 0, Standing)
           else (y1, vy1, Falling)

      _ ->
        if y > ch
        then let vy1 = vy + g * dt
                 y1  = y + vy1 * dt
             in if y1 <= ch
                then (ch, 0, Standing)
                else (y1, vy1, Falling)
        else (ch, 0, stance)

    -- crouching
    stance''
      | y' <= ch && kd = Crouching
      | y' <= ch       = Standing
      | otherwise      = stance'

    -- troca de direção (usando x' depois do movimento)
    dir' 
      | dir == Direita && x' > z && normalAttack' == Nothing = Esquerda
      | dir == Esquerda  && x' < z && normalAttack' == Nothing = Direita
      | otherwise = dir

  -- devolve f1' (já com normalAttack atualizado) e aplica as alterações de posição/estado
  in f1' { fighterPos = (x', y')
         , fighterVelY = vy'
         , fighterStance = stance''
         , fighterDir = dir' }


-- avança a instância do ataque normal dt segundos
stepNormalAttack :: Float -> Fighter -> Maybe AttackInstance -> Maybe AttackInstance
stepNormalAttack _ _ Nothing = Nothing
stepNormalAttack dt f (Just ai@(AttackInstance phase t))
  | t > dt = Just (ai { aiTimer = t - dt })
  | otherwise =
      case phase of
        Windup   -> Just (AttackInstance Peak     (naPeak   (defaultNormalAttack f)))
        Peak     -> Just (AttackInstance Recovery (naRecovery (defaultNormalAttack f)))
        Recovery -> Nothing