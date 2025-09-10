module Movimento where

import Types

gravity :: Float
gravity = -25

jumpInitialVel :: Float
jumpInitialVel = 10

atualiza :: Float -> World -> World
atualiza dt w@(World { player1 = p1, player2 = p2, mapa = mp }) =
  w { player1 = atualizaP dt p1 p2 mp, player2 = atualizaP dt p2 p1 mp}

-- actualiza um fighter dado o delta-time e o mapa
atualizaP :: Float -> Fighter -> Fighter -> Mapa -> Fighter
atualizaP dt f1@(Fighter { fighterPos = (x,y)
                        , fighterVelX = vx
                        , fighterVelY = vy
                        , fighterStance = stance
                        , fighterDir = dir
                        , keyLeft = kl
                        , keyRight = kr
                        , keyDown = kd})
            f2@(Fighter { fighterPos = (z,w)})
           (Mapa {paredeEsq = pe, chao = ch, paredeDir = pd}) =

  let
    -- movimento horizontal
    dx
      | kl && not kr = -vx
      | kr && not kl = vx
      | otherwise    = 0
    x'
      | x + dx <= pe - 2 = pe - 2
      | x + dx >= pd + 1 = pd + 1
      | otherwise = x + dx

    -- movimento vertical
    (y', vy', stance') = case stance of
      Jumping ->
        let vy0 = if vy == 0 then jumpInitialVel else vy
            vy1 = vy0 + gravity * dt
            y1  = y + vy1 * dt
        in if vy1 <= 0
           then (y1, vy1, Falling)
           else (y1, vy1, Jumping)

      Falling ->
        let vy1 = vy + gravity * dt
            y1  = y + vy1 * dt
        in if y1 <= ch
           then (ch, 0, Standing)
           else (y1, vy1, Falling)

      _ ->
        if y > ch
        then let vy1 = vy + gravity * dt
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
      | dir == Direita && x' > z = Esquerda
      | dir == Esquerda  && x' < z = Direita
      | otherwise = dir

  in f1 { fighterPos = (x', y')
        , fighterVelY = vy'
        , fighterStance = stance''
        , fighterDir = dir' }


