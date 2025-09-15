module Movimento where

import Types
import Types (Fighter(normalAttack, fighterTamanho, fighterPeso), AttackInstance)
import Data.Maybe (Maybe(Nothing))

gravityForPeso :: Peso -> Float
gravityForPeso MuitoLeve   = -4500
gravityForPeso Leve        = -6000
gravityForPeso Medio       = -7500
gravityForPeso Pesado      = -9000
gravityForPeso MuitoPesado = -10500

jumpInitialVel :: Float
jumpInitialVel = 2250

velocidadeForPeso :: Peso -> Maybe AttackInstance -> Float
velocidadeForPeso MuitoLeve   Nothing = 13
velocidadeForPeso Leve        Nothing = 11
velocidadeForPeso Medio       Nothing = 9
velocidadeForPeso Pesado      Nothing = 7
velocidadeForPeso MuitoPesado Nothing = 5
velocidadeForPeso MuitoLeve   _ = 13 / 4
velocidadeForPeso Leve        _ = 11 / 4
velocidadeForPeso Medio       _ = 9 / 4
velocidadeForPeso Pesado      _ = 7 / 4
velocidadeForPeso MuitoPesado _ = 5 / 4


atualiza :: Float -> World -> World
atualiza dt w@(World { player1 = p1, player2 = p2, mapa = mp }) =
  let p1' = atualizaP dt p1 p2 mp
      p2' = atualizaP dt p2 p1 mp

      -- decresce timers de invencibilidade antes de checar colisões
      p1d = decayInv dt p1'
      p2d = decayInv dt p2'

      -- depois resolve colisões e aplica danos
      (p1final, p2final) = resolveCollisions p1d p2d
  in w { player1 = p1final, player2 = p2final }

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

    vx = velocidadeForPeso peso (normalAttack f1')

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
stepNormalAttack dt f (Just ai@(AttackInstance phase t hasHit dmg))
  | t > dt = Just (ai { aiTimer = t - dt })
  | otherwise =
      case phase of
        Windup   -> Just (AttackInstance Peak     (naPeak   (defaultNormalAttack f)) False (naDamage (defaultNormalAttack f)))
        Peak     -> Just (AttackInstance Recovery (naRecovery (defaultNormalAttack f)) hasHit (aiDamage ai))
        Recovery -> Nothing


rectsIntersect :: (Float,Float,Float,Float) -> (Float,Float,Float,Float) -> Bool
rectsIntersect (cx1, cy1, w1, h1) (cx2, cy2, w2, h2) =
  (abs (cx1 - cx2) * 2 <= (w1 + w2)) && (abs (cy1 - cy2) * 2 <= (h1 + h2))


attackHitboxWorld :: Fighter -> Maybe (Float,Float,Float,Float)
attackHitboxWorld f@(Fighter { fighterPos = (x,y)
                            , fighterTamanho = altura
                            , fighterStance = stance }) =
  case normalAttackHitbox f of
    Nothing -> Nothing
    Just (cx, cy, w, h) ->
      let largura = altura / 2
          centerX = x + largura/2 + cx
          centerY = case stance of
                      Crouching -> (h/2) - 450 - cy
                      _         -> y + (altura/3*2) - 450 - cy
      in Just (centerX, centerY, w, h)

-- hurtbox do corpo do fighter (mesmas coordenadas que usas no desenho)
fighterHurtbox :: Fighter -> (Float,Float,Float,Float)
fighterHurtbox f@(Fighter { fighterPos = (x,y)
                          , fighterTamanho = altura
                          , fighterStance = stance }) =
  let largura = altura / 2
  in case stance of
       Crouching -> (x + largura/2, (altura/2 - altura/3 - 450), largura, (altura / 3))
       _         -> (x + largura/2, (y + altura/2 - 450), largura, altura)

-- decrementa invincibility timer
decayInv :: Float -> Fighter -> Fighter
decayInv dt f@(Fighter { isInvincible = inv, invincibleTimer = t })
  | not inv = f
  | otherwise =
      let t' = t - dt
      in if t' <= 0 then f { isInvincible = False, invincibleTimer = 0 } else f { invincibleTimer = t' }

-- aplica ataque de attacker a defender, se em Peak e se intersecta e defender não invencível
applyAttackOnce :: Fighter -> Fighter -> (Fighter, Fighter)
applyAttackOnce attacker defender =
  case normalAttack attacker of
    Just ai@(AttackInstance Peak _ hasHit dmg) | not hasHit ->
      case attackHitboxWorld attacker of
        Just hbAtt ->
          let hbDef = fighterHurtbox defender
          in if (not (isInvincible defender)) && rectsIntersect hbAtt hbDef
             then
               let defender' = defender { fighterVida = fighterVida defender - dmg
                                       , isInvincible = True
                                       , invincibleTimer = 0.4  -- 400ms invencibilidade (ajusta)
                                       }
                   ai' = ai { aiHasHit = True }
                   attacker' = attacker { normalAttack = Just ai' }
               in (attacker', defender')
             else (attacker, defender)
        Nothing -> (attacker, defender)
    _ -> (attacker, defender)

-- resolve colisões dos dois fighters (aplica ataques de ambos, sem duplicar)
resolveCollisions :: Fighter -> Fighter -> (Fighter, Fighter)
resolveCollisions p1 p2 =
  let (p1a, p2a) = applyAttackOnce p1 p2   -- p1 tenta acertar p2
      (p2b, p1b) = applyAttackOnce p2a p1a -- p2 (actualizado) tenta acertar p1 (actualizado)
  in (p1b, p2b)