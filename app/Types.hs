module Types where
import GHC.Generics (Associativity(LeftAssociative))
import Foreign.C (CIntMax)



data World = World {player1 :: Fighter, player2 :: Fighter, mapa :: Mapa}

type Tempo = Float

data Mapa = Mapa {paredeEsq :: Float, chao :: Float, paredeDir :: Float}

type Posicao = (Float, Float)

data Fighter = Fighter
    {
        fighterPos :: Posicao,
        fighterTamanho :: Float,
        fighterPeso :: Peso,

        fighterVida :: Float,

        fighterStance :: Stance,

        fighterDir :: Direcao,

        fighterVelY   :: Float,

        keyLeft       :: Bool,
        keyRight      :: Bool,
        keyDown       :: Bool,

        normalAttack  :: Maybe AttackInstance,

        isInvincible  :: Bool,
        invincibleTimer:: Float
    }
    deriving (Eq, Show)


data AttackPhase = Windup | Peak | Recovery
  deriving (Eq, Show)

data AttackInstance = AttackInstance
  { aiPhase  :: AttackPhase
  , aiTimer  :: Float
  , aiHasHit :: Bool
  , aiDamage :: Float
  , aiDir    :: DirecaoAtaque
  } deriving (Eq, Show)


-- definição de parâmetros do ataque normal (constante global)
data NormalAttackDef = NormalAttackDef
  { naWindup   :: Tempo   -- duracao da windup (s)
  , naPeak     :: Tempo   -- duracao da peak (s)
  , naRecovery :: Tempo   -- duracao da recovery (s)
  , naWidth    :: Float   -- largura do hitbox
  , naHeight   :: Float   -- altura do hitbox
  , naDamage   :: Float   -- dano do ataque
  } deriving (Eq, Show)

defaultNormalAttack :: Fighter -> NormalAttackDef
defaultNormalAttack f@(Fighter {fighterTamanho = tam}) = NormalAttackDef
  { naWindup   = 0.1
  , naPeak     = 0.08
  , naRecovery = 0.1
  , naWidth    = tam / 5 * 3
  , naHeight   = tam / 5
  , naDamage   = 10    -- escolhe o valor que quiseres
  }

defaultNormalAttackDown :: Fighter -> NormalAttackDef
defaultNormalAttackDown f@(Fighter {fighterTamanho = tam}) = NormalAttackDef
  { naWindup   = 0
  , naPeak     = 1
  , naRecovery = 0
  , naWidth    = tam / 2
  , naHeight   = tam / 3
  , naDamage   = 12
  }


data Peso 
  = MuitoLeve
  | Leve
  | Medio
  | Pesado
  | MuitoPesado
  deriving (Eq, Show)

data Direcao
    = Esquerda
    | Direita
    deriving (Eq, Show)

data DirecaoAtaque
    = Esq
    | Dir
    | Cima
    | Baixo
    | CimaDir
    | CimaEsq
    | BaixoDir
    | BaixoEsq
    deriving (Eq, Show)

data Stance
    = Standing
    | Jumping
    | Falling
    | Crouching
    deriving (Eq, Show)

type IsHit = Bool

type IsInvincible = Bool 


normalAttackHitbox :: Fighter -> Maybe (Float, Float, Float, Float)
normalAttackHitbox (Fighter { normalAttack = Nothing }) = Nothing
normalAttackHitbox f@(Fighter { normalAttack = Just (AttackInstance phase _ _ _ aiDir)
                             , fighterTamanho = tam
                             , fighterStance = stance
                             , keyLeft = kl
                             , keyRight = kr
                             }) =

  let def     = defaultNormalAttack f
      defDown = defaultNormalAttackDown f
      w       = naWidth def
      h       = naHeight def
      wDown   = naWidth defDown
      hDown   = naHeight defDown
      mult    = case phase of
                  Windup   -> 0.6
                  Peak     -> 1
                  Recovery -> 0.6

      isDownAttack = case aiDir of
                       Baixo -> True; BaixoDir -> True; BaixoEsq -> True
                       _     -> False

      -- sinal horizontal segundo a direcção do ataque guardada
      signX = case aiDir of
                Esq      -> -1
                CimaEsq  -> -1
                BaixoEsq -> -1
                Dir      -> 1
                CimaDir  -> 1
                BaixoDir -> 1
                _        -> 1 -- default para vertical/none usa facing à direita por segurança

      w' = w * mult
      offX = signX * w' / 2
      offY = tam / 3 * 2

  in case (stance, isDownAttack) of
       (Jumping, True)  -> Just (0, offY + hDown/2, wDown, hDown)
       (Falling, True)  -> Just (0, offY + hDown/2, wDown, hDown)
       (Jumping, False) -> Just (offX, 0, w', h)
       (Falling, False) -> Just (offX, 0, w', h)
       (_, True)        -> Just (offX, 0, wDown, hDown) -- ground down attack
       (_, False)       -> Just (offX, 0, w', h)



chooseAttackDir :: Fighter -> DirecaoAtaque
chooseAttackDir f =
  let kl = keyLeft f
      kr = keyRight f
      kd = keyDown f
      st = fighterStance f
      dirBase = case fighterDir f of Esquerda -> Esq; Direita -> Dir
      vy = fighterVelY f
      -- sinais base X/Y
      signX
        | kl && not kr = -1
        | kr && not kl = 1
        | otherwise    = 0
      signY
        -- prioridade a "down" se o jogador estiver a carregar down
        | kd && signX < 0 = -1  -- down+left
        | kd && signX > 0 = -1  -- down+right
        | kd              = -1
        -- se estiver no ar, considera up ou down segundo vy
        | st /= Standing && vy > 0 = 1  -- subir = up
        | st /= Standing && vy <= 0 = -1 -- cair = down
        | otherwise = 0
  in case (signX, signY) of
       (-1, -1) -> BaixoEsq
       ( 1, -1) -> BaixoDir
       ( 0, -1) -> Baixo
       (-1,  1) -> CimaEsq
       ( 1,  1) -> CimaDir
       ( 0,  1) -> Cima
       (-1,  0) -> Esq
       ( 1,  0) -> Dir
       ( 0,  0) -> dirBase