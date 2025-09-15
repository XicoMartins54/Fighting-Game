module Types where
import GHC.Generics (Associativity(LeftAssociative))



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
  { aiPhase   :: AttackPhase   -- fase actual
  , aiTimer   :: Float         -- tempo restante na fase (segundos)
  , aiHasHit  :: Bool          -- já acertou durante esta instância?
  , aiDamage  :: Float         -- dano desta instância
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
normalAttackHitbox f@(Fighter { normalAttack = Just (AttackInstance phase _ _ _)
                             , fighterTamanho = tam
                             , fighterStance = stance
                             , fighterDir = dir
                             , keyLeft = kl
                             , keyRight = kr
                             , keyDown = kd }) =

  let def   = defaultNormalAttack f
      defDown = defaultNormalAttackDown f
      w     = naWidth def
      h     = naHeight def
      wDown = naWidth defDown
      hDown = naHeight defDown
      mult  = case phase of
                Windup   -> 0.6
                Peak     -> 1
                Recovery -> 0.6
      sign
        | kl && not kr   = -1
        | not kl && kr   = 1
        | dir == Esquerda = -1
        | otherwise      = 1
      w'   = w * mult
      offX = sign * w' / 2
      offY = tam / 3 * 2
  in case stance of
       Jumping  -> if kd
                   then Just (0, offY + hDown/2, wDown, hDown)
                   else Just (offX, 0, w', h)
       Falling  -> if kd
                   then Just (0, offY + hDown/2, wDown, hDown)
                   else Just (offX, 0, w', h)
       Standing -> Just (offX, 0, w', h)
       Crouching -> Just (offX, 0, w', h)
