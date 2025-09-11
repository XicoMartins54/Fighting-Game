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

        fighterVida :: Float,

        fighterStance :: Stance,

        fighterDir :: Direcao,

        fighterVelX   :: Float,
        fighterVelY   :: Float,

        keyLeft       :: Bool,
        keyRight      :: Bool,
        keyDown       :: Bool,

        normalAttack  :: Maybe AttackInstance
    }
    deriving (Eq, Show)


data AttackPhase = Windup | Peak | Recovery
  deriving (Eq, Show)

data AttackInstance = AttackInstance
  { aiPhase :: AttackPhase   -- fase actual
  , aiTimer :: Float         -- tempo restante na fase (segundos)
  } deriving (Eq, Show)

-- definição de parâmetros do ataque normal (constante global)
data NormalAttackDef = NormalAttackDef
  { naWindup  :: Tempo   -- duracao da windup (s)
  , naPeak    :: Tempo   -- duracao da peak (s)
  , naRecovery:: Tempo   -- duracao da recovery (s)
  , naWidth   :: Float   -- largura do hitbox
  , naHeight  :: Float   -- altura do hitbox
  } deriving (Eq, Show)

defaultNormalAttack :: NormalAttackDef
defaultNormalAttack = NormalAttackDef
  { naWindup   = 0.1
  , naPeak     = 0.08
  , naRecovery = 0.1
  , naWidth    = 150
  , naHeight   = 60
  }


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