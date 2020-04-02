-- The helper protocol for computing aggregated signatures of correct computation.

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Sonic.Signature
  ( HscProof(..)
  , hscProve
  , hscVerify
  ) where

import Protolude
import Control.Monad.Random (MonadRandom)
import Data.Field.Galois (rnd)
import Data.List (zip3)
import Data.Pairing.BLS12381 (Fr, G1, BLS12381)
import Data.Poly.Sparse.Laurent (eval)
import Sonic.Utils (BiVLaurent, evalX, evalY)
import Sonic.CommitmentScheme (commitPoly, openPoly, pcV)
import Sonic.SRS (SRS(..))

data HscProof = HscProof
  { hscS :: [(G1 BLS12381, (Fr, G1 BLS12381))]
  , hscW :: [(Fr, G1 BLS12381, G1 BLS12381)]
  , hscQv :: G1 BLS12381
  , hscC :: G1 BLS12381
  , hscU :: Fr
  , hscV :: Fr
  } deriving (Eq, Show, Generic, NFData)

-- Common input: info = bp, srs, {z_j, y_j}_{j=1}^m, s(X,Y)
hscProve
  :: (MonadRandom m)
  => SRS               -- srs
  -> BiVLaurent Fr     -- S(X,Y)
  -> [(Fr, Fr)]        -- {(y_j, z_j)}_{j=1}^m
  -> m HscProof
hscProve srs@SRS{..} sXY yzs = do
  -- hscP1(info) -> ({S_j,s_j,W_j}_{j=1}^m
  let ss = (\(yi, zi) ->
              let sXy = evalY yi sXY            -- s(X,y_j)
                  cm = commitPoly srs srsD sXy  -- S_j<-Commit(bp,srs,d,s(X,y_j))
                  op = openPoly srs zi sXy      -- (s_j,W_j)<-Open(S_j,z_j,s(X,y_j))
              in (cm, op)
           ) <$> yzs

  -- hscV1(info,{S_j,s_j,W_j}_{j=1}^m)->u
  u <- rnd

  -- hscP2(u) -> {s'_j,W'_j,Q_j}_{j=1}^m
  let suX = evalX u sXY
      c = commitPoly srs srsD suX                               -- C <- Commit(bp,srs,d,s(u,X))
      sW = (\(yi, _zi)
            -> let (_, wj') = openPoly srs u (evalY yi sXY) -- (s'_j,W'_j)<-Open(S_j,u,s(X,y_j))
                   (sj', qj) = openPoly srs yi suX           -- (s'_j,Q_j)<-Open(C,y_j,s(u,X))
               in (sj', wj', qj)
           ) <$> yzs

  -- hscV2({s'_j,W'_j,Q_j}_{j=1}^m)-> v
  v <- rnd

  -- hscP2(v) -> Q_v
  let (_, qv) = openPoly srs v suX  -- (s(u,v),Q_v) <- Open(C,v,s(u,X))

  pure HscProof
    { hscS = ss
    , hscW = sW
    , hscQv = qv
    , hscC = c
    , hscU = u
    , hscV = v
    }

hscVerify
  :: SRS
  -> BiVLaurent Fr     -- S(X,Y)
  -> [(Fr, Fr)]        -- {(y_j, z_j)}_{j=1}^m
  -> HscProof
  -> Bool
hscVerify srs@SRS{..} sXY yzs proof@HscProof{..}
  = let sv = eval (evalY hscV sXY) hscU
        checks = foldl' (\acc ((yi, zi), (ci, (si, wi)), (si', wi', qi))
                         -> and [ acc
                                , pcV srs srsD ci zi (si, wi)       -- check pcV(bp,srs,S_j,d,z_j,(s_j,W_j)
                                , pcV srs srsD ci hscU (si', wi')   -- check pcV (bp,srs,S_j,d,u,(s'_j,W'_j))
                                , pcV srs srsD hscC yi (si', qi)    -- check pcV(bp,srs,C,d,y_j,(s'_j,Q_j)
                                ]
                        ) True (zip3 yzs hscS hscW)
        check = pcV srs srsD hscC hscV (sv, hscQv) -- check pcV(bp,srs,C,d,v,(s_v,Q_v))
    in check && checks
