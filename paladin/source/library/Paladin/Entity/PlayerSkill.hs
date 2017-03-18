{-# LANGUAGE DeriveGeneric #-}

module Paladin.Entity.PlayerSkill where

import qualified Paladin.Entity.Common as Common
import qualified Paladin.Entity.Player as Player
import qualified Paladin.Entity.Playlist as Playlist

type PlayerSkillId = Common.Tagged PlayerSkill Int

data PlayerSkill = PlayerSkill
  { playerSkillId :: PlayerSkillId
  , playerSkillCreatedAt :: Common.UTCTime
  , playerSkillPlayerId :: Player.PlayerId
  , playerSkillPlaylistId :: Playlist.PlaylistId
  , playerSkillMatchesPlayed :: Int
  , playerSkillDivision :: Int
  , playerSkillTier :: Int
  , playerSkillMmr :: Double
  , playerSkillMu :: Double
  , playerSkillSigma :: Double
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow PlayerSkill

instance Common.ToJSON PlayerSkill where
  toEncoding = Common.genericToEncoding "playerSkill"

playerSkillProxy :: Common.Proxy PlayerSkill
playerSkillProxy = Common.Proxy
