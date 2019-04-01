-- | User profiles display the articles they have written as well as the articles they have
-- | favorited. It's also the main way users choose to follow one another. Users can view their
-- | own profile.
module Puregit.Page.Profile where

import Prelude

import Control.Monad.Reader (class MonadAsk, asks)
import Data.Lens (Traversal')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), _Success, fromMaybe, toMaybe)

import Puregit.Capability.Resource.User (class ManageUser, getAuthor)
import Puregit.Component.HTML.Footer (footer)
import Puregit.Component.HTML.Header (header)
import Puregit.Component.HTML.Utils (css, maybeElem)
import Puregit.Component.Part.FollowButton (follow, followButton, unfollow)
import Puregit.Data.Avatar as Avatar
import Puregit.Data.Profile (Profile, Author)
import Puregit.Data.Route (Route(..))
import Puregit.Data.Username (Username)
import Puregit.Data.Username as Username

type State =
  { author :: RemoteData String Author
  , page :: Int
  , currentUser :: Maybe Profile
  , username :: Username
  }

type Input =
  { username :: Username }

data Query a
  = Initialize a
  | Receive Input a
  | LoadAuthor a
  | FollowAuthor a
  | UnfollowAuthor a

component
  :: forall m r
   . MonadAff m
  => MonadAsk { currentUser :: Ref (Maybe Profile) | r } m
  => ManageUser m
  => H.Component HH.HTML Query Input Void m
component =
  H.lifecycleComponent
    { initialState
    , render
    , eval
    , receiver: HE.input Receive
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }
  where 
  initialState :: Input -> State
  initialState { username } =
    { author: NotAsked
    , currentUser: Nothing
    , page: 1
    , username
    }

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Initialize a -> do
      mbProfile <- H.liftEffect <<< Ref.read =<< asks _.currentUser
      st <- H.modify _ { currentUser = mbProfile }
      void $ H.fork $ eval $ LoadAuthor a
      pure a
    
    Receive { username } a -> do
      st <- H.get
      when (st.username /= username) do
        H.modify_ _ { username = username }
        void $ H.fork $ eval $ Initialize a
      pure a

    LoadAuthor a -> do
      st <- H.modify _ { author = Loading }
      author <- getAuthor st.username
      H.modify_ _ { author = fromMaybe author }
      pure a
    
    FollowAuthor a -> 
      follow _author $> a

    UnfollowAuthor a -> 
      unfollow _author $> a
  
  _author :: Traversal' State Author
  _author = prop (SProxy :: SProxy "author") <<< _Success

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
    [ header state.currentUser (Profile state.username)
    , HH.div
      [ css "profile-page" ]
      [ userInfo state ]
    , footer
    ]
  
  userInfo state =
    HH.div
    [ css "user-info"]
    [ HH.div 
      [ css "container" ]
      [ HH.div 
        [ css "row" ]
        [ HH.div
          [ css "col-xs-12 col-md-10 offset-md-1" ]
          [ HH.img 
            [ css "user-img" 
            , HP.src $ Avatar.toStringWithDefault (_.image =<< toMaybe state.author)
            ]
          , HH.h4_
            [ HH.text $ Username.toString state.username ]
          , maybeElem (_.bio =<< toMaybe state.author) \str ->
              HH.p_
                [ HH.text str ]
          , maybeElem (toMaybe state.author) (followButton FollowAuthor UnfollowAuthor)
          ]
        ]
      ]
    ]
