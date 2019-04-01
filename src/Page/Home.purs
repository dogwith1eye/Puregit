module Puregit.Page.Home where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Data.Const (Const)
import Data.Maybe (Maybe(..), isNothing)
import Halogen as H
import Halogen.HTML as HH

import Puregit.Capability.Navigate (class Navigate)
import Puregit.Component.HTML.Footer (footer)
import Puregit.Component.HTML.Header (header)
import Puregit.Component.HTML.Utils (css, whenElem)
import Puregit.Component.Utils (guardSession)
import Puregit.Data.Profile (Profile)
import Puregit.Data.Route (Route(..))

data Query a
  = NoOp a
  | Initialize a

type State =
  { currentUser :: Maybe Profile }

type Input = Unit

type Message = Void

type ChildSlot = Unit
type ChildQuery = Const Void

component
  :: forall m r
   . MonadAff m
  => MonadAsk { currentUser :: Ref (Maybe Profile) | r } m
  => Navigate m
  => H.Component HH.HTML Query Unit Void m
component =
  H.lifecycleComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }
  where 

  initialState :: Unit -> State
  initialState _ =
    { currentUser: Nothing }

  render :: State -> H.ComponentHTML Query
  render state@{ currentUser } =
    HH.div_
    [ header currentUser Home
    , HH.div
      [ css "home-page" ]
      [ whenElem (isNothing currentUser) \_ -> banner
      , HH.div
        [ css "container page" ]
        [ HH.div
          [ css "row" ]
          [ mainView state
          , HH.div
            [ css "col-md-3" ]
            [ HH.div
              [ css "sidebar" ]
              [ HH.p_ 
                [ HH.text "Popular Tags" ]
              , HH.div_ 
                [ HH.text "Tags not loaded" ]
              ]
            ]
          ]
        ]
      ]
    , footer
    ]

  mainView :: forall i. State -> H.HTML i Query
  mainView state =
    HH.div_
       [ HH.text $ "Puregit" ]

  banner :: forall i p. HH.HTML i p 
  banner =
    HH.div
    [ css "banner" ]
    [ HH.div
      [ css "container" ]
      [ HH.h1
        [ css "logo-font" ]
        [ HH.text "conduit" ]
      , HH.p_ 
        [ HH.text "A place to share your knowledge." ]
      ]
    ]
    
  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Initialize a -> do
      guardSession >>= case _ of
        Nothing -> do 
          void $ H.fork $ eval $ NoOp a
        profile -> do
          H.modify_ _ { currentUser = profile }
      pure a
    NoOp a -> do
      pure a
