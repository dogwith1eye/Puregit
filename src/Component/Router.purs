-- | The `Router` component is the root of our Halogen application. Every other component is a 
-- | direct descendent of this component. We'll use the router to choose which component to render
-- | given a particular `Route` and to manage the user's location in the application.
-- |
-- | See `Main` to understand how this component is used as the root of the application.
module Puregit.Component.Router where

import Prelude

import Puregit.Capability.LogMessages (class LogMessages)
import Puregit.Capability.Navigate (class Navigate)
import Puregit.Capability.Now (class Now)
import Puregit.Capability.Resource.User (class ManageUser)
import Puregit.Data.Profile (Profile)
import Puregit.Data.Route (Route(..))
import Puregit.Page.Home as Home
import Puregit.Page.Login as Login
import Puregit.Page.Profile as Profile
import Puregit.Page.Register as Register
import Puregit.Page.Settings as Settings
import Control.Monad.Reader (class MonadAsk)
import Data.Either.Nested (Either5)
import Data.Functor.Coproduct.Nested (Coproduct5)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH

type State =
  { route :: Route }

data Query a
  = Navigate Route a

type Input =
  Maybe Route

-- If you haven't seen nested `Coproduct` or `Either` before, or you haven't worked with multiple types of
-- child component, then these types are probably confusing. They're a little tedious to define and are
-- being removed in favor of a much nicer mechanism in Halogen 5, but are necessary in Halogen 4.
-- 
-- For a detailed explanation of what's going on here, please see this issue:
-- https://github.com/thomashoneyman/purescript-halogen-realworld/issues/20
type ChildQuery = Coproduct5
  Home.Query
  Login.Query
  Register.Query
  Settings.Query
  Profile.Query

type ChildSlot = Either5
  Unit
  Unit
  Unit
  Unit
  Unit

component
  :: forall m r
   . MonadAff m
  => MonadAsk { currentUser :: Ref (Maybe Profile) | r } m
  => Now m
  => LogMessages m
  => Navigate m
  => ManageUser m
  => H.Component HH.HTML Query Input Void m
component =
  H.parentComponent
    { initialState: \initialRoute -> { route: fromMaybe Home initialRoute } 
    , render
    , eval
    , receiver: const Nothing
    }

  where 

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval (Navigate dest a) = do
    { route } <- H.get 
    when (route /= dest) do
      H.modify_ _ { route = dest }
    pure a

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render { route } = case route of
    Home -> 
      HH.slot' CP.cp1 unit Home.component unit absurd
    Login -> 
      HH.slot' CP.cp2 unit Login.component unit absurd
    Register -> 
      HH.slot' CP.cp3 unit Register.component unit absurd
    Settings -> 
      HH.slot' CP.cp4 unit Settings.component unit absurd
    Profile username -> 
      HH.slot' CP.cp5 unit Profile.component { username } absurd
