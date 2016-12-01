module Main where

import Prelude

import Partial.Unsafe (unsafePartial)

import DOM (DOM) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.Node.ParentNode (querySelector) as DOM

import React (ReactElement, createFactory) as R
import ReactDOM (render) as R
import React.DOM (text, h1', a, div, button, ul, ul', li, li', input) as R
import React.DOM.Props (className, href, target, onClick, _type, placeholder) as RP
import Thermite as T

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)

import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)

import Control.Monad.Aff (Aff, launchAff)
import Network.HTTP.Affjax (affjax, defaultRequest)

import Data.HTTP.Method (Method(..))

import Data.Either
import Data.Maybe 
import Data.Nullable (toMaybe)
import Data.Array

type Task = { description :: String }

mkTask description = { description: description }

type TasksState = { tasks :: Array Task }

tasks = map mkTask [ "Acordar", "Levantar", "Comer", "Durmir" ]

data Action = Add | CallServer

performAction :: T.PerformAction _ TasksState _ Action
performAction Add _ _ =
  void (T.cotransform
        (\state -> state { tasks = ([ { description: "1"} ] <> state.tasks) }))
performAction CallServer _ _ = do
  amount <- lift fetchData
  void (T.cotransform
        (\state -> state { tasks = ([ { description: amount } ] <> state.tasks) }))

initialState :: TasksState
initialState = { tasks: tasks }

renderTask :: Task -> Array R.ReactElement
renderTask task =
  [ R.text task.description ]

render :: T.Render TasksState _ _
render dispatch _ state _ =
  [ R.div [ RP.className "container" ]
    [ R.div [ RP.className "modal show agenda" ]
      [ R.div [ RP.className "modal-content" ]
          [ R.div [ RP.className "modal-header" ]
            [ R.h1' [ R.text "Agenda" ] ]
          ,
            R.div [ RP.className "modal-body" ]
            [ R.div []
              [ R.ul' (map (R.li' <<< renderTask) state.tasks) ]
            ]
          ,
            R.div [ RP.className "modal-footer" ]
            [
              R.input [ RP._type "text"
                      , RP.placeholder "something here..."]
              []
              ,
              R.button [ RP.className "btn btn-default"
                       , RP.onClick \_ -> dispatch Add ]
              [ R.text "Add" ]
              ,
                            R.button [ RP.className "btn btn-default"
                       , RP.onClick \_ -> dispatch Shit ]
              [ R.text "Shit" ]
            ]
        ]
      ]
    ]
  ]

spec :: T.Spec _ _ _ _
spec = T.simpleSpec performAction render

fetchData :: Aff _ String
fetchData = do
  res <- affjax $ defaultRequest { url = "http://localhost/agenda/data.json", method = Left GET }
  pure res.response

main :: Eff (dom :: DOM.DOM) Unit
main = void do
  let component = T.createClass spec initialState
  document <- DOM.window >>= DOM.document
  container <- unsafePartial (fromJust <<< toMaybe <$> DOM.querySelector "#container" (DOM.htmlDocumentToParentNode document))
  R.render (R.createFactory component {}) container
