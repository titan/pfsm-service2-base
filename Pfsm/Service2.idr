module Pfsm.Service2

import Data.Maybe
import Data.List
import Data.List1
import Pfsm
import Pfsm.Data

public export
data FsmIdStyle = FsmIdStyleGenerate
                | FsmIdStyleSession
                | FsmIdStyleDomain
                | FsmIdStyleUrl

export
Eq FsmIdStyle where
  (==) FsmIdStyleGenerate FsmIdStyleGenerate = True
  (==) FsmIdStyleSession  FsmIdStyleSession  = True
  (==) FsmIdStyleDomain   FsmIdStyleDomain   = True
  (==) FsmIdStyleUrl      FsmIdStyleUrl      = True
  (==) _                  _                  = False

public export
data MappingStyle = MappingStyleOneToOne
                  | MappingStyleOneToMany
                  | MappingStyleManyToOne
                  | MappingStyleManyToMany

export
Eq MappingStyle where
  (==) MappingStyleOneToOne   MappingStyleOneToOne   = True
  (==) MappingStyleOneToMany  MappingStyleOneToMany  = True
  (==) MappingStyleManyToOne  MappingStyleManyToOne  = True
  (==) MappingStyleManyToMany MappingStyleManyToMany = True
  (==) _                      _                      = False

export
manyToOneFieldFilter : Parameter -> Bool
manyToOneFieldFilter (_, _, ms)
  = case lookup "reference" ms of
         Just (MVString _) => case lookup "mapping" ms of
                                   Just (MVString "many-to-one") => True
                                   _ => False
         _ => False

----------
-- List --
----------

derefState : String -> List1 State -> State
derefState sname states
  = let filtered = filter (\s => s.name == sname) states in
        if length filtered > 0
           then fromMaybe (head states) $ head' filtered
           else head states

export
listOutputActionFilter : Action -> Bool
listOutputActionFilter (OutputAction (MkPort "add-to-state-list" _) _)      = True
listOutputActionFilter (OutputAction (MkPort "remove-from-state-list" _) _) = True
listOutputActionFilter _                                                    = False

export
liftStateNameFromListOutputAction : Action -> Maybe String
liftStateNameFromListOutputAction (OutputAction (MkPort "add-to-state-list" _) (sname :: _))      = Just (show sname)
liftStateNameFromListOutputAction (OutputAction (MkPort "remove-from-state-list" _) (sname :: _)) = Just (show sname)
liftStateNameFromListOutputAction _                                                               = Nothing

export
listOutputActionOfParticipantFilter : Action -> Bool
listOutputActionOfParticipantFilter (OutputAction (MkPort "add-to-state-list-of-participant" _) _)      = True
listOutputActionOfParticipantFilter (OutputAction (MkPort "remove-from-state-list-of-participant" _) _) = True
listOutputActionOfParticipantFilter _                                                                   = False

export
liftStateNameFromListOutputActionOfParticipant : Action -> Maybe (String, String)
liftStateNameFromListOutputActionOfParticipant (OutputAction (MkPort "add-to-state-list-of-participant" _) (pname :: _ :: sname :: _))      = Just (show sname, show pname)
liftStateNameFromListOutputActionOfParticipant (OutputAction (MkPort "remove-from-state-list-of-participant" _) (pname :: _ :: sname :: _)) = Just (show sname, show pname)
liftStateNameFromListOutputActionOfParticipant _                                                                                            = Nothing

export
liftListStates : List1 State -> List1 Transition -> List State
liftListStates states transitions
  = let actions = liftOutputActions states transitions
        listActions = filter listOutputActionFilter actions
        stateNames = nub $ filter nonblank $ map (fromMaybe "") $ map liftStateNameFromListOutputAction listActions in
        filter (\s => elem s.name stateNames) states

export
liftListStatesOfParticipants : List1 State -> List1 Transition -> List (State, String)
liftListStatesOfParticipants states transitions
  = let actions = liftOutputActions states transitions
        listActions = filter listOutputActionOfParticipantFilter actions
        pairs = nub $ filter (\(sname, pname) => sname /= "" && pname /= "") $ map (\x => case x of Just x' => x'; _ => ("", "")) $ map liftStateNameFromListOutputActionOfParticipant listActions in
        map (\(sname, pname) => ((derefState sname states), pname)) pairs

-----------
-- Index --
-----------

export
isSearchable : Maybe (List Meta) -> Bool
isSearchable metas
  = case lookup "gateway.searchable" metas of
         Just (MVString "true") => True
         _ => False

export
indexOutputActionFilter : Action -> Bool
indexOutputActionFilter (OutputAction (MkPort "push-to-state-index" _) _)  = True
indexOutputActionFilter (OutputAction (MkPort "flush-to-state-index" _) _) = True
indexOutputActionFilter _                                                  = False

export
indexOutputActionOfParticipantFilter : Action -> Bool
indexOutputActionOfParticipantFilter (OutputAction (MkPort "push-to-state-index-of-participant" _) _)    = True
indexOutputActionOfParticipantFilter (OutputAction (MkPort "flush-to-state-index-of-participant" _) _)   = True
indexOutputActionOfParticipantFilter _                                                                   = False

export
liftStateNameFromIndexOutputAction : Action -> Maybe String
liftStateNameFromIndexOutputAction (OutputAction (MkPort "push-to-state-index" _) (sname :: _))  = Just (show sname)
liftStateNameFromIndexOutputAction (OutputAction (MkPort "flush-to-state-index" _) (sname :: _)) = Just (show sname)
liftStateNameFromIndexOutputAction _                                                             = Nothing

liftStateNameFromIndexOutputActionOfParticipant : Action -> Maybe (String, String)
liftStateNameFromIndexOutputActionOfParticipant (OutputAction (MkPort "push-to-state-index-of-participant" _) (pname :: _ :: sname :: _))    = Just (show sname, show pname)
liftStateNameFromIndexOutputActionOfParticipant (OutputAction (MkPort "flush-to-state-index-of-participant" _) (pname :: _ :: sname :: _))   = Just (show sname, show pname)
liftStateNameFromIndexOutputActionOfParticipant _                                                                                            = Nothing

export
liftIndexStates : List1 State -> List1 Transition -> List State
liftIndexStates states transitions
  = let actions = liftOutputActions states transitions
        indexActions = filter indexOutputActionFilter actions
        stateNames = nub $ filter nonblank $ map (fromMaybe "") $ map liftStateNameFromIndexOutputAction indexActions in
        filter (\s => elem s.name stateNames) states

export
liftIndexStatesOfParticipants : List1 State -> List1 Transition -> List (State, String)
liftIndexStatesOfParticipants states transitions
  = let actions = liftOutputActions states transitions
        indexActions = filter indexOutputActionOfParticipantFilter actions
        pairs = nub $ filter (\(sname, pname) => sname /= "" && pname /= "") $ map (\x => case x of Just x' => x'; _ => ("", "")) $ map liftStateNameFromIndexOutputActionOfParticipant indexActions in
        map (\(sname, pname) => ((derefState sname states), pname)) pairs

-------------
-- IdStyle --
-------------

export
fsmIdStyleOfEvent : Event -> FsmIdStyle
fsmIdStyleOfEvent (MkEvent _ _ metas)
  = case lookup "gateway.fsmid-style" metas of
         Just (MVString "generate") => FsmIdStyleGenerate
         Just (MVString "session") => FsmIdStyleSession
         Just (MVString "domain") => FsmIdStyleDomain
         Just (MVString "url") => FsmIdStyleUrl
         _ => FsmIdStyleUrl

export
fsmIdStyleOfFsm : Fsm -> FsmIdStyle
fsmIdStyleOfFsm (MkFsm _ _ _ _ _ _ _ metas)
  = case lookup "gateway.fsmid-style" metas of
         Just (MVString "generate") => FsmIdStyleGenerate
         Just (MVString "session") => FsmIdStyleSession
         Just (MVString "domain") => FsmIdStyleDomain
         Just (MVString "url") => FsmIdStyleUrl
         _ => FsmIdStyleUrl

---------------
-- Parameter --
---------------

export
payloadParameterFilter : Parameter -> Bool
payloadParameterFilter (_, _, ms)
  = case lookup "in-service-context" ms of
         Just (MVString "true") => False
         _ => True
