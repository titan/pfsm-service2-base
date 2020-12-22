module Pfsm.Service2

import Data.Maybe
import Data.List
import Data.List1
import Pfsm
import Pfsm.Data

------------
-- Action --
------------

export
liftActionsFromTrigger : Trigger -> List Action
liftActionsFromTrigger (MkTrigger _ _ _ (Just actions)) = List1.toList actions
liftActionsFromTrigger (MkTrigger _ _ _ Nothing)        = []

export
liftActionsFromTransition : Transition -> List Action
liftActionsFromTransition (MkTransition _ _ triggers)
  = flatten $ map liftActionsFromTrigger $ List1.toList triggers

export
liftActionsFromFsm : List1 State -> List1 Transition -> List Action
liftActionsFromFsm states transitions
  = let stateActions = flatten $ map liftActionsFromState $ List1.toList states
        transitionActions = flatten $ map liftActionsFromTransition $ List1.toList transitions in
        transitionActions ++ stateActions

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
listOutputActionFilter (OutputAction "add-to-state-list" _)      = True
listOutputActionFilter (OutputAction "remove-from-state-list" _) = True
listOutputActionFilter _                                         = False

export
liftStateNameFromListOutputAction : Action -> Maybe String
liftStateNameFromListOutputAction (OutputAction "add-to-state-list" (sname :: _))      = Just (show sname)
liftStateNameFromListOutputAction (OutputAction "remove-from-state-list" (sname :: _)) = Just (show sname)
liftStateNameFromListOutputAction _                                                    = Nothing

export
listOutputActionOfParticipantFilter : Action -> Bool
listOutputActionOfParticipantFilter (OutputAction "add-to-state-list-of-participant" _)      = True
listOutputActionOfParticipantFilter (OutputAction "remove-from-state-list-of-participant" _) = True
listOutputActionOfParticipantFilter _                                                        = False

export
liftStateNameFromListOutputActionOfParticipant : Action -> Maybe (String, String)
liftStateNameFromListOutputActionOfParticipant (OutputAction "add-to-state-list-of-participant" (pname :: _ :: sname :: _))      = Just (show sname, show pname)
liftStateNameFromListOutputActionOfParticipant (OutputAction "remove-from-state-list-of-participant" (pname :: _ :: sname :: _)) = Just (show sname, show pname)
liftStateNameFromListOutputActionOfParticipant _                                                                                 = Nothing

export
liftListStates : List1 State -> List1 Transition -> List State
liftListStates states transitions
  = let actions = liftActionsFromFsm states transitions
        listActions = filter listOutputActionFilter actions
        stateNames = nub $ filter nonblank $ map (fromMaybe "") $ map liftStateNameFromListOutputAction listActions in
        filter (\s => elem s.name stateNames) states

export
liftListStatesOfParticipants : List1 State -> List1 Transition -> List (State, String)
liftListStatesOfParticipants states transitions
  = let actions = liftActionsFromFsm states transitions
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
indexOutputActionFilter (OutputAction "push-to-state-index" _)  = True
indexOutputActionFilter (OutputAction "flush-to-state-index" _) = True
indexOutputActionFilter _                                       = False

export
indexOutputActionOfParticipantFilter : Action -> Bool
indexOutputActionOfParticipantFilter (OutputAction "push-to-state-index-of-participant" _)    = True
indexOutputActionOfParticipantFilter (OutputAction "flush-to-state-index-of-participant" _)   = True
indexOutputActionOfParticipantFilter _                                                        = False

export
liftStateNameFromIndexOutputAction : Action -> Maybe String
liftStateNameFromIndexOutputAction (OutputAction "push-to-state-index" (sname :: _))  = Just (show sname)
liftStateNameFromIndexOutputAction (OutputAction "flush-to-state-index" (sname :: _)) = Just (show sname)
liftStateNameFromIndexOutputAction _                                                  = Nothing

liftStateNameFromIndexOutputActionOfParticipant : Action -> Maybe (String, String)
liftStateNameFromIndexOutputActionOfParticipant (OutputAction "push-to-state-index-of-participant" (pname :: _ :: sname :: _))    = Just (show sname, show pname)
liftStateNameFromIndexOutputActionOfParticipant (OutputAction "flush-to-state-index-of-participant" (pname :: _ :: sname :: _))   = Just (show sname, show pname)
liftStateNameFromIndexOutputActionOfParticipant _                                                                                 = Nothing

export
liftIndexStates : List1 State -> List1 Transition -> List State
liftIndexStates states transitions
  = let actions = liftActionsFromFsm states transitions
        indexActions = filter indexOutputActionFilter actions
        stateNames = nub $ filter nonblank $ map (fromMaybe "") $ map liftStateNameFromIndexOutputAction indexActions in
        filter (\s => elem s.name stateNames) states

export
liftIndexStatesOfParticipants : List1 State -> List1 Transition -> List (State, String)
liftIndexStatesOfParticipants states transitions
  = let actions = liftActionsFromFsm states transitions
        indexActions = filter indexOutputActionOfParticipantFilter actions
        pairs = nub $ filter (\(sname, pname) => sname /= "" && pname /= "") $ map (\x => case x of Just x' => x'; _ => ("", "")) $ map liftStateNameFromIndexOutputActionOfParticipant indexActions in
        map (\(sname, pname) => ((derefState sname states), pname)) pairs
