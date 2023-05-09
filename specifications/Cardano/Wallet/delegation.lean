
section

parameter Slot : Type 
parameter PoolId : Type 

-- possible status for the delegation at a specific slot 
inductive Status : Type
    -- no pool assined and staking off
    | Inactive : Status
    -- no pool delegateed and staking on
    | Registered : Status
    -- pool delegateed and staking on
    | Active  : PoolId -> Status
open Status

parameter after
    :  Slot -- what is non-strictly after of
    -> Slot -- reference 
    -> Prop

parameter before
    :  Slot -- what is strictly before of
    -> Slot -- reference 
    -> Prop

parameter Delegations : Type

parameter status : Slot -> Delegations -> Status

def Delta := Slot -> Delegations -> Delegations
def Change := Status -> Status -> Prop

def property 
  :  Delta -> Change -> Prop 
  := λ delta change
  , ∀ (x : Slot) (y : Slot) (xs : Delegations)
  , let   xs'  := delta x xs
  ,       old  := status x xs  -- at slot 'x'
  ,       new  := status y xs' -- forall slots
  in 
    before y x → new = status y xs
    ∧ after y x → change old new

-- set staking on, from slot
parameter register : Delta
-- set staking off and undelegate if necessary, from slot
parameter deregister : Delta
-- delegate to given pool, if registered or active, from slot
parameter delegate :  PoolId -> Delta
-- forget changes, from slot
parameter rollback : Delta 

def precond (check : Status -> Prop) (target : Status) : Change 
    := λ old new
    , let cond := check old 
      in (cond → new = target) ∧ ((¬ cond) → new = old)

structure DelegationsProps :=

  (register_property
   : property register 
        ( precond 
           (λ old, old = Inactive) 
           Registered
        )
  )

  (deregister_property
   : ∀ p , property deregister
      (precond
        (λ old, old = Registered ∨ old = Active p)
        Inactive
      )
  )

  (delegate_property
  : ∀ p p' , property (delegate p) 
      ( precond 
          (λ old, old = Registered ∨ old = Active p') 
          (Active p)
      )
  )

  (rollback_property
  : property rollback (λ old new, old = new)
  ) 

end