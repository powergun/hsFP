
| Action | Values | Link  |
| --- | --- | ---  |
| get, modify | extactly 1 value | [Lens](#lens) |
| get, modify, filtering | 0 to multiple values |  [Traversal](#traversal) |
| get, modify | 0 or 1 value | [Prism](#prism) or [Traversal](#traversal) |

All code assumes `import DA.ASX.Main.Common.Lens`

## Lens
> `Lens` is an `Optic` focused on a single value. An `Action` performed with a `Lens` applies only to that single value.


```Haskell
-- field lens

account ^. fieldLens @"operator"
-- ^ get `operator` from account

account ^. fieldLens @"registeredHolderAddress" . fieldLens@"townName"
-- ^ get `townName` under `registeredHolderAddress` from account

account & fieldLens @"operator" .~ newValue
-- ^ set `operator` on account

account & fieldLens @"registeredHolderAddress" . fieldLens@"townName" .~ newValue
-- ^ set `townName` under `registeredHolderAddress` from account

account & fieldLens @"operator" %~ (\_ -> newValue)
-- ^ modify `operator` on account

account & fieldLens @"registeredHolderAddress" . fieldLens@"townName" %~ \townName -> townName <> " NSW"
-- ^ modify `townName` under `registeredHolderAddress` from account


-- tuple

(1, 2) ^. _1
-- ^ 1

(1, account) ^. _2 . fieldLens @"operator"
-- ^ same as "(.operator) . snd"

(1, 2) & _1 %~ (+1)
-- ^ (2, 2)

(1, 2) & _1 .~ 3
-- ^ (3, 2)

```

## Traversal
> `Traversal` is an `Optic` focuses on zero to multiple values. `Action` performed with `Traversal` applies to the values focused.

```Haskell
[[1, 2], [3, 4]] ^.. traversed . traversed
-- ^ get the elements of the second layer list.
-- [1, 2, 3, 4]
```
| `Data`&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  |   `Action`   |  `Optic` | `Notes` |
| ------- | ------------ | --------  | ------  |
| `[[1, 2], [3, 4]]` | `^..` or `toListOf`. This concates the focuses into a list | `traversed . traversed` | the first `traversed` focuses on the outer list elements namely `[1, 2]` and `[3, 4]`. The second `traversed` focuses on the elements namely `1`, `2`, `3` and `4` of these 2 lists. By using `^..`(`toListOf`) as an `Action`, the final focuses are concated together to form a list, hence `[1, 2, 3, 4]` |

```Haskell
-- works with many values

( [ [ 1, 2 ], [ 3, 4 ] ], 2 ) ^.. _1 . traversed . traversed
-- ^ combine with lens `_1`
-- [1, 2, 3, 4]

( [ [ 1, 2 ], [ 3, 4 ] ], 2 ) & _1 . traversed . traversed %~ (+ 1)
-- add 1 to the focuses (second layer list elements inside the first element of a tuple)
-- ( [ [ 2, 3 ], [ 4, 5 ] ], 2 )

( [ [ 1, 2 ], [ 3, 4 ] ], 2 ) & _1 . traversed . traversed .~ 5
-- set the focuses to 5s
-- ( [ [ 5, 5 ], [ 5, 5 ] ], 2 )

( [ [ 1, 2 ], [ 3, 4 ] ], 2 ) & _1 . traversed . traversed . filtered (> 1) .~ 0
-- set the focuses that are larger than 1 to 0
-- ( [ [ 1, 0 ], [ 0, 0 ] ], 2 )


-- works with 0 or 1 value

(Left 2 ^? traversed) : Optional Int
-- ^ get value of type `a` out of `Either Int a`
-- None

(Right 2 ^? traversed) : Optional Int
-- ^ get value of type `Int` out of `Either b Int `
-- Some 2

Left 2 & traversed .~ 3
-- ^ `traversed` picks the `Right` branch thus `3` is not set inside `Left`
-- same as `if isRight (Left 2) then Right 3 else Left 2
-- Left 2

Right 2 & traversed .~ 3
-- ^ Right 3

-- more complex examples

newHolding : Holding.Model
newHolding =
    holding & fieldLens @"locked"   -- ^ `: [Lock]`. focus on holding.locked
            . traversed             -- ^ focus on each element of locked
            . filtered isAdminLock  -- ^ focus on only adminLock
            . fieldLens @"quantity" -- ^ focus on all the adminLock quantity fields
            %~ increaseBy100        -- ^ increase each quantity field by 100 unit

newHolding' : STX.Transaction Holding.Model
newHolding' =
    ( holding & fieldLens @"locked"
              . traversed
              . filtered isAdminLock
              %%~ \locked -> do -- ^ instead of `%~`(`modify`), using `%%~` allows a effectful modification
                     ensureFutureLock locked
                     -- ^ `: STX.Transaction ()`. `ensureFutureLock` is effectful. It fetches `AdminLock` model to ensure it's a future lock
                     pure $ locked & fieldLens @"quantity"
                                   %~ increaseBy100
    )
    >>= persistence.save
```

## Prism
> `Prism` is an `Optic` focuses on zero or exact one value. `Action` performed with `Prism` applies to the value focused.

```Haskell
Left 2 & _Left .~ 3
-- ^ `_Left` picks the `Left` branch thus 3 is set inside `Left`
-- Left 3

Left 2 & _Right .~ 3
-- ^ Left 2

data CommunicationPreference = Email | Post deriving (Show, Eq)
preference = Email
emailP = prism' embed match -- create a prism picks(focuses on) the `Email` constructor
  where
    -- given a value of type `CommunicationPreference`, how do we match on `Email` constructor
    match Email = Some Email
    match _ = None
    -- how do we generate value of type `CommunicationPreference` that will always be picked by this prism
    embed _ = Email

preference ^? emailP
-- ^ Get the communication preference `Email`
-- equivelant to `if preference == Email then Some Email else None`
-- Some Email

preference' = Post
preference ^? emailP
-- ^ Get the communication preference `Email`
-- None

-- account = Account with { ...; communicationPreference = Some Email; ... }
account ^? fieldLens @"communicationPreference" . traversed . emailP
-- ^ use field lens, traversal and prism together
-- `fieldLens @"communicationPreference"` focuses on `communicationPreference` field
-- `traversed` focuses on the `Some` branch
-- `emailP` focuses on the `Email` constructor
-- Some Email

-- account = Account with { ...; communicationPreference = None; ... }
account & fieldLens @"communicationPreference" . _Some .~ Email
-- ^ `_Some` picks/matches/focuses on the `Some` constructor when setting `Email`. But it's `None` here. Thus `Email` cannot be set. `communicationPreference` field will stay `None`.
-- account = Account with { ...; communicationPreference = None; ... }
```
