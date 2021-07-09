import           Cont

type Checkpointed a = (a -> Cont a a) -> Cont a a

addTens :: Int -> Checkpointed Int
addTens x1 =
  \checkpoint -> do
    checkpoint x1
    let x2 = x1 + 10
    checkpoint x2
    let x3 = x2 + 10
    checkpoint x3
    let x4 = x3 + 10
    return x4

runCheckpointed :: (a -> Bool) -> Checkpointed a -> a
runCheckpointed pred ch =
  evalCont $
  ch $ \a -> do
    cont $ \c ->
      if (pred (c a))
        then c a
        else a
        -- это на самом деле пиздец очевидно
        -- да, предыдущее значение забывается
        -- но в том и дело, что это последовательное suspended вычисление
        -- и каждый новый раз подается x_i в вычисление, а потом забывается
        -- и оно в тот момент становится suspended
        -- и к нему мы применяем функции
        -- \c -> c a
