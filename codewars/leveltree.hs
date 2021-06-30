data TreeNode a =
  TreeNode
    { left  :: Maybe (TreeNode a)
    , right :: Maybe (TreeNode a)
    , value :: a
    }
  deriving (Show)

treeByLevels :: Maybe (TreeNode a) -> [a]
treeByLevels = fold (:) []

next Nothing                                          = []
next (Just TreeNode {left = l, right = r, value = x}) = [l, r]

val Nothing                                          = []
val (Just TreeNode {left = l, right = r, value = x}) = [x]

fold f x tree = calc [tree]
  where
    calc []    = x
    calc level = foldr f (calc (level >>= next)) (level >>= val)
