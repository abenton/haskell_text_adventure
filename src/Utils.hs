module Utils where
import Types2

-- | Contains helper methods for the rest of the engine.

-- | ToString a container's inventory.
inventoryStr :: (Container c) => c -> String
inventoryStr c = intercalate "\n" strs where
  names = fmap show (contains c)
  strs  = fmap strItem (nub names) where
    strItem n = "X" ++ show (length $ filter (==n) names) ++ " " ++ n

-- | ToString the doors leading out of a room.
exitStr :: Room -> String

