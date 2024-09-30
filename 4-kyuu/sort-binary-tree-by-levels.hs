module TreeByLevels where
import TreeByLevels.TreeNode
import qualified Data.Sequence as Seq
import Data.Maybe (catMaybes)

treeByLevels :: Maybe (TreeNode a) -> [a]
treeByLevels Nothing = []
treeByLevels (Just n) = bfs (Seq.singleton n)
  where bfs (Seq.Empty) = []
        bfs (n Seq.:<| rest) =
          let children = Seq.fromList . catMaybes $ [left n, right n] 
          in value n : bfs (rest Seq.>< children)
