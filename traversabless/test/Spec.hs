
import qualified DBQueryPipeline
import qualified HttpQuery
import qualified RedoFunctor
import qualified RedoFold
import qualified DemoQuickCheckTraversableLaws

main :: IO ()
main = do
  DBQueryPipeline.demo
  HttpQuery.demo
  RedoFunctor.demo
  RedoFold.demo
  DemoQuickCheckTraversableLaws.demo
