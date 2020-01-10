
import qualified DBQueryPipeline
import qualified HttpQuery
import qualified RedoFunctor
import qualified RedoFold

main :: IO ()
main = do
  DBQueryPipeline.demo
  HttpQuery.demo
  RedoFunctor.demo
  RedoFold.demo
