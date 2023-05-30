import DDLewd
import Test.Hspec
import DDHC.Scanner.Spec.Construction

main :: IO ()
main = hspec $ 
    describe "DDHC Scanner" $
           satisfy
        >> char
        >> string
        >> oneOf
        >> choice
        >> follow
        >> position
