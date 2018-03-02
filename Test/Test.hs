module Test.Test where

import Test.Samples
import Test.TestViews
import Utils.StringUtils
import SLDTree
import Type


runAllTests :: IO()
runAllTests = do
  prettyTest
  substTest
  unifyTest



prettyTest :: IO()
prettyTest = do

  showTitle "prettyTest"

  -- pretty Test Variable
  putStr "\n::Variables\n"

  showVarTest var1
  showVarTest var2

  putStrLn seperator

  -- pretty Test Term
  putStr "\n::Terms\n"

  showTermTest term1
  showTermTest term2
  showTermTest term3
  showTermTest term6
  showTermTest term7
  showTermTest term8
  showTermTest term9
  showTermTest term10
  showTermTest term105
  showTermTest term11

  putStrLn seperator

   -- pretty Test Subst
  putStr "\n::Substitutions\n"

  showSubstTest sub1
  showSubstTest sub3
  showSubstTest sub4

  putStrLn seperator

  -- pretty Test Rule
  -- ...

  -- pretty Test Program
  -- ...

  -- pretty Test Goal
  -- ...



substTest :: IO()
substTest = do

  showTitle "substTest"

  -- subst Test Apply
  putStr "\n::Apply Substitution\n"

  showApplySubstTest sub1 term1
  showApplySubstTest sub1 term4

  -- Subst Test Compose
  putStr "\n::Compose Substitutions\n"

  showComposeSubstTest sub1 sub2
  showComposeSubstTest sub1 sub3
  showComposeSubstTest sub3 sub4



unifyTest :: IO()
unifyTest = do

  showTitle "unifyTest"

  -- unify Test ds
  putStr "\n::ds Terms -> get disagreement set\n"

  showDsTest term1 term2
  showDsTest term2 term3
  showDsTest term3 term3
  showDsTest term4 term5

  -- unify Test unify
  putStr "\n::unify Terms\n"

  showUnifyTest term1 term2
  showUnifyTest term2 term3
  showUnifyTest term1 term4
  showUnifyTest term1 term5
<<<<<<< HEAD
=======
  showUnifyTest term8 term9
  showUnifyTest term9 term8



  
>>>>>>> 0831784caaedef6c01245172fbecdd2fc13ac00b
