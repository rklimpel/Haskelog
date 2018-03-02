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

  showPrettyTest var1
  showPrettyTest var2

  putStrLn seperator

  -- pretty Test Term
  putStr "\n::Terms\n"

  showPrettyTest term1
  showPrettyTest term2
  showPrettyTest term3
  showPrettyTest term6
  showPrettyTest term7
  showPrettyTest term8
  showPrettyTest term9
  showPrettyTest term10
  showPrettyTest term105
  -- showTermTest term11

  putStrLn seperator

   -- pretty Test Subst
  putStr "\n::Substitutions\n"

  showPrettyTest sub1
  showPrettyTest sub3
  showPrettyTest sub4

  putStrLn seperator

  -- pretty Test Rule
  putStr "\n::Rules\n"

  showPrettyTest ruleA1
  showPrettyTest ruleB2
  showPrettyTest ruleC2
  showPrettyTest ruleC3

  putStrLn seperator

  -- pretty Test Goal
  putStr "\n::Goals\n"

  showPrettyTest goalA
  showPrettyTest goalB
  showPrettyTest goalC

  putStrLn seperator

   -- pretty Test Program
  putStr "\n::Progs"

  showPrettyTest progA
  showPrettyTest progB
  showPrettyTest progC

  putStrLn seperator


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

  showUnifyTest term12 term13
  showUnifyTest term13 term12



  
