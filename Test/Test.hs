module Test.Test where

import Test.Samples
import Test.TestViews
import Utils.StringUtils
import Utils.TermUtils
import SLDTree
import Type
import Sub
import Unify
import Pretty 



runAllTests :: IO()
runAllTests = do
  prettyTest
  substTest
  unifyTest



prettyTest :: IO()
prettyTest = do

  showTitle "prettyTest"

  -- pretty Test Variable
  showSubtitle "Variables"

  showPrettyTest var1
  showPrettyTest var2

  putStrLn seperator

  -- pretty Test Term
  showSubtitle "Terms"

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
  showSubtitle "Substitutions"

  showPrettyTest sub1
  showPrettyTest sub3
  showPrettyTest sub4

  putStrLn seperator

  -- pretty Test Rule
  showSubtitle "Rules"

  showPrettyTest ruleA1
  showPrettyTest ruleB2
  showPrettyTest ruleC2
  showPrettyTest ruleC3

  putStrLn seperator

  -- pretty Test Goal
  showSubtitle "Goals"

  showPrettyTest goalA
  showPrettyTest goalB
  showPrettyTest goalC

  putStrLn seperator

   -- pretty Test Program
  showSubtitle "Programms"

  showPrettyTest progA
  showPrettyTest progB
  showPrettyTest progC

  putStrLn seperator

  -- pretty Test SLDTrees
  showSubtitle "SLDTrees"

  showPrettyTest sld1
  showPrettyTest sld2
  showPrettyTest sld3

  putStrLn seperator


substTest :: IO()
substTest = do

  showTitle "substTest"

  -- subst Test Apply
  showSubtitle "apply Substitutions"

  showApplySubstTest sub1 term1
  showApplySubstTest sub1 term4

  putStrLn seperator

  -- Subst Test Compose
  showSubtitle "compose Substitutions"

  showComposeSubstTest sub1 sub2
  showComposeSubstTest sub1 sub3
  showComposeSubstTest sub3 sub4

  putStrLn seperator



unifyTest :: IO()
unifyTest = do

  showTitle "unifyTest"

  -- unify Test ds
  showSubtitle "ds Terms -> get disagreement set"

  showDsTest term1 term2
  showDsTest term2 term3
  showDsTest term3 term3
  showDsTest term4 term5

  putStrLn seperator

  -- unify Test unify
  showSubtitle "unify Terms"

  showUnifyTest term1 term2
  showUnifyTest term2 term3
  showUnifyTest term1 term4
  showUnifyTest term1 term5

  showUnifyTest term12 term13
  showUnifyTest term13 term12

  putStrLn seperator


  
