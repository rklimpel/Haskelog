module Test.Test (
  runAllTests,
  prettyTest,
  substTest,
  unifyTest,
  sldTest,
  searchStrategyTest)
  where

import Test.Samples
import Test.TestViews

import Utils.StringUtils


runAllTests :: IO()
runAllTests = do
  prettyTest
  substTest
  unifyTest
  sldTest
  searchStrategyTest

  showSubtitle "ALL Tests done."


prettyTest :: IO()
prettyTest = do

  showTitle "prettyTest"

  -- pretty Test Variable
  showSubtitle "Variables"

  showPrettyTest var1
  showPrettyTest var2
  showPrettyTest var3
  showPrettyTest var4
  showPrettyTest var5

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
  showPrettyTest term14

  putStrLn seperator

   -- pretty Test Subst
  showSubtitle "Substitutions"

  showPrettyTest sub1
  showPrettyTest sub3
  showPrettyTest sub4
  showPrettyTest sub5

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
  showPrettyTest goalD

  putStrLn seperator

   -- pretty Test Program
  showSubtitle "Programms"

  showPrettyTest progA
  showPrettyTest progB
  showPrettyTest progC
  showPrettyTest progD

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

sldTest :: IO()
sldTest = do

  showTitle "sld Test"

  showSldTest progA goalA
  showSldTest progB goalB
  showSldTest progC goalC
  showSldTest progD goalD
  showSldTest progE goalE
  showSldTest progF goalF
  showSldTest progB goalB2
  -- endless tree building... : showSldTest progVorstand goalVorstand

searchStrategyTest :: IO()
searchStrategyTest = do

  showTitle "Search Strategy Test"

  showSubtitle "dfs - Tiefensuche"

  showDFSSearchTest progF goalF
  -- showDFSSearchTest progE goalE
  showDFSSearchTest progB goalB2

  showSubtitle "bfs - Breitensuche"

  showBFSSearchTest progF goalF
  -- showBFSSearchTest progE goalE
  showBFSSearchTest progB goalB2

