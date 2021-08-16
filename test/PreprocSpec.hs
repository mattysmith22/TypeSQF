module PreprocSpec (preprocSpec) where

import Test.Hspec
import Preproc
import Text.Megaparsec

streamStartLineMacro :: PreprocStream 
streamStartLineMacro = buildPreprocStream "#line 0 fileName\ntesting\nfile"

streamNormal :: PreprocStream 
streamNormal = buildPreprocStream "testing\nfile"

streamNewlines :: PreprocStream 
streamNewlines = buildPreprocStream "\ntesting\nfile"

streamMultipleStartLineMacro :: PreprocStream 
streamMultipleStartLineMacro = buildPreprocStream "#line 0 fileName\n#line 7 filename2\ntesting\nfile"

streamEmpty :: PreprocStream 
streamEmpty = buildPreprocStream ""

streamEmptyMacro :: PreprocStream 
streamEmptyMacro = buildPreprocStream "#line 0 fileName\n#line 7 filename2\n"

streamMidMacro :: PreprocStream 
streamMidMacro = buildPreprocStream "#line 0 fileName\ntest\n#line 7 filename2\ning\nfile"

take1_Spec :: Spec 
take1_Spec = describe "take1_" $ do
    it "Should be able to take a normal character" $ do
        take1_ streamNormal `shouldBe` Just ('t', PreprocStream "esting\nfile" False)
    it "Should be able to take a character after a #line directive" $ do
        take1_ streamStartLineMacro `shouldBe` Just ('t', PreprocStream "esting\nfile" False)
        take1_ streamMultipleStartLineMacro `shouldBe` Just ('t', PreprocStream "esting\nfile" False)
    it "Should be able to take a newline" $ do
        take1_ streamNewlines `shouldBe` Just ('\n', PreprocStream "testing\nfile" True)
    it "Should not be able to take from an empty string" $ do
        take1_ streamEmpty`shouldBe` Nothing 
        take1_ streamEmptyMacro `shouldBe` Nothing

{-# ANN takeN_Spec "HLint: Use camelCase" #-}
takeN_Spec :: Spec 
takeN_Spec = describe "takeN_" $ do
    it "Should always be able to parse a number of tokens <= 0" $ do
        takeN_ (-1) streamNormal `shouldBe` Just ("", streamNormal)
        takeN_ (0) streamNormal `shouldBe` Just ("", streamNormal)
        takeN_ (-1) streamEmpty `shouldBe` Just ("", streamEmpty)
        takeN_ (0) streamEmpty `shouldBe` Just ("", streamEmpty)
    it "Should fail to take from an empty string if n > 0" $ do
        takeN_ 1 streamEmpty `shouldBe` Nothing
        takeN_ 1 streamEmptyMacro `shouldBe` Nothing
    it "Should be able to take normal characters" $ do
        takeN_ 1 streamNormal `shouldBe` Just ("t", PreprocStream "esting\nfile" False)
        takeN_ 4 streamNormal `shouldBe` Just ("test", PreprocStream "ing\nfile" False)
    it "Should be able to handle line directives" $ do
        takeN_ 1 streamStartLineMacro `shouldBe` Just ("t", PreprocStream "esting\nfile" False)
        takeN_ 4 streamStartLineMacro `shouldBe` Just ("test", PreprocStream "ing\nfile" False)
        takeN_ 1 streamMultipleStartLineMacro `shouldBe` Just ("t", PreprocStream "esting\nfile" False)
        takeN_ 4 streamMultipleStartLineMacro `shouldBe` Just ("test", PreprocStream "ing\nfile" False)
    it "Should be able to take characters through line directives" $ do
        takeN_ 1 streamNormal `shouldBe` Just ("t", PreprocStream "esting\nfile" False)
    it "Should be able to take characters through line directives" $ do
        takeN_ 8 streamMidMacro `shouldBe` Just ("test\ning", PreprocStream "\nfile" False)

takeWhile_Spec :: Spec 
takeWhile_Spec = describe "takeWhile_" $ do
    it "Should be able to correctly parse an empty stream" $ do
        takeWhile_ (const True) streamEmpty `shouldBe` ("", streamEmpty)
        takeWhile_ (const True) streamEmptyMacro `shouldBe` ("", streamEmpty)
    it "Should be able to correctly parse all items in a stream" $ do
        takeWhile_ (const True) streamStartLineMacro `shouldBe` ("testing\nfile", PreprocStream "" False)
        takeWhile_ (const True) streamNormal `shouldBe` ("testing\nfile", PreprocStream "" False)
        takeWhile_ (const True) streamNewlines `shouldBe` ("\ntesting\nfile", PreprocStream "" False)
        takeWhile_ (const True) streamMidMacro `shouldBe` ("test\ning\nfile", PreprocStream "" False)
    it "Should listen to the predicate passed in" $ do
        takeWhile_ (`elem` "tes") streamNormal `shouldBe` ("test", PreprocStream "ing\nfile" False)

genSourcePos :: FilePath -> Int -> Int -> SourcePos
genSourcePos file l c = SourcePos file (mkPos l) (mkPos c)

genPosState :: PreprocStream -> PosState PreprocStream
genPosState stream = PosState {
    pstateInput = stream,
    pstateOffset = 0,
    pstateSourcePos = genSourcePos "" 1 1,
    pstateTabWidth = mkPos 4,
    pstateLinePrefix = ""
}

reachOffsetSpec :: Spec 
reachOffsetSpec = describe "reachOffset" $ do
    it "Should be able to correctly parse an empty stream" $ do
        reachOffset 1 (genPosState streamEmpty) `shouldBe` (Just "<empty line>", (genPosState streamEmpty){pstateOffset = 1})
        reachOffset 1 (genPosState streamEmptyMacro) `shouldBe` (Just "<empty line>", (genPosState streamEmpty){
            pstateOffset = 1,
            pstateSourcePos = genSourcePos "filename2" 8 1
        })
    it "Should be able to correctly reach a normal offset" $ do
        reachOffset 2 (genPosState streamNormal) `shouldBe` (Just "testing", (genPosState streamNormal){
            pstateOffset = 2,
            pstateInput = PreprocStream "sting\nfile" False,
            pstateSourcePos = genSourcePos "" 1 3,
            pstateLinePrefix = "te"
        })
    it "Should be able to correctly reach a normal offset on the other side of newline" $ do
        reachOffset 9 (genPosState streamNormal) `shouldBe` (Just "file", (genPosState streamNormal){
            pstateOffset = 9,
            pstateInput = PreprocStream "ile" False,
            pstateSourcePos = genSourcePos "" 2 2,
            pstateLinePrefix = "f"
        })
    it "Should be able to correctly handle a line directive" $ do
        reachOffset 3 (genPosState streamStartLineMacro) `shouldBe` (Just "testing", (genPosState streamStartLineMacro){
            pstateOffset = 3,
            pstateInput = PreprocStream "ting\nfile" False,
            pstateSourcePos = genSourcePos "fileName" 1 4,
            pstateLinePrefix = "tes"
        })

reachOffsetNoLineSpec :: Spec 
reachOffsetNoLineSpec = describe "reachOffsetNoLine" $ do
    it "Should be able to correctly parse an empty stream" $ do
        reachOffsetNoLine 1 (genPosState streamEmpty) `shouldBe` (genPosState streamEmpty){pstateOffset = 1}
        reachOffsetNoLine 1 (genPosState streamEmptyMacro) `shouldBe` (genPosState streamEmpty){
            pstateOffset = 1,
            pstateSourcePos = genSourcePos "filename2" 8 1
        }
    it "Should be able to correctly reach a normal offset" $ do
        reachOffsetNoLine 2 (genPosState streamNormal) `shouldBe` (genPosState streamNormal){
            pstateOffset = 2,
            pstateInput = PreprocStream "sting\nfile" False,
            pstateSourcePos = genSourcePos "" 1 3
        }
    it "Should be able to correctly reach a normal offset on the other side of newline" $ do
        reachOffsetNoLine 9 (genPosState streamNormal) `shouldBe` (genPosState streamNormal){
            pstateOffset = 9,
            pstateInput = PreprocStream "ile" False,
            pstateSourcePos = genSourcePos "" 2 2
        }
    it "Should be able to correctly handle a line directive" $ do
        reachOffsetNoLine 3 (genPosState streamStartLineMacro) `shouldBe` (genPosState streamStartLineMacro){
            pstateOffset = 3,
            pstateInput = PreprocStream "ting\nfile" False,
            pstateSourcePos = genSourcePos "fileName" 1 4
        }

preprocStreamSpec :: Spec
preprocStreamSpec = describe "PreprocStream" $ do
    take1_Spec
    takeN_Spec
    takeWhile_Spec
    reachOffsetSpec
    reachOffsetNoLineSpec

preprocSpec :: Spec
preprocSpec = describe "Preproc" $ do
    preprocStreamSpec
