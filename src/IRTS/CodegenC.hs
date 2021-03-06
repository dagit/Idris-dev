module IRTS.CodegenC where

import Idris.AbsSyntax
import IRTS.Bytecode
import IRTS.Lang
import IRTS.Simplified
import Core.TT
import Paths_idris
import Util.System

import Data.Char
import System.Process
import System.Exit
import System.IO
import System.Directory
import Control.Monad

data DbgLevel = NONE | DEBUG | TRACE

codegenC :: Maybe FilePath -> -- dump output
            [(Name, SDecl)] ->
            String -> -- output file name
            Bool ->   -- generate executable if True, only .o if False 
            [FilePath] -> -- include files
            String -> -- extra object files 
            String -> -- extra compiler flags
            DbgLevel ->
            IO ()
codegenC dump defs out exec incs objs libs dbg
    = do -- print defs
         let bc = map toBC defs
         let h = concatMap toDecl (map fst bc)
         let cc = concatMap (uncurry toC) bc
         d <- getDataDir
         mprog <- readFile (d ++ "/rts/idris_main.c")
         let cout = headers incs ++ debug dbg ++ h ++ cc ++ 
                     (if exec then mprog else "")
         (tmpn, tmph) <- tempfile
         hPutStr tmph cout
         hFlush tmph
         hClose tmph
         let useclang = False
         comp <- getCC
         let gcc = comp ++ " -I. " ++ objs ++ " -x c " ++ 
                     (if exec then "" else " - c ") ++
                     gccDbg dbg ++
                     " " ++ tmpn ++
                     " `idris --link` `idris --include` " ++ libs ++
                     " -o " ++ out
         case dump of
            Just co -> do writeFile co cout
            Nothing -> return ()
         exit <- system gcc
         when (exit /= ExitSuccess) $
             putStrLn ("FAILURE: " ++ gcc)

headers [] = "#include <idris_rts.h>\n#include <idris_stdfgn.h>\n#include <assert.h>\n"
headers (x : xs) = "#include <" ++ x ++ ">\n" ++ headers xs

debug TRACE = "#define IDRIS_TRACE\n\n"
debug _ = ""

gccDbg DEBUG = "-g"
gccDbg TRACE = "-O2"
gccDbg _ = "-O2"

cname :: Name -> String
cname n = "_idris_" ++ concatMap cchar (show n)
  where cchar x | isAlpha x || isDigit x = [x]
                | otherwise = "_" ++ show (fromEnum x) ++ "_"

indent i = take (i * 4) (repeat ' ')

creg RVal = "RVAL"
creg (L i) = "LOC(" ++ show i ++ ")"
creg (T i) = "TOP(" ++ show i ++ ")"
creg Tmp = "REG1"

toDecl :: Name -> String
toDecl f = "void " ++ cname f ++ "(VM*, VAL*);\n" 

toC :: Name -> [BC] -> String
toC f code 
    = -- "/* " ++ show code ++ "*/\n\n" ++ 
      "void " ++ cname f ++ "(VM* vm, VAL* oldbase) {\n" ++
                 indent 1 ++ "INITFRAME;\n" ++ 
                 concatMap (bcc 1) code ++ "}\n\n"

bcc :: Int -> BC -> String
bcc i (ASSIGN l r) = indent i ++ creg l ++ " = " ++ creg r ++ ";\n"
bcc i (ASSIGNCONST l c) 
    = indent i ++ creg l ++ " = " ++ mkConst c ++ ";\n"
  where
    mkConst (I i) = "MKINT(" ++ show i ++ ")"
    mkConst (BI i) | i < (2^30) = "MKINT(" ++ show i ++ ")"
                   | otherwise = "MKBIGC(vm,\"" ++ show i ++ "\")"
    mkConst (Fl f) = "MKFLOAT(vm, " ++ show f ++ ")"
    mkConst (Ch c) = "MKINT(" ++ show (fromEnum c) ++ ")"
    mkConst (Str s) = "MKSTR(vm, " ++ show s ++ ")"
    mkConst _ = "MKINT(42424242)"
bcc i (MKCON l tag args)
    = indent i ++ creg Tmp ++ " = allocCon(vm, " ++ show (length args) ++ 
         "); " ++ "SETTAG(" ++ creg Tmp ++ ", " ++ show tag ++ ");\n" ++
      indent i ++ setArgs 0 args ++ "\n" ++ 
      indent i ++ creg l ++ " = " ++ creg Tmp ++ ";\n"
         
--         "MKCON(vm, " ++ creg l ++ ", " ++ show tag ++ ", " ++
--         show (length args) ++ concatMap showArg args ++ ");\n"
  where showArg r = ", " ++ creg r
        setArgs i [] = ""
        setArgs i (x : xs) = "SETARG(" ++ creg Tmp ++ ", " ++ show i ++ ", " ++ creg x ++
                             "); " ++ setArgs (i + 1) xs

bcc i (PROJECT l loc a) = indent i ++ "PROJECT(vm, " ++ creg l ++ ", " ++ show loc ++ 
                                      ", " ++ show a ++ ");\n"
bcc i (PROJECTINTO r t idx)
    = indent i ++ creg r ++ " = GETARG(" ++ creg t ++ ", " ++ show idx ++ ");\n" 
bcc i (CASE r code def) 
    = indent i ++ "switch(TAG(" ++ creg r ++ ")) {\n" ++
      concatMap (showCase i) code ++
      showDef i def ++
      indent i ++ "}\n"
  where
    showCase i (t, bc) = indent i ++ "case " ++ show t ++ ":\n"
                         ++ concatMap (bcc (i+1)) bc ++ indent (i + 1) ++ "break;\n"
    showDef i Nothing = ""
    showDef i (Just c) = indent i ++ "default:\n" 
                         ++ concatMap (bcc (i+1)) c ++ indent (i + 1) ++ "break;\n"
bcc i (CONSTCASE r code def) 
    = indent i ++ "switch(GETINT(" ++ creg r ++ ")) {\n" ++
      concatMap (showCase i) code ++
      showDef i def ++
      indent i ++ "}\n"
  where
    showCase i (t, bc) = indent i ++ "case " ++ show t ++ ":\n"
                         ++ concatMap (bcc (i+1)) bc ++ indent (i + 1) ++ "break;\n"
    showDef i Nothing = ""
    showDef i (Just c) = indent i ++ "default:\n" 
                         ++ concatMap (bcc (i+1)) c ++ indent (i + 1) ++ "break;\n"
bcc i (CALL n) = indent i ++ "CALL(" ++ cname n ++ ");\n"
bcc i (TAILCALL n) = indent i ++ "TAILCALL(" ++ cname n ++ ");\n"
bcc i (SLIDE n) = indent i ++ "SLIDE(vm, " ++ show n ++ ");\n"
bcc i REBASE = indent i ++ "REBASE;\n"
bcc i (RESERVE n) = indent i ++ "RESERVE(" ++ show n ++ ");\n"
bcc i (ADDTOP n) = indent i ++ "ADDTOP(" ++ show n ++ ");\n"
bcc i (TOPBASE n) = indent i ++ "TOPBASE(" ++ show n ++ ");\n"
bcc i (BASETOP n) = indent i ++ "BASETOP(" ++ show n ++ ");\n"
bcc i STOREOLD = indent i ++ "STOREOLD;\n"
bcc i (OP l fn args) = indent i ++ doOp (creg l ++ " = ") fn args ++ ";\n"
bcc i (FOREIGNCALL l LANG_C rty fn args)
      = indent i ++ 
        c_irts rty (creg l ++ " = ") 
                   (fn ++ "(" ++ showSep "," (map fcall args) ++ ")") ++ ";\n"
    where fcall (t, arg) = irts_c t (creg arg)
bcc i (NULL r) = indent i ++ creg r ++ " = NULL;\n" -- clear, so it'll be GCed
bcc i (ERROR str) = indent i ++ "fprintf(stderr, " ++ show str ++ "); assert(0); exit(-1);"
-- bcc i _ = indent i ++ "// not done yet\n"

c_irts FInt l x = l ++ "MKINT((i_int)(" ++ x ++ "))"
c_irts FChar l x = l ++ "MKINT((i_int)(" ++ x ++ "))"
c_irts FString l x = l ++ "MKSTR(vm, " ++ x ++ ")"
c_irts FUnit l x = x
c_irts FPtr l x = l ++ "MKPTR(vm, " ++ x ++ ")"
c_irts FDouble l x = l ++ "MKFLOAT(vm, " ++ x ++ ")"
c_irts FAny l x = l ++ x

irts_c FInt x = "GETINT(" ++ x ++ ")"
irts_c FChar x = "GETINT(" ++ x ++ ")"
irts_c FString x = "GETSTR(" ++ x ++ ")"
irts_c FUnit x = x
irts_c FPtr x = "GETPTR(" ++ x ++ ")"
irts_c FDouble x = "GETFLOAT(" ++ x ++ ")"
irts_c FAny x = x

doOp v LPlus [l, r] = v ++ "ADD(" ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LMinus [l, r] = v ++ "INTOP(-," ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LTimes [l, r] = v ++ "MULT(" ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LDiv [l, r] = v ++ "INTOP(/," ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LEq [l, r] = v ++ "INTOP(==," ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LLt [l, r] = v ++ "INTOP(<," ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LLe [l, r] = v ++ "INTOP(<=," ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LGt [l, r] = v ++ "INTOP(>," ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LGe [l, r] = v ++ "INTOP(>=," ++ creg l ++ ", " ++ creg r ++ ")"

doOp v LFPlus [l, r] = v ++ "FLOATOP(+," ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LFMinus [l, r] = v ++ "FLOATOP(-," ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LFTimes [l, r] = v ++ "FLOATOP(*," ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LFDiv [l, r] = v ++ "FLOATOP(/," ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LFEq [l, r] = v ++ "FLOATBOP(==," ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LFLt [l, r] = v ++ "FLOATBOP(<," ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LFLe [l, r] = v ++ "FLOATBOP(<=," ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LFGt [l, r] = v ++ "FLOATBOP(>," ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LFGe [l, r] = v ++ "FLOATBOP(>=," ++ creg l ++ ", " ++ creg r ++ ")"

doOp v LBPlus [l, r] = v ++ "idris_bigPlus(vm, " ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LBMinus [l, r] = v ++ "idris_bigMinus(vm, " ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LBTimes [l, r] = v ++ "idris_bigTimes(vm, " ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LBDiv [l, r] = v ++ "idris_bigDivide(vm, " ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LBEq [l, r] = v ++ "idris_bigEq(vm, " ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LBLt [l, r] = v ++ "idris_bigLt(vm, " ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LBLe [l, r] = v ++ "idris_bigLe(vm, " ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LBGt [l, r] = v ++ "idris_bigGt(vm, " ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LBGe [l, r] = v ++ "idris_bigGe(vm, " ++ creg l ++ ", " ++ creg r ++ ")"

doOp v LStrConcat [l,r] = v ++ "idris_concat(vm, " ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LStrLt [l,r] = v ++ "idris_strlt(vm, " ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LStrEq [l,r] = v ++ "idris_streq(vm, " ++ creg l ++ ", " ++ creg r ++ ")"
doOp v LStrLen [x] = v ++ "idris_strlen(vm, " ++ creg x ++ ")"

doOp v LIntFloat [x] = v ++ "idris_castIntFloat(" ++ creg x ++ ")"
doOp v LFloatInt [x] = v ++ "idris_castFloatInt(" ++ creg x ++ ")"
doOp v LIntStr [x] = v ++ "idris_castIntStr(vm, " ++ creg x ++ ")"
doOp v LStrInt [x] = v ++ "idris_castStrInt(vm, " ++ creg x ++ ")"
doOp v LIntBig [x] = v ++ "idris_castIntBig(vm, " ++ creg x ++ ")"
doOp v LBigInt [x] = v ++ "idris_castBigInt(vm, " ++ creg x ++ ")"
doOp v LStrBig [x] = v ++ "idris_castStrBig(vm, " ++ creg x ++ ")"
doOp v LBigStr [x] = v ++ "idris_castBigStr(vm, " ++ creg x ++ ")"
doOp v LFloatStr [x] = v ++ "idris_castFloatStr(vm, " ++ creg x ++ ")"
doOp v LStrFloat [x] = v ++ "idris_castStrFloat(vm, " ++ creg x ++ ")"

doOp v LReadStr [x] = v ++ "idris_readStr(vm, GETPTR(" ++ creg x ++ "))"
doOp _ LPrintNum [x] = "printf(\"%ld\\n\", GETINT(" ++ creg x ++ "))"
doOp _ LPrintStr [x] = "fputs(GETSTR(" ++ creg x ++ "), stdout)"

doOp v LFExp [x] = v ++ "MKFLOAT(exp(GETFLOAT(" ++ creg x ++ ")))"
doOp v LFLog [x] = v ++ "MKFLOAT(log(GETFLOAT(" ++ creg x ++ ")))"
doOp v LFSin [x] = v ++ "MKFLOAT(sin(GETFLOAT(" ++ creg x ++ ")))"
doOp v LFCos [x] = v ++ "MKFLOAT(cos(GETFLOAT(" ++ creg x ++ ")))"
doOp v LFTan [x] = v ++ "MKFLOAT(tan(GETFLOAT(" ++ creg x ++ ")))"
doOp v LFASin [x] = v ++ "MKFLOAT(asin(GETFLOAT(" ++ creg x ++ ")))"
doOp v LFACos [x] = v ++ "MKFLOAT(acos(GETFLOAT(" ++ creg x ++ ")))"
doOp v LFATan [x] = v ++ "MKFLOAT(atan(GETFLOAT(" ++ creg x ++ ")))"
doOp v LFSqrt [x] = v ++ "MKFLOAT(floor(GETFLOAT(" ++ creg x ++ ")))"
doOp v LFFloor [x] = v ++ "MKFLOAT(ceil(GETFLOAT(" ++ creg x ++ ")))"
doOp v LFCeil [x] = v ++ "MKFLOAT(sqrt(GETFLOAT(" ++ creg x ++ ")))"

doOp v LStrHead [x] = v ++ "idris_strHead(vm, " ++ creg x ++ ")"
doOp v LStrTail [x] = v ++ "idris_strTail(vm, " ++ creg x ++ ")"
doOp v LStrCons [x, y] = v ++ "idris_strCons(vm, " ++ creg x ++ "," ++ creg y ++ ")"
doOp v LStrIndex [x, y] = v ++ "idris_strIndex(vm, " ++ creg x ++ "," ++ creg y ++ ")"
doOp v LStrRev [x] = v ++ "idris_strRev(vm, " ++ creg x ++ ")"

doOp v LStdIn [] = v ++ "MKPTR(vm, stdin)"
doOp v LStdOut [] = v ++ "MKPTR(vm, stdout)"
doOp v LStdErr [] = v ++ "MKPTR(vm, stderr)"

doOp v LFork [x] = v ++ "MKPTR(vm, vmThread(vm, " ++ cname (MN 0 "EVAL") ++ ", " ++ creg x ++ "))"
doOp v LVMPtr [] = v ++ "MKPTR(vm, vm)"
doOp v LNoOp args = ""
doOp _ _ _ = "FAIL"
