module Sample01 where

import Language.TCC
import Foreign.C.String (withCString)

myProgram =
  "int fib(int n)\n"
  ++ "{\n"
  ++ "    if (n <= 2)\n"
  ++ "        return 1;\n"
  ++ "    else\n"
  ++ "        return fib(n-1) + fib(n-2);\n"
  ++ "}\n"
  ++ "\n"
  ++ "int foo(int n)\n"
  ++ "{\n"
  ++ "    printf(\"Hello World!\\n\");\n"
  ++ "    printf(\"fib(%d) = %d\\n\", n, fib(n));\n"
  ++ "    /* printf(\"add(%d, %d) = %d\\n\", n, 2 * n, add(n, 2 * n)); */\n"
  ++ "    return 0;\n"
  ++ "}\n"

main = do
  s <- c_new
--    if (!s) {
--        fprintf(stderr, "Could not create tcc state\n");
--        exit(1);
--    }

-- MUST BE CALLED before any compilation or file loading
  c_set_output_type s tcc_OUTPUT_MEMORY

  withCString myProgram (c_compile_string s)
    
  c_relocate s

  Just addr <- getSymbol s "foo"
  c_calli addr 5

  c_delete s


