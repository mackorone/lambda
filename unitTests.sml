(*
 * The MIT License (MIT)
 * 
 * Copyright (c) 2014 Mack Ward
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *)

(* Include the functions that we're testing *)
use "lambda.sml";

(* TODO: Use the variables, not the hard coded characters for the tests *)
(* TODO: Print out the expected and the actual values here too *)

val failed_string = "XXX Failed XXX\n";
val passed_string = "--- Passed ---\n";

fun boolToString(false) = failed_string
  | boolToString(true) = passed_string
;

(* TODO: Test isIn??? *)

fun test_charIn() =
    let
        val s1 = "aaaaa";
        val s2 = "abcde";
        val s3 = "abada";
        val s4 = "";
    in (
        print("\ntest_charIn:\n");
        print("    test 1:  "^boolToString(    charIn(#"a", s1) ));
        print("    test 2:  "^boolToString(    charIn(#"a", s2) ));
        print("    test 3:  "^boolToString(    charIn(#"d", s2) ));
        print("    test 4:  "^boolToString(    charIn(#"e", s2) ));
        print("    test 5:  "^boolToString(    charIn(#"a", s3) ));
        print("    test 6:  "^boolToString(    charIn(#"b", s3) ));
        print("    test 7:  "^boolToString(not(charIn(#"b", s1))));
        print("    test 8:  "^boolToString(not(charIn(#"f", s2))));
        print("    test 9:  "^boolToString(not(charIn(#"c", s3))));
        print("    test 10: "^boolToString(not(charIn(#"a", s4))));
    ()) end
;

(*
fun test_indexOf() =
    let
        val s1 = "aaaaa";
        val s2 = "abcde";
        val s3 = "abada";
        val s4 = "";
    in (
        print("\ntest_indexOf:\n");
        print("    test 1:  "^boolToString(indexOf(#"a", s1) =  0));
        print("    test 2:  "^boolToString(indexOf(#"a", s2) =  0));
        print("    test 3:  "^boolToString(indexOf(#"d", s2) =  3));
        print("    test 4:  "^boolToString(indexOf(#"e", s2) =  4));
        print("    test 5:  "^boolToString(indexOf(#"a", s3) =  0));
        print("    test 6:  "^boolToString(indexOf(#"b", s3) =  1));
        print("    test 7:  "^boolToString(indexOf(#"b", s1) = ~1));
        print("    test 8:  "^boolToString(indexOf(#"f", s2) = ~1));
        print("    test 9:  "^boolToString(indexOf(#"c", s3) = ~1));
        print("    test 10: "^boolToString(indexOf(#"a", s4) = ~1));
    ()) end
;
*)

(* TODO: Redo some of this testing *)
fun test_indexOfClosingPar() = 
(
    print("\ntest_indexOfClosingPar:\n");
    print("    test 1:  "^boolToString(indexOfClosingPar("(abc)"           ) =  4));
    print("    test 2:  "^boolToString(indexOfClosingPar("(abc)de"         ) =  4));
    print("    test 3:  "^boolToString(indexOfClosingPar("()"              ) =  1));
    print("    test 4:  "^boolToString(indexOfClosingPar("((abc))"         ) =  6));
    print("    test 5:  "^boolToString(indexOfClosingPar("((abc)abc)"      ) =  9));
    print("    test 6:  "^boolToString(indexOfClosingPar("(123(abc)abc)"   ) = 12));
    print("    test 7:  "^boolToString(indexOfClosingPar("(123(abc))"      ) =  9));
    print("    test 8:  "^boolToString(indexOfClosingPar("(())"            ) =  3));
    print("    test 9:  "^boolToString(indexOfClosingPar("(())123"         ) =  3));
    print("    test 10: "^boolToString(indexOfClosingPar("(a()b)"          ) =  5));
    print("    test 11: "^boolToString(indexOfClosingPar("(a()b)abc"       ) =  5));
    print("    test 12: "^boolToString(indexOfClosingPar("(a(b(c(d)e)f)g)" ) = 14));
    print("    test 13: "^boolToString(indexOfClosingPar("(a(b(c(d)e)f)g)h") = 14));
    print("    test 14: "^boolToString(indexOfClosingPar("()"              ) =  1));
    print("    test 15: "^boolToString(indexOfClosingPar("(   )"           ) =  4));
    print("    test 16: "^boolToString(indexOfClosingPar("(   ) "          ) =  4));
    print("    test 17: "^boolToString(indexOfClosingPar("() ()"           ) =  1));
    print("    test 18: "^boolToString(indexOfClosingPar("(()())"          ) =  5));
    print("    test 19: "^boolToString(indexOfClosingPar("((()())) "       ) =  7));
    print("    test 20: "^boolToString(indexOfClosingPar("(()()()abc())()" ) = 12));
());

(*
fun test_indexOfLambdaEnd() =
(
    print("\ntest_indexOfLambdaEnd:\n");
    print("    test 1:  "^boolToString(indexOfLambdaEnd("L."                              ) =  1));
    print("    test 1:  "^boolToString(indexOfLambdaEnd("La."                             ) =  2));
    print("    test 1:  "^boolToString(indexOfLambdaEnd("La.a"                            ) =  3));
    print("    test 3:  "^boolToString(indexOfLambdaEnd("La.a "                           ) =  3));
    print("    test 1:  "^boolToString(indexOfLambdaEnd("L. L."                           ) =  1));
    print("    test 2:  "^boolToString(indexOfLambdaEnd("La.abc"                          ) =  5));
    print("    test 1:  "^boolToString(indexOfLambdaEnd("La. La."                         ) =  2));
    print("    test 1:  "^boolToString(indexOfLambdaEnd("L.(x y)"                      ) =  1)); (* TODO *)
    print("    test 6:  "^boolToString(indexOfLambdaEnd("L.L.abc"                         ) =  6));
    print("    test 4:  "^boolToString(indexOfLambdaEnd("La.abc "                         ) =  5));
    print("    test 1:  "^boolToString(indexOfLambdaEnd("La.a La.a"                       ) =  3));
    print("    test 6:  "^boolToString(indexOfLambdaEnd("Lx.Ly.abc"                       ) =  8));
    print("    test 5:  "^boolToString(indexOfLambdaEnd("Labc.cde  "                      ) =  7));
    print("    test 3:  "^boolToString(indexOfLambdaEnd("La.a La.a "                      ) =  3));
    print("    test 7:  "^boolToString(indexOfLambdaEnd("Lx.(Ly.abc)"                     ) = 10));
    print("    test 6:  "^boolToString(indexOfLambdaEnd("Lxb.L.abc Lx"                    ) =  8));
    print("    test 2:  "^boolToString(indexOfLambdaEnd("La.abc La.abc"                   ) =  5));
    print("    test 4:  "^boolToString(indexOfLambdaEnd("La.abc La.abc "                  ) =  5));
    print("    test 8:  "^boolToString(indexOfLambdaEnd("Lx.Ly.(Lz.abc)   "               ) = 13));
    print("    test 5:  "^boolToString(indexOfLambdaEnd("Labc.cde Labc.cde  "             ) =  7));
    print("    test 6:  "^boolToString(indexOfLambdaEnd("Lx.Ly.abc Lx.Ly.abc"             ) =  8));
    print("    test 9:  "^boolToString(indexOfLambdaEnd("Lx.Ly.((Lz.a)(Lz.b))"            ) = 19));
    print("    test 7:  "^boolToString(indexOfLambdaEnd("Lx.(Ly.abc) Lx.(Ly.abc)"         ) = 10));
    print("    test 8:  "^boolToString(indexOfLambdaEnd("Lx.Ly.(Lz.abc) Lx.Ly.(Lz.abc)   ") = 13));
());
*)

(*
fun test_uniqueVars() =
(
    print("\ntest_uniqueVars:\n");
    print("    test 1:  "^boolToString(    uniqueVars(""    ) ));
    print("    test 2:  "^boolToString(    uniqueVars("a"   ) ));
    print("    test 3:  "^boolToString(    uniqueVars("abc" ) ));
    print("    test 4:  "^boolToString(not(uniqueVars("aba" ))));
    print("    test 5:  "^boolToString(not(uniqueVars("abcb"))));
    print("    test 6:  "^boolToString(not(uniqueVars("aa"  ))));
    print("    test 7:  "^boolToString(not(uniqueVars(" "   ))));
    print("    test 8:  "^boolToString(not(uniqueVars("  "  ))));
());
*)

fun test_replace() =
    let
        val s1 = "aaa";
        val s2 = "abc";
        val s3 = "aba";
        val s4 = "";
    in (
        print("\ntest_replace:\n");
        print("    test 1:  "^boolToString(replace(s1,    "a",  "b") =    "bbb"));
        print("    test 2:  "^boolToString(replace(s1,    "a", "aa") = "aaaaaa"));
        print("    test 3:  "^boolToString(replace(s1,   "ab",  "b") =    "aaa"));
        print("    test 4:  "^boolToString(replace(s1,   "ab",   "") =    "aaa"));
        print("    test 5:  "^boolToString(replace(s1,   "aa",  "b") =     "ba"));
        print("    test 6:  "^boolToString(replace(s2,    "a",  "b") =    "bbc"));
        print("    test 7:  "^boolToString(replace(s2,   "ab",  "b") =     "bc"));
        print("    test 8:  "^boolToString(replace(s2,  "abc",  "b") =      "b"));
        print("    test 9:  "^boolToString(replace(s2, "abc ",  "b") =    "abc"));
        print("    test 10: "^boolToString(replace(s3,    "a",  "b") =    "bbb"));
        print("    test 11: "^boolToString(replace(s3,    "a",   "") =      "b"));
        print("    test 12: "^boolToString(replace(s3,  "abc",  "f") =    "aba"));
        print("    test 13: "^boolToString(replace(s3,    " ",  "b") =    "aba"));
        print("    test 14: "^boolToString(replace(s4,    "a",  "b") =       ""));
        print("    test 15: "^boolToString(replace(s4,    " ",  "b") =       ""));
    ()) end
;

(*
fun test_trimWhitespace() =
(
    print("\ntest_trimWhitespace:\n");
    print("    test 1:  "^boolToString(    uniqueVars(""    ) ));
    print("    test 2:  "^boolToString(    uniqueVars("a"   ) ));
    print("    test 3:  "^boolToString(    uniqueVars("abc" ) ));
    print("    test 4:  "^boolToString(not(uniqueVars("aba" ))));
    print("    test 5:  "^boolToString(not(uniqueVars("abcb"))));
    print("    test 6:  "^boolToString(not(uniqueVars("aa"  ))));
    print("    test 7:  "^boolToString(not(uniqueVars(" "   ))));
    print("    test 8:  "^boolToString(not(uniqueVars("  "  ))));
());
*)

fun test_stringify() =
(
    print("\ntest_stringify:\n");
    print("    test 1:  "^boolToString(stringify([                    ]) =        ""));
    print("    test 2:  "^boolToString(stringify([               " a "]) =       "a"));
    print("    test 3:  "^boolToString(stringify([            "b a a "]) =   "b a a"));
    print("    test 4:  "^boolToString(stringify([       "b", "b a a "]) = "b b a a"));
    print("    test 5:  "^boolToString(stringify([  "a", "a",      "a"]) =   "a a a"));
    print("    test 6:  "^boolToString(stringify([  "a", "b",      "c"]) =   "a b c"));
    print("    test 7:  "^boolToString(stringify(["abc",  "",    "abc"]) = "abc abc"));
    print("    test 8:  "^boolToString(stringify([   "",  "",       ""]) =        ""));
    print("    test 9:  "^boolToString(stringify([  " ", "a",      " "]) =       "a"));
    print("    test 10: "^boolToString(stringify([  " ", " ",      "b"]) =       "b"));
());

fun test_tokenize() =
(
(*
    print("\ntest_tokenize:\n");
    print("    test 1:  "^boolToString(tokenize(""                  ) = [                            ]));
    print("    test 2:  "^boolToString(tokenize(" "                 ) = [                            ]));
    print("    test 3:  "^boolToString(tokenize("a"                 ) = [                         "a"]));
    print("    test 4:  "^boolToString(tokenize("(a) "              ) = [                       "(a)"]));
    print("    test 5:  "^boolToString(tokenize("c(a) "             ) = [                      "c(a)"]));
    print("    test 6:  "^boolToString(tokenize("a b"               ) = [               "a",      "b"]));
    print("    test 7:  "^boolToString(tokenize("a b"               ) = [               "a",      "b"]));
    print("    test 8:  "^boolToString(tokenize(" a b"              ) = [               "a",      "b"]));
    print("    test 9:  "^boolToString(tokenize(" a b "             ) = [               "a",      "b"]));
    print("    test 10: "^boolToString(tokenize("a c(a)"            ) = [               "a",   "c(a)"]));
    print("    test 11: "^boolToString(tokenize("a c(a()) "         ) = [               "a", "c(a())"]));
    print("    test 12: "^boolToString(tokenize("a() c(a()) "       ) = [             "a()", "c(a())"]));
    print("    test 13: "^boolToString(tokenize("(a() c(a()) (p)) a") = ["(a() c(a()) (p))",      "a"]));
    print("    test 14: "^boolToString(tokenize(" a() c(a()) (p)"   ) = [   "a()", "c(a())",    "(p)"]));
*)
());


(*
fun test_validLambdaSyntax() =
(
    (* Valid test cases *)
    print("\ntest_validLambdaSyntax:\n");
    print("    test 1:  "^boolToString(validLambdaSyntax("L."                          )));
    print("    test 2:  "^boolToString(validLambdaSyntax("L.()"                        )));
    print("    test 3:  "^boolToString(validLambdaSyntax("L.ab"                        )));
    print("    test 4:  "^boolToString(validLambdaSyntax("L.L."                        )));
    print("    test 5:  "^boolToString(validLambdaSyntax("L.L.ab"                      )));
    print("    test 6:  "^boolToString(validLambdaSyntax("Lx."                         )));
    print("    test 7:  "^boolToString(validLambdaSyntax("Lx.x"                        )));
    print("    test 8:  "^boolToString(validLambdaSyntax("Lxy.x"                       )));
    print("    test 9:  "^boolToString(validLambdaSyntax("Lx.xy"                       )));
    print("    test 10: "^boolToString(validLambdaSyntax("Lx.()"                       )));
    print("    test 11: "^boolToString(validLambdaSyntax("Lx.(x)"                      )));
    print("    test 12: "^boolToString(validLambdaSyntax("Lx.x(y)"                     )));
    print("    test 13: "^boolToString(validLambdaSyntax("Lx.(x)y"                     )));
    print("    test 14: "^boolToString(validLambdaSyntax("Lx.(xy)"                     )));
    print("    test 15: "^boolToString(validLambdaSyntax("Lx.Ly."                      )));
    print("    test 16: "^boolToString(validLambdaSyntax("Lx.Ly.x"                     )));
    print("    test 17: "^boolToString(validLambdaSyntax("Lx.Lyz.x"                    )));
    print("    test 18: "^boolToString(validLambdaSyntax("Lx.Ly.xy"                    )));
    print("    test 19: "^boolToString(validLambdaSyntax("Lx.Ly.()"                    )));
    print("    test 20: "^boolToString(validLambdaSyntax("Lx.Ly.(x)"                   )));
    print("    test 21: "^boolToString(validLambdaSyntax("Lx.Ly.(x)y"                  )));
    print("    test 22: "^boolToString(validLambdaSyntax("Lx.Ly.x(y)"                  )));
    print("    test 23: "^boolToString(validLambdaSyntax("Lx.Ly.(xy)"                  )));
    print("    test 25: "^boolToString(validLambdaSyntax("Lx.Ly.a(Lz.xy)"              )));
    print("    test 26: "^boolToString(validLambdaSyntax("Lx.Ly.a(Lz.xy)b"             )));
    print("    test 24: "^boolToString(validLambdaSyntax("Lx.Ly.(Lz.(x y))"            ))); (* TODO *)
    print("    test 27: "^boolToString(validLambdaSyntax("Lx.Ly.a()(Lz.xy)()b"         )));
    print("    test 28: "^boolToString(validLambdaSyntax("Lx.Ly.a((Lz.xy))b()"         )));
    print("    test 29: "^boolToString(validLambdaSyntax("Lx.(Ly.)"                    )));
    print("    test 30: "^boolToString(validLambdaSyntax("Lx.(Ly.xy)"                  )));
    print("    test 31: "^boolToString(validLambdaSyntax("Lx.(Ly.x)"                   )));
    print("    test 32: "^boolToString(validLambdaSyntax("Lx.(Lyz.x)"                  )));
    print("    test 33: "^boolToString(validLambdaSyntax("Lx.(Ly.())"                  )));
    print("    test 34: "^boolToString(validLambdaSyntax("Lx.(Ly.(xy))"                )));
    print("    test 35: "^boolToString(validLambdaSyntax("Lx.(Ly.(x))"                 )));
    print("    test 36: "^boolToString(validLambdaSyntax("Lx.(Lyz.((x)))"              )));
    print("    test 37: "^boolToString(validLambdaSyntax("Lx.(Lyz.()())x"              )));
    print("    test 38: "^boolToString(validLambdaSyntax("Lx.(Lyz.)x()"                )));
    print("    test 39: "^boolToString(validLambdaSyntax("Lx.(Lyz.())x()"              )));
    print("    test 40: "^boolToString(validLambdaSyntax("Lx.(Lyz.(()))x()"            )));
    print("    test 41: "^boolToString(validLambdaSyntax("Lx.(Lyz.(a))x(()a)La."       )));
    print("    test 42: "^boolToString(validLambdaSyntax("Lx.(Lyz.(a))x(()a)La.b"      )));
    print("    test 43: "^boolToString(validLambdaSyntax("Lx.(Lyz.(a))x(()a)(La.b)"    )));
    print("    test 44: "^boolToString(validLambdaSyntax("Lx.(Lyz.(a))x(()a)(La.b)()"  )));
    print("    test 45: "^boolToString(validLambdaSyntax("Lx.(Lyz.(a))x(()a)(La.b())()")));

    (* Invalid test cases *)
    print("    test 46: "^boolToString(not(validLambdaSyntax("."        ))));
    print("    test 47: "^boolToString(not(validLambdaSyntax("L"        ))));
    print("    test 48: "^boolToString(not(validLambdaSyntax("()"       ))));
    print("    test 49: "^boolToString(not(validLambdaSyntax("Lx"       ))));
    print("    test 50: "^boolToString(not(validLambdaSyntax("Lxy"      ))));
    print("    test 51: "^boolToString(not(validLambdaSyntax("L(xy)."   ))));
    print("    test 52: "^boolToString(not(validLambdaSyntax("Lxy.."    ))));
    print("    test 53: "^boolToString(not(validLambdaSyntax("Lxy.L"    ))));
    print("    test 54: "^boolToString(not(validLambdaSyntax("Lxy.LL"   ))));
    print("    test 55: "^boolToString(not(validLambdaSyntax("Lxy.L.L"  ))));
    print("    test 56: "^boolToString(not(validLambdaSyntax("Lxy.(LL)" ))));
    print("    test 57: "^boolToString(not(validLambdaSyntax("Lxy.L.."  ))));
    print("    test 58: "^boolToString(not(validLambdaSyntax("Lxy.L..L" ))));
    print("    test 59: "^boolToString(not(validLambdaSyntax("Lxy.(L.L)"))));
());
*)

(*
fun test_parseInput() =
;

fun test_flatten() =
;

*)

fun test_all() =
(
    test_charIn();
    (*test_indexOf();*)
    test_indexOfClosingPar();
    (*test_indexOfLambdaEnd();*)
    (*test_uniqueVars();*)
    test_replace();
    (*test_trimWhitespace();*)
    test_stringify();
    (*test_tokenize();*)
    (*test_validLambdaSyntax();*)
());
