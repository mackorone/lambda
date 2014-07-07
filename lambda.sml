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

(* Prompts*)
val errorPrompt   = " Err: ";
val inputPrompt   = "{L}-: ";
val redStepPrompt = "    - ";
val resultPrompt  = "    = ";

(* Special characters *)
val sc_lambda    = #"L";
val sc_separator = #".";
val sc_openPar   = #"(";
val sc_closePar  = #")";
val sc_load      = #"$";

(* Whether or not to show the steps of the reduction *)
val showRedSteps = true;

(* A string containing all valid variable characters *)
val variableChars = "abcdefghijklmnopqrstuvwxyz";

(* A string containing all valid whitespace characters *)
val whitespaceChars = " \t\n";

(* Requirement violation exception - programmer error *)
exception REQ_VIOLATION;

(* Syntax exception - user error *)
exception INVALID_SYNTAX;

(* Invalid file exception - user error *)
exception INVALID_FILE;

(* 
 * Returns true if the character 'c' is in the string 's', false otherwise
 *
 * REQ: None
 *)
fun charIn(c, s) =
    let
        fun helper(  []  ) = false
          | helper(hd::tl) = if (c = hd) then true else helper(tl)
        ;
    in
        helper(String.explode(s))
    end
;

(*
 * Returns the index of the first occurance of the string 'a' in the string 'b'
 * Returns ~1 if the string 'a' is not in the string 'b'
 *
 * REQ: None
 *)
fun indexOf(a, b) =
    let
        fun helper(str, i) =
            if (String.size(a) > String.size(str)) then ~1
            else if (a = String.substring(str, 0, String.size(a))) then i
            else helper(String.extract(str, 1, NONE), i + 1)
        ;
    in
        helper(b, 0)
    end
;

(* 
 * Returns the index of the closing parenthesis corresponding to the opening
 * parenthesis that is at the start of the string 's'.
 *
 * Raises INVALID_SYNTAX if no closing parenthesis is found.
 *
 * REQ: The first character in the string must be sc_openPar
 *)
fun indexOfClosingPar(s) =
    let
        fun helper(  []  , i, n) = raise INVALID_SYNTAX
          | helper(hd::tl, i, n) =
            if (hd = sc_closePar) then
                if (n = 1) then i
                else helper(tl, i+1, n-1)
            else if (hd = sc_openPar) then helper(tl, i+1, n+1)
            else helper(tl, i+1, n)
        ;
    in
        if (String.sub(s, 0) <> sc_openPar) then raise REQ_VIOLATION
        else helper(String.explode(s), 0, 0)
    end
;

(* 
 * Returns the index of the last character in the file name at the start of the
 * string 's'.
 *
 * REQ: None
 *)
fun indexOfFileEnd(s) =
    let
        fun helper(  []  , i) = i
          | helper(hd::tl, i) =
            if (charIn(hd, whitespaceChars)) orelse (hd = sc_openPar) orelse (hd = sc_closePar) then i
            else helper(tl, i + 1)
        ;
    in
        helper(String.explode(s), 0)
    end
;

(* 
 * If the lambda function at the start of the string 's' is well formed, this
 * function returns the index of the last character of the lambda function.
 *
 * Raises INVALID_SYNTAX if the lambda function at the start of the string 's'
 * is not well formed,
 *
 * REQ: The first character of 's' must be sc_lambda
 *)
fun indexOfLambdaEnd(s) =
    let
        fun helper([]                   ) = raise REQ_VIOLATION
          | helper(lam::[]              ) = raise INVALID_SYNTAX
          | helper(lam::var::[]         ) = raise INVALID_SYNTAX
          | helper(lam::var::sep::[]    ) = raise INVALID_SYNTAX
          | helper(lam::var::sep::hd::tl) =
                if (lam <> sc_lambda)
                orelse (not(charIn(var, variableChars)))
                orelse (sep <> sc_separator) then raise INVALID_SYNTAX
                else
                    if (hd = sc_lambda) then helper(hd::tl) + 3
                    else if (hd = sc_openPar) then
                        let
                            val str = String.implode(hd::tl);
                            val i = indexOfClosingPar(str);
                        in
                            (* Enforce correctness of the contents *)
                            tokenize(String.substring(str, 0, i + 1));
                            i + 3
                        end
                    else if (charIn(hd, variableChars)) then
                        if (tl = []) then 3
                        else 
                            let
                                val md = List.hd(tl);
                            in
                                if charIn(md, whitespaceChars)
                                orelse (md = sc_openPar)
                                orelse (md = sc_closePar) then 3
                                else raise INVALID_SYNTAX
                            end
                    else raise INVALID_SYNTAX
    in
        helper(String.explode(s))
    end

(*
 * Returns a list containing the lambda expressions within the string 's'.
 *
 * Raises INVALID_SYNTAX if the string is an invalid lambda
 * expression.
 *
 * REQ: None
 *)
and tokenize(s) =
    let
        fun helper([], lst) = if (lst = []) then raise INVALID_SYNTAX
                              else lst

          | helper(hd::[], lst) =
            if (charIn(hd, whitespaceChars)) then helper([], lst)
            else if (charIn(hd, variableChars)) then lst @ [Char.toString(hd)]
            else raise INVALID_SYNTAX

          | helper(hd::md::tl, lst) =
            if (charIn(hd, whitespaceChars)) then helper(md::tl, lst)
            else if (charIn(hd, variableChars)) then
                if charIn(md, whitespaceChars) orelse (md = sc_openPar) then
                    helper(md::tl, lst @ [Char.toString(hd)])
                else raise INVALID_SYNTAX

            else if (hd = sc_openPar) then
                let
                    val str = String.implode(hd::md::tl);
                    val i = indexOfClosingPar(str);
                    val expr = String.substring(str, 0, i + 1);
                    val rest = String.extract(str, i + 1, NONE);
                in
                    (* Enforce correctness of the contents *)
                    tokenize(String.substring(expr, 1, String.size(expr) - 2));
                    helper(String.explode(rest), lst @ [expr])
                end

            else if (hd = sc_lambda) then
                let
                    val str = String.implode(hd::md::tl);
                    val i = indexOfLambdaEnd(str);
                    val expr = String.substring(str, 0, i + 1);
                    val rest = String.extract(str, i + 1, NONE);
                in
                    helper(String.explode(rest), lst @ [expr])
                end

            else if (hd = sc_load) then
                let
                    val str = String.implode(hd::md::tl);
                    val i = indexOfFileEnd(str);
                    val file = String.substring(String.implode(hd::md::tl), 1, i - 1);
                    val rest = String.extract(String.implode(hd::md::tl), i, NONE)
                    val stream = TextIO.openIn(file);
                    val input = TextIO.inputAll(stream);
                in
                    TextIO.closeIn(stream);
                    helper(String.explode(input^rest), lst)
                end
                handle Io => raise INVALID_FILE

            else raise INVALID_SYNTAX
        ;
    in
        helper(String.explode(s), [])
    end
;

(*
 * Returns the string 's' with all unique occurances of the string 'a' replaced
 * with the string 'b'
 *
 * REQ: 'a' cannot be the empty string
 *)
fun replace( s, "", b) = raise REQ_VIOLATION
  | replace("",  a, b) = ""
  | replace( s,  a, b) =
    let
        val size = String.size(a);
        fun helper(str, acc) =
            if (size <= String.size(str)) then
                if (a = String.substring(str, 0, size)) then
                    helper(String.extract(str, size, NONE), acc^b)
                else helper(String.extract(str, 1, NONE), acc^String.substring(str, 0, 1))
            else acc^str
        ;
    in
        helper(s, "")
    end
;

(*
 * Replaces all free occurances of 'a' with 'b' in the string 's'
 *
 * REQ: The string 's' must be a valid lambda expression
 *)
fun replaceFreeVars(s, a, b) =
    let
        fun helper( "", acc) = acc
          | helper(str, acc) =
            let
                val i = indexOf(Char.toString(sc_lambda)^a, str);
            in
                if (i = ~1) then acc^replace(str, a, b)
                else
                    let
                        val e = indexOfLambdaEnd(String.extract(str, i, NONE));
                        val free = String.substring(str, 0, i);
                        val bound = String.substring(str, i, e + 1);
                        val rest = String.extract(str, e + i + 1, NONE);
                    in
                        helper(rest, acc^replace(free, a, b)^bound)
                    end
            end
        ;
    in
        helper(s, "")
    end
;

(*
 * Returns the string resulting from trimming all whitespace from the
 * beginning and end of the string 's'
 *
 * REQ: None
 *)
fun trimWhitespace(s) =
    let
        fun helper([]) = []
          | helper(hd::tl) = if (charIn(hd, whitespaceChars)) then helper(tl) else hd::tl
        ;
    in
        String.implode(rev(helper(rev(helper(String.explode(s))))))
    end
;

(*
 * Returns the whitespace-trimmed string resulting from whitespace-trimming
 * and concatenating all strings within the list of strings, 'lst'
 *
 * REQ: None
 *)
fun stringify([]) = ""
  | stringify(lst) =
    let
        fun helper(  []  , s) = trimWhitespace(s)
          | helper(hd::tl, s) =
            let
                val thd = trimWhitespace(hd);
            in
                helper(tl, s^(thd)^(if (tl = [] orelse thd = "") then "" else " "))
            end
        ;
    in
        helper(lst, "")
    end
;

(*
 * Returns the list of strings that result from beta reducing 'hd' into the the
 * lambda function given by 'a'
 *
 * REQ: The string 'a' must be a valid lambda function
 *)
fun betaReduce(a,   []  ) = [a]
  | betaReduce(a, hd::tl) =
    let
        val body = String.extract(a, 3, NONE);
        val var = Char.toString(String.sub(a, 1));
        val newBody = replaceFreeVars(body, var, hd);
    in
        [newBody] @ tl
    end

(*
 * Returns the list of strings the result after reducing the input list, 'lst'
 *
 * REQ: The list must be tokenized properly (via the tokenize function)
 *)
and reduce(lst) = 
    let
        fun helper(  []  , acc) = acc
          | helper(hd::tl, acc) =
            if (hd = "") then raise REQ_VIOLATION
            else
                let
                    val chd::ctl = String.explode(hd);
                in
                    if (chd = sc_lambda) then
                        if (tl = []) then acc @ [hd]
                        else (
                            if (showRedSteps) then print(redStepPrompt^stringify(acc @ hd::tl)^"\n") else ();
                            helper(betaReduce(hd, tl), acc)
                        )
                    else if (chd = sc_openPar) then ( (* strip the parenthesis and reduce *)
                        if (showRedSteps) then print(redStepPrompt^stringify(acc @ hd::tl)^"\n") else ();
                        helper(tokenize(String.implode(List.take(ctl, List.length(ctl)-1))) @ tl, acc)
                    )
                    else helper(tl, acc @ [hd])
                end
        ;
    in
        helper(lst, [])    
    end
;

(* Main loop for the interpreter *)
fun main() =
(
    print(inputPrompt);
    let
        val inputOption = TextIO.inputLine(TextIO.stdIn);
    in
        (* If CTRL-D, then exit *)
        if (inputOption = NONE) then ()
        else
            let
                val input = Option.valOf(inputOption);
            in
                (* If there is no non-whitespace input, don't print a result *)
                if (trimWhitespace(input) = "") then ()
                else print(resultPrompt^stringify(reduce(tokenize(input)))^"\n")
                ; main()
            end
    end
    handle error => (
    case error of
        INVALID_FILE   => print(errorPrompt^"Invalid file"^"\n")
      | INVALID_SYNTAX => print(errorPrompt^"Invalid syntax"^"\n")
      | REQ_VIOLATION  => print("    * Congratulations! You have found a bug in the "^
                                "interpreter.\n    * Please send an email to "^
                                "mward4@buffalo.edu containing the input\n    * "^
                                "that caused this output - it is greatly appreciated!\n")
    ; main())
);
