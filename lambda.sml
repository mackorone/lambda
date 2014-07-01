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
val inputPrompt   = "{L}-: ";
val resultPrompt  = "    = ";

(* Special characters *)
val sc_lambda    = #"L";
val sc_separator = #".";
val sc_openPar   = #"(";
val sc_closePar  = #")";
val sc_load      = #"$";

(* The output resulting from invalid input *)
val errorString = "Error";
 
(* A string containing all valid variable characters *)
val variableChars = "abcdefghijklmnopqrstuvwxyz";

(* A string containing all valid whitespace characters *)
val whitespaceChars = " \t\n";

(* Requirement violation exception *)
exception REQ_VIOLATION;

(* 
 * Returns true if the element 'e' is in the list, false otherwise
 *
 * REQ: None
 *)
fun isIn(e,   []  ) = false
  | isIn(e, hd::tl) =
    if (e = hd) then true
    else isIn(e, tl)
;

(* 
 * Returns true if the character 'c' is in the string 's', false otherwise
 *
 * REQ: None
 *)
fun charIn(c, s) = isIn(c, String.explode(s));

(* 
 * Returns true if any of the character in the string 'a' are in the string 'b'
 *
 * REQ: None
 *)
fun anyIn(a, b) =
    let
        fun helper(  []  ) = false
          | helper(hd::tl) = if charIn(hd, b) then true else helper(tl)
    in
        helper(String.explode(a))
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
 * parenthesis that is at the start of the string 's'. Returns ~1 if no
 * corresponding closing parenthesis is found.
 *
 * REQ: The first character in the string must be sc_openPar
 *)
fun indexOfClosingPar(s) =
    let
        fun helper(  []  , i, n) = ~1
          | helper(hd::tl, i, n) =
            if (hd = sc_closePar) then
                if (n = 1) then i
                else helper(tl, i+1, n-1)
            else if (hd = sc_openPar) then helper(tl, i+1, n+1)
            else helper(tl, i+1, n)
        ;
    in
        helper(String.explode(s), 0, 0)
    end
;

(* 
 * Returns the index of the last character in the file name at the start of the
 * string 's'.
 *
 * REQ: The first character in the string must be sc_openPar
 *)
fun indexOfFileEnd(s) =
    let
        fun helper(  []  , i) = ~1
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
 * If the lambda function at the start of the string 's' is not well formed,
 * this function returns ~1.
 *
 * REQ: The first character of 's' must be sc_lambda
 *)
fun indexOfLambdaEnd(s) =
    let
        fun helper([],                 i) = raise REQ_VIOLATION
          | helper(lam::[],            i) = ~1
          | helper(lam::var::[],       i) = ~1
          | helper(lam::var::sep::[],  i) = ~1
          | helper(lam::var::sep::hd::tl, i) =
                if (lam <> sc_lambda) then raise REQ_VIOLATION
                else if (not(charIn(var, variableChars))) then ~1
                else if (sep <> sc_separator) then ~1
                else
                    if (hd = sc_lambda) then
                        let
                            val i = indexOfLambdaEnd(String.implode(hd::tl));
                        in
                            if (i = ~1) then ~1
                            else i + 3
                        end
                    else if (hd = sc_openPar) then
                        let
                            val str = String.implode(hd::tl);
                            val i = indexOfClosingPar(str);
                            val check = tokenize(String.substring(str, 1, i - 1));
                        in
                            if (i = ~1) then ~1
                            else if (check = []) then ~1
                            else if (isIn(errorString, check)) then ~1
                            else i + 3
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
                                else ~1
                            end
                    else ~1
    in
        helper(String.explode(s), 0)
    end

(*
 * Returns a list containing the lambda expressions within the string 's'.
 * If the string contains an invalid lambda expression it returns a list
 * containing a single error message describing the error.
 *
 * REQ: None
 *)
and tokenize(s) =
    let
        fun helper(  []  , lst) = lst

          | helper(hd::[], lst) =
            if (charIn(hd, whitespaceChars)) then lst
            else if (charIn(hd, variableChars)) then lst @ [Char.toString(hd)]
            else [errorString]

          | helper(hd::md::tl, lst) =
            if (charIn(hd, whitespaceChars)) then helper(md::tl, lst)
            else if (charIn(hd, variableChars)) then
                if charIn(md, whitespaceChars) orelse (md = sc_openPar) then
                    helper(md::tl, lst @ [Char.toString(hd)])
                else [errorString]

            else if (hd = sc_openPar) then
                let
                    val str = String.implode(hd::md::tl);
                    val i = indexOfClosingPar(str);
                in
                    if (i = ~1) then [errorString]
                    else
                        let
                            val expr = String.substring(str, 0, i + 1);
                            val rest = String.extract(str, i + 1, NONE);
                            val check = tokenize(String.substring(expr, 1, i - 1));
                        in
                            if (check = []) then [errorString]
                            else if (isIn(errorString, check)) then [errorString]
                            else helper(String.explode(rest), lst @ [expr])
                        end
                end

            else if (hd = sc_lambda) then
                let
                    val str = String.implode(hd::md::tl);
                    val i = indexOfLambdaEnd(str);
                in
                    if (i = ~1) then [errorString]
                    else
                        let
                            val expr = String.substring(str, 0, i + 1);
                            val rest = String.extract(str, i + 1, NONE);
                        in
                            helper(String.explode(rest), lst @ [expr])
                        end
                end

            else if (hd = sc_load) then
                let
                    val str = String.implode(hd::md::tl);
                    val i = indexOfFileEnd(str);
                    val file =
                        if (i = ~1) then String.implode(md::tl)
                        else String.substring(String.implode(hd::md::tl), 1, i - 1);
                    val rest = 
                        if (i = ~1) then ""
                        else String.extract(String.implode(hd::md::tl), i, NONE)
                    val stream = TextIO.openIn(file);
                    val input = TextIO.inputAll(stream);
                in
                    TextIO.closeIn(stream);
                    helper(String.explode(input^rest), lst)
                end
                handle Io => [errorString]

            else [errorString]
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
        fun helper(str) =
            if (size <= String.size(str)) then
                if (a = String.substring(str, 0, size)) then
                    b^helper(String.extract(str, size, NONE))
                else String.substring(str, 0, 1)^helper(String.extract(str, 1, NONE))
            else str
        ;
    in
        helper(s)
    end
;

(*
 * Replaces all free occurances of 'a' with 'b' in the string 's'
 *
 * REQ: The string 's' must be a valid lambda expression
 *)
fun replaceFreeVars(s, a, b) =
    let
        fun helper(out, "") = out
          | helper(out, str) =
            let
                val i = indexOf(Char.toString(sc_lambda)^a, str);
            in
                if (i = ~1) then out^replace(str, a, b)
                else
                    let
                        val e = indexOfLambdaEnd(String.extract(str, i, NONE));
                        val free = String.substring(str, 0, i);
                        val bound = String.substring(str, i, e + 1);
                        val rest = String.extract(str, e + i + 1, NONE);
                    in
                        helper(out^replace(free, a, b)^bound, rest)
                    end
            end
        ;
    in
        helper("", s)
    end
;

(*
 * Returns the list of strings that result from beta reducing 'hd' into the the
 * lambda function given by 'a'
 *
 * REQ: The string 'a' must be a valid lambda expression
 *)
fun betaReduce(a,   []  ) = [a]
  | betaReduce(a, hd::tl) =
    let
        val body = String.extract(a, 3, NONE);
        val var = Char.toString(String.sub(a, 1));
        val newBody = replaceFreeVars(body, var, hd);
    in
        reduce(tokenize(newBody) @ tl)
    end

(*
 * Returns the list of strings the result after reducing the input list
 *
 * REQ: The elements in the list must be tokenized properly
 *)
and reduce(  []  ) = []
  | reduce(hd::tl) =
    let
        fun helper(   []   ) = reduce(tl)
          | helper(chd::ctl) =
            if (chd = sc_lambda) then betaReduce(hd, tl)
            else if (chd = sc_openPar) then
                reduce(tokenize(String.implode(List.take(ctl, List.length(ctl)-1))) @ tl)
            else hd::reduce(tl)
        ;
    in
        helper(String.explode(hd))
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
                val result = reduce(tokenize(input));
            in (
                (* If there is no result, don't print anything*)
                if (result = []) then ()
                else (
                    print(resultPrompt);
                    print(stringify(result)^"\n")
                )
            );
            main()
            end
    end
);
