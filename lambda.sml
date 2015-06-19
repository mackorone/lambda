(* Prompts *)
val errorPrompt   = " Err: "
val inputPrompt   = "{L}-: "
val redStepPrompt = "    - "
val resultPrompt  = "    = "

(* Special characters *)
val sc_lambda    = #"L"
val sc_separator = #"."
val sc_openPar   = #"("
val sc_closePar  = #")"
val sc_load      = #"$"

(* Whether or not to show the steps of the reduction *)
val showRedSteps = true

(* A string containing all valid variable characters *)
val variableChars = "abcdefghijklmnopqrstuvwxyz"

(* A string containing all valid whitespace characters *)
val whitespaceChars = " \t\n"

(* Requirement violation exception - programmer error *)
exception REQ_VIOLATION

(* Syntax exception - user error *)
exception INVALID_SYNTAX

(* Invalid file exception - user error *)
exception INVALID_FILE

val charIn = Char.contains

(*
 * fn : string * string -> int option
 * Returns the index of the first occurrence of the string 'a' in the string 'b', if 'a' is a substring of 'b'.
 *
 * REQ: None.
 *)
fun indexOf(a, b) =
    let
        fun helper(str, i) =
            if (String.size(a) > String.size(str)) then NONE
            else if (a = String.substring(str, 0, String.size(a))) then (SOME i)
            else helper(String.extract(str, 1, NONE), i + 1)
    in
        helper(b, 0)
    end

(*
 * Returns the index of the closing parenthesis corresponding to the opening
 * parenthesis that is at the start of the string 's'.
 *
 * Raises INVALID_SYNTAX if no closing parenthesis is found.
 *
 * REQ: The first character in the string must be sc_openPar.
 *)
fun indexOfClosingPar(s) =
    let
        fun helper(  []  , indexInString, numOpenParentheses) = raise INVALID_SYNTAX
          | helper(hd::tl, indexInString, numOpenParentheses) =
            if (hd = sc_closePar) then
                if (numOpenParentheses = 1) then indexInString
                else helper(tl, indexInString + 1, numOpenParentheses - 1)
            else if (hd = sc_openPar) then helper(tl, indexInString + 1, numOpenParentheses + 1)
            else helper(tl, indexInString + 1, numOpenParentheses)
    in
        if (String.sub(s, 0) <> sc_openPar) then raise REQ_VIOLATION
        else helper(String.explode(s), 0, 0)
    end

(*
 * Returns the index of the last character in the file name at the start of the
 * string 's'.
 *
 * REQ: None.
 *)
fun indexOfFileEnd(s) =
    let
        fun helper(  []  , i) = i - 1
          | helper(hd::tl, i) =
            if (charIn whitespaceChars hd) orelse (hd = sc_openPar) orelse (hd = sc_closePar) then i - 1
            else helper(tl, i + 1)
    in
        helper(String.explode(s), 0)
    end

(*
 * If the lambda function at the start of the string 's' is well formed, this
 * function returns the index of the last character of the lambda function.
 *
 * Raises INVALID_SYNTAX if the lambda function at the start of the string 's'
 * is not well formed.
 *
 * REQ: The first character of 's' must be sc_lambda.
 *)
fun indexOfLambdaEnd(s) =
    let
        fun helper([]) = raise REQ_VIOLATION
          | helper(outerHd::outerTl) =
            if (outerHd <> sc_lambda) then raise REQ_VIOLATION
            else
                case outerTl of
                    [] => raise INVALID_SYNTAX
                  | var::[] => raise INVALID_SYNTAX
                  | var::sep::[] => raise INVALID_SYNTAX
                  | var::sep::hd::tl =>
                        if (not(charIn variableChars var)) orelse (sep <> sc_separator) then raise INVALID_SYNTAX
                        else
                            if (hd = sc_lambda) then helper(hd::tl) + 3
                            else if (hd = sc_openPar) then
                                let
                                    val str = String.implode(hd::tl)
                                    val i = indexOfClosingPar(str)
                                in
                                    (* Enforce correctness of the contents *)
                                    tokenize(String.substring(str, 0, i + 1));
                                    i + 3
                                end
                            else if (charIn variableChars hd) then
                                if (tl = []) then 3
                                else
                                    let
                                        val md = List.hd(tl)
                                    in
                                        if (charIn whitespaceChars md)
                                        orelse (md = sc_openPar)
                                        orelse (md = sc_closePar) then 3
                                        else raise INVALID_SYNTAX
                                    end
                            else raise INVALID_SYNTAX
    in
        helper(String.explode(s))
    end

(*
 * Returns a list containing the expressions within the string 's'.
 *
 * Raises INVALID_SYNTAX if the string is an invalid lambda expression.
 *
 * REQ: None.
 *)
and tokenize(s) =
    let
        fun helper([], lst) = if (lst = []) then raise INVALID_SYNTAX
                              else lst

          | helper(hd::[], lst) =
            if (charIn whitespaceChars hd) then helper([], lst)
            else if (charIn variableChars hd) then lst @ [Char.toString(hd)]
            else raise INVALID_SYNTAX

          | helper(hd::md::tl, lst) =
            if (charIn whitespaceChars hd) then helper(md::tl, lst)
            else if (charIn variableChars hd) then
                if (charIn whitespaceChars md) orelse (md = sc_openPar) then
                    helper(md::tl, lst @ [Char.toString(hd)])
                else raise INVALID_SYNTAX

            else if (hd = sc_openPar) then
                let
                    val str = String.implode(hd::md::tl)
                    val i = indexOfClosingPar(str)
                    val expr = String.substring(str, 0, i + 1)
                    val rest = String.extract(str, i + 1, NONE)
                in
                    (* Enforce correctness of the contents *)
                    tokenize(String.substring(expr, 1, String.size(expr) - 2));
                    helper(String.explode(rest), lst @ [expr])
                end

            else if (hd = sc_lambda) then
                let
                    val str = String.implode(hd::md::tl)
                    val i = indexOfLambdaEnd(str)
                    val expr = String.substring(str, 0, i + 1)
                    val rest = String.extract(str, i + 1, NONE)
                in
                    helper(String.explode(rest), lst @ [expr])
                end

            else if (hd = sc_load) then
                let
                    val str = String.implode(hd::md::tl)
                    val i = indexOfFileEnd(str)
                    val file = String.substring(String.implode(hd::md::tl), 1, i)
                    val rest = String.extract(String.implode(hd::md::tl), i + 1, NONE)
                    val stream = TextIO.openIn(file)
                    val input = TextIO.inputAll(stream)
                in
                    TextIO.closeIn(stream);
                    helper(String.explode(input ^ rest), lst)
                end
                handle Io => raise INVALID_FILE

            else raise INVALID_SYNTAX
    in
        helper(String.explode(s), [])
    end

(*
 * Returns the string 's' with all unique occurrences of the string 'a' replaced with the string 'b'.
 *
 * REQ: 'a' cannot be the empty string.
 *)
fun replace( s, "", b) = raise REQ_VIOLATION
  | replace("",  a, b) = ""
  | replace( s,  a, b) =
    let
        val size = String.size(a)
        fun helper(str, acc) =
            if (size <= String.size(str)) then
                if (a = String.substring(str, 0, size)) then
                    helper(String.extract(str, size, NONE), acc ^ b)
                else helper(String.extract(str, 1, NONE), acc ^ String.substring(str, 0, 1))
            else acc ^ str
    in
        helper(s, "")
    end

(*
 * Replaces all free occurrences of 'a' with 'b' in the string 's'.
 *
 * REQ: The string 's' must be a valid lambda expression.
 *)
fun replaceFreeVars(s, a, b) =
    let
        fun helper( "", acc) = acc
          | helper(str, acc) =
            case indexOf(Char.toString(sc_lambda) ^ a, str) of
                NONE => acc ^ replace(str, a, b)
              | SOME i =>
                    let
                        val e = indexOfLambdaEnd(String.extract(str, i, NONE))
                        val free = String.substring(str, 0, i)
                        val bound = String.substring(str, i, e + 1)
                        val rest = String.extract(str, e + i + 1, NONE)
                    in
                        helper(rest, acc ^ replace(free, a, b) ^ bound)
                    end
    in
        helper(s, "")
    end

(*
 * Returns the string resulting from trimming all whitespace from the beginning and end of the string 's'.
 *
 * REQ: None.
 *)
fun trimWhitespace(s) =
    let
        fun helper([]) = []
          | helper(hd::tl) = if (charIn whitespaceChars hd) then helper(tl) else hd::tl
    in
        String.implode(rev(helper(rev(helper(String.explode(s))))))
    end

(*
 * Returns the whitespace-trimmed string resulting from whitespace-trimming
 * and concatenating all strings within the list of strings, 'lst'.
 *
 * REQ: None.
 *)
fun stringify([]) = ""
  | stringify(lst) =
    let
        fun helper(  []  , s) = trimWhitespace(s)
          | helper(hd::tl, s) =
            let
                val thd = trimWhitespace(hd)
            in
                helper(tl, s ^ thd ^ (if (tl = [] orelse thd = "") then "" else " "))
            end
    in
        helper(lst, "")
    end

(*
 * Returns the list of strings that result from beta reducing 'hd' into the lambda function given by 'a'.
 *
 * REQ: The string 'a' must be a valid lambda function.
 *)
fun betaReduce(a,   []  ) = [a]
  | betaReduce(a, hd::tl) =
    let
        val body = String.extract(a, 3, NONE)
        val var = Char.toString(String.sub(a, 1))
        val newBody = replaceFreeVars(body, var, hd)
    in
        [newBody] @ tl
    end

(*
 * Returns the list of strings the result after reducing the input list, 'lst'.
 *
 * REQ: The list must be tokenized properly (via the tokenize function).
 *)
fun reduce(lst) =
    let
        fun helper(  []  , acc) = acc
          | helper(hd::tl, acc) =
            if (hd = "") then raise REQ_VIOLATION
            else
                let
                    val chd::ctl = String.explode(hd)
                in
                    if (chd = sc_lambda) then
                        if (tl = []) then acc @ [hd]
                        else (
                            if (showRedSteps) then print(redStepPrompt ^ stringify(acc @ hd::tl) ^ "\n") else ();
                            helper(betaReduce(hd, tl), acc)
                        )
                    else if (chd = sc_openPar) then ( (* strip the parenthesis and reduce *)
                        if (showRedSteps) then print(redStepPrompt ^ stringify(acc @ hd::tl) ^ "\n") else ();
                        helper(tokenize(String.implode(List.take(ctl, List.length(ctl)-1))) @ tl, acc)
                    )
                    else helper(tl, acc @ [hd])
                end
    in
        helper(lst, [])
    end

(* Main loop for the interpreter *)
fun main() =
(
    print(inputPrompt);
    let
        val inputOption = TextIO.inputLine(TextIO.stdIn)
    in
        case inputOption of
            NONE => () (* If CTRL-D, then exit *)
          | SOME input =>
            ((if (trimWhitespace(input) = "") then () (* If there is no non-whitespace input, don't print a result *)
             else print(resultPrompt ^ stringify(reduce(tokenize(input))) ^ "\n"))
            ; main())
    end
    handle error => (
    case error of
        INVALID_FILE   => print(errorPrompt ^ "Invalid file\n")
      | INVALID_SYNTAX => print(errorPrompt ^ "Invalid syntax\n")
      | _              => print("    * Congratulations! You have found a bug in the " ^
                                "interpreter.\n    * Please send an email to " ^
                                "mward4@buffalo.edu containing the input\n    * " ^
                                "that caused this output - it is greatly appreciated!\n")
    ; main())
)
