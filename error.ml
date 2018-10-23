open Parser

exception SyntaxError of string
exception TypeError of string
exception NameError of string
exception OverflowError of string
exception IndentationError of string
exception ZeroDivisionError of string
exception EmptyInput
exception Multiline of (expr * string option)