; SPDX-FileCopyrightText: 2023 Leorize <leorize+oss@disroot.org>, aMOPel <>
; SPDX-License-Identifier: MPL-2.0

; =============================================================================
; catch all rules

((identifier) @variable
  (#set! "priority" 99))
((accent_quoted (identifier) @variable)
  (#set! "priority" 99))
; NOTE: needs lower priority since it's more specific than `(type_expression) @type`

; catch all rule
; captures everything inside a type_expression,
; false positives need to be overruled
; this is necessary, since type_expression can nest identifiers very deep
; for example with bracket_expression as generic types
(type_expression) @type

; =============================================================================
; @comment               ; line and block comments

[
  (comment)
  (block_comment)
] @comment

; =============================================================================
; @comment.documentation ; comments documenting code

[
  (documentation_comment)
  (block_documentation_comment)
] @comment.documentation

; =============================================================================
; @error                 ; syntax/parser errors

(ERROR) @error

; =============================================================================
; @none                  ; completely disable the highlight
; unused

; =============================================================================
; @preproc               ; various preprocessor directives & shebangs

(pragma_list ["{." "}" ".}"] @preproc)

; =============================================================================
; @define                ; preprocessor definition directives
; unused

; =============================================================================
; @punctuation.delimiter ; delimiters (e.g. `;` / `.` / `,`)

[ "." ";" "," ":" ] @punctuation.delimiter

; (_ "=" @punctuation.delimiter [body: (_) value: (_)])

; =============================================================================
; @punctuation.bracket   ; brackets (e.g. `()` / `{}` / `[]`)

[ "(" ")" "[" "[:" "]" "{" "}" ] @punctuation.bracket

; =============================================================================
; @punctuation.special   ; special symbols (e.g. `{}` in string interpolation)

(accent_quoted "`" @punctuation.special)

(exported_symbol "*" @punctuation.special)

; dereference operator
(bracket_expression !right "[" @punctuation.special . "]" @punctuation.special)

; =============================================================================
; @string               ; string literals

(string_literal) @string

; =============================================================================
; @string.documentation ; string documenting code (e.g. Python docstrings)
; unused

; =============================================================================
; @string.regex         ; regular expressions
; unused

; =============================================================================
; @string.escape        ; escape sequences

(escape_sequence) @string.escape

; =============================================================================
; @string.special       ; other special strings (e.g. dates)
; unused

; =============================================================================
; @character            ; character literals

(char_literal) @character

; =============================================================================
; @character.special    ; special characters (e.g. wildcards)
; unused

; =============================================================================
; @boolean              ; boolean literals

((identifier) @boolean
  (#any-of? @boolean "true" "false" "on" "off"))

; =============================================================================
; @number               ; numeric literals

(integer_literal) @number

(custom_numeric_literal) @number

; =============================================================================
; @float                ; floating-point number literals

(float_literal) @float

; =============================================================================
; @function         ; function definitions

(proc_declaration name: (_) @function)

(func_declaration name: (_) @function)

(iterator_declaration name: (_) @function)

(converter_declaration name: (_) @function)

; =============================================================================
; @function.call    ; function calls

(call
  function: [
    (identifier) @function.call
    (accent_quoted (identifier) @function.call)
    ; generic types
    (bracket_expression left: (identifier) @function.call)
    (bracket_expression left: (accent_quoted (identifier) @function.call) )
    ; dot accessor
    (dot_expression right: (identifier) @function.call)
    (dot_expression right: (accent_quoted (identifier) @function.call) )
    ; both
    (bracket_expression left: 
      (dot_expression right: (identifier) @function.call))
    (bracket_expression left: 
      (dot_expression right: (accent_quoted (identifier) @function.call)))
  ])

;generalized_string is a function call
; `identifier"string literal"`
; is short for
; `identifier(r"string literal")`
(generalized_string . [
  (identifier) @function.call
  (accent_quoted (identifier) @function.call)
])

; =============================================================================
; @function.builtin ; built-in functions

; (call
;   function:
;     (identifier) @function.builtin
;   (#any-of? @function.builtin "new" "echo" "default"))
; NOTE: is it worth it to even start?

; =============================================================================
; @function.macro   ; preprocessor macros

(template_declaration name: (_) @function.macro)

(macro_declaration name: (_) @function.macro)

; =============================================================================
; @method           ; method definitions

(method_declaration name: (_) @method)

; =============================================================================
; @method.call      ; method calls
; unused

; =============================================================================
; @constructor      ; constructor calls and definitions

(call
  function: [
    (identifier) @constructor
    (accent_quoted (identifier) @constructor)
    (bracket_expression left: (identifier) @constructor)
    (bracket_expression left: (dot_expression right: (identifier) @constructor))
  ]
  (argument_list
    (colon_expression)+))
; NOTE: this cannot detect constructors with 0 arguments
; those will be matched as @function.call instead

; =============================================================================
; @parameter        ; parameters of a function

; named parameters when calling
; call(parameter_name=arg)
(argument_list
  (equal_expression left: (_) @parameter))

; parameters in function declaration
(parameter_declaration_list
  (parameter_declaration
    (symbol_declaration_list
      (symbol_declaration name: (_) @parameter))))

; =============================================================================
; @keyword             ; various keywords

; unhandled but reserved keywords
; end
; interface

((call
  function: (identifier) @keyword)
  (#any-of? @keyword "static" "addr"))
; static expression
; addr operator

[
  "const"
  "let"
  "var"
  "type"
  "concept"
  "asm"
  "bind"
  "defer"
  "do"
  "mixin"
  "static"
  "object"
  "tuple"
  "enum"
  "block"
  "using"
  "discard"
] @keyword

; =============================================================================
; @keyword.coroutine   ; keywords related to coroutines (e.g. `go` in Go, `async/await` in Python)
; unused

; =============================================================================
; @keyword.function    ; keywords that define a function (e.g. `func` in Go, `def` in Python)

[
  "proc"
  "func"
  "method"
  "converter"
  "iterator"
  "macro"
  "template"
] @keyword.function

; =============================================================================
; @keyword.operator    ; operators that are English words (e.g. `and` / `or`)

[
  "and"
  "or"
  "xor"
  "not"
  "div"
  "mod"
  "shl"
  "shr"
  "from"
  "as"
  "of"
  "in"
  "notin"
  "is"
  "isnot"
  "cast"
] @keyword.operator

; =============================================================================
; @keyword.return      ; keywords like `return` and `yield`

[
  "return"
  "yield"
] @keyword.return

; =============================================================================
; @conditional         ; keywords related to conditionals (e.g. `if` / `else`)

[
  "if"
  "when"
  "case"
  "elif"
  "else"
] @conditional

(of_branch "of" @conditional)

; =============================================================================
; @conditional.ternary ; ternary operator (e.g. `?` / `:`)

; =============================================================================
; @repeat              ; keywords related to loops (e.g. `for` / `while`)

[
  "for"
  "while"
  "continue"
  "break"
] @repeat

(for "in" @repeat)

; =============================================================================
; @debug               ; keywords related to debugging
; unused

; =============================================================================
; @label               ; GOTO and other labels (e.g. `label:` in C)

(block label: (_) @label)

; =============================================================================
; @include             ; keywords for including modules (e.g. `import` / `from` in Python)

[
  "import"
  "include"
  "export"
] @include

(import_from_statement "from" @include)

(except_clause "except" @include)

; =============================================================================
; @exception           ; keywords related to exceptions (e.g. `throw` / `catch`)

[
  "try"
  "except"
  "finally"
  "raise"
] @exception

; =============================================================================
; @type            ; type or class definitions and annotations

; generic types when declaring
((generic_parameter_list
  (parameter_declaration
    (symbol_declaration_list
      (symbol_declaration name: (_) @type)))))

; generic types when calling
(call
  function: (bracket_expression
    right: (argument_list (_) @type)))
; NOTE: this also falsy matches 
; when accessing and directly call elements from an array of routines
; eg `array_of_routines[index](arguments)

(type_symbol_declaration name: (_) @type)

; right side of `is` operator is always type
(infix_expression operator: [ "is" "isnot" ] right: (_) @type)

; `except module.exception[gen_type] as variable:`
(except_branch values: (expression_list
  [
    (identifier) @type
    (infix_expression
      left: (_) @type
      operator: "as")
  ]))
; TODO: is there another way?

; for inline tuple types
; `type a = tuple[a: int]`
(tuple_type
  "tuple" @type
  (field_declaration_list))
; NOTE: this is consistent with othere builtin types like `seq[int]`
; but inconsistent with multiline tuple declaration,
; where `tuple` is captured as @keyword

; =============================================================================
; @type.builtin    ; built-in types


; ; overrule identifiers in type_expression if they match builtin type string
; (
;  [
;    (type_expression (identifier) @type.builtin)
;    (type_expression (_ (identifier) @type.builtin))
;    (type_expression (_ (_ (identifier) @type.builtin)))
;    (type_expression (_ (_ (_ (identifier) @type.builtin))))
;    (type_expression (_ (_ (_ (_ (identifier) @type.builtin)))))
;    (type_expression (_ (_ (_ (_ (_ (identifier) @type.builtin))))))
;    (type_expression (_ (_ (_ (_ (_ (_ (identifier) @type.builtin)))))))
;    (type_expression (_ (_ (_ (_ (_ (_ (_ (identifier) @type.builtin))))))))
;    (type_expression (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type.builtin)))))))))
;    (type_expression (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type.builtin))))))))))
;    (type_expression (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type.builtin)))))))))))
;    (type_expression (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type.builtin))))))))))))
;    (type_expression (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type.builtin)))))))))))))
;    (type_expression (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type.builtin))))))))))))))
;  ]
;  (#any-of? @type.builtin
;   "byte"
;   "int"
;   "int8"
;   "int16"
;   "int32"
;   "int64"
;   "uint"
;   "uint8"
;   "uint16"
;   "uint32"
;   "uint64"
;   "float"
;   "float32"
;   "float64"
;   "bool"
;   "char"
;   "string"
;   "cstring"
;   "ref"
;   "ptr"
;   "range"
;   "array"
;   "seq"
;   "openArray"
;   "varargs"
;   "set"
;   "itarable"
;   "typedesc"
;   "untyped"
;   "auto"
;   "pointer"
;   "void"
;   "Rune"
;   "UncheckedArray"
;   "RootObj"
;   "SomeFloat"
;   "SomeInteger"
;   "SomeOrdinal"
;   "SomeNumber"
;   "SomeSignedInt"
;   "SomeUnsignedInt"
;   "Natural"
;   "Ordinal"
;   "Positive"
;  ))
; ; TODO: is there another way?

; =============================================================================
; @type.definition ; type definitions (e.g. `typedef` in C)

(type_section ("type") @type.definition)

; =============================================================================
; @type.qualifier  ; type qualifiers (e.g. `const`)

(var_type "var" @type.qualifier)

(out_type "out" @type.qualifier)

(distinct_type "distinct" @type.qualifier)

(ref_type "ref" @type.qualifier)

(pointer_type "ptr" @type.qualifier)

; =============================================================================
; @storageclass    ; modifiers that affect storage in memory or life-time
; unused

; =============================================================================
; @attribute       ; attribute annotations (e.g. Python decorators)
; unused

; =============================================================================
; @field           ; object and struct fields

; fields in object/tuple declaration
(field_declaration
  (symbol_declaration_list
    (symbol_declaration name: (_) @field)))

; fields in object construction
(call
  (argument_list
    (colon_expression left: (_) @field)))

; fields in tuple construction
(tuple_construction
  (colon_expression left: (_) @field))

; ; fields in assignments
; (assignment left: [
;   (dot_expression right: (_) @field)
;   (_ (dot_expression right: (_) @field))
;   (_ (_ (dot_expression right: (_) @field)))
;   (_ (_ (_ (dot_expression right: (_) @field))))
;   (_ (_ (_ (_ (dot_expression right: (_) @field)))))
;   ])
; NOTE: inaccurate, since it can be dot_expression, bracket_expression and calls
; in various combinations. Calls should not be matched as fields.

; (dot_expression
;   right: (identifier) @property)
; NOTE: inaccurate, since dot_expression can also be
; `first_arg.function`
; `external_module.identifier_from_module`
; `enum_type.enum_element`
; and probably more

; =============================================================================
; @property        ; similar to `@field`
; unused

; =============================================================================
; @variable         ; various variable names

; overrule identifiers in proc type pragmas
; `type a = proc() {.x.a: [y].}`
(proc_type (pragma_list [
  (identifier) @variable
  (_ (identifier) @variable)
  (_ (_ (identifier) @variable))
  (_ (_ (_ (identifier) @variable)))
  (_ (_ (_ (_ (identifier) @variable))))
  (_ (_ (_ (_ (_ (identifier) @variable)))))
  (_ (_ (_ (_ (_ (_ (identifier) @variable))))))
  (_ (_ (_ (_ (_ (_ (_ (identifier) @variable)))))))
  (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @variable))))))))
]))
; TODO: is there another way?

; =============================================================================
; @variable.builtin ; built-in variable names (e.g. `this`)

(blank_identifier) @variable.builtin

((identifier) @variable.builtin
  (#eq? @variable.builtin "result"))

; =============================================================================
; @constant         ; constant identifiers

; identifiers in "case" "of" branches have to be enums
(case
  (of_branch values:
    (expression_list
      [
        (identifier) @constant
        (_ (identifier) @constant)
        (_ (_ (identifier) @constant))
      ])))

; in variant objects with "case" "of"
(variant_declaration
  (of_branch values:
    (expression_list
      [
        (identifier) @constant
        (_ (identifier) @constant)
        (_ (_ (identifier) @constant))
      ])))

; enum elements are constants
(enum_field_declaration
  (symbol_declaration name: (_) @constant))

; constants/enums in array construction
(array_construction
  (colon_expression left: [
      (identifier) @constant
      (_ (identifier) @constant)
      (_ (_ (identifier) @constant))
    ]))

; constant declaration
(const_section
  (variable_declaration
    (symbol_declaration_list
      (symbol_declaration name: (_) @constant))))

; ranges in generic types and in calls with generic types
; array[enum1..enum5, int]
; range[nkAdd..nkSub](unknownKind)
(bracket_expression 
  right: (argument_list 
    (infix_expression 
      left: (_) @constant
      operator: (operator) @operator 
      (#eq? @operator "..")
      right: (_) @constant)))

; =============================================================================
; @constant.builtin ; built-in constant values

; ; NaN
; ((identifier) @constant.builtin
;   (#match? @constant.builtin "^N[aA][nN]$"))
; ; Inf
; ((identifier) @constant.builtin
;   (#match? @constant.builtin "^I[nN][fF]$"))
; ; NegInf
; ((identifier) @constant.builtin
;   (#match? @constant.builtin "^N[eE][gG][iI][nN][fF]$"))
;
; ; stdin
; ((identifier) @constant.builtin
;   (#match? @constant.builtin "^s[tT][dD][iI][nN]$"))
; ; stdout
; ((identifier) @constant.builtin
;   (#match? @constant.builtin "^s[tT][dD][oO][uU][tT]$"))
; ; stderr
; ((identifier) @constant.builtin
;   (#match? @constant.builtin "^s[tT][dD][eE][rR][rR]$"))

(nil_literal) @constant.builtin

; =============================================================================
; @constant.macro   ; constants defined by the preprocessor
; unused

; =============================================================================
; @namespace        ; modules or namespaces
; unused

; =============================================================================
; @symbol           ; symbols or atoms
; unused

; =============================================================================
; @operator              ; symbolic operators (e.g. `+` / `*`)

; NOTE: it's down here, to overrule @type in calls with generics
(operator) @operator

[ "=" ] @operator

; =============================================================================
; overrule things

; left identifier in dot_expression
(dot_expression left: [
    (identifier) @none
    (accent_quoted (identifier) @none)
  ])
; NOTE: it can't be know what the left identifier is, so better leave it alone
; for consistency

; =============================================================================
; highlight exceptions for injection queries

; regex
(generalized_string
  (identifier) @_string_prefix (#any-of? @_string_prefix "re" "rex") .
  [
    (string_literal "\"" @string . "\"" @string)
    (string_literal "\"\"\"" @string . "\"\"\"" @string)
  ] @none)

; sql
(generalized_string
  (identifier) @_string_prefix (#eq? @_string_prefix "sql") .
  [
    (string_literal "\"" @string . "\"" @string)
    (string_literal "\"\"\"" @string . "\"\"\"" @string)
  ] @none)

; format string
(generalized_string
  (identifier) @_string_prefix (#eq? @_string_prefix "fmt") .
  (string_literal) @none)

(prefix_expression
  operator: (operator) @_string_prefix (#eq? @_string_prefix "&") .
  (string_literal) @none)
; NOTE: the whole string including quotes is sent to the nim_format_string parser
; and it captures everything outside as @string again,
; so no overruling the quotes necessary

; emit pragma
((comment) .
  (pragma_statement
    (pragma_list
      (colon_expression
        left: (identifier) @emit (#eq? @emit "emit")
        right: [
          (string_literal "\"" @string . "\"" @string)
          (string_literal "\"\"\"" @string . "\"\"\"" @string)
        ] @none))))

; doc comments
((documentation_comment) @none
 (#offset! @none 0 2 0 0)) ; leading `##`

((block_documentation_comment) @none
 (#offset! @none 0 3 0 -3)) ; wrapping `##[` `]##`

