; SPDX-FileCopyrightText: 2023 Leorize <leorize+oss@disroot.org>, aMOPel <>
; SPDX-License-Identifier: MPL-2.0

; Reasoning:

; # No `#set! "priority"`
;
; I avoided using `#set! "priority"`, since it's nvim specific and
; the ts cli doesn't support it.
; ~~This way, these queries could be tested with the cli.~~
;
; This decision entailed that I had to explicitly capture (identifier)s
; and could not used (_) @capture,
; since the catch all rule for @variable is most specific and will overrule
; anything not directly applied to (identifier)s.
;
; Another problem is the structure of the grammar.
; There are various grammar rules that allow for (infinitely) deep nesting
; of identifiers in important places, like @type captures.
; Among others (bracket_expression) and (dot_expression).
; Because of this, there are some places where I had to use a structure like
;
; (type_expression [
;   (identifier) @type
;   (_ (identifier) @type)
;   (_ (_ (identifier) @type))
;   (_ (_ (_ (identifier) @type)))
;   ...
;
; to catch those deeply nested identifiers.
; This means, highlighting will break down for identifiers that nest deeper
; than what I have provide queries for.
;
; I would have had to use this structure too, if I would have used
; `#set! priority`. Then I would have used `(type_expression) @type`
; but would have to overrule identifiers in pragmas of proc types,
; since those are (identifier)s and would otherwise be captured as @type

; # builtins
; The captured builtin constants, types and functions are incomplete.
; There is a vast amount and the question is where to draw the line.
;
; Also I decided against the more accurrate method of capturing builtin
; identifiers with case insensitive regex matching.
; I don't believe the performance cost of the many regexes is worth the gain.

; # injections

; =============================================================================
; catch all rules

((identifier) @variable (#set! "priority" 95))

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
; @operator              ; symbolic operators (e.g. `+` / `*`)

(operator) @operator

; =============================================================================
; @punctuation.delimiter ; delimiters (e.g. `;` / `.` / `,`)

[ "." ";" "," ":" "=" ] @punctuation.delimiter

; needs to be after @punctuation.delimiter
(assignment "=" @operator)

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

(proc_declaration
  name: [
    (identifier) @function
    (accent_quoted (identifier) @function)
    (exported_symbol (identifier) @function)
    (exported_symbol (accent_quoted (identifier) @function))
  ])

(func_declaration
  name: [
    (identifier) @function
    (accent_quoted (identifier) @function)
    (exported_symbol (identifier) @function)
    (exported_symbol (accent_quoted (identifier) @function))
  ])

(iterator_declaration
  name: [
    (identifier) @function
    (accent_quoted (identifier) @function)
    (exported_symbol (identifier) @function)
    (exported_symbol (accent_quoted (identifier) @function))
  ])

(converter_declaration
  name: [
    (identifier) @function
    (accent_quoted (identifier) @function)
    (exported_symbol (identifier) @function)
    (exported_symbol (accent_quoted (identifier) @function))
  ])

; =============================================================================
; @function.call    ; function calls

(call
  function: [
    (identifier) @function.call
    (accent_quoted (identifier) @function.call)
    ; generic types
    (bracket_expression left: (identifier) @function.call)
    (bracket_expression left: (accent_quoted (identifier) @function.call))
    ; dot accessor
    (dot_expression right: (identifier) @function.call)
    (dot_expression right: (accent_quoted (identifier) @function.call))
    ; both
    (bracket_expression left:
      (dot_expression right: (identifier) @function.call))
    (bracket_expression left:
      (dot_expression right: (accent_quoted (identifier) @function.call)))
  ])

; generalized_string is a function call
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
;   (#any-of? @function.builtin
;    "echo"
;    "new"
;    "default"
;    "quit"
;    "typeof"
;    ))
; ; NOTE: there are too many builtin functions. where to draw the line?

; =============================================================================
; @function.macro   ; preprocessor macros

(template_declaration
  name: [
    (identifier) @function
    (accent_quoted (identifier) @function)
    (exported_symbol (identifier) @function)
    (exported_symbol (accent_quoted (identifier) @function))
  ])

(macro_declaration
  name: [
    (identifier) @function
    (accent_quoted (identifier) @function)
    (exported_symbol (identifier) @function)
    (exported_symbol (accent_quoted (identifier) @function))
  ])

; =============================================================================
; @method           ; method definitions

(method_declaration
  name: [
    (identifier) @method
    (accent_quoted (identifier) @method)
    (exported_symbol (identifier) @method)
    (exported_symbol (accent_quoted (identifier) @method))
  ])

; =============================================================================
; @method.call      ; method calls
; unused

; =============================================================================
; @constructor      ; constructor calls and definitions

(call
  function: [
    (identifier) @constructor
    (accent_quoted (identifier) @constructor)
    ; generic types
    (bracket_expression left: (identifier) @constructor)
    (bracket_expression left: (accent_quoted (identifier) @constructor))
    ; dot accessor
    (dot_expression right: (identifier) @constructor)
    (dot_expression right: (accent_quoted (identifier) @constructor))
    ; both
    (bracket_expression left:
      (dot_expression right: (identifier) @constructor))
    (bracket_expression left:
      (dot_expression right: (accent_quoted (identifier) @constructor)))
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
  (equal_expression
    left: [
      (identifier) @parameter
      (accent_quoted (identifier) @parameter)
    ]))

; parameters in function declaration
(parameter_declaration_list
  (parameter_declaration
    (symbol_declaration_list
      (symbol_declaration
        name: [
          (identifier) @parameter
          (accent_quoted (identifier) @parameter)
        ]))))

; =============================================================================
; @keyword             ; various keywords

; unhandled but reserved keywords
; end
; interface

; static expression
; addr operator
((call
  function: (identifier) @keyword)
  (#any-of? @keyword "static" "addr"))

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

(block
  label: [
    (identifier) @label
    (accent_quoted (identifier) @label)
  ])

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

; (type_expression) @type
;
; (pragma_list [
;   (identifier) @variable
;   (_ (identifier) @variable)
;   (_ (_ (identifier) @variable))
;   (_ (_ (_ (identifier) @variable)))
;   (_ (_ (_ (_ (identifier) @variable))))
;   (_ (_ (_ (_ (_ (identifier) @variable)))))
;   (_ (_ (_ (_ (_ (_ (identifier) @variable))))))
;   (_ (_ (_ (_ (_ (_ (_ (identifier) @variable)))))))
;   (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @variable))))))))
;   (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @variable)))))))))
;   (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @variable))))))))))
;   (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @variable)))))))))))
;   (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @variable))))))))))))
; ])


((identifier) @type
  (#has-ancestor? @type type_expression)
  (#not-has-ancestor? @type pragma_list)
  (#set! "priority" 98) ; for parameters in proc_type
  )

; (type_expression [
;   (identifier) @type
;   (_ (identifier) @type)
;   (_ (_ (identifier) @type))
;   (_ (_ (_ (identifier) @type)))
;   (_ (_ (_ (_ (identifier) @type))))
;   (_ (_ (_ (_ (_ (identifier) @type)))))
;   (_ (_ (_ (_ (_ (_ (identifier) @type))))))
;   (_ (_ (_ (_ (_ (_ (_ (identifier) @type)))))))
;   (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type))))))))
;   (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type)))))))))
;   (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type))))))))))
;   (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type)))))))))))
;   (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type))))))))))))
; ])

; generic types when declaring
((generic_parameter_list
  (parameter_declaration
    (symbol_declaration_list
      (symbol_declaration
        name: [
          (identifier) @type
          (accent_quoted (identifier) @type)
        ])))))

; generic types when calling
(call
  function: (bracket_expression
    right: (argument_list [
      (identifier) @type
      (_ (identifier) @type)
      (_ (_ (identifier) @type))
      (_ (_ (_ (identifier) @type)))
      (_ (_ (_ (_ (identifier) @type))))
      (_ (_ (_ (_ (_ (identifier) @type)))))
      (_ (_ (_ (_ (_ (_ (identifier) @type))))))
      (_ (_ (_ (_ (_ (_ (_ (identifier) @type)))))))
      (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type))))))))
      (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type)))))))))
      (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type))))))))))
      (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type)))))))))))
      (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type))))))))))))
    ])))
; NOTE: this also falsely matches
; when accessing and directly call elements from an array of routines
; eg `array_of_routines[index](arguments)

; left side of type declaration
(type_symbol_declaration
  name: [
    (identifier) @type
    (accent_quoted (identifier) @type)
    (exported_symbol (identifier) @type)
    (exported_symbol (accent_quoted (identifier) @type))
  ])

; right side of `is` operator is always type
(infix_expression
  operator: [ "is" "isnot" ]
  right: [
    (identifier) @type
    (_ (identifier) @type)
    (_ (_ (identifier) @type))
    (_ (_ (_ (identifier) @type)))
    (_ (_ (_ (_ (identifier) @type))))
    (_ (_ (_ (_ (_ (identifier) @type)))))
    (_ (_ (_ (_ (_ (_ (identifier) @type))))))
    (_ (_ (_ (_ (_ (_ (_ (identifier) @type)))))))
    (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type))))))))
    (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type)))))))))
    (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type))))))))))
    (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type)))))))))))
    (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type))))))))))))
  ])

; except branch always contains types of errors
; Eg: `except module.exception[gen_type]:`
; Or `except module.exception[gen_type] as variable:`
(except_branch
  values: (expression_list [
    (identifier) @type
    (accent_quoted (identifier) @type)
    (infix_expression
      left: [
        (identifier) @type
        (_ (identifier) @type)
        (_ (_ (identifier) @type))
        (_ (_ (_ (identifier) @type)))
        (_ (_ (_ (_ (identifier) @type))))
        (_ (_ (_ (_ (_ (identifier) @type)))))
        (_ (_ (_ (_ (_ (_ (identifier) @type))))))
        (_ (_ (_ (_ (_ (_ (_ (identifier) @type)))))))
        (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type))))))))
        (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type)))))))))
        (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type))))))))))
        (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type)))))))))))
        (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type))))))))))))
      ]
      operator: "as")
  ]))

; for inline tuple types
; `type a = tuple[a: int]`
(tuple_type
  "tuple" @type
  (field_declaration_list))
; NOTE: this is consistent with other generic types like `seq[int]`
; but inconsistent with multiline tuple declaration,
; where `tuple` is captured as @keyword

; =============================================================================
; @type.builtin    ; built-in types

; NOTE: to make it consistent, I would also need all the @type queries
; for the @type.builtin again. Not worth it.

; ; overrule identifiers in type_expression if they match builtin type string
; (type_expression
;  [
;    (identifier) @type.builtin
;    (_ (identifier) @type.builtin)
;    (_ (_ (identifier) @type.builtin))
;    (_ (_ (_ (identifier) @type.builtin)))
;    (_ (_ (_ (_ (identifier) @type.builtin))))
;    (_ (_ (_ (_ (_ (identifier) @type.builtin)))))
;    (_ (_ (_ (_ (_ (_ (identifier) @type.builtin))))))
;    (_ (_ (_ (_ (_ (_ (_ (identifier) @type.builtin)))))))
;    (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type.builtin))))))))
;    (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type.builtin)))))))))
;    (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type.builtin))))))))))
;    (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type.builtin)))))))))))
;    (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (_ (identifier) @type.builtin))))))))))))
;  ]
;  (#any-of? @type.builtin
;   "bool"
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
;   "char"
;   "string"
;   "cstring"
;   "range"
;   "array"
;   "seq"
;   "set"
;   "openArray"
;   "varargs"
;   "itarable"
;   "typedesc"
;   "typed"
;   "untyped"
;   "auto"
;   "pointer"
;   "void"
;  ))

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
    (symbol_declaration
      name: [
        (identifier) @field
        (accent_quoted (identifier) @field)
        (exported_symbol (identifier) @field)
        (exported_symbol (accent_quoted (identifier) @field))
      ])))

; fields in object construction
(call
  (argument_list
    (colon_expression
      left: [
        (identifier) @field
        (accent_quoted (identifier) @field)
      ])))

; fields in tuple construction
(tuple_construction
  (colon_expression
    left: [
      (identifier) @field
      (accent_quoted (identifier) @field)
    ]))

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

; ; fields with dot accessor syntax
; (dot_expression
;   right: (identifier) @field)
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
    (expression_list [
      (identifier) @constant
      (_ (identifier) @constant)
      (_ (_ (identifier) @constant))
      (_ (_ (_ (identifier) @constant)))
      (_ (_ (_ (_ (identifier) @constant))))
      (_ (_ (_ (_ (_ (identifier) @constant)))))
      (_ (_ (_ (_ (_ (_ (identifier) @constant))))))
    ])))

; in variant objects with "case" "of"
(variant_declaration
  (of_branch values:
    (expression_list [
      (identifier) @constant
      (_ (identifier) @constant)
      (_ (_ (identifier) @constant))
      (_ (_ (_ (identifier) @constant)))
      (_ (_ (_ (_ (identifier) @constant))))
      (_ (_ (_ (_ (_ (identifier) @constant)))))
      (_ (_ (_ (_ (_ (_ (identifier) @constant))))))
    ])))

; enum declaration
(enum_field_declaration
  (symbol_declaration
    name: [
      (identifier) @constant
      (accent_quoted (identifier) @constant)
    ]))

; constants/enums in array construction
(array_construction
  (colon_expression
    left: [
      (identifier) @constant
      (_ (identifier) @constant)
      (_ (_ (identifier) @constant))
      (_ (_ (_ (identifier) @constant)))
      (_ (_ (_ (_ (identifier) @constant))))
      (_ (_ (_ (_ (_ (identifier) @constant)))))
      (_ (_ (_ (_ (_ (_ (identifier) @constant))))))
    ]))

; constant declaration
(const_section
  (variable_declaration
    (symbol_declaration_list
      (symbol_declaration
        name: [
          (identifier) @constant
          (accent_quoted (identifier) @constant)
          (exported_symbol (identifier) @constant)
          (exported_symbol (accent_quoted (identifier) @constant))
        ]))))

; ; ranges in generic types
; ; array[enum1..enum5, int]
; (type_expression
;   (bracket_expression
;     right: (argument_list
;       (infix_expression
;         left: [
;           (identifier) @constant
;           (_ (identifier) @constant)
;           (_ (_ (identifier) @constant))
;           (_ (_ (_ (identifier) @constant)))
;           (_ (_ (_ (_ (identifier) @constant))))
;           (_ (_ (_ (_ (_ (identifier) @constant)))))
;           (_ (_ (_ (_ (_ (_ (identifier) @constant))))))
;         ]?
;         operator: (operator) @operator
;         (#eq? @operator "..")
;         right: [
;           (identifier) @constant
;           (_ (identifier) @constant)
;           (_ (_ (identifier) @constant))
;           (_ (_ (_ (identifier) @constant)))
;           (_ (_ (_ (_ (identifier) @constant))))
;           (_ (_ (_ (_ (_ (identifier) @constant)))))
;           (_ (_ (_ (_ (_ (_ (identifier) @constant))))))
;         ]?))))
;
; ; ranges in in calls with generic types
; ; gen_proc[low..high]()
; (call
;   function: (bracket_expression
;     right: (argument_list
;       (infix_expression
;         left: [
;           (identifier) @constant
;           (_ (identifier) @constant)
;           (_ (_ (identifier) @constant))
;           (_ (_ (_ (identifier) @constant)))
;           (_ (_ (_ (_ (identifier) @constant))))
;           (_ (_ (_ (_ (_ (identifier) @constant)))))
;           (_ (_ (_ (_ (_ (_ (identifier) @constant))))))
;         ]?
;         operator: (operator) @operator
;         (#eq? @operator "..")
;         right: [
;           (identifier) @constant
;           (_ (identifier) @constant)
;           (_ (_ (identifier) @constant))
;           (_ (_ (_ (identifier) @constant)))
;           (_ (_ (_ (_ (identifier) @constant))))
;           (_ (_ (_ (_ (_ (identifier) @constant)))))
;           (_ (_ (_ (_ (_ (_ (identifier) @constant))))))
;         ]?))))

; =============================================================================
; @constant.builtin ; built-in constant values

; ((identifier) @constant.builtin
;   (#any-of? @constant.builtin "NaN" "Inf" "NegInf" "stdin" "stdout" "stderr"))

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
; overrule things

; left identifier in dot_expression
(dot_expression left: [
  (identifier) @none
  (accent_quoted (identifier) @none)
] (#set! "priority" 110))
; NOTE: it can't be know what the left identifier is, so better leave it alone
; for consistency

; =============================================================================
; highlight exceptions for injection queries

; To make injections look better, these queries
; capture the content of (comment)s and (string_literal)s as @none.
; If this is not done, every token, the injected language doesn't capture
; will be highlighted as @comment or @string.
; There is still the caveat, that highlighting in the injected region
; will be inconsistent under specific circumstances.

; ; regex
; (generalized_string
;   (identifier) @_string_prefix (#any-of? @_string_prefix "re" "rex") .
;   [
;     (string_literal "\"" @string . "\"" @string)
;     (string_literal "\"\"\"" @string . "\"\"\"" @string)
;   ] @none)
;
; ; sql
; (generalized_string
;   (identifier) @_string_prefix (#eq? @_string_prefix "sql") .
;   [
;     (string_literal "\"" @string . "\"" @string)
;     (string_literal "\"\"\"" @string . "\"\"\"" @string)
;   ] @none)
;
; ; format string
; (generalized_string
;   (identifier) @_string_prefix (#eq? @_string_prefix "fmt") .
;   (string_literal) @none)
;
; (prefix_expression
;   operator: (operator) @_string_prefix (#eq? @_string_prefix "&") .
;   (string_literal) @none)
; ; NOTE: the whole string including quotes is sent to the nim_format_string parser
; ; and it captures everything outside as @string again,
; ; so no overruling the quotes necessary
;
; ; emit pragma
; ((comment) .
;   (pragma_statement
;     (pragma_list
;       (colon_expression
;         left: (identifier) @emit (#eq? @emit "emit")
;         right: [
;           (string_literal "\"" @string . "\"" @string)
;           (string_literal "\"\"\"" @string . "\"\"\"" @string)
;         ] @none))))
;
; ; doc comments
; ((documentation_comment) @none
;  (#offset! @none 0 2 0 0)) ; leading `##`
;
; ((block_documentation_comment) @none
;  (#offset! @none 0 3 0 -3)) ; wrapping `##[` `]##`
