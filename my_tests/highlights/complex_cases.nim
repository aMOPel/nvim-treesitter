# many of these code snippets come from
# https://nim-lang.org/docs/manual.html
# Authors: Andreas Rumpf, Zahary Karadjov

# enum
type 
  Enum1* {.pure.} = enum one, two, three
  Enum2* = enum 
    one="hi", two=4, three=(5,"test")

discard Enum1.one

# array
type 
  Array1* = array[0..1, seq[array[Enum1, int]]]
  Array2* = array[Enum1.one..Enum1.two, int]
const arr1*: Array1 = [@[], @[[Enum1.one:1,Enum1.two:2,3]]]
discard arr1[0]
const test = 3
let arr2* = [1:"hi", 2:"bye", test:"shy"]
let arr3* = [1..5, 6..8]

# varargs
# BUG: `$` falsly highlighted as @type, but impossible to distinguish
proc myWriteln*(f: File, a: varargs[string, `$`]) = discard

# tuple
type
  Person = tuple[name: string, age: int]
  OneField* = tuple
    name: string

let one_field*: (int,) = (name:5,)
var person*: Person
person = (name: "Peter", age: 30)
assert person.name == "Peter"
person = ("Peter", 30)
assert person[0] == "Peter"
assert Person is (string, int)
assert (string, int) is Person
assert person is Person
assert person isnot Person
assert Person isnot tuple[other: string, age: int]

proc returnsNestedTuple(): (int, (int, int), int, int) = (4, (5, 7), 2, 3)
let (t1*, (_, t2*), _, t3*) = returnsNestedTuple()

# object
type
  Person1* = object of RootObj
    name*: string = "default"
    age: int = 5
  Pref* = ref Person1
  GenObj[T] = object
    a, b, c*: T
  `12`* = object
    a: int

discard `12`(a:5)

import ../module_tests
let
  p1 = Person1(name:"a", age:5)
  # BUG: not recognized as constructor
  A* = GenObj[int](a:5)
  B* = module_tests.ExternalGenObj[int](a:5)

type
  NodeKind = enum  # the different node types
    nkInt,          # a leaf with an integer value
    nkFloat,        # a leaf with a float value
    nkString,       # a leaf with a string value
    nkAdd,          # an addition
    nkSub,          # a subtraction
    nkIf            # an if statement
  Node = ref NodeObj
  NodeObj = object
    case kind: NodeKind  # the `kind` field is the discriminator
    of nkInt: intVal: int
    of nkFloat: floatVal: float
    of nkString: strVal: string
    of nkAdd, nkSub:
      leftOp, rightOp: seq[Node]
    of nkIf:
      condition*, thenPart, elsePart: Node
  NodeObj1* = object
    when true:
      a: int
    else:
      b: int

var x* = Node(kind: nkAdd, 
  leftOp: @[
    Node(kind: nkAdd, leftOp: @[Node(kind: nkInt, intVal: 2)],
                      rightOp: @[Node(kind: nkInt, intVal: 2)])
    ],
  rightOp: @[Node(kind: nkInt, intVal: 2)])

x.leftOp[0].leftOp[0].kind = nkInt

proc getIdentity(x: var Node): var Node =
  result = x

x.getIdentity().kind = nkInt
  

# generics
let
  C* = GenObj[seq[array[Enum1.one..Enum1.two, seq[array[-5.. -1, int]]]]](a: @[])
  unknownKindBounded* = range[Enum1.one..Enum1.two](Enum1.one)

# sets
var set1*: set[char]
set1 = {'a'..'z', '0'..'9'}
discard 'a' in set1

# pointer and refs
var p2 = (ref Person1)(name:"a", age:5)
p2[] = p1 # ref accessor
p2 = nil
var d* = cast[ptr Person1](alloc0(sizeof(Person1)))

# procs
type Proc1* = proc(a, b: var int, c: GenObj[seq[array[0..5, int]]]): void
proc proc1*[T: int|float, S](a, b: T, _: seq[S]): var GenObj[void] {.nimcall.} = 
  return result
discard proc1(5, 1, @["hi"])
discard proc1[int, string](5, 1, @["hi"])

module_tests.externalGenProc1[int](5)

# type qualifiers
type 
  TypeQualifiers* = distinct ref ptr int
  Dollar = distinct int
  Euro* {.borrow: `.`.} = distinct Dollar

proc `+` (x, y: Dollar): Dollar =
  result = Dollar(int(x) + int(y))

let cash = 15.Dollar
discard cash + 12.Dollar

# template
template additive(typ: typedesc) =
  proc `+` *(x, y: typ): typ {.borrow.}

# iterator
iterator iota(n: int): int =
  for i in 0..<n: yield i

# statement list expression
let stmtList = (( discard; 
discard; 5 ))

# static
static:
  discard

echo static(123)

# const
const roundPi = 3.1415

# generalized string
proc `0`(`a`: string) = discard
`0`"hi"

