# many of these code snippets come from
# https://nim-lang.org/docs/manual.html
# Authors: Andreas Rumpf, Zahary Karadjov

# builtin simple
let 
  a* = (0b01010, 0o01234, 0xadf01234, 5, 5.5, '\n', "hi\n", r"hi\n", """
  hi
  hi
  """, true, false, on, off)
