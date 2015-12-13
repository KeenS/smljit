# SML JIT
A JIT Engine fo SML#

# Status
Under construction


# What does it look like?

``` sml
_require "basis.smi"
_require "jit.smi"
_require "att.smi"
```

``` sml
open ATT
val f = Jit.jit [
  xorl eax eax,
  addl ($1) eax,
  ret
]: _import (int) -> int
val () = print ((Int.toString (f 1)) ^ "\n")
```
