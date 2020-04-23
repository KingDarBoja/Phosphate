# Journey on Nim

## Partial-like function in Nim (By Varriount)

> Their are two ways. The fastest is to create a closure function, the most efficient is to create a callable type containing the parameters required.

```nim
proc addTriple(a, b, c: int): int =
result = a+b+c

when true:
  ## Implementation with closures ##
  proc wrapAddTriple(a, b, c: int): auto =
    result = proc(): auto = addTriple(a, b, c)

  when isMainModule:
    var wrapper = wrapAddTriple(1, 2, 3)
    echo wrapper()

else:
  ## Implementation with explicit types
  ## This is roughly what the compiler does to implement the above
  ## logic, with the exception that the compiler adds a field to
  ## `addTripleWrapper` that contains the procedural type
  ## `proc(w: addTripleWrapper): int`. This is because it is
  ## possible for multiple closure values to contain different procedures.
  type addTripleWrapper = ref object
    a, b, c: int

  proc wrapAddTriple(a, b, c: int): auto =
    result = addTripleWrapper(a: a, b: b, c: c)

  proc call(w: addTripleWrapper): int = addTriple(w.a, w.b, w.c)

  when isMainModule:
    var wrapper = wrapAddTriple(1, 2, 3)
    echo wrapper.call()
```

> That is a brief snippet which shows (generally) what the compiler translates closures to.
Note that is an estimation. It gets a lot more intricate when you have scenarios involving closures that share the same parent scope.