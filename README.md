# purescript-sequences

An efficient, general-purpose sequence type for PureScript.

The implementation uses 2-3 finger trees annotated with sizes, as described in
the paper [_Finger Trees: A Simple General-Purpose Data Structure_][1], Ralf
Hinze and Ross Paterson (2006).

## Why not just use Arrays?

JavaScript's Array type is designed for use in an imperative programming
environment, where mutation is rife. This means that reusing them is usually
not possible. For example:

```javascript
var as = [1,2,3]
var bs = as.concat([4,5,6])
bs[0] = 10
```

`as[0]` should still be 1 after executing these commands. Therefore, `concat`
has to copy the whole array, and is therefore O(this.length + n), where n is
the length of the argument.

However in PureScript, values are immutable (ignoring the FFI). So we may take
advantage of this by writing functions that reuse parts of data structures
where possible. Sequences are one such structure - in this case, the amortized
complexity of `concat` is reduced to O(log(min(n1, n2))), where n1 and n2 are
the lengths of the arguments.

Amortized complexities of other operations:

|               | Native array | Sequence            |
|---------------|--------------|---------------------|
| push()        | O(1)         | O(1)                |
| pop()         | O(1)         | O(1)                |
| shift()       | O(n)         | O(1)                |
| unshift()     | O(n)         | O(1)                |
| get(i)/set(i) | O(1)         | O(log(min(i, n-1))) |
| splitAt(i)    | O(n)         | O(log(min(i, n-1))) |

[1]: http://staff.city.ac.uk/~ross/papers/FingerTree.pdf
