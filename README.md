# Implementation of NFA Regular Expression Matcher in Scala

## usage
In scala console
```
scala> Regexp.compile("(foo|bar|baz)+").matchString("")
res0: Boolean = false

scala> Regexp.compile("(foo|bar|baz)+").matchString("foofoobazbazbarfoo")
res1: Boolean = true

scala> Regexp.compile("(foo|bar|baz)+").matchString("ABCDEFG_foofoobazbazbarfoo")
res2: Boolean = true
```

## rule

| expr | description |
| -- | -- |
| . | matches any char |
| e+  | matches 0 or more times |
| e*  | matches 1 or more times |
| e?  | matches 0 or 1 times |
| e1 \| e2 | matches either a match to e1 or a match to e2 |
| (e) | groups e |
