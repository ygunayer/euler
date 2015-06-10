My humble solutions for [Project Euler](https://projecteuler.net/) problems, written in Scala. Since these are just my own solutions they're probably not the most optimal ones.

For simplicity, solutions are implemented as unit tests which can be run individually, and are grouped into 10-problem files so as not to clutter the containing folder. All solutions also provide the problem texts as comments on the class definitions.

### Running

Run the `sbt` prompt and then type:

* `test` to run all solutions
* `test-only euler.ProblemN` where N is the problem number to run a specific solution

The resulting output will be in the following format:

##### Example

```
> test-only euler.Problem1
```

```
[info] Problem1:
[info] - Problem #1
[info]   + Result: 234168 (0 ms)
```

### License

[Public Domain](http://choosealicense.com/licenses/unlicense/), or in other words, do whatever you want with it, but I provide no warranties of any kind so I can't be held responsible for any damages it may cause.
