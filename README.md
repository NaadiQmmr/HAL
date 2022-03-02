# HAL

To run the project, run :
```bash
make
```
You will find our compiled program named `hal`.
You can either run the binary with your scheme files as arguments, or you can add the `-i` option, which enables the shell interactive mode.

> :bulb: Note : You can also add `-i` after your scheme files, it will loads them into the interactive console.

## Available features

| name | description | syntax |
|------|-------------|--------|
| **cons**| construct a new list | (cons 1 2) |
| **car** | return the first argument of constructed list | (car (cons 1 2)) |
| **cdr** | return the second argument of a cons | (cdr '(1 3 4)) |
| **eq?** | check if its two arguments are equal | (eq? 1 1) |
| **atom?** | check if its one argument is an atom | (atom? 'foo) |
| **div, mod, +, -**| performs the corresponding operation | (div 10 5) |
| **<** | check if the first argument is smaller than the second one | (< (* 2 2) 5) |
| **quote, '** | returns the argument without evaluating it | (quote (1 1)) |
| **lambda** | Takes a list of parameters as first argument, and an expresion to evaluate as second argument, returns a lambda (procedure) which can be subsequently called. | (lambda (a b ) (+ a b)) |
| **define** |If it’s first argument is a symbol, associate the symbol to its second argument, and returns it’s name.If it’s first argument is a list, defines a function which name is the first elemnt of the list, the rest of the listits parameters, and the second argument the function’s body. | (define add (lambda (a b )(+ a b))) |
| **let** | Takes a list of key/values as first argument, and an expression as a second argument, evaluate this secondargument within an environement where the key / value pairs are bound. | (let ((a 2) (b (+ 1 2))) (+ a b)) |
| **cond** | Evaluates each expression of the lists. If the first expression on a list is true, it returns the value of the second element. Otherwise, it tries the next list argument. | (cond (#f 1) (#t (+ 1 1))) |
