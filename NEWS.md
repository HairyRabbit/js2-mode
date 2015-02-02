## 20141115

Support for:

* Unicode characters in identifiers (improved).
* [Delegating yield](http://wiki.ecmascript.org/doku.php?id=harmony:generators#delegating_yield).
* [ES6 numeric literals](https://people.mozilla.org/~jorendorff/es6-draft.html#sec-literals-numeric-literals) (octal, binary).
* Harmony [array and generator comprehensions](http://wingolog.org/archives/2014/03/07/es6-generator-and-array-comprehensions-in-spidermonkey).

## 20131106

Support for:

* [Arrow functions](http://wiki.ecmascript.org/doku.php?id=harmony:arrow_function_syntax)
* [Generators](http://wiki.ecmascript.org/doku.php?id=harmony:generators)
* [Spread operator](http://wiki.ecmascript.org/doku.php?id=harmony:spread)

## 20130510

### Support for JSLint global declaration

See the docstring for `js2-include-jslint-globals`.

## 20130216

### We don't rebind `RET` anymore

Because well-behaving major modes aren't supposed to do that.

So pressing it won't continue a block comment, or turn a string into a concatenation.
Pressing `M-j`, however, will.

The options `js2-indent-on-enter-key` and `js2-enter-indents-newline` were also removed.

To bring back the previous behavior, put this in your init file:

    (eval-after-load 'js2-mode
      '(define-key js2-mode-map (kbd "RET") 'js2-line-break))

## 20120617

### Support for [default](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/default_parameters) and [rest](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/rest_parameters) parameters

## 20120614

### Support for [for..of loops](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/for...of)

## Older changes

### Popular indentation style

    [foo, bar, baz].forEach(function (v) {
        if (validate(v))
            process(v);
    });

    [a, b, c].some(function (v) {
        return validate(v);
    });

### Pretty multiline variable declaration

In the original mode,

    var foo = 10,
    bar = 20,
    baz = 30;

In this mode when the value of `js2-pretty-multiline-declarations` is non-nil,

    var foo = 10,
        bar = 20,
        baz = 30;

### Abbreviated destructuring assignments

    let {a, b}       = {a: 10, b: 20}; // Abbreviated   (Not supported in the original mode)
    let {a: a, b: b} = {a: 10, b: 20}; // Same as above (Supported in the original mode)

    (function ({responseText}) { /* */ })(xhr); // As the argument of function

    for (let [k, { name, age }] in Iterator(obj)) // nested
        print(k, name, age);

### Expression closure in property value

    let worker = {
        get age() 20,
        get sex() "male",
        fire: function () _fire()
    };

### Fix for odd indentation of "else if" with no braces

In the original mode,

    if (foo)
        return foo;
    else if (bar)
    return bar;      // here

In this mode,

    if (foo)
        return foo;
    else if (bar)
        return bar;  // fixed

### Imenu support for function nesting

Supports function nesting and anonymous wrappers:

    (function() {
      var foo = function() {
        function bar() { // shown as foo.bar.<definition-1>
          function baz() {} // foo.bar.baz
          var qux = function() {}; // foo.bar.quux
        }
      };
    });

Examples of output:

* [jQuery 1.5](https://gist.github.com/845449)
* [Underscore.js](https://gist.github.com/824262)
* [Backbone.js](https://gist.github.com/824260)

For library-specific extension methods like `$.extend` and `dojo.declare`, see [js2-imenu-extras](/mooz/js2-mode/blob/master/js2-imenu-extras.el).

### Undeclared/external variables highlighting

Original mode highlights them only on the left side of assignments:

    var house;
    hose = new House(); // highlights "hose"

Here they are highlighted in all expressions:

    function feed(fishes, food) {
        for each (var fish in fshes) { // highlights "fshes"
            food.feed(fsh); // highlights "fsh"
        }
        hood.discard(); // highlights "hood"
    }

Destructuring assignments and array comprehensions (JS 1.7) are supported:

    let three, [one, two] = [1, 2];
    thee = one + two; // highlights "thee"

    function revenue(goods) {
        // highlights "coast"
        return [price - coast for each ({price, cost} in goods)].reduce(add);
    }