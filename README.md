## Lox-fs is an F# interpreter

Lox-fs is a WIP based of the excellent [Crafting Interpreters](https://craftinginterpreters.com).

The project is current to Chapter 10. Functions, with closure scoping, arithmetic, and basic language features have all be implemented. Functions are first class so lambda calculus works as you'd expect.

**Lox Examples**

    // Fibonacci
    fun fib(n)
    {
        if (n <= 1) return n;
        return fib(n - 2) + fib(n - 1);
    }

    for (var i = 0; i < 20; i = i + 1)
    {
        print fib(i);
    }

    // Church Neumerals

    // Zero is the identity function.
    fun zero(f)
    {
        fun identity(x)
        {
            return x;
        }

        return identity;
    };

    // Successor: apply function one more time.
    fun succ(n)
    {
        fun succF(f)
        {
            fun succX(x)
            {
                return f(n(f)(x));
            }
            return succX;
        }
        return succF;
    }

    fun plusOne(x)
    {
        return x + 1;
    }

    // Convert a Church numeral into a concrete integer.
    fun churchToInt(n)
    {
        return n(plusOne)(0);
    }

    // Convert a concrete integer into a church numeral.
    fun nToChurch(n) 
    {
        if(n == 0)
        {
            return zero;
        }
        else
        {
            return succ(nToChurch(n - 1));
        }
    }

    // Add two Church numerals.
    fun add(m)
    {
        fun addN(n)
        {
            fun addF(f)
            {
                fun addX(x)
                {
                    return n(f)(m(f)(x));
                }
                return addX;
            }
            return addF;
        }
        return addN;
    }

    // Multiply two Church numerals.
    fun mul(m)
    {
        fun mulN(n)
        {
            fun mulF(f)
            {
                fun mulX(x)
                {
                    return n(m(f))(x);
                }
                return mulX;
            }
            return mulF;
        }
        return mulN;
    }

    // Exponentiation: n^m
    fun exp(m)
    {
        fun expN(n)
        {
            return n(m);
        }
        return expN;
    }

    var one = succ(zero);
    var two = succ(one);
    var three = succ(two);

    print churchToInt(one);
    print churchToInt(two);
    print churchToInt(three);

    print churchToInt(nToChurch(999));
    print churchToInt(add(two)(two));
    print churchToInt(mul(three)(three));
    print churchToInt(exp(three)(three));
