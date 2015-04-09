The ReflectionDocBlock Component [![Build Status](https://secure.travis-ci.org/phpDocumentor/ReflectionDocBlock.png)](https://travis-ci.org/phpDocumentor/ReflectionDocBlock)
================================

Introduction
------------

The ReflectionDocBlock component of phpDocumentor provides a DocBlock parser
that is 100% compatible with the [PHPDoc standard](http://phpdoc.org/docs/latest).

With this component, a library can provide support for annotations via DocBlocks
or otherwise retrieve information that is embedded in a DocBlock.

> **Note**: *this is a core component of phpDocumentor and is constantly being
> optimized for performance.*

Installation
------------

You can install the component in the following ways:

* Use the official Github repository (https://github.com/phpDocumentor/ReflectionDocBlock)
* Via Composer (http://packagist.org/packages/phpdocumentor/reflection-docblock)

Usage
-----

The ReflectionDocBlock component is designed to work in an identical fashion to
PHP's own Reflection extension (http://php.net/manual/en/book.reflection.php).

Parsing can be initiated by instantiating the
`\phpDocumentor\Reflection\DocBlock()` class and passing it a string containing
a DocBlock (including asterisks) or by passing an object supporting the
`getDocComment()` method.

> *Examples of objects having the `getDocComment()` method are the
> `ReflectionClass` and the `ReflectionMethod` classes of the PHP
> Reflection extension*

Example:

    $class = new ReflectionClass('MyClass');
    $phpdoc = new \phpDocumentor\Reflection\DocBlock($class);

or

    $docblock = <<<DOCBLOCK
    /**
     * This is a short description.
     *
     * This is a *long* description.
     *
     * @return void
     */
    DOCBLOCK;

    $phpdoc = new \phpDocumentor\Reflection\DocBlock($docblock);

