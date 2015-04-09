<?php
class ParentClassWithPrivateAttributes
{
    private static $privateStaticParentAttribute = 'foo';
    private $privateParentAttribute = 'bar';
}

class ParentClassWithProtectedAttributes extends ParentClassWithPrivateAttributes
{
    protected static $protectedStaticParentAttribute = 'foo';
    protected $protectedParentAttribute = 'bar';
}

class ClassWithNonPublicAttributes extends ParentClassWithProtectedAttributes
{
    public static $publicStaticAttribute = 'foo';
    protected static $protectedStaticAttribute = 'bar';
    protected static $privateStaticAttribute = 'baz';

    public $publicAttribute = 'foo';
    public $foo = 1;
    public $bar = 2;
    protected $protectedAttribute = 'bar';
    protected $privateAttribute = 'baz';

    public $publicArray = array('foo');
    protected $protectedArray = array('bar');
    protected $privateArray = array('baz');
}
