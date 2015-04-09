<?php
trait AbstractTrait
{
    abstract public function doSomething();

    public function mockableMethod()
    {
        return TRUE;
    }

    public function anotherMockableMethod()
    {
        return TRUE;
    }
}
