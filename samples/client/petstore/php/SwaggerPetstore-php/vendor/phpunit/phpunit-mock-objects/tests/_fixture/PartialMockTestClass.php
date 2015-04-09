<?php
class PartialMockTestClass
{
    public $constructorCalled = FALSE;

    public function __construct()
    {
        $this->constructorCalled = TRUE;
    }

    public function doSomething()
    {
    }

    public function doAnotherThing()
    {
    }
}
