<?php
class Error extends PHPUnit_Framework_TestCase
{
    protected function runTest()
    {
        throw new Exception;
    }
}
