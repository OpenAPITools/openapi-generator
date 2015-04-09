<?php
/**
 * @see https://bugs.php.net/bug.php?id=51800
 */
class Issue1340Test extends PHPUnit_Framework_TestCase
{
    private static function get4KB()
    {
        return str_repeat('1', 4096 + 1);
    }

    /**
     * Also fails despite no isolation, because a phpt test is executed in
     * subprocess on its own.
     */
    public function testLargeStderrOutputDoesNotBlock()
    {
        // STDERR of a phpt test is not caught/validated at this point, so this
        // error output does not cause this test to fail.
        // @see https://github.com/sebastianbergmann/phpunit/issues/1169
        error_log("\n" . __FUNCTION__ . ": stderr:" . self::get4KB() . "\n");
        $this->assertTrue(true);
    }

    /**
     * @runInSeparateProcess
     */
    public function testLargeStderrOutputDoesNotBlockInIsolation()
    {
        error_log("\n" . __FUNCTION__ . ": stderr:" . self::get4KB() . "\n");
        $this->assertTrue(true);
    }

    /**
     * @runInSeparateProcess
     * @expectedException \PHPUnit_Framework_Error_Notice
     * @expectedExceptionMessage Undefined variable: foo
     */
    public function testPhpNoticeIsCaught()
    {
        $bar = $foo['foo'];
    }

    /**
     * @runInSeparateProcess
     * @expectedException \PHPUnit_Framework_Error_Notice
     * @expectedExceptionMessage Undefined variable: foo
     */
    public function testPhpNoticeWithStderrOutputIsAnError()
    {
        register_shutdown_function(__CLASS__ . '::onShutdown');
        $bar = $foo['foo'];
    }

    /**
     * @runInSeparateProcess
     */
    public function testFatalErrorDoesNotPass()
    {
        register_shutdown_function(__CLASS__ . '::onShutdown');
        $undefined = 'undefined_function';
        $undefined();
    }

    public static function onShutdown()
    {
        echo "\nshutdown: stdout:", self::get4KB(), "\n";
        error_log("\nshutdown: stderr:" . self::get4KB());
    }
}
