@echo off
REM PHP Code Beautifier and Fixer
REM 
REM PHP version 5
REM 
REM @category  PHP
REM @package   PHP_CodeSniffer
REM @author    Greg Sherwood <gsherwood@squiz.net>
REM @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
REM @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
REM @link      http://pear.php.net/package/PHP_CodeSniffer

if "%PHPBIN%" == "" set PHPBIN=@php_bin@
if not exist "%PHPBIN%" if "%PHP_PEAR_PHP_BIN%" neq "" goto USE_PEAR_PATH
GOTO RUN
:USE_PEAR_PATH
set PHPBIN=%PHP_PEAR_PHP_BIN%
:RUN
"%PHPBIN%" "@bin_dir@\phpcbf" %*