<?php
/*
 * This file is part of the PHPUnit_MockObject package.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

/**
 * Invocation matcher which looks for sets of specific parameters in the invocations.
 *
 * Checks the parameters of the incoming invocations, the parameter list is
 * checked against the defined constraints in $parameters. If the constraint
 * is met it will return true in matches().
 *
 * It takes a list of match groups and and increases a call index after each invocation.
 * So the first invocation uses the first group of constraints, the second the next and so on.
 *
 * @package    PHPUnit_MockObject
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @version    Release: @package_version@
 * @link       http://github.com/sebastianbergmann/phpunit-mock-objects
 */
class PHPUnit_Framework_MockObject_Matcher_ConsecutiveParameters
  extends PHPUnit_Framework_MockObject_Matcher_StatelessInvocation
{

  /**
   * @var array
   */
  private $_parameterGroups = array();

  /**
   * @var array
   */
  private $_invocations = array();

  /**
   * @param array $parameterGroups
   */
  public function __construct(array $parameterGroups)
  {
      foreach ($parameterGroups as $index => $parameters) {
          foreach ($parameters as $parameter) {
              if (!($parameter instanceof \PHPUnit_Framework_Constraint))
              {
                  $parameter = new \PHPUnit_Framework_Constraint_IsEqual($parameter);
              }
              $this->_parameterGroups[$index][] = $parameter;
          }
      }
  }

    /**
     * @return string
     */
    public function toString()
    {
        $text = 'with consecutive parameters';

        return $text;
    }

  /**
   * @param PHPUnit_Framework_MockObject_Invocation $invocation
   * @return bool
   */
  public function matches(PHPUnit_Framework_MockObject_Invocation $invocation)
  {
      $this->_invocations[] = $invocation;
      $callIndex = count($this->_invocations) - 1;
      $this->verifyInvocation($invocation, $callIndex);
      return FALSE;
  }

  public function verify()
  {
      foreach ($this->_invocations as $callIndex => $invocation) {
        $this->verifyInvocation($invocation, $callIndex);
      }
  }

  /**
   * Verify a single invocation
   *
   * @param PHPUnit_Framework_MockObject_Invocation $invocation
   * @param int $callIndex
   * @throws PHPUnit_Framework_ExpectationFailedException
   */
  private function verifyInvocation(PHPUnit_Framework_MockObject_Invocation $invocation, $callIndex)
  {

      if (isset($this->_parameterGroups[$callIndex])) {
          $parameters = $this->_parameterGroups[$callIndex];
      } else {
        // no parameter assertion for this call index
        return;
      }

      if ($invocation === NULL) {
          throw new PHPUnit_Framework_ExpectationFailedException(
            'Mocked method does not exist.'
          );
      }

      if (count($invocation->parameters) < count($parameters)) {
          throw new PHPUnit_Framework_ExpectationFailedException(
              sprintf(
                'Parameter count for invocation %s is too low.',
                $invocation->toString()
              )
          );
      }

      foreach ($parameters as $i => $parameter) {
          $parameter->evaluate(
              $invocation->parameters[$i],
              sprintf(
                'Parameter %s for invocation #%d %s does not match expected ' .
                'value.',
                $i,
                $callIndex,
                $invocation->toString()
              )
          );
      }
  }
}
