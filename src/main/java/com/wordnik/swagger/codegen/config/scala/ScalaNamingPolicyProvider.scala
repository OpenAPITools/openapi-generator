package com.wordnik.swagger.codegen.config.scala

import com.wordnik.swagger.codegen.config.common.CamelCaseNamingPolicyProvider

/**
 * User: ramesh
 * Date: 3/29/12
 * Time: 3:49 PM
 */

class ScalaNamingPolicyProvider extends CamelCaseNamingPolicyProvider {

  /**
   * Gets the signature of the method that gets value for give attribute name.
   *
   * Example: If class name is user and attibute name is email the out in java language will be
   *  <code>user.getEmail</code>
   *
   * @param className
   * @param attributeName
   * @return
   */
  override def createGetterMethodName(className: String, attributeName: String): String = {
    return className + ".get" + applyClassNamingPolicy(attributeName)
  }
}
