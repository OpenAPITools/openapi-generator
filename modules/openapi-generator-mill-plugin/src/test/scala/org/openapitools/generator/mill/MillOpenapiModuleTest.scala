package org.openapitools.generator.mill

import mill.*
import mill.api.{BuildCtx, Discover}
import mill.javalib.{Dep, JavaModule}
import mill.testkit.{TestRootModule, UnitTester}
import org.scalatest.matchers.should.Matchers
import org.testng.annotations.Test
import scala.language.postfixOps

object MillOpenapiModuleTestRoot extends TestRootModule {

  lazy val millDiscover = Discover[this.type]

  /** a project definition for Mill which should generate and compile */
  object petstoreMicroprofile extends JavaModule with OpenApiModule {

    override def mvnDeps: T[Seq[Dep]] = Seq(
      Dep.parse("jakarta.ws.rs:jakarta.ws.rs-api:3.1.0"),
      Dep.parse("jakarta.json.bind:jakarta.json.bind-api:3.0.0"),
      Dep.parse("jakarta.json:jakarta.json-api:2.1.0"),
      Dep.parse("org.eclipse.microprofile.rest.client:microprofile-rest-client-api:3.0.1"),
    )

    object openapi extends OpenApiConfig {

      override def inputSpec: T[PathRef] = Task.Source(BuildCtx.workspaceRoot / "petstore-v3.1.yaml")
      override def generatorName: T[String] = "java-microprofile"
      override def apiPackage: T[String] = "com.acme.foo.boundary.web.api"
      override def modelPackage: T[String] = "com.acme.foo.boundary.web.model"
      override def sourceFolder: T[String] = "src/gen/java"
      override def additionalProperties: T[Map[String, String]] = Map(
        "microprofileRestClientVersion" -> "3.0",
        "library" -> "microprofile",
        "dateLibrary" -> "java8",
        "interfaceOnly" -> "true",
        "performBeanValidation" -> "true",
        "useBeanValidation" -> "false",
        "skipDefaultInterface" -> "true",
        "useTags" -> "true",
      )
    }

    override def generatedSources: T[Seq[PathRef]] = Seq(
      PathRef(Task.dest),
      openapi.generate(),
    )
  }

  /** a project definition for Mill with an invalid openapi-spec */
  object petstoreInvalid extends JavaModule with OpenApiModule {

    override def mvnDeps: T[Seq[Dep]] = Seq(
      Dep.parse("jakarta.ws.rs:jakarta.ws.rs-api:3.1.0"),
      Dep.parse("jakarta.json.bind:jakarta.json.bind-api:3.0.0"),
      Dep.parse("jakarta.json:jakarta.json-api:2.1.0"),
      Dep.parse("org.eclipse.microprofile.rest.client:microprofile-rest-client-api:3.0.1"),
    )

    object openapi extends OpenApiConfig {

      override def inputSpec: T[PathRef] = Task.Source(BuildCtx.workspaceRoot / "petstore-v3.0-invalid-due-to-missing-info-attribute.yaml")
      override def generatorName: T[String] = "java-microprofile"
      override def apiPackage: T[String] = "com.acme.foo.boundary.web.api"
      override def modelPackage: T[String] = "com.acme.foo.boundary.web.model"
      override def sourceFolder: T[String] = "src/gen/java"
      override def additionalProperties: T[Map[String, String]] = Map(
        "microprofileRestClientVersion" -> "3.0",
        "library" -> "microprofile",
        "dateLibrary" -> "java8",
        "interfaceOnly" -> "true",
        "performBeanValidation" -> "true",
        "useBeanValidation" -> "false",
        "skipDefaultInterface" -> "true",
        "useTags" -> "true",
      )
    }

    override def generatedSources: T[Seq[PathRef]] = Seq(
      PathRef(Task.dest),
      openapi.generate(),
    )
  }
}

class MillOpenapiModuleTest extends Matchers {

  private val resourcePath = os.Path(sys.env("MILL_TEST_RESOURCE_DIR")) / "specs"
  private def testEval() = UnitTester(MillOpenapiModuleTestRoot, resourcePath)

  @Test
  def petstoreMicroprofileGeneratesAndCompiles(): Unit = {
    val result = testEval().scoped{ eval =>
      // execute 'compile` task
      eval.apply(MillOpenapiModuleTestRoot.petstoreMicroprofile.compile)
    }

    result shouldBe a[Right[_, _]]
  }

  @Test
  def petstoreMicroprofileInvalidSpec(): Unit = {
    val result = testEval().scoped { eval =>
      // execute 'compile` task
      eval.apply(MillOpenapiModuleTestRoot.petstoreInvalid.compile)
    }
    result shouldBe a[Left[_, _]]
  }
}
