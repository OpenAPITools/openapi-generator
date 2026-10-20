#!/usr/bin/env scala
//> using toolkit 0.6.0
//> using scala 3.3.1

import os.*

object Debug:
  var enabled = false

enum Command:
  case Clean, Generate, GenerateStrict, GenerateOldGen, Strict, Exit

enum Target:
  case Specific(name: String)
  case All

def say(message: String)(using pwd: Path) =
  val command = List("say", message)
  if Debug.enabled then println(s"Running command: ${command.mkString(" ")}")
  os.proc(command).call(stdout = os.Inherit, stderr = os.Inherit)

case class Project(
  project: String,
  projectName: String,
  projectGroupId: String,
  projectArtifactId: String,
  projectMainPackage: String,
  projectVersion: String,
  generatorName: String,
  skipValidate: Boolean,
  additionalProps: String = "",
  schemaMappings: String = "",
  typeMappings: String = "",
  importMappings: String = ""
)

val projects = Map(
  "mattermost" -> Project(
    project = "mattermost-scala",
    projectName = "mattermost-scala",
    projectGroupId = "ma.chinespirit",
    projectArtifactId = "mattermost-scala",
    projectMainPackage = "ma.chinespirit.mm",
    projectVersion = "1.0.0-SNAPSHOT",
    generatorName = "scala-sttp4-jsoniter",
    skipValidate = false
  ),
  "kubernetes" -> Project(
    project = "kubeapi-scala",
    projectName = "kubeapi-scala",
    projectGroupId = "ma.chinespirit",
    projectArtifactId = "kubeapi-scala",
    projectMainPackage = "ma.chinespirit.kube",
    projectVersion = "1.0.0-SNAPSHOT",
    generatorName = "scala-sttp4-jsoniter",
    skipValidate = false,
    schemaMappings = "",
    typeMappings = "",
    importMappings = ""
  ),
  "stripe" -> Project(
    project = "stripe-scala",
    projectName = "stripe-scala",
    projectGroupId = "ma.chinespirit",
    projectArtifactId = "stripe-scala",
    projectMainPackage = "ma.chinespirit.stripe",
    projectVersion = "1.0.0-SNAPSHOT",
    generatorName = "scala-sttp4-jsoniter",
    skipValidate = false
  ),
  "github" -> Project(
    project = "github-scala",
    projectName = "github-scala",
    projectGroupId = "ma.chinespirit",
    projectArtifactId = "github-scala",
    projectMainPackage = "ma.chinespirit.github",
    projectVersion = "1.0.0-SNAPSHOT",
    generatorName = "scala-sttp4-jsoniter",
    skipValidate = false
  ),
  "spotify" -> Project(
    project = "spotify-scala",
    projectName = "spotify-scala",
    projectGroupId = "ma.chinespirit",
    projectArtifactId = "spotify-scala",
    projectMainPackage = "ma.chinespirit.spotify",
    projectVersion = "1.0.0-SNAPSHOT",
    generatorName = "scala-sttp4-jsoniter",
    skipValidate = false
  )
)

def cleanMaven(using pwd: Path) =
  val command = List("./mvnw", "clean")
  if Debug.enabled then println(s"Running command: ${command.mkString(" ")}")
  os.proc(command).call(stdout = os.Inherit, stderr = os.Inherit)

def installMaven(using pwd: Path) =
  val command = List("./mvnw", "install", "-DskipTests", "-Dmaven.javadoc.skip=true")
  if Debug.enabled then println(s"Running command: ${command.mkString(" ")}")
  os.proc(command).call(stdout = os.Inherit, stderr = os.Inherit)

def cleanupGeneratedFiles(project: Project, projectRootPath: Path) =
  val basePackage = os.SubPath(project.projectMainPackage.replace(".", "/"))
  os.remove.all(projectRootPath / "build.sbt")
  os.remove.all(projectRootPath / "target")
  os.remove.all(projectRootPath / "project")
  os.remove.all(projectRootPath / "README.md")
  os.remove.all(projectRootPath / "src" / "main" / "scala" / basePackage / "api")
  os.remove.all(projectRootPath / "src" / "main" / "scala" / basePackage / "model")
  os.remove.all(projectRootPath / "src" / "main" / "scala" / basePackage / "core")

def runGeneratorJsoniter(project: Project, projectRootPath: Path, strict: Boolean = false)(using pwd: Path) =
  val additionalProps = {
    val base = s"mainPackage=${project.projectMainPackage},groupId=${project.projectGroupId},artifactId=${project.projectArtifactId},artifactVersion=${project.projectVersion}"
    val extended = if project.additionalProps.nonEmpty then s"$base,${project.additionalProps}" else base

    List(s"--additional-properties", extended)
  }
  val validateFlag = if project.skipValidate && !strict then List("--skip-validate-spec") else Nil
  val schemaMappings = if project.schemaMappings.nonEmpty then List("--schema-mappings", project.schemaMappings) else Nil
  val typeMappings = if project.typeMappings.nonEmpty then List("--type-mappings", project.typeMappings) else Nil
  val importMappings = if project.importMappings.nonEmpty then List("--import-mappings", project.importMappings) else Nil

  val command = List(
    List("java", "-jar", "modules/openapi-generator-cli/target/openapi-generator-cli.jar", "generate",
    "-i", s"$projectRootPath/openapi.json",
    "--generator-name", project.generatorName,
    "-o", projectRootPath.toString),
    validateFlag,
    additionalProps,
    schemaMappings,
    typeMappings,
    importMappings
  ).flatten

  if Debug.enabled then println(s"Running command: ${command.mkString(" ")}")
  os.proc(command).call(stdout = os.Inherit, stderr = os.Inherit)

def runGeneratorOld(project: Project, projectRootPath: Path)(using pwd: Path) =
  val additionalProps = {
    val base = s"mainPackage=${project.projectMainPackage},groupId=${project.projectGroupId},artifactId=${project.projectArtifactId},artifactVersion=${project.projectVersion},jsonLibrary=circe"
    val extended = if project.additionalProps.nonEmpty then s"$base,${project.additionalProps}" else base

    List("--additional-properties", extended)
  }
  val validateFlag = if project.skipValidate then List("--skip-validate-spec") else Nil
  val schemaMappings = if project.schemaMappings.nonEmpty then List("--schema-mappings", project.schemaMappings) else Nil
  val typeMappings = if project.typeMappings.nonEmpty then List("--type-mappings", project.typeMappings) else Nil
  val importMappings = if project.importMappings.nonEmpty then List("--import-mappings", project.importMappings) else Nil

  val command = List(
    List("openapi-generator-cli", "generate",
    "-i", s"$projectRootPath/openapi.json",
    "--generator-name", "scala-sttp4",
    "-o", projectRootPath.toString),
    validateFlag,
    additionalProps,
    schemaMappings,
    typeMappings,
    importMappings
  ).flatten

  if Debug.enabled then println(s"Running command: ${command.mkString(" ")}")
  os.proc(command).call(stdout = os.Inherit, stderr = os.Inherit)

def processProject(project: Project, cmd: Command)(using pwd: Path) =
  val projectRootPath = pwd / os.up / project.project
  cmd match
    case Command.Clean =>
      cleanMaven
      cleanupGeneratedFiles(project, projectRootPath)
    case Command.Generate =>
      runGeneratorJsoniter(project, projectRootPath)
    case Command.GenerateStrict =>
      runGeneratorJsoniter(project, projectRootPath, strict = true)
    case Command.GenerateOldGen =>
      cleanupGeneratedFiles(project, projectRootPath)
      runGeneratorOld(project, projectRootPath)
    case Command.Strict =>
      cleanMaven
      cleanupGeneratedFiles(project, projectRootPath)
      installMaven
      runGeneratorJsoniter(project, projectRootPath, strict = true)
    case Command.Exit =>
      // Do nothing

def main(): Unit =
  given pwd: Path = os.pwd

  println("Args: " + args.mkString(" "))

  try
    // Handle debug flag
    val filteredArgs = args.toList.filter { arg =>
      if arg == "--debug-script" then
        Debug.enabled = true
        false
      else true
    }

    val (cmdStr, target) = filteredArgs match
      case cmd :: "all" :: _ => (cmd, Target.All)
      case cmd :: name :: _ => (cmd, Target.Specific(name))
      case cmd :: Nil => (cmd, Target.All)
      case _ => ("exit", Target.All)

    val cmd = cmdStr match
      case "clean" => Command.Clean
      case "generate" => Command.Generate
      case "generate-strict" => Command.GenerateStrict
      case "generate-old-gen" => Command.GenerateOldGen
      case "strict" => Command.Strict
      case "exit" => Command.Exit
      case _ => Command.Exit

    (target, cmd) match
      case (_, Command.Exit) =>
        println("Usage: scala rebuild.sc <command> [project]")
        println("Available commands: clean, generate, generate-strict, upstream, strict")
      case (Target.All, _) =>
        projects.values.foreach(project => processProject(project, cmd))
        say("It is done")
      case (Target.Specific(projectName), _) =>
        projects.get(projectName) match
          case Some(project) => 
            processProject(project, cmd)
            say("It is done")
          case None => 
            System.err.println(s"Project '$projectName' not found. Available projects: ${projects.keys.mkString(", ")}")
            sys.exit(1)        
  catch
    case e: Exception =>
      say("You dun goofed")
      throw e

main()
