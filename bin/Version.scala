val version = scala.util.Properties.scalaPropOrElse("version.number", "unknown").toString match {
  case "2.10.0" => "2.10"
  case e: String => e
}
println(version)