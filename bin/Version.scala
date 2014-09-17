val version = scala.util.Properties.scalaPropOrElse("version.number", "unknown").toString match {
  case s if s startsWith "2.10" => "2.10"
  case s if s startsWith "2.11" => "2.11"
  case e: String => e
}
println(version)
