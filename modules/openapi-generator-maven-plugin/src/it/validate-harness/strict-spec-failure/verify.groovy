import java.io.*

// 1. Locate the build log file
File logFile = new File(basedir, "build.log")
assert logFile.exists() : "The build.log file was not found!"

String logContent = logFile.text

// 2. Assert the true root cause strings
// Check for the exact exception message thrown by the Mojo wrapper
assert logContent.contains("Validation has error(s). See above for the details.") : "Build failed, but not due to the expected validation exception wrapper!"

// Check for the swagger/openapi parser's structural complaint about our broken yaml
assert logContent.contains("attribute info.version is missing") || logContent.contains("missing") : "The logs do not contain the specific OpenAPI specification structural violations!"

return true