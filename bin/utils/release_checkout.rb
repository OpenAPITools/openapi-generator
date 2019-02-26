#!/usr/bin/env ruby
#
# release_chekcout.rb
#
require 'rubygems'
require 'open-uri'
require 'net/http'

def check_npmjs
  print "Checking npmjs... "

  url = "https://www.npmjs.com/package/@openapitools/openapi-generator-cli?activeTab=versions"
  open(url) do |f|
    content = f.read
    if !content.nil? && content.include?($version)
      puts "[OK]"
    else
      puts "[ERROR]"
      puts "> #{url} not yet updated with #{$version}"
    end
  end
end

def check_homebrew
  print "Checking homebrew forumla ... "

  url = "https://raw.githubusercontent.com/Homebrew/homebrew-core/master/Formula/openapi-generator.rb"
  new_maven_url = "https://search.maven.org/remotecontent?filepath=org/openapitools/openapi-generator-cli/#{$version}/openapi-generator-cli-#{$version}.jar"
  open(url) do |f|
    content = f.read
    if !content.nil? && content.include?(new_maven_url)
      puts "[OK]"
    else
      puts "[ERROR]"
      puts "> #{url} not yet updated with #{new_maven_url}"
    end
  end
end

def check_openapi_generator_online_docker
  print "Checking openapi-generator-online docker ... "

  url = "https://hub.docker.com/r/openapitools/openapi-generator-online/tags/"
  docker_tag = "v#{$version}"
  open(url) do |f|
    content = f.read
    if !content.nil? && content.include?(docker_tag)
      puts "[OK]"
    else
      puts "[ERROR]"
      puts "> #{url} does not have tag #{docker_tag}"
    end
  end
end

def check_openapi_generator_cli_docker
  print "Checking openapi-generator-cli docker ... "

  url = "https://hub.docker.com/r/openapitools/openapi-generator-cli/tags/"
  docker_tag = "v#{$version}"
  open(url) do |f|
    content = f.read
    if !content.nil? && content.include?(docker_tag)
      puts "[OK]"
    else
      puts "[ERROR]"
      puts "> #{url} does not have tag #{docker_tag}"
    end
  end
end

def check_readme
  print "Checking openapi-generator README.md ... "

  url = "https://raw.githubusercontent.com/OpenAPITools/openapi-generator/master/README.md"
  matches = ["[#{$version}](https://github.com/OpenAPITools/openapi-generator/releases/tag/v#{$version})",
           "JAR location: `http://central.maven.org/maven2/org/openapitools/openapi-generator-cli/#{$version}/openapi-generator-cli-#{$version}.jar`",
           "wget http://central.maven.org/maven2/org/openapitools/openapi-generator-cli/#{$version}/openapi-generator-cli-#{$version}.jar -O openapi-generator-cli.jar",
           "Invoke-WebRequest -OutFile openapi-generator-cli.jar http://central.maven.org/maven2/org/openapitools/openapi-generator-cli/#{$version}/openapi-generator-cli-#{$version}.jar"]
  open(url) do |f|
    content = f.read
    has_outdated = false
    not_matched = []
    matches.each do |match|
      if !content.nil? && content.include?(match)
        # matched
      else
        has_outdated = true
        not_matched << match
      end
    end

    if has_outdated
      puts "[ERROR]"
      not_matched.each do |str|
        puts "> '#{str}' not found in README.md"
      end
    else
      puts "[OK]"
    end
  end
end

def check_openapi_generator_jar
  print "Checking openapi-generator JAR ... "
  url = "http://central.maven.org/maven2/org/openapitools/openapi-generator/#{$version}/openapi-generator-#{$version}.jar"

  if check_url(url)
    puts "[OK]"
  else
    puts "[ERROR]"
    puts "> #{url} not found"
  end

end


def check_openapi_generator_cli_jar
  print "Checking openapi-generator-cli JAR ... "
  url = "http://central.maven.org/maven2/org/openapitools/openapi-generator-cli/#{$version}/openapi-generator-cli-#{$version}.jar"

  if check_url(url)
    puts "[OK]"
  else
    puts "[ERROR]"
    puts "> #{url} not found"
  end
end

def check_openapi_generator_maven_plugin_jar
  print "Checking openapi-generator-maven-plugin JAR ... "
  url = "http://central.maven.org/maven2/org/openapitools/openapi-generator-maven-plugin/#{$version}/openapi-generator-maven-plugin-#{$version}.jar"

  if check_url(url)
    puts "[OK]"
  else
    puts "[ERROR]"
    puts "> #{url} not found"
  end
end

def check_openapi_generator_gradle_plugin_jar
  print "Checking openapi-generator-gradle-plugin JAR ... "
  url = "http://central.maven.org/maven2/org/openapitools/openapi-generator-gradle-plugin/#{$version}/openapi-generator-gradle-plugin-#{$version}.jar"

  if check_url(url)
    puts "[OK]"
  else
    puts "[ERROR]"
    puts "> #{url} not found"
  end
end

def check_openapi_generator_online_jar
  print "Checking openapi-generator-online JAR ... "
  url = "http://central.maven.org/maven2/org/openapitools/openapi-generator-online/#{$version}/openapi-generator-online-#{$version}.jar"

  if check_url(url)
    puts "[OK]"
  else
    puts "[ERROR]"
    puts "> #{url} not found"
  end
end

def check_openapi_generator_project_pom
  print "Checking openapi-generator-project pom.xml ... "
  url = "http://central.maven.org/maven2/org/openapitools/openapi-generator-project/#{$version}/openapi-generator-project-#{$version}.pom"

  if check_url(url)
    puts "[OK]"
  else
    puts "[ERROR]"
    puts "> #{url} not found"
  end
end

def check_url url
  content = Net::HTTP.get(URI.parse(url))
  url = URI.parse(url)
  req = Net::HTTP.new(url.host, url.port)
  res = req.request_head(url.path)
  if res.code == "200"
    true
  else
    false
  end
end

def usage
  puts "ERROR!! Version (e.g. 3.0.2) missing"
  puts "Usage example: ruby #{$0} 3.0.2"
end


if (!ARGV[0])
  usage
  exit
end

$version = ARGV[0]

puts "Running checkout on OpenAPI Generator release #{$version}"

check_npmjs
check_homebrew
check_openapi_generator_jar
check_openapi_generator_cli_jar
check_openapi_generator_maven_plugin_jar
check_openapi_generator_gradle_plugin_jar
check_openapi_generator_online_jar
check_openapi_generator_project_pom
check_readme
check_openapi_generator_online_docker
check_openapi_generator_cli_docker
