#!/usr/bin/env ruby
#
# release_checkout.rb
#
require 'rubygems'
require 'open-uri'
require 'net/http'

def check_sbt_openapi_generator
  print "Checking sbt-openapi-generator... "

  url = "https://raw.githubusercontent.com/upstart-commerce/sbt-openapi-generator/master/build.sbt"
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
  print "Checking homebrew formula ... "

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

  url = "https://hub.docker.com/v2/repositories/openapitools/openapi-generator-online/tags/?page_size=25&page=1"

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

  url = "https://hub.docker.com/v2/repositories/openapitools/openapi-generator-cli/tags/?page_size=25&page=1"
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
           "JAR location: `https://repo1.maven.org/maven2/org/openapitools/openapi-generator-cli/#{$version}/openapi-generator-cli-#{$version}.jar`",
           "wget https://repo1.maven.org/maven2/org/openapitools/openapi-generator-cli/#{$version}/openapi-generator-cli-#{$version}.jar -O openapi-generator-cli.jar",
           "Invoke-WebRequest -OutFile openapi-generator-cli.jar https://repo1.maven.org/maven2/org/openapitools/openapi-generator-cli/#{$version}/openapi-generator-cli-#{$version}.jar"]
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
  url = "https://repo1.maven.org/maven2/org/openapitools/openapi-generator/#{$version}/openapi-generator-#{$version}.jar"

  if check_url(url)
    puts "[OK]"
  else
    puts "[ERROR]"
    puts "> #{url} not found"
  end

end


def check_openapi_generator_cli_jar
  print "Checking openapi-generator-cli JAR ... "
  url = "https://repo1.maven.org/maven2/org/openapitools/openapi-generator-cli/#{$version}/openapi-generator-cli-#{$version}.jar"

  if check_url(url)
    puts "[OK]"
  else
    puts "[ERROR]"
    puts "> #{url} not found"
  end
end

def check_openapi_generator_maven_plugin_jar
  print "Checking openapi-generator-maven-plugin JAR ... "
  url = "https://repo1.maven.org/maven2/org/openapitools/openapi-generator-maven-plugin/#{$version}/openapi-generator-maven-plugin-#{$version}.jar"

  if check_url(url)
    puts "[OK]"
  else
    puts "[ERROR]"
    puts "> #{url} not found"
  end
end

def check_openapi_generator_gradle_plugin_jar
  print "Checking openapi-generator-gradle-plugin JAR ... "
  url = "https://repo1.maven.org/maven2/org/openapitools/openapi-generator-gradle-plugin/#{$version}/openapi-generator-gradle-plugin-#{$version}.jar"

  if check_url(url)
    puts "[OK]"
  else
    puts "[ERROR]"
    puts "> #{url} not found"
  end
end

def check_openapi_generator_online_jar
  print "Checking openapi-generator-online JAR ... "
  url = "https://repo1.maven.org/maven2/org/openapitools/openapi-generator-online/#{$version}/openapi-generator-online-#{$version}.jar"

  if check_url(url)
    puts "[OK]"
  else
    puts "[ERROR]"
    puts "> #{url} not found"
  end
end

def check_openapi_generator_project_pom
  print "Checking openapi-generator-project pom.xml ... "
  url = "https://repo1.maven.org/maven2/org/openapitools/openapi-generator-project/#{$version}/openapi-generator-project-#{$version}.pom"

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
  req.use_ssl = true
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

check_sbt_openapi_generator
check_openapi_generator_online_docker
check_openapi_generator_cli_docker
check_npmjs
check_homebrew
check_openapi_generator_jar
check_openapi_generator_cli_jar
check_openapi_generator_maven_plugin_jar
check_openapi_generator_gradle_plugin_jar
check_openapi_generator_online_jar
check_openapi_generator_project_pom
check_readme
