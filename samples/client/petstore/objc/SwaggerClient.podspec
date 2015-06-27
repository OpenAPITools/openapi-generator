#
# Be sure to run `pod lib lint SwaggerClient.podspec' to ensure this is a
# valid spec and remove all comments before submitting the spec.
#
# Any lines starting with a # are optional, but encouraged
#
# To learn more about a Podspec see http://guides.cocoapods.org/syntax/podspec.html
#

Pod::Spec.new do |s|
  s.name             = "SwaggerClient"
  s.version          = "0.1.0"
  s.summary          = "A short description of SwaggerClient."
  s.description      = <<-DESC
                       An optional longer description of SwaggerClient

                       * Markdown format.
                       * Don't worry about the indent, we strip it!
                       DESC
  s.homepage         = "https://github.com/<GITHUB_USERNAME>/SwaggerClient"
  # s.screenshots     = "www.example.com/screenshots_1", "www.example.com/screenshots_2"
  s.license          = 'MIT'
  s.author           = { "geekerzp" => "geekerzp@gmail.com" }
  s.source           = { :git => "https://github.com/<GITHUB_USERNAME>/SwaggerClient.git", :tag => s.version.to_s }
  # s.social_media_url = 'https://twitter.com/<TWITTER_USERNAME>'

  s.platform     = :ios, '7.0'
  s.requires_arc = true

  s.source_files = 'SwaggerClient/**/*'
  s.public_header_files = 'SwaggerClient/**/*.h'

  s.resource_bundles = {
    'SwaggerClient' => ['Pod/Assets/*.png']
  }

  s.dependency 'AFNetworking', '~> 2.3'
  s.dependency 'JSONModel'
  s.dependency 'ISO8601'
end
