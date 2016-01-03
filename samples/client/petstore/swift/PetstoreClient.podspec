Pod::Spec.new do |s|
  s.name = 'PetstoreClient'
  s.ios.deployment_target = '8.0'
  s.osx.deployment_target = '10.9'
  s.version = '0.0.1'
  s.source = { :git => 'git@github.com:swagger-api/swagger-mustache.git', :tag => 'v1.0.0' }
  s.license = 'Apache License, Version 2.0'
  s.source_files = 'PetstoreClient/Classes/Swaggers/**/*.swift'
  s.dependency 'PromiseKit', '~> 3.0.0'
  s.dependency 'Alamofire', '~> 3.1.4'
end
