Pod::Spec.new do |s|
  s.name = 'TestClient'
  s.ios.deployment_target = '9.0'
  s.osx.deployment_target = '10.11'
  s.tvos.deployment_target = '9.0'
  s.version = '0.0.1'
  s.source = { :git => 'git@github.com:swagger-api/swagger-mustache.git', :tag => 'v1.0.0' }
  s.authors = ''
  s.license = 'Proprietary'
  s.homepage = 'https://github.com/swagger-api/swagger-codegen'
  s.summary = 'TestClient'
  s.source_files = 'TestClient/Classes/**/*.swift'
  s.dependency 'Alamofire', '~> 4.5.0'
end
