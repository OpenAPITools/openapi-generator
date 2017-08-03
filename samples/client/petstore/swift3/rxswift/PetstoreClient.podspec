Pod::Spec.new do |s|
  s.name = 'PetstoreClient'
  s.ios.deployment_target = '9.0'
  s.osx.deployment_target = '10.11'
  s.version = '0.0.1'
  s.source = { :git => 'git@github.com:swagger-api/swagger-mustache.git', :tag => 'v1.0.0' }
  s.authors = ''
  s.license = 'Proprietary'
  s.homepage = 'https://github.com/swagger-api/swagger-codegen'
  s.summary = 'PetstoreClient'
  s.source_files = 'PetstoreClient/Classes/**/*.swift'
  s.dependency 'RxSwift', '~> 3.4.1'
  s.dependency 'Alamofire', '~> 4.0'
end
