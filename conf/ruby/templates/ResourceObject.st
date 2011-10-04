module $resource$

$methods:{ method |
  def $method.name$(opts={})
$if(method.pathParameters)$    requiredKeys=[:$method.pathParameters:{ param | $param.name$};separator=", "$]$endif$
$if(method.queryParameters)$    queryparamKeys=[:$method.queryParameters:{ param | $param.name$};separator=", "$]$endif$

$if(method.pathParameters)$
    #check required options
    requiredKeys.each do |key|
      raise "#{key} is required" unless options.has_key?(key) 
    end
$endif$


    # set default values and merge with input
    options = {
$if(method.arguments)$$method.arguments:{ param | $if(param.defaultValue)$      :$param.name$="$param.defaultValue$"$endif$}$$endif$
    }.merge(opts)
  
    #resource path
    path = "$method.resourcePath$".sub('{format}', '.json')
  
$if(method.pathParameters)$
    # sub pathparams
$method.pathParameters:{ param |
    path.sub!('{$param.name$}', URI.encode(options[:$param.name$]))
}$$endif$
    #pull querystring keys from options
    queryopts = options.select do |key,value|
      queryparamKeys.include? key
    end

$if(method.hasResponseValue)$
    results = Swagger::request.new(:get, path, queryopts, nil)
$if(method.returnValueList)$
    output = results.map {|result| $method.returnValue$.new(result)}
$else$
    output = $method.returnValue$.new(result)
$endif$
$else$
    Swagger::request.new(:get, path, queryopts, nil)
$endif$

  end

}$
