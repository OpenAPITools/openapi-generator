function createEnum(input) {
  if (input && input.toString().indexOf(",") > 0) {
    var output = [];
    var array = input.split(",");
    array.forEach(function(item) {
      output.push(item);
    })
    return output;
  }
}

exports.query = exports.q = function(name, description, dataType, required, allowMultiple, allowableValues, defaultValue) {
  return {
    "name" : name,
    "description" : description,
    "dataType" : dataType,
    "required" : required,
    "allowMultiple" : allowMultiple,
    "allowableValues" : createEnum(allowableValues),
    "defaultValue" : defaultValue,
    "paramType" : "query"
  };
};

exports.path = function(name, description, dataType, allowableValues) {
  return {
    "name" : name,
    "description" : description,
    "dataType" : dataType,
    "required" : true,
    "allowMultiple" : false,
    "allowableValues" : createEnum(allowableValues),
    "paramType" : "path"
  };
};

exports.post = function(dataType, description) {
  return {
    "description" : description,
    "dataType" : dataType,
    "required" : true,
    "paramType" : "body"
  };
};

exports.header = function(name, description, dataType, required) {
  return {
    "name" : name,
    "description" : description,
    "dataType" : dataType,
    "required" : true,
    "allowMultiple" : false,
    "allowableValues" : createEnum(allowableValues),
    "paramType" : "header"
  };
};