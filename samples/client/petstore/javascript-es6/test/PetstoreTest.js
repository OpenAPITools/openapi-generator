if (typeof module === 'object' && module.exports) {
  var expect = require('expect.js');
  var OpenAPIPetstore = require('../src/index');
  var sinon = require('sinon');
}

var apiClient = OpenAPIPetstore.ApiClient.instance;

var getProperty = function(object, getter, property) {
  // Use getter method if present; otherwise, get the property directly.
  if (typeof object[getter] === 'function')
    return object[getter]();
  else
    return object[property];
}

var setProperty = function(object, setter, property, value) {
  // Use setter method if present; otherwise, set the property directly.
  if (typeof object[setter] === 'function')
    object[setter](value);
  else
    object[property] = value;
}

describe('Petstore', function() {
  describe('models', function() {
    it('should serialize oneOf models correctly', function() {
      var pig = new OpenAPIPetstore.Pig();
      expect(JSON.stringify(pig)).to.be('null');

      // set basque big as the payload
      var bpig = new OpenAPIPetstore.BasquePig();
      setProperty(bpig, "setClassName", "className", "BasquePig");
      setProperty(bpig, "setColor", "color", "red");
      expect(JSON.stringify(bpig)).to.be('{"className":"BasquePig","color":"red"}');
     
      pig.setActualInstance(bpig);
      expect(JSON.stringify(pig)).to.be('{"className":"BasquePig","color":"red"}');

    });

    it('should serialize nested oneOf models correctly', function() {
      var nested_one_of = new OpenAPIPetstore.NestedOneOf();
      setProperty(nested_one_of, "setSize", "size", 28);
      expect(JSON.stringify(nested_one_of)).to.be('{"size":28}');
     
      // set nested oneOf `Pig` 
      var pig = new OpenAPIPetstore.Pig();
      // set basque big as the payload
      var bpig = new OpenAPIPetstore.BasquePig();
      setProperty(bpig, "setClassName", "className", "BasquePig");
      setProperty(bpig, "setColor", "color", "red");
      pig.setActualInstance(bpig);
      setProperty(nested_one_of, "setNestedPig", "nested_pig", pig);

      expect(JSON.stringify(nested_one_of)).to.be('{"size":28,"nested_pig":{"className":"BasquePig","color":"red"}}');
    });

    it('should run BasquePig constructFromObject correctly', function() {
      var bpig_json = '{"className":"BasquePig","color":"red"}';
      var bpig = OpenAPIPetstore.BasquePig.constructFromObject(JSON.parse(bpig_json), null);
      expect(JSON.stringify(bpig)).to.be('{"className":"BasquePig","color":"red"}');

      OpenAPIPetstore.BasquePig.validateJSON(JSON.parse(bpig_json)); // should not throw error
    });

    it('should throw error from Pet.validateJSON', function() {
      var bpig_json = '{"className":"BasquePig","color":"red"}';
      try {
        OpenAPIPetstore.Pet.validateJSON(JSON.parse(bpig_json));
        expect(true).to.be(false); // this line should not run if the error is thrown correctly
      } catch (err) {
        expect(err).to.be.eql(new Error('The required field `name` is not found in the JSON data: {"className":"BasquePig","color":"red"}'));
      }
    });

    it('should run Pig constructFromObject correctly', function() {
      var bpig = '{"className":"BasquePig","color":"red"}';
      var pig = OpenAPIPetstore.Pig.constructFromObject(JSON.parse(bpig));
      expect(JSON.stringify(pig)).to.be('{"className":"BasquePig","color":"red"}');
    });

    it('should deserialize simple models correctly', function() {
      var tag_json = '{"id":1,"name":"tag_name"}';
      var tag_result = OpenAPIPetstore.ApiClient.convertToType(tag_json, OpenAPIPetstore.Tag);
      expect(tag_result).to.be.a(OpenAPIPetstore.Tag);
      //expect(tag_result.id).to.be(1);
      //expect(JSON.stringify(tag_result)).to.be(tag_json);
    });

    it('should deserialize nested oneOf models correctly', function() {
      var nested_one_of_json = '{"size":28,"nested_pig":{"className":"BasquePig","color":"red"}}'
      var result = OpenAPIPetstore.ApiClient.convertToType(JSON.parse(nested_one_of_json), OpenAPIPetstore.NestedOneOf);
      expect(result).to.be.a(OpenAPIPetstore.NestedOneOf);
      expect(JSON.stringify(result)).to.be(nested_one_of_json);
    });

  });
});

