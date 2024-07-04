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

    it('should run Pig new correctly', function() {
      var bpig = OpenAPIPetstore.BasquePig.constructFromObject(JSON.parse('{"className":"BasquePig","color":"red"}'));
      var pig = new OpenAPIPetstore.Pig(bpig);
      expect(JSON.stringify(pig)).to.be('{"className":"BasquePig","color":"red"}');
    });

    it('should run Pig constructFromObject correctly', function() {
      var bpig = '{"className":"BasquePig","color":"red"}';
      var pig = OpenAPIPetstore.Pig.constructFromObject(JSON.parse(bpig));
      expect(JSON.stringify(pig)).to.be('{"className":"BasquePig","color":"red"}');
    });

    it('should throw an error when running Pig constructFromObject with incorrect data', function() {
      try {
        var bpig = '[1,2,3]';
        var pig = OpenAPIPetstore.Pig.constructFromObject(JSON.parse(bpig));
      } catch (err) {
        expect(err).to.be.eql(new Error('No match found constructing `Pig` with oneOf schemas BasquePig, DanishPig. Details: Failed to construct BasquePig: Error: The required field `className` is not found in the JSON data: [1,2,3], Failed to construct DanishPig: Error: The required field `className` is not found in the JSON data: [1,2,3]'));
      }
    });

    it('should deserialize simple models correctly', function() {
      var tag_json = '{"id":1,"name":"tag_name"}';
      var tag_result = OpenAPIPetstore.ApiClient.convertToType(tag_json, OpenAPIPetstore.Tag);
      expect(tag_result).to.be.a(OpenAPIPetstore.Tag);
      //expect(tag_result.id).to.be(1);
      //expect(JSON.stringify(tag_result)).to.be(tag_json);
    });
    it('should run Color constructFromObject correctly from array', function() {
      // construct from RgbColor
      let array_integer = [0,128,255];
      let color = OpenAPIPetstore.Color.constructFromObject(array_integer, null);
      expect(color).to.be.a(OpenAPIPetstore.Color);
      expect(color.getActualInstance()).to.be.eql(array_integer);
    });

    it('should throw an error when running Color constructFromObject with invalid array', function() {
      // construct from RgbColor
      let array_integer = [0,128,9255];
      try {
        let color = OpenAPIPetstore.Color.constructFromObject(array_integer, null);
        expect(true).to.be(false); // this line should not run if the error is thrown correctly
      } catch (err) {
        expect(err).to.be.eql(new Error('No match found constructing `Color` with oneOf schemas String, [Number]. Details: Failed to construct [Number]: Error: Invalid integer value in an array items. Max.: 255. Min.: 0. Data: 0,128,9255, Failed to construct [Number]: Error: Invalid array size. Minimim: 4. Maximum: 4. Data: 0,128,9255, Failed to construct String: Error: Invalid data. Must be string. Data: [0,128,9255]'));
      }
    });

    it('should run Color new correctly', function() {
      // valid hex color
      var input = "#00FF00";
      var color = new OpenAPIPetstore.Color(input);
      expect(color.getActualInstance()).to.be(input);

      // valid RgbColor
      input = [0,128,255];
      color = new OpenAPIPetstore.Color(input);
      expect(color.getActualInstance()).to.be(input);

      // valid RgbaColor 
      input = [0,128,200,255];
      color = new OpenAPIPetstore.Color(input);
      expect(color.getActualInstance()).to.be(input);
    });

    it('should run Color constructFromObject correctly', function() {
      // valid hex color
      var json = '"#00FF00"';
      var color = OpenAPIPetstore.Color.constructFromObject(JSON.parse(json), null);
      expect(JSON.stringify(color)).to.be(json);

      // valid RgbColor
      json = '[0,128,255]';
      color = OpenAPIPetstore.Color.constructFromObject(JSON.parse(json), null);
      expect(JSON.stringify(color)).to.be(json);

      // valid RgbaColor 
      json = '[0,128,200,255]';
      color = OpenAPIPetstore.Color.constructFromObject(JSON.parse(json), null);
      expect(JSON.stringify(color)).to.be(json);
    });

    it('should thrown an error when running Color new with invalid data', function() {
      // invalid hex color
      try {
        let input = "#00FF00ZZZZ";
        new OpenAPIPetstore.Color(input);
        expect(true).to.be(false); // this line should not run if the error is thrown correctly
      } catch (err) {
        expect(err).to.be.eql(new Error('No match found constructing Color with oneOf schemas String, [Number]. Details: Failed to desserialize JSON data into [Number]: Error: Invalid data type. Expecting array. Data: #00FF00ZZZZ, Failed to desserialize JSON data into [Number]: Error: Invalid data type. Expecting array. Data: #00FF00ZZZZ, Failed to desserialize JSON data into String: Error: Invalid string value in an array items. Must conform to /^#(?:[0-9a-fA-F]{3}){1,2}$/. Data: "#00FF00ZZZZ"'));
      }

      // invalid RgbColor <0
      try {
        let input = [-1,128,255];
        new OpenAPIPetstore.Color(input);
        expect(true).to.be(false); // this line should not run if the error is thrown correctly
      } catch (err) {
        expect(err).to.be.eql(new Error('No match found constructing Color with oneOf schemas String, [Number]. Details: Failed to desserialize JSON data into [Number]: Error: Invalid integer value in an array items. Max.: 255. Min.: 0. Data: -1,128,255, Failed to desserialize JSON data into [Number]: Error: Invalid array size. Minimim: 4. Maximum: 4. Data: -1,128,255, Failed to desserialize JSON data into String: Error: Invalid data. Must be string. Data: [-1,128,255]'));
      }

      // invalid RgbColor >255
      try {
        let input = [1,128,256];
        new OpenAPIPetstore.Color(input);
        expect(true).to.be(false); // this line should not run if the error is thrown correctly
      } catch (err) {
        expect(err).to.be.eql(new Error('No match found constructing Color with oneOf schemas String, [Number]. Details: Failed to desserialize JSON data into [Number]: Error: Invalid integer value in an array items. Max.: 255. Min.: 0. Data: 1,128,256, Failed to desserialize JSON data into [Number]: Error: Invalid array size. Minimim: 4. Maximum: 4. Data: 1,128,256, Failed to desserialize JSON data into String: Error: Invalid data. Must be string. Data: [1,128,256]'));
      }

      // invalid RgbaColor <0
      try {
        let input = [-1,1,128,255];
        new OpenAPIPetstore.Color(input);
        expect(true).to.be(false); // this line should not run if the error is thrown correctly
      } catch (err) {
        expect(err).to.be.eql(new Error('No match found constructing Color with oneOf schemas String, [Number]. Details: Failed to desserialize JSON data into [Number]: Error: Invalid array size. Minimim: 3. Maximum: 3. Data: -1,1,128,255, Failed to desserialize JSON data into [Number]: Error: Invalid integer value in an array items. Max.: 255. Min.: 0. Data: -1,1,128,255, Failed to desserialize JSON data into String: Error: Invalid data. Must be string. Data: [-1,1,128,255]'));
      }

      // invalid RgbaColor >255
      try {
        let input = [1,11,128,256];
        new OpenAPIPetstore.Color(input);
        expect(true).to.be(false); // this line should not run if the error is thrown correctly
      } catch (err) {
        expect(err).to.be.eql(new Error('[Error: No match found constructing Color with oneOf schemas String, [Number]. Details: Failed to desserialize JSON data into [Number]: Error: Invalid array size. Minimim: 3. Maximum: 3. Data: 1,11,128,256, Failed to desserialize JSON data into [Number]: Error: Invalid integer value in an array items. Max.: 255. Min.: 0. Data: 1,11,128,256, Failed to desserialize JSON data into String: Error: Invalid data. Must be string. Data: [1,11,128,256]'));
      }
    });


    it('should thrown an error when running Color constructFromObject with invalid data', function() {
      // invalid hex color
      try {
        let json = '"#00FF00ZZZZ"';
        OpenAPIPetstore.Color.constructFromObject(JSON.parse(json), null);
        expect(true).to.be(false); // this line should not run if the error is thrown correctly
      } catch (err) {
        expect(err).to.be.eql(new Error('No match found constructing Color with oneOf schemas String, [Number]. Details: Failed to desserialize JSON data into [Number]: Error: Invalid data type. Expecting array. Data: #00FF00ZZZZ, Failed to desserialize JSON data into [Number]: Error: Invalid data type. Expecting array. Data: #00FF00ZZZZ, Failed to desserialize JSON data into String: Error: Invalid string value in an array items. Must conform to /^#(?:[0-9a-fA-F]{3}){1,2}$/. Data: "#00FF00ZZZZ"'));
      }

      // invalid RgbColor <0
      try {
        let json = '[-1,128,255]';
        OpenAPIPetstore.Color.constructFromObject(JSON.parse(json), null);
        expect(true).to.be(false); // this line should not run if the error is thrown correctly
      } catch (err) {
        expect(err).to.be.eql(new Error('No match found constructing Color with oneOf schemas String, [Number]. Details: Failed to desserialize JSON data into [Number]: Error: Invalid integer value in an array items. Max.: 255. Min.: 0. Data: -1,128,255, Failed to desserialize JSON data into [Number]: Error: Invalid array size. Minimim: 4. Maximum: 4. Data: -1,128,255, Failed to desserialize JSON data into String: Error: Invalid data. Must be string. Data: [-1,128,255]'));
      }

      // invalid RgbColor >255
      try {
        let json = '[1,128,256]';
        OpenAPIPetstore.Color.constructFromObject(JSON.parse(json), null);
        expect(true).to.be(false); // this line should not run if the error is thrown correctly
      } catch (err) {
        expect(err).to.be.eql(new Error('No match found constructing Color with oneOf schemas String, [Number]. Details: Failed to desserialize JSON data into [Number]: Error: Invalid integer value in an array items. Max.: 255. Min.: 0. Data: 1,128,256, Failed to desserialize JSON data into [Number]: Error: Invalid array size. Minimim: 4. Maximum: 4. Data: 1,128,256, Failed to desserialize JSON data into String: Error: Invalid data. Must be string. Data: [1,128,256]'));
      }

      // invalid RgbaColor <0
      try {
        let json = '[-1,1,128,255]';
        OpenAPIPetstore.Color.constructFromObject(JSON.parse(json), null);
        expect(true).to.be(false); // this line should not run if the error is thrown correctly
      } catch (err) {
        expect(err).to.be.eql(new Error('No match found constructing Color with oneOf schemas String, [Number]. Details: Failed to desserialize JSON data into [Number]: Error: Invalid array size. Minimim: 3. Maximum: 3. Data: -1,1,128,255, Failed to desserialize JSON data into [Number]: Error: Invalid integer value in an array items. Max.: 255. Min.: 0. Data: -1,1,128,255, Failed to desserialize JSON data into String: Error: Invalid data. Must be string. Data: [-1,1,128,255]'));
      }

      // invalid RgbaColor >255
      try {
        let json = '[1,11,128,256]';
        OpenAPIPetstore.Color.constructFromObject(JSON.parse(json), null);
        expect(true).to.be(false); // this line should not run if the error is thrown correctly
      } catch (err) {
        expect(err).to.be.eql(new Error('[Error: No match found constructing Color with oneOf schemas String, [Number]. Details: Failed to desserialize JSON data into [Number]: Error: Invalid array size. Minimim: 3. Maximum: 3. Data: 1,11,128,256, Failed to desserialize JSON data into [Number]: Error: Invalid integer value in an array items. Max.: 255. Min.: 0. Data: 1,11,128,256, Failed to desserialize JSON data into String: Error: Invalid data. Must be string. Data: [1,11,128,256]'));
      }
    });

    it('should test fromJSON in oneOf models', function() {
      // invalid RgbaColor >255
      try {
        let json = '[1,11,128,256]';
        OpenAPIPetstore.Color.fromJSON(json);
        expect(true).to.be(false); // this line should not run if the error is thrown correctly
      } catch (err) {
        expect(err).to.be.eql(new Error('[Error: No match found constructing Color with oneOf schemas String, [Number]. Details: Failed to desserialize JSON data into [Number]: Error: Invalid array size. Minimim: 3. Maximum: 3. Data: 1,11,128,256, Failed to desserialize JSON data into [Number]: Error: Invalid integer value in an array items. Max.: 255. Min.: 0. Data: 1,11,128,256, Failed to desserialize JSON data into String: Error: Invalid data. Must be string. Data: [1,11,128,256]'));
      }

      // valid RgbColor
      let json = '[0,128,255]';
      let color = OpenAPIPetstore.Color.fromJSON(json);
      expect(JSON.stringify(color)).to.be(json);
    });

    it('should deserialize NestedColor with nested oneOf model color correctly', function() {
      var json = '{"nested":"#00FF00","size":256}'
      var result = OpenAPIPetstore.ApiClient.convertToType(JSON.parse(json), OpenAPIPetstore.NestedColor);
      expect(result).to.be.a(OpenAPIPetstore.NestedColor);
      expect(JSON.stringify(result)).to.be('{"size":256,"nested":"#00FF00"}');
    });

    it('should deserialize NestedOneOf with nested oneOf model correctly', function() {
      var nested_one_of_json = '{"size":28,"nested_pig":{"className":"BasquePig","color":"red"}}'
      var result = OpenAPIPetstore.ApiClient.convertToType(JSON.parse(nested_one_of_json), OpenAPIPetstore.NestedOneOf);
      expect(result).to.be.a(OpenAPIPetstore.NestedOneOf);
      expect(JSON.stringify(result)).to.be(nested_one_of_json);
    });

    it('should serialize and deserialize StringOrBoolean correctly', function() {
      // string
      var json = '"Hello World"'
      var result = OpenAPIPetstore.ApiClient.convertToType(JSON.parse(json), OpenAPIPetstore.StringOrBoolean);
      expect(result).to.be.a(OpenAPIPetstore.StringOrBoolean);
      expect(JSON.stringify(result)).to.be(json);

      // boolean
      json = 'true'
      result = OpenAPIPetstore.ApiClient.convertToType(JSON.parse(json), OpenAPIPetstore.StringOrBoolean);
      expect(result).to.be.a(OpenAPIPetstore.StringOrBoolean);
      expect(JSON.stringify(result)).to.be(json);
    });

  });
});

