(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['../ApiClient'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('../ApiClient'));
  } else {
    // Browser globals (root is window)
    if (!root.SwaggerPetstore) {
      root.SwaggerPetstore = {};
    }
    root.SwaggerPetstore.FormatTest = factory(root.SwaggerPetstore.ApiClient);
  }
}(this, function(ApiClient) {
  'use strict';

  /**
   * The FormatTest model module.
   * @module model/FormatTest
   * @version 1.0.0
   */

  /**
   * Constructs a new <code>FormatTest</code>.
   * @alias module:model/FormatTest
   * @class
   * @param _number
   */
  var exports = function(_number) {




    this['number'] = _number;







  };

  /**
   * Constructs a <code>FormatTest</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/FormatTest} obj Optional instance to populate.
   * @return {module:model/FormatTest} The populated <code>FormatTest</code> instance.
   */
  exports.constructFromObject = function(data, obj) {
    if (data) { 
      obj = obj || new exports();

      if (data.hasOwnProperty('integer')) {
        obj['integer'] = ApiClient.convertToType(data['integer'], 'Integer');
      }
      if (data.hasOwnProperty('int32')) {
        obj['int32'] = ApiClient.convertToType(data['int32'], 'Integer');
      }
      if (data.hasOwnProperty('int64')) {
        obj['int64'] = ApiClient.convertToType(data['int64'], 'Integer');
      }
      if (data.hasOwnProperty('number')) {
        obj['number'] = ApiClient.convertToType(data['number'], 'Number');
      }
      if (data.hasOwnProperty('float')) {
        obj['float'] = ApiClient.convertToType(data['float'], 'Number');
      }
      if (data.hasOwnProperty('double')) {
        obj['double'] = ApiClient.convertToType(data['double'], 'Number');
      }
      if (data.hasOwnProperty('string')) {
        obj['string'] = ApiClient.convertToType(data['string'], 'String');
      }
      if (data.hasOwnProperty('byte')) {
        obj['byte'] = ApiClient.convertToType(data['byte'], 'String');
      }
      if (data.hasOwnProperty('binary')) {
        obj['binary'] = ApiClient.convertToType(data['binary'], 'String');
      }
      if (data.hasOwnProperty('date')) {
        obj['date'] = ApiClient.convertToType(data['date'], 'Date');
      }
      if (data.hasOwnProperty('dateTime')) {
        obj['dateTime'] = ApiClient.convertToType(data['dateTime'], 'String');
      }
    }
    return obj;
  }


  /**
   * @member {Integer} integer
   */
  exports.prototype['integer'] = undefined;

  /**
   * @member {Integer} int32
   */
  exports.prototype['int32'] = undefined;

  /**
   * @member {Integer} int64
   */
  exports.prototype['int64'] = undefined;

  /**
   * @member {Number} number
   */
  exports.prototype['number'] = undefined;

  /**
   * @member {Number} float
   */
  exports.prototype['float'] = undefined;

  /**
   * @member {Number} double
   */
  exports.prototype['double'] = undefined;

  /**
   * @member {String} string
   */
  exports.prototype['string'] = undefined;

  /**
   * @member {String} byte
   */
  exports.prototype['byte'] = undefined;

  /**
   * @member {String} binary
   */
  exports.prototype['binary'] = undefined;

  /**
   * @member {Date} date
   */
  exports.prototype['date'] = undefined;

  /**
   * @member {String} dateTime
   */
  exports.prototype['dateTime'] = undefined;




  return exports;
}));
