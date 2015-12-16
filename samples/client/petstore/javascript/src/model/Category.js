// require files in Node.js environment

if (typeof module === 'object' && module.exports) {
  
}




//export module
if ( typeof define === "function" && define.amd ) {
	define('Category', ['jquery'],
		function($) {
        return Category;
	 });
}


var Category = function Category() { 
  	var self = this;
  	
  	/**
	  * datatype: Integer
	  **/
	  self.id = null;
	 
  	/**
	  * datatype: String
	  **/
	  self.name = null;
	 
  
  	self.constructFromObject = function(data) {
	  	
	  	self.id = data.id;
	  	
	  	self.name = data.name;
	  	
	}
  
  
  /**
   * @return {Integer}
   **/
  self.getId = function() {
    return self.id;
  }
  
  /**
   * @param {Integer} id
   **/
  self.setId = function (id) {
    self.id = id;
  }
  
  /**
   * @return {String}
   **/
  self.getName = function() {
    return self.name;
  }
  
  /**
   * @param {String} name
   **/
  self.setName = function (name) {
    self.name = name;
  }
  

  self.toJson = function () {
  	return JSON.stringify(self);
  }
}

if (typeof module === 'object' && module.exports) {
  module.exports = Category;
}
