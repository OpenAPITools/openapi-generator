


//export module
if ( typeof define === "function" && define.amd ) {     
	define('Tag', ['jquery'], 
		function($) {
        return Tag;
	 });
}


var Tag = function Tag() { 
  	var self = this;
  	
  	/**
	  * datatype: Long
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
   * @return {Long}
   **/
  self.getId = function() {
    return self.id;
  }
  
  /**
   * @param {Long} id
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
