

//export module
if ( typeof define === "function" && define.amd ) {     
	define('StatusEnum', ['jquery'], function($) {
        return StatusEnum;
	 });
}

var StatusEnum = function StatusEnum() {
	var self = this;
	

	/**
	 * @const 
	 */ 
	self.AVAILABLE = "available",
	
	/**
	 * @const 
	 */ 
	self.PENDING = "pending",
	
	/**
	 * @const 
	 */ 
	self.SOLD = "sold";

}


//export module
if ( typeof define === "function" && define.amd ) {     
	define('Pet', ['jquery', 'Category', 'Array'], 
		function($, Category, Array) {
        return Pet;
	 });
}


var Pet = function Pet(photoUrls, name) { 
  	var self = this;
  	
  	/**
	  * datatype: Long
	  **/
	  self.id = null;
	 
  	/**
	  * datatype: Category
	  **/
	  self.category = new Category();
	 
  	/**
	  * datatype: String
	  * required
	  **/
	  self.name = name;
	 
  	/**
	  * datatype: Array
	  * required
	  **/
	  self.photoUrls = photoUrls;
	 
  	/**
	  * datatype: Array
	  **/
	  self.tags = new Array();
	 
  	/**
	  * pet status in the store
	  * datatype: StatusEnum
	  **/
	  self.status = null;
	 
  
  	self.constructFromObject = function(data) {
	  	
	  	self.id = data.id;
	  	
	  	self.category.constructFromObject(data.category);
	  	
	  	self.name = data.name;
	  	
	  	self.photoUrls = new Array();
	  	
	  	self.tags = new Array();
	  	
	  	self.status = data.status;
	  	
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
   * @return {Category}
   **/
  self.getCategory = function() {
    return self.category;
  }
  
  /**
   * @param {Category} category
   **/
  self.setCategory = function (category) {
    self.category = category;
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
  
  /**
   * @return {Array}
   **/
  self.getPhotoUrls = function() {
    return self.photoUrls;
  }
  
  /**
   * @param {Array} photoUrls
   **/
  self.setPhotoUrls = function (photoUrls) {
    self.photoUrls = photoUrls;
  }
  
  /**
   * @return {Array}
   **/
  self.getTags = function() {
    return self.tags;
  }
  
  /**
   * @param {Array} tags
   **/
  self.setTags = function (tags) {
    self.tags = tags;
  }
  
  /**
   * get pet status in the store
   * @return {StatusEnum}
   **/
  self.getStatus = function() {
    return self.status;
  }
  
  /**
   * set pet status in the store
   * @param {StatusEnum} status
   **/
  self.setStatus = function (status) {
    self.status = status;
  }
  

  self.toJson = function () {
  	return JSON.stringify(self);
  }
}
