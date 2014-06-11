var swagger = require("swagger-node-express");
var url = require("url");
var errors = swagger.errors;
var params = swagger.params;

/* add model includes */

function writeResponse (response, data) {
  response.header('Access-Control-Allow-Origin', "*");
  response.header("Access-Control-Allow-Methods", "GET, POST, DELETE, PUT");
  response.header("Access-Control-Allow-Headers", "Content-Type");
  response.header("Content-Type", "application/json; charset=utf-8");
  response.send(JSON.stringify(data));
}

exports.models = models = require("../models.js");

exports.getPetById = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/pet/{petId}",
    "notes" : "Returns a pet based on ID",
    "summary" : "Find pet by ID",
    "method": "GET",
    "params" : [].concat([params.path("petId", "ID of pet that needs to be fetched")]).concat([]).concat([]),
    "type" : "Pet",
    "responseMessages" : [errors.invalid('id'), errors.notFound('Pet')],
    "nickname" : "getPetById"
  },
  'action': function (req,res) {
    if (!req.params.petId) {
      throw errors.invalid('petId');
    }
    writeResponse(res, {message: "how about implementing getPetById as a GET method?"});    
  }
};
exports.deletePet = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/pet/{petId}",
    "notes" : "",
    "summary" : "Deletes a pet",
    "method": "DELETE",
    "params" : [].concat([params.path("petId", "Pet id to delete")]).concat([]).concat([]),
    "type" : "",
    "responseMessages" : [errors.invalid('id'), errors.notFound('')],
    "nickname" : "deletePet"
  },
  'action': function (req,res) {
    if (!req.params.petId) {
      throw errors.invalid('petId');
    }
    writeResponse(res, {message: "how about implementing deletePet as a DELETE method?"});    
  }
};
exports.partialUpdate = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/pet/{petId}",
    "notes" : "",
    "summary" : "partial updates to a pet",
    "method": "PATCH",
    "params" : [].concat([params.path("petId", "ID of pet that needs to be fetched")]).concat([]).concat([params.body("body", "Pet", "Pet object that needs to be added to the store", true)
    ]),
    "type" : "List[Pet]",
    "responseMessages" : [errors.invalid('id'), errors.notFound('List[Pet]')],
    "nickname" : "partialUpdate"
  },
  'action': function (req,res) {
    if (!req.params.petId) {
      throw errors.invalid('petId');
    }
    if (!req.params.body) {
      throw errors.invalid('body');
    }
    writeResponse(res, {message: "how about implementing partialUpdate as a PATCH method?"});    
  }
};
exports.updatePetWithForm = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/pet/{petId}",
    "notes" : "",
    "summary" : "Updates a pet in the store with form data",
    "method": "POST",
    "params" : [].concat([params.path("petId", "ID of pet that needs to be updated")]).concat([]).concat([]),
    "type" : "",
    "responseMessages" : [errors.invalid('id'), errors.notFound('')],
    "nickname" : "updatePetWithForm"
  },
  'action': function (req,res) {
    if (!req.params.petId) {
      throw errors.invalid('petId');
    }
    writeResponse(res, {message: "how about implementing updatePetWithForm as a POST method?"});    
  }
};
exports.uploadFile = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/pet/uploadImage",
    "notes" : "",
    "summary" : "uploads an image",
    "method": "POST",
    "params" : [].concat([]).concat([]).concat([params.body("body", "File", "file to upload", false)
    ]),
    "type" : "",
    "responseMessages" : [errors.invalid('id'), errors.notFound('')],
    "nickname" : "uploadFile"
  },
  'action': function (req,res) {
    writeResponse(res, {message: "how about implementing uploadFile as a POST method?"});    
  }
};
exports.addPet = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/pet",
    "notes" : "",
    "summary" : "Add a new pet to the store",
    "method": "POST",
    "params" : [].concat([]).concat([]).concat([params.body("body", "Pet", "Pet object that needs to be added to the store", true)
    ]),
    "type" : "",
    "responseMessages" : [errors.invalid('id'), errors.notFound('')],
    "nickname" : "addPet"
  },
  'action': function (req,res) {
    if (!req.params.body) {
      throw errors.invalid('body');
    }
    writeResponse(res, {message: "how about implementing addPet as a POST method?"});    
  }
};
exports.updatePet = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/pet",
    "notes" : "",
    "summary" : "Update an existing pet",
    "method": "PUT",
    "params" : [].concat([]).concat([]).concat([params.body("body", "Pet", "Pet object that needs to be updated in the store", true)
    ]),
    "type" : "",
    "responseMessages" : [errors.invalid('id'), errors.notFound('')],
    "nickname" : "updatePet"
  },
  'action': function (req,res) {
    if (!req.params.body) {
      throw errors.invalid('body');
    }
    writeResponse(res, {message: "how about implementing updatePet as a PUT method?"});    
  }
};
exports.findPetsByStatus = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/pet/findByStatus",
    "notes" : "Multiple status values can be provided with comma seperated strings",
    "summary" : "Finds Pets by status",
    "method": "GET",
    "params" : [params.query("status", "Status values that need to be considered for filter", "string", true, true, "LIST[available,pending,sold]", "available")].concat([]).concat([]).concat([]),
    "type" : "List[Pet]",
    "responseMessages" : [errors.invalid('id'), errors.notFound('List[Pet]')],
    "nickname" : "findPetsByStatus"
  },
  'action': function (req,res) {
    if (!req.params.status) {
      throw errors.invalid('status');
    }
    writeResponse(res, {message: "how about implementing findPetsByStatus as a GET method?"});    
  }
};
exports.findPetsByTags = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/pet/findByTags",
    "notes" : "Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.",
    "summary" : "Finds Pets by tags",
    "method": "GET",
    "params" : [params.query("tags", "Tags to filter by", "string", true, true, "")].concat([]).concat([]).concat([]),
    "type" : "List[Pet]",
    "responseMessages" : [errors.invalid('id'), errors.notFound('List[Pet]')],
    "nickname" : "findPetsByTags"
  },
  'action': function (req,res) {
    if (!req.params.tags) {
      throw errors.invalid('tags');
    }
    writeResponse(res, {message: "how about implementing findPetsByTags as a GET method?"});    
  }
};

