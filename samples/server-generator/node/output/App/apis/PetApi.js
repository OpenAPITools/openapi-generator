var swagger = require("swagger-node-express");
var url = require("url");
var errors = swagger.errors;

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
    "path" : "/pet.{format}/{petId}",
    "notes" : "Returns a pet based on ID",
    "summary" : "Find pet by ID",
    "method": "GET",
    "params" : [].concat([swagger.pathParam("petId", "ID of pet that needs to be fetched")]).concat([]).concat([]),
    "responseClass" : "Pet",
    "errorResponses" : [errors.invalid('id'), errors.notFound('Pet')],
    "nickname" : "getPetById"
  },
  'action': function (req,res) {
    if (!req.params.petId) {
      throw errors.invalid('petId');
    }
    writeResponse(res, {message: "how about implementing getPetById as a GET method?"});    
  }
};
exports.addPet = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/pet.{format}",
    "notes" : "",
    "summary" : "Add a new pet to the store",
    "method": "POST",
    "params" : [].concat([]).concat([]).concat([swagger.postParam("Pet", "Pet object that needs to be added to the store", true)
    ]),
    "responseClass" : "",
    "errorResponses" : [errors.invalid('id'), errors.notFound('')],
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
    "path" : "/pet.{format}",
    "notes" : "",
    "summary" : "Update an existing pet",
    "method": "PUT",
    "params" : [].concat([]).concat([]).concat([swagger.postParam("Pet", "Pet object that needs to be updated in the store", true)
    ]),
    "responseClass" : "",
    "errorResponses" : [errors.invalid('id'), errors.notFound('')],
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
    "path" : "/pet.{format}/findByStatus",
    "notes" : "Multiple status values can be provided with comma seperated strings",
    "summary" : "Finds Pets by status",
    "method": "GET",
    "params" : [swagger.queryParam("status", "Status values that need to be considered for filter", "string", true, true, "", "available")].concat([]).concat([]).concat([]),
    "responseClass" : "List[Pet]",
    "errorResponses" : [errors.invalid('id'), errors.notFound('List[Pet]')],
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
    "path" : "/pet.{format}/findByTags",
    "notes" : "Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.",
    "summary" : "Finds Pets by tags",
    "method": "GET",
    "params" : [swagger.queryParam("tags", "Tags to filter by", "string", true, true, "")].concat([]).concat([]).concat([]),
    "responseClass" : "List[Pet]",
    "errorResponses" : [errors.invalid('id'), errors.notFound('List[Pet]')],
    "nickname" : "findPetsByTags"
  },
  'action': function (req,res) {
    if (!req.params.tags) {
      throw errors.invalid('tags');
    }
    writeResponse(res, {message: "how about implementing findPetsByTags as a GET method?"});    
  }
};

