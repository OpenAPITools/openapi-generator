exports.models = {
  "Pet": {
  "id" : "Pet",
  "properties" : {
    "id" : {
      "type" : "long",
      "required" : false
    },
    "tags" : {
      "type" : "Array",
      "required" : false,
      "items" : {
        "$ref" : "Tag"
      }
    },
    "category" : {
      "type" : "Category",
      "required" : false
    },
    "status" : {
      "type" : "string",
      "required" : false,
      "description" : "pet status in the store",
      "allowableValues" : {
        "values" : [ "available", "pending", "sold" ],
        "valueType" : "LIST"
      }
    },
    "name" : {
      "type" : "string",
      "required" : false
    },
    "photoUrls" : {
      "type" : "Array",
      "required" : false,
      "items" : {
        "type" : "string"
      }
    }
  }
},"Category": {
  "id" : "Category",
  "properties" : {
    "id" : {
      "type" : "long",
      "required" : false
    },
    "name" : {
      "type" : "string",
      "required" : false
    }
  }
},"Tag": {
  "id" : "Tag",
  "properties" : {
    "id" : {
      "type" : "long",
      "required" : false
    },
    "name" : {
      "type" : "string",
      "required" : false
    }
  }
},"User": {
  "id" : "User",
  "properties" : {
    "id" : {
      "type" : "long",
      "required" : false
    },
    "lastName" : {
      "type" : "string",
      "required" : false
    },
    "username" : {
      "type" : "string",
      "required" : false
    },
    "phone" : {
      "type" : "string",
      "required" : false
    },
    "email" : {
      "type" : "string",
      "required" : false
    },
    "userStatus" : {
      "type" : "int",
      "required" : false,
      "description" : "User Status",
      "allowableValues" : {
        "values" : [ "1-registered", "2-active", "3-closed" ],
        "valueType" : "LIST"
      }
    },
    "firstName" : {
      "type" : "string",
      "required" : false
    },
    "password" : {
      "type" : "string",
      "required" : false
    }
  }
},"Order": {
  "id" : "Order",
  "properties" : {
    "id" : {
      "type" : "long",
      "required" : false
    },
    "petId" : {
      "type" : "long",
      "required" : false
    },
    "status" : {
      "type" : "string",
      "required" : false,
      "description" : "Order Status",
      "allowableValues" : {
        "values" : [ "placed", " approved", " delivered" ],
        "valueType" : "LIST"
      }
    },
    "quantity" : {
      "type" : "int",
      "required" : false
    },
    "shipDate" : {
      "type" : "Date",
      "required" : false
    }
  }
}}
