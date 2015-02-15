exports.models = {
  "User": {
  "properties" : {
    "id" : {
      "type" : "integer",
      "format" : "int64"
    },
    "username" : {
      "type" : "string"
    },
    "firstName" : {
      "type" : "string"
    },
    "lastName" : {
      "type" : "string"
    },
    "email" : {
      "type" : "string"
    },
    "password" : {
      "type" : "string"
    },
    "phone" : {
      "type" : "string"
    },
    "userStatus" : {
      "type" : "integer",
      "format" : "int32",
      "description" : "User Status"
    }
  },
  "xml" : {
    "name" : "User"
  },
  "id" : "User"
},"Category": {
  "properties" : {
    "id" : {
      "type" : "integer",
      "format" : "int64"
    },
    "name" : {
      "type" : "string"
    }
  },
  "xml" : {
    "name" : "Category"
  },
  "id" : "Category"
},"Pet": {
  "required" : [ "name", "photoUrls" ],
  "properties" : {
    "id" : {
      "type" : "integer",
      "format" : "int64"
    },
    "category" : {
      "$ref" : "Category"
    },
    "name" : {
      "type" : "string",
      "example" : "doggie"
    },
    "photoUrls" : {
      "type" : "array",
      "items" : {
        "type" : "string"
      }
    },
    "tags" : {
      "type" : "array",
      "items" : {
        "$ref" : "Tag"
      }
    },
    "status" : {
      "type" : "string",
      "description" : "pet status in the store",
      "enum" : [ "available", "pending", "sold" ]
    }
  },
  "xml" : {
    "name" : "Pet"
  },
  "id" : "Pet"
},"Tag": {
  "properties" : {
    "id" : {
      "type" : "integer",
      "format" : "int64"
    },
    "name" : {
      "type" : "string"
    }
  },
  "xml" : {
    "name" : "Tag"
  },
  "id" : "Tag"
},"Order": {
  "properties" : {
    "id" : {
      "type" : "integer",
      "format" : "int64"
    },
    "petId" : {
      "type" : "integer",
      "format" : "int64"
    },
    "quantity" : {
      "type" : "integer",
      "format" : "int32"
    },
    "shipDate" : {
      "type" : "string",
      "format" : "date-time"
    },
    "status" : {
      "type" : "string",
      "description" : "Order Status",
      "enum" : [ "placed", "approved", "delivered" ]
    },
    "complete" : {
      "type" : "boolean"
    }
  },
  "xml" : {
    "name" : "Order"
  },
  "id" : "Order"
}
}