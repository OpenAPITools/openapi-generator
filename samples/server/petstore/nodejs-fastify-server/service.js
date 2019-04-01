class Service {
  constructor() {}

/**
 * Add a new pet to the store
 *
 * body Pet Pet object that needs to be added to the store
 * no response value expected for this operation
 **/
  async addPet(req, resp) {
    console.log("addPet");

    // TODO implement this endpoint

    return;
  };
}

/**
 * Deletes a pet
 *
 * petId Long Pet id to delete
 * apiKey String  (optional)
 * no response value expected for this operation
 **/
  async deletePet(req, resp) {
    console.log("deletePet");

    // TODO implement this endpoint

    return;
  };
}

/**
 * Finds Pets by status
 * Multiple status values can be provided with comma separated strings
 *
 * status List Status values that need to be considered for filter
 * returns List
 **/
  async findPetsByStatus(req, resp) {
    console.log("findPetsByStatus");

    // TODO implement this endpoint

    /*return {
  "photoUrls" : [ "photoUrls", "photoUrls" ],
  "name" : "doggie",
  "id" : 0,
  "category" : {
    "name" : "name",
    "id" : 6
  },
  "tags" : [ {
    "name" : "name",
    "id" : 1
  }, {
    "name" : "name",
    "id" : 1
  } ],
  "status" : "available"
};
    */

    /*return <Pet>
  <id>123456789</id>
  <name>doggie</name>
  <photoUrls>
    <photoUrls>aeiou</photoUrls>
  </photoUrls>
  <tags>
  </tags>
  <status>aeiou</status>
</Pet>;
    */

    return {};
  };
}

/**
 * Finds Pets by tags
 * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
 *
 * tags List Tags to filter by
 * returns List
 **/
  async findPetsByTags(req, resp) {
    console.log("findPetsByTags");

    // TODO implement this endpoint

    /*return {
  "photoUrls" : [ "photoUrls", "photoUrls" ],
  "name" : "doggie",
  "id" : 0,
  "category" : {
    "name" : "name",
    "id" : 6
  },
  "tags" : [ {
    "name" : "name",
    "id" : 1
  }, {
    "name" : "name",
    "id" : 1
  } ],
  "status" : "available"
};
    */

    /*return <Pet>
  <id>123456789</id>
  <name>doggie</name>
  <photoUrls>
    <photoUrls>aeiou</photoUrls>
  </photoUrls>
  <tags>
  </tags>
  <status>aeiou</status>
</Pet>;
    */

    return {};
  };
}

/**
 * Find pet by ID
 * Returns a single pet
 *
 * petId Long ID of pet to return
 * returns Pet
 **/
  async getPetById(req, resp) {
    console.log("getPetById");

    // TODO implement this endpoint

    /*return {
  "photoUrls" : [ "photoUrls", "photoUrls" ],
  "name" : "doggie",
  "id" : 0,
  "category" : {
    "name" : "name",
    "id" : 6
  },
  "tags" : [ {
    "name" : "name",
    "id" : 1
  }, {
    "name" : "name",
    "id" : 1
  } ],
  "status" : "available"
};
    */

    /*return <Pet>
  <id>123456789</id>
  <name>doggie</name>
  <photoUrls>
    <photoUrls>aeiou</photoUrls>
  </photoUrls>
  <tags>
  </tags>
  <status>aeiou</status>
</Pet>;
    */

    return {};
  };
}

/**
 * Update an existing pet
 *
 * body Pet Pet object that needs to be added to the store
 * no response value expected for this operation
 **/
  async updatePet(req, resp) {
    console.log("updatePet");

    // TODO implement this endpoint

    return;
  };
}

/**
 * Updates a pet in the store with form data
 *
 * petId Long ID of pet that needs to be updated
 * name String Updated name of the pet (optional)
 * status String Updated status of the pet (optional)
 * no response value expected for this operation
 **/
  async updatePetWithForm(req, resp) {
    console.log("updatePetWithForm");

    // TODO implement this endpoint

    return;
  };
}

/**
 * uploads an image
 *
 * petId Long ID of pet to update
 * additionalMetadata String Additional data to pass to server (optional)
 * file File file to upload (optional)
 * returns ApiResponse
 **/
  async uploadFile(req, resp) {
    console.log("uploadFile");

    // TODO implement this endpoint

    /*return {
  "code" : 0,
  "type" : "type",
  "message" : "message"
};
    */

    return {};
  };
}

/**
 * Delete purchase order by ID
 * For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
 *
 * orderId String ID of the order that needs to be deleted
 * no response value expected for this operation
 **/
  async deleteOrder(req, resp) {
    console.log("deleteOrder");

    // TODO implement this endpoint

    return;
  };
}

/**
 * Returns pet inventories by status
 * Returns a map of status codes to quantities
 *
 * returns Map
 **/
  async getInventory(req, resp) {
    console.log("getInventory");

    // TODO implement this endpoint

    return {};
  };
}

/**
 * Find purchase order by ID
 * For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions
 *
 * orderId Long ID of pet that needs to be fetched
 * returns Order
 **/
  async getOrderById(req, resp) {
    console.log("getOrderById");

    // TODO implement this endpoint

    /*return {
  "petId" : 6,
  "quantity" : 1,
  "id" : 0,
  "shipDate" : "2000-01-23T04:56:07.000+00:00",
  "complete" : false,
  "status" : "placed"
};
    */

    /*return <Order>
  <id>123456789</id>
  <petId>123456789</petId>
  <quantity>123</quantity>
  <shipDate>2000-01-23T04:56:07.000Z</shipDate>
  <status>aeiou</status>
  <complete>true</complete>
</Order>;
    */

    return {};
  };
}

/**
 * Place an order for a pet
 *
 * body Order order placed for purchasing the pet
 * returns Order
 **/
  async placeOrder(req, resp) {
    console.log("placeOrder");

    // TODO implement this endpoint

    /*return {
  "petId" : 6,
  "quantity" : 1,
  "id" : 0,
  "shipDate" : "2000-01-23T04:56:07.000+00:00",
  "complete" : false,
  "status" : "placed"
};
    */

    /*return <Order>
  <id>123456789</id>
  <petId>123456789</petId>
  <quantity>123</quantity>
  <shipDate>2000-01-23T04:56:07.000Z</shipDate>
  <status>aeiou</status>
  <complete>true</complete>
</Order>;
    */

    return {};
  };
}

/**
 * Create user
 * This can only be done by the logged in user.
 *
 * body User Created user object
 * no response value expected for this operation
 **/
  async createUser(req, resp) {
    console.log("createUser");

    // TODO implement this endpoint

    return;
  };
}

/**
 * Creates list of users with given input array
 *
 * body List List of user object
 * no response value expected for this operation
 **/
  async createUsersWithArrayInput(req, resp) {
    console.log("createUsersWithArrayInput");

    // TODO implement this endpoint

    return;
  };
}

/**
 * Creates list of users with given input array
 *
 * body List List of user object
 * no response value expected for this operation
 **/
  async createUsersWithListInput(req, resp) {
    console.log("createUsersWithListInput");

    // TODO implement this endpoint

    return;
  };
}

/**
 * Delete user
 * This can only be done by the logged in user.
 *
 * username String The name that needs to be deleted
 * no response value expected for this operation
 **/
  async deleteUser(req, resp) {
    console.log("deleteUser");

    // TODO implement this endpoint

    return;
  };
}

/**
 * Get user by user name
 *
 * username String The name that needs to be fetched. Use user1 for testing.
 * returns User
 **/
  async getUserByName(req, resp) {
    console.log("getUserByName");

    // TODO implement this endpoint

    /*return {
  "firstName" : "firstName",
  "lastName" : "lastName",
  "password" : "password",
  "userStatus" : 6,
  "phone" : "phone",
  "id" : 0,
  "email" : "email",
  "username" : "username"
};
    */

    /*return <User>
  <id>123456789</id>
  <username>aeiou</username>
  <firstName>aeiou</firstName>
  <lastName>aeiou</lastName>
  <email>aeiou</email>
  <password>aeiou</password>
  <phone>aeiou</phone>
  <userStatus>123</userStatus>
</User>;
    */

    return {};
  };
}

/**
 * Logs user into the system
 *
 * username String The user name for login
 * password String The password for login in clear text
 * returns String
 **/
  async loginUser(req, resp) {
    console.log("loginUser");

    // TODO implement this endpoint

    return {};
  };
}

/**
 * Logs out current logged in user session
 *
 * no response value expected for this operation
 **/
  async logoutUser(req, resp) {
    console.log("logoutUser");

    // TODO implement this endpoint

    return;
  };
}

/**
 * Updated user
 * This can only be done by the logged in user.
 *
 * username String name that need to be deleted
 * body User Updated user object
 * no response value expected for this operation
 **/
  async updateUser(req, resp) {
    console.log("updateUser");

    // TODO implement this endpoint

    return;
  };
}


module.exports = config => new Service();
