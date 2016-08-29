# Documentation for Swagger Petstore 1.0.0 Tizen SDK

## How do I get the doc files?
First generate source code by running `swagger-codegen`
Then run `doc/generateDocumentation.sh` from the output folder. It will generate all the doc files and put them in the `doc/SDK` directory.
To successfully generate documentation it needs `Doxygen` installed in the path.
*Note* - Before generating the documentation, put the logo of the project as the file `doc/logo.png` before running `doxygen`.


## How do I use this?
This is the structure of the doc folder:

```
.
├── logo.png                  \\Logo of the project
├── Doxyfile                  \\Doxygen config files
├── generateDocumentation.sh  \\Script to run to generate documentation
├── README.md                 \\This file
├── SDK                       \\Documentation for all classes in Tizen Client SDK for Artik Cloud. See ./html/index.html
│   └── html

```

## *tl;dr* run this:

```
doc/generateDocumentation.sh
```

The above SDK folder will be generated. See the index.html inside the SDK folder.


## What's Doxygen?
Doxygen is the de facto standard tool for generating/extracting documentation from annotated/unannotated C++ sources, but it also supports other popular programming languages such as C, Objective-C, C#, PHP, Java, Python, IDL (Corba, Microsoft, and UNO/OpenOffice flavors), Fortran, VHDL, Tcl, and to some extent D.

Check out [Doxygen](https://www.doxygen.org/) for additional information about the Doxygen project.

## I Don't want to run Doxygen. What are the API files for accessing the REST endpoints?

* [PetManager](../src/PetManager.cpp)
  * *addPetSync* / *addPetAsync* - Add a new pet to the store.
  * *deletePetSync* / *deletePetAsync* - Deletes a pet.
  * *findPetsByStatusSync* / *findPetsByStatusAsync* - Finds Pets by status.
  * *findPetsByTagsSync* / *findPetsByTagsAsync* - Finds Pets by tags.
  * *getPetByIdSync* / *getPetByIdAsync* - Find pet by ID.
  * *updatePetSync* / *updatePetAsync* - Update an existing pet.
  * *updatePetWithFormSync* / *updatePetWithFormAsync* - Updates a pet in the store with form data.
  * *uploadFileSync* / *uploadFileAsync* - uploads an image.

* [StoreManager](../src/StoreManager.cpp)
  * *deleteOrderSync* / *deleteOrderAsync* - Delete purchase order by ID.
  * *getInventorySync* / *getInventoryAsync* - Returns pet inventories by status.
  * *getOrderByIdSync* / *getOrderByIdAsync* - Find purchase order by ID.
  * *placeOrderSync* / *placeOrderAsync* - Place an order for a pet.

* [UserManager](../src/UserManager.cpp)
  * *createUserSync* / *createUserAsync* - Create user.
  * *createUsersWithArrayInputSync* / *createUsersWithArrayInputAsync* - Creates list of users with given input array.
  * *createUsersWithListInputSync* / *createUsersWithListInputAsync* - Creates list of users with given input array.
  * *deleteUserSync* / *deleteUserAsync* - Delete user.
  * *getUserByNameSync* / *getUserByNameAsync* - Get user by user name.
  * *loginUserSync* / *loginUserAsync* - Logs user into the system.
  * *logoutUserSync* / *logoutUserAsync* - Logs out current logged in user session.
  * *updateUserSync* / *updateUserAsync* - Updated user.


## What are the Model files for the data structures/objects?

* [ApiResponse](../src/ApiResponse.cpp) - 
* [Category](../src/Category.cpp) - 
* [Order](../src/Order.cpp) - 
* [Pet](../src/Pet.cpp) - 
* [Tag](../src/Tag.cpp) - 
* [User](../src/User.cpp) - 
