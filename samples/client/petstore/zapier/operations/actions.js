const PetApi = require('../apis/PetApi');
const StoreApi = require('../apis/StoreApi');
const UserApi = require('../apis/UserApi');
const { triggerMiddleware, isTrigger, searchMiddleware, hasSearchRequisites, isSearchAction } = require('../utils/utils');

const operations = {
    [PetApi.addPet.key]: PetApi.addPet,
    [PetApi.deletePet.key]: PetApi.deletePet,
    [PetApi.findPetsByStatus.key]: PetApi.findPetsByStatus,
    [PetApi.findPetsByTags.key]: PetApi.findPetsByTags,
    [PetApi.getPetById.key]: PetApi.getPetById,
    [PetApi.updatePet.key]: PetApi.updatePet,
    [PetApi.updatePetWithForm.key]: PetApi.updatePetWithForm,
    [PetApi.uploadFile.key]: PetApi.uploadFile,
    [StoreApi.deleteOrder.key]: StoreApi.deleteOrder,
    [StoreApi.getInventory.key]: StoreApi.getInventory,
    [StoreApi.getOrderById.key]: StoreApi.getOrderById,
    [StoreApi.placeOrder.key]: StoreApi.placeOrder,
    [UserApi.createUser.key]: UserApi.createUser,
    [UserApi.createUsersWithArrayInput.key]: UserApi.createUsersWithArrayInput,
    [UserApi.createUsersWithListInput.key]: UserApi.createUsersWithListInput,
    [UserApi.deleteUser.key]: UserApi.deleteUser,
    [UserApi.getUserByName.key]: UserApi.getUserByName,
    [UserApi.loginUser.key]: UserApi.loginUser,
    [UserApi.logoutUser.key]: UserApi.logoutUser,
    [UserApi.updateUser.key]: UserApi.updateUser,
}

module.exports = {
    searchActions: () => Object.entries(operations).reduce((operations, [key, value]) => isSearchAction(key) && hasSearchRequisites(value) ? {...operations, [key]: searchMiddleware(value)} : operations, {}),
    createActions: () => Object.entries(operations).reduce((operations, [key, value]) => !isSearchAction(key) ? {...operations, [key]: value} : operations, {}),
    triggers: () => Object.entries(operations).reduce((operations, [key, value]) => isTrigger(key) ? {...operations, [key]: triggerMiddleware(value)} : operations, {}),
}
