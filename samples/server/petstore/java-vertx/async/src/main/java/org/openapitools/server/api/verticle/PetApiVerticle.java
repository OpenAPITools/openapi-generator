package org.openapitools.server.api.verticle;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.eventbus.Message;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

import java.io.File;
import org.openapitools.server.api.MainApiException;
import org.openapitools.server.api.model.ModelApiResponse;
import org.openapitools.server.api.model.Pet;

import java.util.List;
import java.util.Map;

public class PetApiVerticle extends AbstractVerticle {
    final static Logger LOGGER = LoggerFactory.getLogger(PetApiVerticle.class); 
    
    final static String ADDPET_SERVICE_ID = "addPet";
    final static String DELETEPET_SERVICE_ID = "deletePet";
    final static String FINDPETSBYSTATUS_SERVICE_ID = "findPetsByStatus";
    final static String FINDPETSBYTAGS_SERVICE_ID = "findPetsByTags";
    final static String GETPETBYID_SERVICE_ID = "getPetById";
    final static String UPDATEPET_SERVICE_ID = "updatePet";
    final static String UPDATEPETWITHFORM_SERVICE_ID = "updatePetWithForm";
    final static String UPLOADFILE_SERVICE_ID = "uploadFile";
    
    final PetApi service;

    public PetApiVerticle() {
        try {
            Class serviceImplClass = getClass().getClassLoader().loadClass("org.openapitools.server.api.verticle.PetApiImpl");
            service = (PetApi)serviceImplClass.newInstance();
        } catch (Exception e) {
            logUnexpectedError("PetApiVerticle constructor", e);
            throw new RuntimeException(e);
        }
    }

    @Override
    public void start() throws Exception {
        
        //Consumer for addPet
        vertx.eventBus().<JsonObject> consumer(ADDPET_SERVICE_ID).handler(message -> {
            try {
                // Workaround for #allParams section clearing the vendorExtensions map
                String serviceId = "addPet";
                JsonObject bodyParam = message.body().getJsonObject("body");
                if (bodyParam == null) {
                    manageError(message, new MainApiException(400, "body is required"), serviceId);
                    return;
                }
                Pet body = Json.mapper.readValue(bodyParam.encode(), Pet.class);
                service.addPet(body, result -> {
                    if (result.succeeded())
                        message.reply(null);
                    else {
                        Throwable cause = result.cause();
                        manageError(message, cause, "addPet");
                    }
                });
            } catch (Exception e) {
                logUnexpectedError("addPet", e);
                message.fail(MainApiException.INTERNAL_SERVER_ERROR.getStatusCode(), MainApiException.INTERNAL_SERVER_ERROR.getStatusMessage());
            }
        });
        
        //Consumer for deletePet
        vertx.eventBus().<JsonObject> consumer(DELETEPET_SERVICE_ID).handler(message -> {
            try {
                // Workaround for #allParams section clearing the vendorExtensions map
                String serviceId = "deletePet";
                String petIdParam = message.body().getString("petId");
                if(petIdParam == null) {
                    manageError(message, new MainApiException(400, "petId is required"), serviceId);
                    return;
                }
                Long petId = Json.mapper.readValue(petIdParam, Long.class);
                String apiKeyParam = message.body().getString("api_key");
                String apiKey = (apiKeyParam == null) ? null : apiKeyParam;
                service.deletePet(petId, apiKey, result -> {
                    if (result.succeeded())
                        message.reply(null);
                    else {
                        Throwable cause = result.cause();
                        manageError(message, cause, "deletePet");
                    }
                });
            } catch (Exception e) {
                logUnexpectedError("deletePet", e);
                message.fail(MainApiException.INTERNAL_SERVER_ERROR.getStatusCode(), MainApiException.INTERNAL_SERVER_ERROR.getStatusMessage());
            }
        });
        
        //Consumer for findPetsByStatus
        vertx.eventBus().<JsonObject> consumer(FINDPETSBYSTATUS_SERVICE_ID).handler(message -> {
            try {
                // Workaround for #allParams section clearing the vendorExtensions map
                String serviceId = "findPetsByStatus";
                JsonArray statusParam = message.body().getJsonArray("status");
                if(statusParam == null) {
                    manageError(message, new MainApiException(400, "status is required"), serviceId);
                    return;
                }
                List<String> status = Json.mapper.readValue(statusParam.encode(),
                    Json.mapper.getTypeFactory().constructCollectionType(List.class, String.class));
                service.findPetsByStatus(status, result -> {
                    if (result.succeeded())
                        message.reply(new JsonArray(Json.encode(result.result())).encodePrettily());
                    else {
                        Throwable cause = result.cause();
                        manageError(message, cause, "findPetsByStatus");
                    }
                });
            } catch (Exception e) {
                logUnexpectedError("findPetsByStatus", e);
                message.fail(MainApiException.INTERNAL_SERVER_ERROR.getStatusCode(), MainApiException.INTERNAL_SERVER_ERROR.getStatusMessage());
            }
        });
        
        //Consumer for findPetsByTags
        vertx.eventBus().<JsonObject> consumer(FINDPETSBYTAGS_SERVICE_ID).handler(message -> {
            try {
                // Workaround for #allParams section clearing the vendorExtensions map
                String serviceId = "findPetsByTags";
                JsonArray tagsParam = message.body().getJsonArray("tags");
                if(tagsParam == null) {
                    manageError(message, new MainApiException(400, "tags is required"), serviceId);
                    return;
                }
                List<String> tags = Json.mapper.readValue(tagsParam.encode(),
                    Json.mapper.getTypeFactory().constructCollectionType(List.class, String.class));
                service.findPetsByTags(tags, result -> {
                    if (result.succeeded())
                        message.reply(new JsonArray(Json.encode(result.result())).encodePrettily());
                    else {
                        Throwable cause = result.cause();
                        manageError(message, cause, "findPetsByTags");
                    }
                });
            } catch (Exception e) {
                logUnexpectedError("findPetsByTags", e);
                message.fail(MainApiException.INTERNAL_SERVER_ERROR.getStatusCode(), MainApiException.INTERNAL_SERVER_ERROR.getStatusMessage());
            }
        });
        
        //Consumer for getPetById
        vertx.eventBus().<JsonObject> consumer(GETPETBYID_SERVICE_ID).handler(message -> {
            try {
                // Workaround for #allParams section clearing the vendorExtensions map
                String serviceId = "getPetById";
                String petIdParam = message.body().getString("petId");
                if(petIdParam == null) {
                    manageError(message, new MainApiException(400, "petId is required"), serviceId);
                    return;
                }
                Long petId = Json.mapper.readValue(petIdParam, Long.class);
                service.getPetById(petId, result -> {
                    if (result.succeeded())
                        message.reply(new JsonObject(Json.encode(result.result())).encodePrettily());
                    else {
                        Throwable cause = result.cause();
                        manageError(message, cause, "getPetById");
                    }
                });
            } catch (Exception e) {
                logUnexpectedError("getPetById", e);
                message.fail(MainApiException.INTERNAL_SERVER_ERROR.getStatusCode(), MainApiException.INTERNAL_SERVER_ERROR.getStatusMessage());
            }
        });
        
        //Consumer for updatePet
        vertx.eventBus().<JsonObject> consumer(UPDATEPET_SERVICE_ID).handler(message -> {
            try {
                // Workaround for #allParams section clearing the vendorExtensions map
                String serviceId = "updatePet";
                JsonObject bodyParam = message.body().getJsonObject("body");
                if (bodyParam == null) {
                    manageError(message, new MainApiException(400, "body is required"), serviceId);
                    return;
                }
                Pet body = Json.mapper.readValue(bodyParam.encode(), Pet.class);
                service.updatePet(body, result -> {
                    if (result.succeeded())
                        message.reply(null);
                    else {
                        Throwable cause = result.cause();
                        manageError(message, cause, "updatePet");
                    }
                });
            } catch (Exception e) {
                logUnexpectedError("updatePet", e);
                message.fail(MainApiException.INTERNAL_SERVER_ERROR.getStatusCode(), MainApiException.INTERNAL_SERVER_ERROR.getStatusMessage());
            }
        });
        
        //Consumer for updatePetWithForm
        vertx.eventBus().<JsonObject> consumer(UPDATEPETWITHFORM_SERVICE_ID).handler(message -> {
            try {
                // Workaround for #allParams section clearing the vendorExtensions map
                String serviceId = "updatePetWithForm";
                String petIdParam = message.body().getString("petId");
                if(petIdParam == null) {
                    manageError(message, new MainApiException(400, "petId is required"), serviceId);
                    return;
                }
                Long petId = Json.mapper.readValue(petIdParam, Long.class);
                String nameParam = message.body().getString("name");
                String name = (nameParam == null) ? null : nameParam;
                String statusParam = message.body().getString("status");
                String status = (statusParam == null) ? null : statusParam;
                service.updatePetWithForm(petId, name, status, result -> {
                    if (result.succeeded())
                        message.reply(null);
                    else {
                        Throwable cause = result.cause();
                        manageError(message, cause, "updatePetWithForm");
                    }
                });
            } catch (Exception e) {
                logUnexpectedError("updatePetWithForm", e);
                message.fail(MainApiException.INTERNAL_SERVER_ERROR.getStatusCode(), MainApiException.INTERNAL_SERVER_ERROR.getStatusMessage());
            }
        });
        
        //Consumer for uploadFile
        vertx.eventBus().<JsonObject> consumer(UPLOADFILE_SERVICE_ID).handler(message -> {
            try {
                // Workaround for #allParams section clearing the vendorExtensions map
                String serviceId = "uploadFile";
                String petIdParam = message.body().getString("petId");
                if(petIdParam == null) {
                    manageError(message, new MainApiException(400, "petId is required"), serviceId);
                    return;
                }
                Long petId = Json.mapper.readValue(petIdParam, Long.class);
                String additionalMetadataParam = message.body().getString("additionalMetadata");
                String additionalMetadata = (additionalMetadataParam == null) ? null : additionalMetadataParam;
                String fileParam = message.body().getString("file");
                File file = (fileParam == null) ? null : Json.mapper.readValue(fileParam, File.class);
                service.uploadFile(petId, additionalMetadata, file, result -> {
                    if (result.succeeded())
                        message.reply(new JsonObject(Json.encode(result.result())).encodePrettily());
                    else {
                        Throwable cause = result.cause();
                        manageError(message, cause, "uploadFile");
                    }
                });
            } catch (Exception e) {
                logUnexpectedError("uploadFile", e);
                message.fail(MainApiException.INTERNAL_SERVER_ERROR.getStatusCode(), MainApiException.INTERNAL_SERVER_ERROR.getStatusMessage());
            }
        });
        
    }
    
    private void manageError(Message<JsonObject> message, Throwable cause, String serviceName) {
        int code = MainApiException.INTERNAL_SERVER_ERROR.getStatusCode();
        String statusMessage = MainApiException.INTERNAL_SERVER_ERROR.getStatusMessage();
        if (cause instanceof MainApiException) {
            code = ((MainApiException)cause).getStatusCode();
            statusMessage = ((MainApiException)cause).getStatusMessage();
        } else {
            logUnexpectedError(serviceName, cause); 
        }
            
        message.fail(code, statusMessage);
    }
    
    private void logUnexpectedError(String serviceName, Throwable cause) {
        LOGGER.error("Unexpected error in "+ serviceName, cause);
    }
}
