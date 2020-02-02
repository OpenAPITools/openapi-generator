package org.openapitools.vertxweb.server.api;

import io.vertx.ext.web.FileUpload;
import org.openapitools.vertxweb.server.model.ModelApiResponse;
import org.openapitools.vertxweb.server.model.Pet;

import org.openapitools.vertxweb.server.ParameterCast;
import org.openapitools.vertxweb.server.ApiException;

import com.fasterxml.jackson.core.type.TypeReference;
import io.vertx.core.json.Json;
import io.vertx.core.Handler;
import io.vertx.core.http.HttpServerResponse;
import io.vertx.ext.web.RoutingContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.reactivex.Single;

import java.util.List;
import java.util.Map;

public class PetApiHandler {

    private static final Logger logger = LoggerFactory.getLogger(PetApiHandler.class);
    private PetApi apiImpl = new PetApiImpl();

    public PetApiHandler(Map<String, Handler<RoutingContext>> operationHandlers) {
        operationHandlers.put("addPet", this::addPet);
        operationHandlers.put("deletePet", this::deletePet);
        operationHandlers.put("findPetsByStatus", this::findPetsByStatus);
        operationHandlers.put("findPetsByTags", this::findPetsByTags);
        operationHandlers.put("getPetById", this::getPetById);
        operationHandlers.put("updatePet", this::updatePet);
        operationHandlers.put("updatePetWithForm", this::updatePetWithForm);
        operationHandlers.put("uploadFile", this::uploadFile);
    }

    private void addPet(RoutingContext routingContext) {
        logger.info("addPet()");
        HttpServerResponse response = routingContext.response();

        Single.defer( () -> {

            String jsonString = routingContext.getBodyAsString();
            Pet pet = jsonString == null ? null : Json.decodeValue(jsonString, new TypeReference<Pet>(){});
            logger.info("Parameter pet is {}", pet);
            return apiImpl.addPet(pet);
        })
        .subscribe(
            apiResponse -> {
                response.setStatusCode(apiResponse.getStatusCode())
                        .end(Json.encodePrettily(apiResponse.getData()));
            }, error -> {
                if (error instanceof ApiException) {
                    ApiException apiException = (ApiException) error;
                    response.setStatusCode(apiException.getStatusCode()).end(apiException.getMessage());
                } else {
                    response.setStatusCode(500).end(error.getMessage());
                }
            }).dispose();
    }


    private void deletePet(RoutingContext routingContext) {
        logger.info("deletePet()");
        HttpServerResponse response = routingContext.response();

        Single.defer( () -> {
            Long petId = ParameterCast.toLong(routingContext.pathParams().get("petId"));
            String apiKey = ParameterCast.toString(routingContext.request().getHeader("api_key"));

            logger.info("Parameter petId is {}", petId);
            logger.info("Parameter apiKey is {}", apiKey);
            return apiImpl.deletePet(petId, apiKey);
        })
        .subscribe(
            apiResponse -> {
                response.setStatusCode(apiResponse.getStatusCode())
                        .end(Json.encodePrettily(apiResponse.getData()));
            }, error -> {
                if (error instanceof ApiException) {
                    ApiException apiException = (ApiException) error;
                    response.setStatusCode(apiException.getStatusCode()).end(apiException.getMessage());
                } else {
                    response.setStatusCode(500).end(error.getMessage());
                }
            }).dispose();
    }


    private void findPetsByStatus(RoutingContext routingContext) {
        logger.info("findPetsByStatus()");
        HttpServerResponse response = routingContext.response();

        Single.defer( () -> {
            List<String> status = routingContext.request().params().getAll("status");

            logger.info("Parameter status is {}", status);
            return apiImpl.findPetsByStatus(status);
        })
        .subscribe(
            apiResponse -> {
                response.setStatusCode(apiResponse.getStatusCode())
                        .end(Json.encodePrettily(apiResponse.getData()));
            }, error -> {
                if (error instanceof ApiException) {
                    ApiException apiException = (ApiException) error;
                    response.setStatusCode(apiException.getStatusCode()).end(apiException.getMessage());
                } else {
                    response.setStatusCode(500).end(error.getMessage());
                }
            }).dispose();
    }


    private void findPetsByTags(RoutingContext routingContext) {
        logger.info("findPetsByTags()");
        HttpServerResponse response = routingContext.response();

        Single.defer( () -> {
            List<String> tags = routingContext.request().params().getAll("tags");

            logger.info("Parameter tags is {}", tags);
            return apiImpl.findPetsByTags(tags);
        })
        .subscribe(
            apiResponse -> {
                response.setStatusCode(apiResponse.getStatusCode())
                        .end(Json.encodePrettily(apiResponse.getData()));
            }, error -> {
                if (error instanceof ApiException) {
                    ApiException apiException = (ApiException) error;
                    response.setStatusCode(apiException.getStatusCode()).end(apiException.getMessage());
                } else {
                    response.setStatusCode(500).end(error.getMessage());
                }
            }).dispose();
    }


    private void getPetById(RoutingContext routingContext) {
        logger.info("getPetById()");
        HttpServerResponse response = routingContext.response();

        Single.defer( () -> {
            Long petId = ParameterCast.toLong(routingContext.pathParams().get("petId"));

            logger.info("Parameter petId is {}", petId);
            return apiImpl.getPetById(petId);
        })
        .subscribe(
            apiResponse -> {
                response.setStatusCode(apiResponse.getStatusCode())
                        .end(Json.encodePrettily(apiResponse.getData()));
            }, error -> {
                if (error instanceof ApiException) {
                    ApiException apiException = (ApiException) error;
                    response.setStatusCode(apiException.getStatusCode()).end(apiException.getMessage());
                } else {
                    response.setStatusCode(500).end(error.getMessage());
                }
            }).dispose();
    }


    private void updatePet(RoutingContext routingContext) {
        logger.info("updatePet()");
        HttpServerResponse response = routingContext.response();

        Single.defer( () -> {

            String jsonString = routingContext.getBodyAsString();
            Pet pet = jsonString == null ? null : Json.decodeValue(jsonString, new TypeReference<Pet>(){});
            logger.info("Parameter pet is {}", pet);
            return apiImpl.updatePet(pet);
        })
        .subscribe(
            apiResponse -> {
                response.setStatusCode(apiResponse.getStatusCode())
                        .end(Json.encodePrettily(apiResponse.getData()));
            }, error -> {
                if (error instanceof ApiException) {
                    ApiException apiException = (ApiException) error;
                    response.setStatusCode(apiException.getStatusCode()).end(apiException.getMessage());
                } else {
                    response.setStatusCode(500).end(error.getMessage());
                }
            }).dispose();
    }


    private void updatePetWithForm(RoutingContext routingContext) {
        logger.info("updatePetWithForm()");
        HttpServerResponse response = routingContext.response();

        Single.defer( () -> {
            Long petId = ParameterCast.toLong(routingContext.pathParams().get("petId"));
            String name = ParameterCast.toString(routingContext.request().getFormAttribute("name"));
            String status = ParameterCast.toString(routingContext.request().getFormAttribute("status"));

            logger.info("Parameter petId is {}", petId);
            logger.info("Parameter name is {}", name);
            logger.info("Parameter status is {}", status);
            return apiImpl.updatePetWithForm(petId, name, status);
        })
        .subscribe(
            apiResponse -> {
                response.setStatusCode(apiResponse.getStatusCode())
                        .end(Json.encodePrettily(apiResponse.getData()));
            }, error -> {
                if (error instanceof ApiException) {
                    ApiException apiException = (ApiException) error;
                    response.setStatusCode(apiException.getStatusCode()).end(apiException.getMessage());
                } else {
                    response.setStatusCode(500).end(error.getMessage());
                }
            }).dispose();
    }


    private void uploadFile(RoutingContext routingContext) {
        logger.info("uploadFile()");
        HttpServerResponse response = routingContext.response();

        Single.defer( () -> {
            Long petId = ParameterCast.toLong(routingContext.pathParams().get("petId"));
            String additionalMetadata = ParameterCast.toString(routingContext.request().getFormAttribute("additionalMetadata"));
            FileUpload file = routingContext.fileUploads().iterator().next();

            logger.info("Parameter petId is {}", petId);
            logger.info("Parameter additionalMetadata is {}", additionalMetadata);
            logger.info("Parameter file is {}", file);
            return apiImpl.uploadFile(petId, additionalMetadata, file);
        })
        .subscribe(
            apiResponse -> {
                response.setStatusCode(apiResponse.getStatusCode())
                        .end(Json.encodePrettily(apiResponse.getData()));
            }, error -> {
                if (error instanceof ApiException) {
                    ApiException apiException = (ApiException) error;
                    response.setStatusCode(apiException.getStatusCode()).end(apiException.getMessage());
                } else {
                    response.setStatusCode(500).end(error.getMessage());
                }
            }).dispose();
    }

}
