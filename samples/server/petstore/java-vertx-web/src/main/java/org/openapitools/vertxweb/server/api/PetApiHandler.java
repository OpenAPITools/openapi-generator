package org.openapitools.vertxweb.server.api;

import io.vertx.ext.web.FileUpload;
import org.openapitools.vertxweb.server.model.ModelApiResponse;
import org.openapitools.vertxweb.server.model.Pet;

import com.fasterxml.jackson.core.type.TypeReference;
import io.vertx.core.json.jackson.DatabindCodec;
import io.vertx.ext.web.openapi.RouterFactory;
import io.vertx.ext.web.validation.RequestParameters;
import io.vertx.ext.web.validation.RequestParameter;
import io.vertx.ext.web.validation.ValidationHandler;
import io.vertx.ext.web.RoutingContext;
import io.vertx.core.json.JsonObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;

public class PetApiHandler {

    private static final Logger logger = LoggerFactory.getLogger(PetApiHandler.class);

    private final PetApi apiImpl;

    public PetApiHandler() {
        this.apiImpl = new PetApiImpl();
    }

    public void mount(RouterFactory factory) {
        factory.operation("addPet").handler(this::addPet);
        factory.operation("deletePet").handler(this::deletePet);
        factory.operation("findPetsByStatus").handler(this::findPetsByStatus);
        factory.operation("findPetsByTags").handler(this::findPetsByTags);
        factory.operation("getPetById").handler(this::getPetById);
        factory.operation("updatePet").handler(this::updatePet);
        factory.operation("updatePetWithForm").handler(this::updatePetWithForm);
        factory.operation("uploadFile").handler(this::uploadFile);
    }

    private void addPet(RoutingContext routingContext) {
        logger.info("addPet()");

        // Param extraction
        RequestParameters requestParameters = routingContext.get(ValidationHandler.REQUEST_CONTEXT_KEY);

        RequestParameter body = requestParameters.body();
        Pet pet = body != null ? DatabindCodec.mapper().convertValue(body.get(), new TypeReference<Pet>(){}) : null;

        logger.debug("Parameter pet is {}", pet);

        apiImpl.addPet(pet)
            .onSuccess(apiResponse -> {
                routingContext.response().setStatusCode(apiResponse.getStatusCode());
                if (apiResponse.hasData()) {
                    routingContext.json(apiResponse.getData());
                } else {
                    routingContext.response().end();
                }
            })
            .onFailure(routingContext::fail);
    }

    private void deletePet(RoutingContext routingContext) {
        logger.info("deletePet()");

        // Param extraction
        RequestParameters requestParameters = routingContext.get(ValidationHandler.REQUEST_CONTEXT_KEY);

        Long petId = requestParameters.pathParameter("petId") != null ? requestParameters.pathParameter("petId").getLong() : null;
        String apiKey = requestParameters.headerParameter("api_key") != null ? requestParameters.headerParameter("api_key").getString() : null;

        logger.debug("Parameter petId is {}", petId);
        logger.debug("Parameter apiKey is {}", apiKey);

        apiImpl.deletePet(petId, apiKey)
            .onSuccess(apiResponse -> {
                routingContext.response().setStatusCode(apiResponse.getStatusCode());
                if (apiResponse.hasData()) {
                    routingContext.json(apiResponse.getData());
                } else {
                    routingContext.response().end();
                }
            })
            .onFailure(routingContext::fail);
    }

    private void findPetsByStatus(RoutingContext routingContext) {
        logger.info("findPetsByStatus()");

        // Param extraction
        RequestParameters requestParameters = routingContext.get(ValidationHandler.REQUEST_CONTEXT_KEY);

        List<String> status = requestParameters.queryParameter("status") != null ? DatabindCodec.mapper().convertValue(requestParameters.queryParameter("status").get(), new TypeReference<List<String>>(){}) : null;

        logger.debug("Parameter status is {}", status);

        apiImpl.findPetsByStatus(status)
            .onSuccess(apiResponse -> {
                routingContext.response().setStatusCode(apiResponse.getStatusCode());
                if (apiResponse.hasData()) {
                    routingContext.json(apiResponse.getData());
                } else {
                    routingContext.response().end();
                }
            })
            .onFailure(routingContext::fail);
    }

    private void findPetsByTags(RoutingContext routingContext) {
        logger.info("findPetsByTags()");

        // Param extraction
        RequestParameters requestParameters = routingContext.get(ValidationHandler.REQUEST_CONTEXT_KEY);

        List<String> tags = requestParameters.queryParameter("tags") != null ? DatabindCodec.mapper().convertValue(requestParameters.queryParameter("tags").get(), new TypeReference<List<String>>(){}) : null;

        logger.debug("Parameter tags is {}", tags);

        apiImpl.findPetsByTags(tags)
            .onSuccess(apiResponse -> {
                routingContext.response().setStatusCode(apiResponse.getStatusCode());
                if (apiResponse.hasData()) {
                    routingContext.json(apiResponse.getData());
                } else {
                    routingContext.response().end();
                }
            })
            .onFailure(routingContext::fail);
    }

    private void getPetById(RoutingContext routingContext) {
        logger.info("getPetById()");

        // Param extraction
        RequestParameters requestParameters = routingContext.get(ValidationHandler.REQUEST_CONTEXT_KEY);

        Long petId = requestParameters.pathParameter("petId") != null ? requestParameters.pathParameter("petId").getLong() : null;

        logger.debug("Parameter petId is {}", petId);

        apiImpl.getPetById(petId)
            .onSuccess(apiResponse -> {
                routingContext.response().setStatusCode(apiResponse.getStatusCode());
                if (apiResponse.hasData()) {
                    routingContext.json(apiResponse.getData());
                } else {
                    routingContext.response().end();
                }
            })
            .onFailure(routingContext::fail);
    }

    private void updatePet(RoutingContext routingContext) {
        logger.info("updatePet()");

        // Param extraction
        RequestParameters requestParameters = routingContext.get(ValidationHandler.REQUEST_CONTEXT_KEY);

        RequestParameter body = requestParameters.body();
        Pet pet = body != null ? DatabindCodec.mapper().convertValue(body.get(), new TypeReference<Pet>(){}) : null;

        logger.debug("Parameter pet is {}", pet);

        apiImpl.updatePet(pet)
            .onSuccess(apiResponse -> {
                routingContext.response().setStatusCode(apiResponse.getStatusCode());
                if (apiResponse.hasData()) {
                    routingContext.json(apiResponse.getData());
                } else {
                    routingContext.response().end();
                }
            })
            .onFailure(routingContext::fail);
    }

    private void updatePetWithForm(RoutingContext routingContext) {
        logger.info("updatePetWithForm()");

        // Param extraction
        RequestParameters requestParameters = routingContext.get(ValidationHandler.REQUEST_CONTEXT_KEY);

        Long petId = requestParameters.pathParameter("petId") != null ? requestParameters.pathParameter("petId").getLong() : null;
        RequestParameter body = requestParameters.body();
        JsonObject formBody = body != null ? body.getJsonObject() : null;

        logger.debug("Parameter petId is {}", petId);
        logger.debug("Parameter formBody is {}", formBody);

        apiImpl.updatePetWithForm(petId, formBody)
            .onSuccess(apiResponse -> {
                routingContext.response().setStatusCode(apiResponse.getStatusCode());
                if (apiResponse.hasData()) {
                    routingContext.json(apiResponse.getData());
                } else {
                    routingContext.response().end();
                }
            })
            .onFailure(routingContext::fail);
    }

    private void uploadFile(RoutingContext routingContext) {
        logger.info("uploadFile()");

        // Param extraction
        RequestParameters requestParameters = routingContext.get(ValidationHandler.REQUEST_CONTEXT_KEY);

        Long petId = requestParameters.pathParameter("petId") != null ? requestParameters.pathParameter("petId").getLong() : null;
        FileUpload file = routingContext.fileUploads().iterator().next();

        logger.debug("Parameter petId is {}", petId);
        logger.debug("Parameter file is {}", file);

        apiImpl.uploadFile(petId, file)
            .onSuccess(apiResponse -> {
                routingContext.response().setStatusCode(apiResponse.getStatusCode());
                if (apiResponse.hasData()) {
                    routingContext.json(apiResponse.getData());
                } else {
                    routingContext.response().end();
                }
            })
            .onFailure(routingContext::fail);
    }

}
