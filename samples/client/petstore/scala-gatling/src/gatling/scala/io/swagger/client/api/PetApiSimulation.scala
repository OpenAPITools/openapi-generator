package io.swagger.client.api

import io.swagger.client.model._
import com.typesafe.config.ConfigFactory

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import io.gatling.core.structure.PopulationBuilder

import java.io.File

import scala.collection.mutable

class PetApiSimulation extends Simulation {

    def getCurrentDirectory = new File("").getAbsolutePath
    def userDataDirectory = getCurrentDirectory + "/src/gatling/resources/data"

    // basic test setup
    val configName = System.getProperty("testConfig", "baseline")
    val config = ConfigFactory.load(configName).withFallback(ConfigFactory.load("default"))
    val durationSeconds = config.getInt("performance.durationSeconds")
    val rampUpSeconds = config.getInt("performance.rampUpSeconds")
    val rampDownSeconds = config.getInt("performance.rampDownSeconds")
    val authentication = config.getString("performance.authorizationHeader")
    val acceptHeader = config.getString("performance.acceptType")
    val contentTypeHeader = config.getString("performance.contentType")
    val rateMultiplier = config.getDouble("performance.rateMultiplier")
    val instanceMultiplier = config.getDouble("performance.instanceMultiplier")

    // global assertion data
    val globalResponseTimeMinLTE = config.getInt("performance.global.assertions.responseTime.min.lte")
    val globalResponseTimeMinGTE = config.getInt("performance.global.assertions.responseTime.min.gte")
    val globalResponseTimeMaxLTE = config.getInt("performance.global.assertions.responseTime.max.lte")
    val globalResponseTimeMaxGTE = config.getInt("performance.global.assertions.responseTime.max.gte")
    val globalResponseTimeMeanLTE = config.getInt("performance.global.assertions.responseTime.mean.lte")
    val globalResponseTimeMeanGTE = config.getInt("performance.global.assertions.responseTime.mean.gte")
    val globalResponseTimeFailedRequestsPercentLTE = config.getDouble("performance.global.assertions.failedRequests.percent.lte")
    val globalResponseTimeFailedRequestsPercentGTE = config.getDouble("performance.global.assertions.failedRequests.percent.gte")
    val globalResponseTimeSuccessfulRequestsPercentLTE = config.getDouble("performance.global.assertions.successfulRequests.percent.lte")
    val globalResponseTimeSuccessfulRequestsPercentGTE = config.getDouble("performance.global.assertions.successfulRequests.percent.gte")

// Setup http protocol configuration
    val httpConf = http
        .baseURL("http://petstore.swagger.io/v2")
        .doNotTrackHeader("1")
        .acceptLanguageHeader("en-US,en;q=0.5")
        .acceptEncodingHeader("gzip, deflate")
        .userAgentHeader("Mozilla/5.0 (Windows NT 5.1; rv:31.0) Gecko/20100101 Firefox/31.0")
        .acceptHeader(acceptHeader)
        .contentTypeHeader(contentTypeHeader)

    // set authorization header if it has been modified from config
    if(!authentication.equals("~MANUAL_ENTRY")){
        httpConf.authorizationHeader(authentication)
    }

    // Setup all the operations per second for the test to ultimately be generated from configs
    val addPetPerSecond = config.getDouble("performance.operationsPerSecond.addPet") * rateMultiplier * instanceMultiplier
    val deletePetPerSecond = config.getDouble("performance.operationsPerSecond.deletePet") * rateMultiplier * instanceMultiplier
    val findPetsByStatusPerSecond = config.getDouble("performance.operationsPerSecond.findPetsByStatus") * rateMultiplier * instanceMultiplier
    val findPetsByTagsPerSecond = config.getDouble("performance.operationsPerSecond.findPetsByTags") * rateMultiplier * instanceMultiplier
    val getPetByIdPerSecond = config.getDouble("performance.operationsPerSecond.getPetById") * rateMultiplier * instanceMultiplier
    val updatePetPerSecond = config.getDouble("performance.operationsPerSecond.updatePet") * rateMultiplier * instanceMultiplier
    val updatePetWithFormPerSecond = config.getDouble("performance.operationsPerSecond.updatePetWithForm") * rateMultiplier * instanceMultiplier
    val uploadFilePerSecond = config.getDouble("performance.operationsPerSecond.uploadFile") * rateMultiplier * instanceMultiplier

    val scenarioBuilders: mutable.MutableList[PopulationBuilder] = new mutable.MutableList[PopulationBuilder]()

    // Set up CSV feeders
    val addPetBodyFeeder = csv(userDataDirectory + File.separator + "addPet-bodyParams.csv", escapeChar = '\\').random
    val deletePetHEADERFeeder = csv(userDataDirectory + File.separator + "deletePet-headerParams.csv").random
    val deletePetPATHFeeder = csv(userDataDirectory + File.separator + "deletePet-pathParams.csv").random
    val findPetsByStatusQUERYFeeder = csv(userDataDirectory + File.separator + "findPetsByStatus-queryParams.csv").random
    val findPetsByTagsQUERYFeeder = csv(userDataDirectory + File.separator + "findPetsByTags-queryParams.csv").random
    val getPetByIdPATHFeeder = csv(userDataDirectory + File.separator + "getPetById-pathParams.csv").random
    val updatePetBodyFeeder = csv(userDataDirectory + File.separator + "updatePet-bodyParams.csv", escapeChar = '\\').random
    val updatePetWithFormFORMFeeder = csv(userDataDirectory + File.separator + "updatePetWithForm-formParams.csv").random
    val updatePetWithFormPATHFeeder = csv(userDataDirectory + File.separator + "updatePetWithForm-pathParams.csv").random
    val uploadFileFORMFeeder = csv(userDataDirectory + File.separator + "uploadFile-formParams.csv").random
    val uploadFilePATHFeeder = csv(userDataDirectory + File.separator + "uploadFile-pathParams.csv").random

    // Setup all scenarios

    
    val scnaddPet = scenario("addPetSimulation")
        .feed(addPetBodyFeeder)
        .exec(http("addPet")
        .httpRequest("POST","/pet")
        .body(StringBody(Pet.toStringBody("${id}","${category}","${name}","${tags}","${status}","${photoUrls}")))
        )

    // Run scnaddPet with warm up and reach a constant rate for entire duration
    scenarioBuilders += scnaddPet.inject(
        rampUsersPerSec(1) to(addPetPerSecond) during(rampUpSeconds),
        constantUsersPerSec(addPetPerSecond) during(durationSeconds),
        rampUsersPerSec(addPetPerSecond) to(1) during(rampDownSeconds)
    )

    
    val scndeletePet = scenario("deletePetSimulation")
        .feed(deletePetHEADERFeeder)
        .feed(deletePetPATHFeeder)
        .exec(http("deletePet")
        .httpRequest("DELETE","/pet/${petId}")
        .header("api_key","${api_key}")
)

    // Run scndeletePet with warm up and reach a constant rate for entire duration
    scenarioBuilders += scndeletePet.inject(
        rampUsersPerSec(1) to(deletePetPerSecond) during(rampUpSeconds),
        constantUsersPerSec(deletePetPerSecond) during(durationSeconds),
        rampUsersPerSec(deletePetPerSecond) to(1) during(rampDownSeconds)
    )

    
    val scnfindPetsByStatus = scenario("findPetsByStatusSimulation")
        .feed(findPetsByStatusQUERYFeeder)
        .exec(http("findPetsByStatus")
        .httpRequest("GET","/pet/findByStatus")
        .queryParam("status","${status}")
)

    // Run scnfindPetsByStatus with warm up and reach a constant rate for entire duration
    scenarioBuilders += scnfindPetsByStatus.inject(
        rampUsersPerSec(1) to(findPetsByStatusPerSecond) during(rampUpSeconds),
        constantUsersPerSec(findPetsByStatusPerSecond) during(durationSeconds),
        rampUsersPerSec(findPetsByStatusPerSecond) to(1) during(rampDownSeconds)
    )

    
    val scnfindPetsByTags = scenario("findPetsByTagsSimulation")
        .feed(findPetsByTagsQUERYFeeder)
        .exec(http("findPetsByTags")
        .httpRequest("GET","/pet/findByTags")
        .queryParam("tags","${tags}")
)

    // Run scnfindPetsByTags with warm up and reach a constant rate for entire duration
    scenarioBuilders += scnfindPetsByTags.inject(
        rampUsersPerSec(1) to(findPetsByTagsPerSecond) during(rampUpSeconds),
        constantUsersPerSec(findPetsByTagsPerSecond) during(durationSeconds),
        rampUsersPerSec(findPetsByTagsPerSecond) to(1) during(rampDownSeconds)
    )

    
    val scngetPetById = scenario("getPetByIdSimulation")
        .feed(getPetByIdPATHFeeder)
        .exec(http("getPetById")
        .httpRequest("GET","/pet/${petId}")
)

    // Run scngetPetById with warm up and reach a constant rate for entire duration
    scenarioBuilders += scngetPetById.inject(
        rampUsersPerSec(1) to(getPetByIdPerSecond) during(rampUpSeconds),
        constantUsersPerSec(getPetByIdPerSecond) during(durationSeconds),
        rampUsersPerSec(getPetByIdPerSecond) to(1) during(rampDownSeconds)
    )

    
    val scnupdatePet = scenario("updatePetSimulation")
        .feed(updatePetBodyFeeder)
        .exec(http("updatePet")
        .httpRequest("PUT","/pet")
        .body(StringBody(Pet.toStringBody("${id}","${category}","${name}","${tags}","${status}","${photoUrls}")))
        )

    // Run scnupdatePet with warm up and reach a constant rate for entire duration
    scenarioBuilders += scnupdatePet.inject(
        rampUsersPerSec(1) to(updatePetPerSecond) during(rampUpSeconds),
        constantUsersPerSec(updatePetPerSecond) during(durationSeconds),
        rampUsersPerSec(updatePetPerSecond) to(1) during(rampDownSeconds)
    )

    
    val scnupdatePetWithForm = scenario("updatePetWithFormSimulation")
        .feed(updatePetWithFormFORMFeeder)
        .feed(updatePetWithFormPATHFeeder)
        .exec(http("updatePetWithForm")
        .httpRequest("POST","/pet/${petId}")
        .formParam("name","${name}")
        .formParam("status","${status}")
)

    // Run scnupdatePetWithForm with warm up and reach a constant rate for entire duration
    scenarioBuilders += scnupdatePetWithForm.inject(
        rampUsersPerSec(1) to(updatePetWithFormPerSecond) during(rampUpSeconds),
        constantUsersPerSec(updatePetWithFormPerSecond) during(durationSeconds),
        rampUsersPerSec(updatePetWithFormPerSecond) to(1) during(rampDownSeconds)
    )

    
    val scnuploadFile = scenario("uploadFileSimulation")
        .feed(uploadFileFORMFeeder)
        .feed(uploadFilePATHFeeder)
        .exec(http("uploadFile")
        .httpRequest("POST","/pet/${petId}/uploadImage")
        .formParam("file","${file}")
        .formParam("additionalMetadata","${additionalMetadata}")
)

    // Run scnuploadFile with warm up and reach a constant rate for entire duration
    scenarioBuilders += scnuploadFile.inject(
        rampUsersPerSec(1) to(uploadFilePerSecond) during(rampUpSeconds),
        constantUsersPerSec(uploadFilePerSecond) during(durationSeconds),
        rampUsersPerSec(uploadFilePerSecond) to(1) during(rampDownSeconds)
    )

    setUp(
        scenarioBuilders.toList
    ).protocols(httpConf).assertions(
        global.responseTime.min.lte(globalResponseTimeMinLTE),
        global.responseTime.min.gte(globalResponseTimeMinGTE),
        global.responseTime.max.lte(globalResponseTimeMaxLTE),
        global.responseTime.max.gte(globalResponseTimeMaxGTE),
        global.responseTime.mean.lte(globalResponseTimeMeanLTE),
        global.responseTime.mean.gte(globalResponseTimeMeanGTE),
        global.failedRequests.percent.lte(globalResponseTimeFailedRequestsPercentLTE),
        global.failedRequests.percent.gte(globalResponseTimeFailedRequestsPercentGTE),
        global.successfulRequests.percent.lte(globalResponseTimeSuccessfulRequestsPercentLTE),
        global.successfulRequests.percent.gte(globalResponseTimeSuccessfulRequestsPercentGTE)
    )
}
