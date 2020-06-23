package org.openapitools.client.api

import org.openapitools.client.model._
import com.typesafe.config.ConfigFactory

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import io.gatling.core.structure.PopulationBuilder

import java.io.File

import scala.collection.mutable

class UserApiSimulation extends Simulation {

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
    val createUserPerSecond = config.getDouble("performance.operationsPerSecond.createUser") * rateMultiplier * instanceMultiplier
    val createUsersWithArrayInputPerSecond = config.getDouble("performance.operationsPerSecond.createUsersWithArrayInput") * rateMultiplier * instanceMultiplier
    val createUsersWithListInputPerSecond = config.getDouble("performance.operationsPerSecond.createUsersWithListInput") * rateMultiplier * instanceMultiplier
    val deleteUserPerSecond = config.getDouble("performance.operationsPerSecond.deleteUser") * rateMultiplier * instanceMultiplier
    val getUserByNamePerSecond = config.getDouble("performance.operationsPerSecond.getUserByName") * rateMultiplier * instanceMultiplier
    val loginUserPerSecond = config.getDouble("performance.operationsPerSecond.loginUser") * rateMultiplier * instanceMultiplier
    val logoutUserPerSecond = config.getDouble("performance.operationsPerSecond.logoutUser") * rateMultiplier * instanceMultiplier
    val updateUserPerSecond = config.getDouble("performance.operationsPerSecond.updateUser") * rateMultiplier * instanceMultiplier

    val scenarioBuilders: mutable.MutableList[PopulationBuilder] = new mutable.MutableList[PopulationBuilder]()

    // Set up CSV feeders
    val deleteUserPATHFeeder = csv(userDataDirectory + File.separator + "deleteUser-pathParams.csv").random
    val getUserByNamePATHFeeder = csv(userDataDirectory + File.separator + "getUserByName-pathParams.csv").random
    val loginUserQUERYFeeder = csv(userDataDirectory + File.separator + "loginUser-queryParams.csv").random
    val updateUserPATHFeeder = csv(userDataDirectory + File.separator + "updateUser-pathParams.csv").random

    // Setup all scenarios

    
    val scncreateUser = scenario("createUserSimulation")
        .exec(http("createUser")
        .httpRequest("POST","/user")
)

    // Run scncreateUser with warm up and reach a constant rate for entire duration
    scenarioBuilders += scncreateUser.inject(
        rampUsersPerSec(1) to(createUserPerSecond) during(rampUpSeconds),
        constantUsersPerSec(createUserPerSecond) during(durationSeconds),
        rampUsersPerSec(createUserPerSecond) to(1) during(rampDownSeconds)
    )

    
    val scncreateUsersWithArrayInput = scenario("createUsersWithArrayInputSimulation")
        .exec(http("createUsersWithArrayInput")
        .httpRequest("POST","/user/createWithArray")
)

    // Run scncreateUsersWithArrayInput with warm up and reach a constant rate for entire duration
    scenarioBuilders += scncreateUsersWithArrayInput.inject(
        rampUsersPerSec(1) to(createUsersWithArrayInputPerSecond) during(rampUpSeconds),
        constantUsersPerSec(createUsersWithArrayInputPerSecond) during(durationSeconds),
        rampUsersPerSec(createUsersWithArrayInputPerSecond) to(1) during(rampDownSeconds)
    )

    
    val scncreateUsersWithListInput = scenario("createUsersWithListInputSimulation")
        .exec(http("createUsersWithListInput")
        .httpRequest("POST","/user/createWithList")
)

    // Run scncreateUsersWithListInput with warm up and reach a constant rate for entire duration
    scenarioBuilders += scncreateUsersWithListInput.inject(
        rampUsersPerSec(1) to(createUsersWithListInputPerSecond) during(rampUpSeconds),
        constantUsersPerSec(createUsersWithListInputPerSecond) during(durationSeconds),
        rampUsersPerSec(createUsersWithListInputPerSecond) to(1) during(rampDownSeconds)
    )

    
    val scndeleteUser = scenario("deleteUserSimulation")
        .feed(deleteUserPATHFeeder)
        .exec(http("deleteUser")
        .httpRequest("DELETE","/user/${username}")
)

    // Run scndeleteUser with warm up and reach a constant rate for entire duration
    scenarioBuilders += scndeleteUser.inject(
        rampUsersPerSec(1) to(deleteUserPerSecond) during(rampUpSeconds),
        constantUsersPerSec(deleteUserPerSecond) during(durationSeconds),
        rampUsersPerSec(deleteUserPerSecond) to(1) during(rampDownSeconds)
    )

    
    val scngetUserByName = scenario("getUserByNameSimulation")
        .feed(getUserByNamePATHFeeder)
        .exec(http("getUserByName")
        .httpRequest("GET","/user/${username}")
)

    // Run scngetUserByName with warm up and reach a constant rate for entire duration
    scenarioBuilders += scngetUserByName.inject(
        rampUsersPerSec(1) to(getUserByNamePerSecond) during(rampUpSeconds),
        constantUsersPerSec(getUserByNamePerSecond) during(durationSeconds),
        rampUsersPerSec(getUserByNamePerSecond) to(1) during(rampDownSeconds)
    )

    
    val scnloginUser = scenario("loginUserSimulation")
        .feed(loginUserQUERYFeeder)
        .exec(http("loginUser")
        .httpRequest("GET","/user/login")
        .queryParam("password","${password}")
        .queryParam("username","${username}")
)

    // Run scnloginUser with warm up and reach a constant rate for entire duration
    scenarioBuilders += scnloginUser.inject(
        rampUsersPerSec(1) to(loginUserPerSecond) during(rampUpSeconds),
        constantUsersPerSec(loginUserPerSecond) during(durationSeconds),
        rampUsersPerSec(loginUserPerSecond) to(1) during(rampDownSeconds)
    )

    
    val scnlogoutUser = scenario("logoutUserSimulation")
        .exec(http("logoutUser")
        .httpRequest("GET","/user/logout")
)

    // Run scnlogoutUser with warm up and reach a constant rate for entire duration
    scenarioBuilders += scnlogoutUser.inject(
        rampUsersPerSec(1) to(logoutUserPerSecond) during(rampUpSeconds),
        constantUsersPerSec(logoutUserPerSecond) during(durationSeconds),
        rampUsersPerSec(logoutUserPerSecond) to(1) during(rampDownSeconds)
    )

    
    val scnupdateUser = scenario("updateUserSimulation")
        .feed(updateUserPATHFeeder)
        .exec(http("updateUser")
        .httpRequest("PUT","/user/${username}")
)

    // Run scnupdateUser with warm up and reach a constant rate for entire duration
    scenarioBuilders += scnupdateUser.inject(
        rampUsersPerSec(1) to(updateUserPerSecond) during(rampUpSeconds),
        constantUsersPerSec(updateUserPerSecond) during(durationSeconds),
        rampUsersPerSec(updateUserPerSecond) to(1) during(rampDownSeconds)
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
