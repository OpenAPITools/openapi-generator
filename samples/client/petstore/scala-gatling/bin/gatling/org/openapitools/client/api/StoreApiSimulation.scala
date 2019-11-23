package org.openapitools.client.api

import org.openapitools.client.model._
import com.typesafe.config.ConfigFactory

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import io.gatling.core.structure.PopulationBuilder

import java.io.File

import scala.collection.mutable

class StoreApiSimulation extends Simulation {

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
    val deleteOrderPerSecond = config.getDouble("performance.operationsPerSecond.deleteOrder") * rateMultiplier * instanceMultiplier
    val getInventoryPerSecond = config.getDouble("performance.operationsPerSecond.getInventory") * rateMultiplier * instanceMultiplier
    val getOrderByIdPerSecond = config.getDouble("performance.operationsPerSecond.getOrderById") * rateMultiplier * instanceMultiplier
    val placeOrderPerSecond = config.getDouble("performance.operationsPerSecond.placeOrder") * rateMultiplier * instanceMultiplier

    val scenarioBuilders: mutable.MutableList[PopulationBuilder] = new mutable.MutableList[PopulationBuilder]()

    // Set up CSV feeders
    val deleteOrderPATHFeeder = csv(userDataDirectory + File.separator + "deleteOrder-pathParams.csv").random
    val getOrderByIdPATHFeeder = csv(userDataDirectory + File.separator + "getOrderById-pathParams.csv").random

    // Setup all scenarios

    
    val scndeleteOrder = scenario("deleteOrderSimulation")
        .feed(deleteOrderPATHFeeder)
        .exec(http("deleteOrder")
        .httpRequest("DELETE","/store/order/${orderId}")
)

    // Run scndeleteOrder with warm up and reach a constant rate for entire duration
    scenarioBuilders += scndeleteOrder.inject(
        rampUsersPerSec(1) to(deleteOrderPerSecond) during(rampUpSeconds),
        constantUsersPerSec(deleteOrderPerSecond) during(durationSeconds),
        rampUsersPerSec(deleteOrderPerSecond) to(1) during(rampDownSeconds)
    )

    
    val scngetInventory = scenario("getInventorySimulation")
        .exec(http("getInventory")
        .httpRequest("GET","/store/inventory")
)

    // Run scngetInventory with warm up and reach a constant rate for entire duration
    scenarioBuilders += scngetInventory.inject(
        rampUsersPerSec(1) to(getInventoryPerSecond) during(rampUpSeconds),
        constantUsersPerSec(getInventoryPerSecond) during(durationSeconds),
        rampUsersPerSec(getInventoryPerSecond) to(1) during(rampDownSeconds)
    )

    
    val scngetOrderById = scenario("getOrderByIdSimulation")
        .feed(getOrderByIdPATHFeeder)
        .exec(http("getOrderById")
        .httpRequest("GET","/store/order/${orderId}")
)

    // Run scngetOrderById with warm up and reach a constant rate for entire duration
    scenarioBuilders += scngetOrderById.inject(
        rampUsersPerSec(1) to(getOrderByIdPerSecond) during(rampUpSeconds),
        constantUsersPerSec(getOrderByIdPerSecond) during(durationSeconds),
        rampUsersPerSec(getOrderByIdPerSecond) to(1) during(rampDownSeconds)
    )

    
    val scnplaceOrder = scenario("placeOrderSimulation")
        .exec(http("placeOrder")
        .httpRequest("POST","/store/order")
)

    // Run scnplaceOrder with warm up and reach a constant rate for entire duration
    scenarioBuilders += scnplaceOrder.inject(
        rampUsersPerSec(1) to(placeOrderPerSecond) during(rampUpSeconds),
        constantUsersPerSec(placeOrderPerSecond) during(durationSeconds),
        rampUsersPerSec(placeOrderPerSecond) to(1) during(rampDownSeconds)
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
