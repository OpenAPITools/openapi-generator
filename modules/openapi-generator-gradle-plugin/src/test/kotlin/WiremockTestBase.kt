package org.openapitools.generator.gradle.plugin

import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.client.WireMock.aResponse
import com.github.tomakehurst.wiremock.client.WireMock.get
import com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo
import org.testng.annotations.AfterClass
import org.testng.annotations.BeforeClass
import java.io.InputStreamReader

abstract class WiremockTestBase : TestBase() {
    protected val port: Int = 9697
    private val wireMockServer: WireMockServer = WireMockServer(port)

    @BeforeClass
    fun startWireMockServer() {
        wireMockServer.start()
    }

    @AfterClass
    fun stopWireMockServer() {
        wireMockServer.stop()
    }

    fun mockSpecRemoteRequest(specPath: String = "specs/petstore-v3.0.yaml") {
        wireMockServer.stubFor(get(urlEqualTo("/spec.yaml"))
            .willReturn(aResponse()
                .withStatus(200)
                .withBody(readFileContent(specPath)))
        )
    }

    fun readFileContent(path: String): String {
        return javaClass.classLoader.getResourceAsStream(path)
            .use { inputStream -> InputStreamReader(inputStream).readText() };
    }
}
