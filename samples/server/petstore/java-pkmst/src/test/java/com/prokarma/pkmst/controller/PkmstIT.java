package com.prokarma.pkmst.controller;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.context.embedded.LocalServerPort;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import com.prokarma.pkmst.PkmstApplication;

@ExtendWith(SpringExtension.class)
@SpringBootTest(classes = PkmstApplication.class, webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
public class PkmstIT {

	@LocalServerPort
	private int port;

	TestRestTemplate restTemplate = new TestRestTemplate();

	HttpHeaders headers = new HttpHeaders();

	@Test
	public void testYourMethod() {

		//add your logic
	}

	private String createURLWithPort(String uri) {
		return "http://localhost:" + port + uri;
	}
}
