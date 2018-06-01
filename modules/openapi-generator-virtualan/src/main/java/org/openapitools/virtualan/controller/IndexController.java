package org.openapitools.virtualan.controller;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
public class IndexController {
	
	@RequestMapping("/request")
	public String getIndexPage() {
		System.out.println("index.html");
		return "redirect:index.html";
	}
	
}