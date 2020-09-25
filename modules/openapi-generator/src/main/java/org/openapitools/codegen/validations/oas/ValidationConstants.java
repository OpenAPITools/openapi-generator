package org.openapitools.codegen.validations.oas;

final class ValidationConstants {
    static String ApacheNginxUnderscoreDescription = "Apache and Nginx may fail on headers keys with underscore!";
    static String ApacheNginxUnderscoreFailureMessage = "Apache and Nginx webservers may fail due to legacy CGI constraints enabled by default in which header keys with underscore are disallowed. See https://stackoverflow.com/a/22856867/151445.";
}
