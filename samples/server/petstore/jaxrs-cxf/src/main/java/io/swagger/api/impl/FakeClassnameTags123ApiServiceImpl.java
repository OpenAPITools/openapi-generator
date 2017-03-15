package io.swagger.api.impl;

import io.swagger.api.*;
import io.swagger.model.Client;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import org.apache.cxf.jaxrs.model.wadl.Description;
import org.apache.cxf.jaxrs.model.wadl.DocTarget;

import org.apache.cxf.jaxrs.ext.multipart.*;

import io.swagger.annotations.Api;

public class FakeClassnameTags123ApiServiceImpl implements FakeClassnameTags123Api {
    public Client testClassname(Client body) {
        // TODO: Implement...
        
        return null;
    }
    
}

