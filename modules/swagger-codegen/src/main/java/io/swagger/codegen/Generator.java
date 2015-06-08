package io.swagger.codegen;

import java.io.File;
import java.util.List;

public interface Generator {
    Generator opts(ClientOptInput opts);

    List<File> generate();
}