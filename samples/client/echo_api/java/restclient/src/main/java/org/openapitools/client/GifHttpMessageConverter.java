package org.openapitools.client;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.Collections;
import java.util.List;
import org.springframework.http.HttpInputMessage;
import org.springframework.http.HttpOutputMessage;
import org.springframework.http.MediaType;
import org.springframework.http.converter.HttpMessageConverter;

public class GifHttpMessageConverter implements HttpMessageConverter<File> {

    @Override
    public boolean canRead(Class<?> clazz, MediaType mediaType) {
        return clazz == File.class && mediaType.includes(MediaType.IMAGE_GIF);
    }

    @Override
    public boolean canWrite(Class<?> clazz, MediaType mediaType) {
        return false;
    }

    @Override
    public List<MediaType> getSupportedMediaTypes() {
        return Collections.singletonList(MediaType.IMAGE_GIF);
    }

    @Override
    public File read(Class<? extends File> clazz, HttpInputMessage inputMessage) throws IOException {
        File tempFile = Files.createTempFile("downloaded", ".gif").toFile();
        try (InputStream in = inputMessage.getBody(); FileOutputStream out = new FileOutputStream(tempFile)) {
            byte[] buffer = new byte[1024];
            int bytesRead;
            while ((bytesRead = in.read(buffer)) != -1) {
                out.write(buffer, 0, bytesRead);
            }
        }
        return tempFile;
    }

    @Override
    public void write(File file, MediaType contentType, HttpOutputMessage outputMessage) throws IOException {
        throw new UnsupportedOperationException("This converter is only for reading");
    }
}