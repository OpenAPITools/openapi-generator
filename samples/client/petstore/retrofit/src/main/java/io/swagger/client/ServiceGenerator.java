package io.swagger.client;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonParseException;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.lang.reflect.Type;

import retrofit.RestAdapter;
import retrofit.converter.ConversionException;
import retrofit.converter.Converter;
import retrofit.converter.GsonConverter;
import retrofit.mime.TypedByteArray;
import retrofit.mime.TypedInput;
import retrofit.mime.TypedOutput;

public class ServiceGenerator {
  // No need to instantiate this class.
  private ServiceGenerator() { }

  public static <S> S createService(Class<S> serviceClass) {
    Gson gson = new GsonBuilder()
        .setDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
        .create();
    RestAdapter adapter = new RestAdapter.Builder()
        .setEndpoint("http://petstore.swagger.io/v2")
        .setConverter(new GsonConverterWrapper(gson))
        .build();

    return adapter.create(serviceClass);
  }
}

/**
 * This wrapper is to take care of this case:
 * when the deserialization fails due to JsonParseException and the
 * expected type is String, then just return the body string.
 */
class GsonConverterWrapper implements Converter {
    private GsonConverter converter;

    public GsonConverterWrapper(Gson gson) {
        converter = new GsonConverter(gson);
    }

    @Override public Object fromBody(TypedInput body, Type type) throws ConversionException {
        byte[] bodyBytes = readInBytes(body);
        TypedByteArray newBody = new TypedByteArray(body.mimeType(), bodyBytes);
        try {
            return converter.fromBody(newBody, type);
        } catch (ConversionException e) {
            if (e.getCause() instanceof JsonParseException && type.equals(String.class)) {
                return new String(bodyBytes);
            } else {
                throw e;
            }
        }
    }

    @Override public TypedOutput toBody(Object object) {
        return converter.toBody(object);
    }

    private byte[] readInBytes(TypedInput body) throws ConversionException {
        InputStream in = null;
        try {
            in = body.in();
            ByteArrayOutputStream os = new ByteArrayOutputStream();
            byte[] buffer = new byte[0xFFFF];
            for (int len; (len = in.read(buffer)) != -1;)
                os.write(buffer, 0, len);
            os.flush();
            return os.toByteArray();
        } catch (IOException e) {
            throw new ConversionException(e);
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException ignored) {
                }
            }
        }

    }
}
