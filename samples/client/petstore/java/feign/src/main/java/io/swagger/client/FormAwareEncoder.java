package io.swagger.client;

import java.io.*;
import java.lang.reflect.Type;
import java.net.URLEncoder;
import java.net.URLConnection;
import java.util.*;

import java.text.DateFormat;
import java.text.SimpleDateFormat;

import feign.codec.Encoder;
import feign.RequestTemplate;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2015-12-07T01:11:21.159-05:00")
public class FormAwareEncoder implements Encoder {
  private static final String LINE_FEED = "\r\n";
  private static final String BOUNDARY = "----------------314159265358979323846";

  private final Encoder delegate;
  private DateFormat dateFormat;

  public FormAwareEncoder(Encoder delegate) {
    this.delegate = delegate;
    // Use RFC3339 format for date and datetime.
    // See http://xml2rfc.ietf.org/public/rfc/html/rfc3339.html#anchor14
    this.dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX");

    // Use UTC as the default time zone.
    this.dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
  }

  public void encode(Object object, Type bodyType, RequestTemplate template) {
    if (object instanceof Map) {
      StringBuilder formParamBuilder = new StringBuilder();
      Map<String, Object> formParams = (Map<String, Object>) object;
      boolean isMultiPart = isMultiPart(formParams);
      for (Map.Entry<String, Object> param : formParams.entrySet()) {
        String keyStr = param.getKey();
        if (param.getValue() instanceof File) {
            addFilePart(formParamBuilder, keyStr, (File) param.getValue());
        } else {
          String valueStr = parameterToString(param.getValue());
          if (isMultiPart) {
            addMultiPartFormField(formParamBuilder, keyStr, valueStr);
          } else {
            addEncodedFormField(formParamBuilder, keyStr, valueStr);
          }
        }
      }

      if (isMultiPart) {
        formParamBuilder.append(LINE_FEED);
        formParamBuilder.append("--").append(BOUNDARY).append("--").append(LINE_FEED);
      }

      String contentType = isMultiPart ? "multipart/form-data; boundary=" + BOUNDARY : "application/x-www-form-urlencoded";
      template.header("Content-type");
      template.header("Content-type", contentType);
      template.header("MIME-Version", "1.0");
      template.body(formParamBuilder.toString());
    } else {
      delegate.encode(object, bodyType, template);
    }
  }

  /*
   * Currently only supports text files
   */
  private void addFilePart(StringBuilder formParamBuilder, String fieldName, File uploadFile) {
    try {
      String fileName = uploadFile.getName();
      formParamBuilder.append("--").append(BOUNDARY).append(LINE_FEED);
      formParamBuilder.append(
        "Content-Disposition: form-data; name=\"" + fieldName
          + "\"; filename=\"" + fileName + "\"")
        .append(LINE_FEED);
      formParamBuilder.append(
        "Content-Type: "
          + URLConnection.guessContentTypeFromName(fileName))
        .append(LINE_FEED);
      formParamBuilder.append(LINE_FEED);

      BufferedReader reader = new BufferedReader(new FileReader(uploadFile));
      String line = "";
      while ((line = reader.readLine()) != null) {
        formParamBuilder.append(line).append(LINE_FEED);
      }

      formParamBuilder.append(LINE_FEED);
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  private void addEncodedFormField(StringBuilder formParamBuilder, String name, String value) {
    if (formParamBuilder.length() > 0) {
      formParamBuilder.append("&");
    }

    try {
      formParamBuilder.append(URLEncoder.encode(name, "utf8"))
        .append("=")
        .append(URLEncoder.encode(value, "utf8"));
    } catch (UnsupportedEncodingException e) {
      // move on to next
    }
  }

  private void addMultiPartFormField(StringBuilder formParamBuilder, String name, String value) {
    formParamBuilder.append("--").append(BOUNDARY).append(LINE_FEED);
    formParamBuilder.append("Content-Disposition: form-data; name=\"" + name + "\"")
            .append(LINE_FEED);
    formParamBuilder.append("Content-Type: text/plain; charset=utf-8").append(
            LINE_FEED);
    formParamBuilder.append(LINE_FEED);
    formParamBuilder.append(value).append(LINE_FEED);
  }

  private boolean isMultiPart(Map<String, Object> formParams) {
    boolean isMultiPart = false;
    for (Map.Entry<String, Object> entry : formParams.entrySet()) {
        if (entry.getValue() instanceof File) {
            isMultiPart = true;
            break;
        }
    }
    return isMultiPart;
  }

  /**
   * Format the given parameter object into string.
   */
  public String parameterToString(Object param) {
    if (param == null) {
      return "";
    } else if (param instanceof Date) {
      return formatDate((Date) param);
    } else if (param instanceof Collection) {
      StringBuilder b = new StringBuilder();
      for(Object o : (Collection)param) {
        if(b.length() > 0) {
          b.append(",");
        }
        b.append(String.valueOf(o));
      }
      return b.toString();
    } else {
      return String.valueOf(param);
    }
  }

  /**
   * Format the given Date object into string.
   */
  public String formatDate(Date date) {
    return dateFormat.format(date);
  }
}
