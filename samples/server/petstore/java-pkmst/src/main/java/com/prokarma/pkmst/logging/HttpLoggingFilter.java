package com.prokarma.pkmst.logging;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ReadListener;
import javax.servlet.ServletException;
import javax.servlet.ServletInputStream;
import javax.servlet.ServletOutputStream;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.WriteListener;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;
import javax.servlet.http.HttpServletResponse;
import java.util.Arrays;
import org.apache.commons.io.output.TeeOutputStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
/**
 * Implements javax.servlet.Filter and dump the request/response to syslog
 * @author pkmst
 *
 */

@Component
public class HttpLoggingFilter implements Filter {

  private static final Logger log = LoggerFactory.getLogger(HttpLoggingFilter.class);

  @Override
  public void init(FilterConfig filterConfig) throws ServletException {
  }

  @Override
  public void doFilter(ServletRequest request, ServletResponse response,FilterChain chain) throws IOException, ServletException {
    try {
        HttpServletRequest httpServletRequest = (HttpServletRequest) request;
        HttpServletResponse httpServletResponse = (HttpServletResponse) response;
        Map<String, String> requestMap = this.getTypesafeRequestMap(httpServletRequest);
        BufferedRequestWrapper bufferedRequest = new BufferedRequestWrapper(httpServletRequest);
        BufferedResponseWrapper bufferedResponse = new BufferedResponseWrapper(httpServletResponse);
        final StringBuilder logMessage = new StringBuilder(
                "REST Request - ").append("[HTTP METHOD:")
                .append(httpServletRequest.getMethod())
                .append("] [PATH INFO:")
                .append(httpServletRequest.getServletPath())
                .append("] [REQUEST PARAMETERS:").append(requestMap)
                .append("] [REQUEST BODY:")
                .append(bufferedRequest.getRequestBody())
                .append("] [REMOTE ADDRESS:")
                .append(httpServletRequest.getRemoteAddr()).append("]");
        long start = System.currentTimeMillis();
        chain.doFilter(bufferedRequest, bufferedResponse);
        long elapsedTime = System.currentTimeMillis() - start;
        String respContent = null;
        if (bufferedResponse.getContent() == null || bufferedResponse.getContent() == "") {
          respContent = "No data";
        } else {
          respContent = bufferedResponse.getContent();
        }
        logMessage.append(" [RESPONSE:").append(respContent)
                .append("] [STATUS CODE:")
                .append(bufferedResponse.getStatus())
                .append("] [Response Time(ms):").append(elapsedTime)
                .append("]");
		String [] nonLoggingPaths = {"/v2/api-docs","/swagger-resources","/configuration/security","/swagger-ui.html","/webjars"};
        String urlPath = httpServletRequest.getRequestURL().toString();
        if (! ( Arrays.stream(nonLoggingPaths).parallel().anyMatch(urlPath::contains) )){
             log.info(logMessage.toString());
        } 
    } catch (Throwable a) {
        log.error(a.getMessage());
        a.printStackTrace();
    }
  }

  private Map<String, String> getTypesafeRequestMap(HttpServletRequest request) {
    Map<String, String> typesafeRequestMap = new HashMap<String, String>();
    Enumeration<?> requestParamNames = request.getParameterNames();
    while (requestParamNames.hasMoreElements()) {
      String requestParamName = (String) requestParamNames.nextElement();
      String requestParamValue;
      if (requestParamName.equalsIgnoreCase("password")) {
        requestParamValue = "********";
      } else {
        requestParamValue = request.getParameter(requestParamName);
      }
      typesafeRequestMap.put(requestParamName, requestParamValue);
    }
    return typesafeRequestMap;
  }

  @Override
  public void destroy() {
  }

  private static final class BufferedRequestWrapper extends HttpServletRequestWrapper {

    private ByteArrayInputStream bais = null;
    private ByteArrayOutputStream baos = null;
    private BufferedServletInputStream bsis = null;
    private byte[] buffer = null;

    public BufferedRequestWrapper(HttpServletRequest req) throws IOException {
      super(req);
      // Read InputStream and store its content in a buffer.
      InputStream is = req.getInputStream();
      this.baos = new ByteArrayOutputStream();
      byte buf[] = new byte[1024];
      int read;
      while ((read = is.read(buf)) > 0) {
        this.baos.write(buf, 0, read);
      }
      this.buffer = this.baos.toByteArray();
    }

    @Override
    public ServletInputStream getInputStream() {
      this.bais = new ByteArrayInputStream(this.buffer);
      this.bsis = new BufferedServletInputStream(this.bais);
      return this.bsis;
    }

    String getRequestBody() throws IOException {
      BufferedReader reader = new BufferedReader(new InputStreamReader(this.getInputStream()));
      String line = null;
      StringBuilder inputBuffer = new StringBuilder();
      do {
        line = reader.readLine();
        if (null != line) {
          inputBuffer.append(line.trim());
        }
      } while (line != null);
      reader.close();
      return inputBuffer.toString().trim();
    }

  }

  private static final class BufferedServletInputStream extends ServletInputStream {

    private ByteArrayInputStream bais;

    public BufferedServletInputStream(ByteArrayInputStream bais) {
      this.bais = bais;
    }

    @Override
    public int available() {
      return this.bais.available();
    }

    @Override
    public int read() {
      return this.bais.read();
    }

    @Override
    public int read(byte[] buf, int off, int len) {
      return this.bais.read(buf, off, len);
    }

    @Override
    public boolean isFinished() {
      return false;
    }

    @Override
    public boolean isReady() {
      return true;
    }

    @Override
    public void setReadListener(ReadListener readListener) {

    }
  }

  public class TeeServletOutputStream extends ServletOutputStream {

    private final TeeOutputStream targetStream;

    public TeeServletOutputStream(OutputStream one, OutputStream two) {
      targetStream = new TeeOutputStream(one, two);
    }

    @Override
    public void write(int arg0) throws IOException {
      this.targetStream.write(arg0);
    }

    public void flush() throws IOException {
      super.flush();
      this.targetStream.flush();
    }

    public void close() throws IOException {
      super.close();
      this.targetStream.close();
    }

    @Override
    public boolean isReady() {
      return false;
    }

    @Override
    public void setWriteListener(WriteListener writeListener) {

    }
  }

  public class BufferedResponseWrapper implements HttpServletResponse {

    HttpServletResponse original;
    TeeServletOutputStream tee;
    ByteArrayOutputStream bos;

    public BufferedResponseWrapper(HttpServletResponse response) {
      original = response;
    }

    public String getContent() {
      if (bos == null) {
        return null;
      } else
        return bos.toString();
    }

    public PrintWriter getWriter() throws IOException {
      return original.getWriter();
    }

    public ServletOutputStream getOutputStream() throws IOException {
      if (tee == null) {
        bos = new ByteArrayOutputStream();
        tee = new TeeServletOutputStream(original.getOutputStream(),bos);
      }
      return tee;

    }

    @Override
    public String getCharacterEncoding() {
      return original.getCharacterEncoding();
    }

    @Override
    public String getContentType() {
      return original.getContentType();
    }

    @Override
    public void setCharacterEncoding(String charset) {
      original.setCharacterEncoding(charset);
    }

    @Override
    public void setContentLength(int len) {
      original.setContentLength(len);
    }

    @Override
    public void setContentLengthLong(long l) {
      original.setContentLengthLong(l);
    }

    @Override
    public void setContentType(String type) {
      original.setContentType(type);
    }

    @Override
    public void setBufferSize(int size) {
      original.setBufferSize(size);
    }

    @Override
    public int getBufferSize() {
      return original.getBufferSize();
    }

    @Override
    public void flushBuffer() throws IOException {
      tee.flush();
    }

    @Override
    public void resetBuffer() {
      original.resetBuffer();
    }

    @Override
    public boolean isCommitted() {
      return original.isCommitted();
    }

    @Override
    public void reset() {
      original.reset();
    }

    @Override
    public void setLocale(Locale loc) {
      original.setLocale(loc);
    }

    @Override
    public Locale getLocale() {
      return original.getLocale();
    }

    @Override
    public void addCookie(Cookie cookie) {
      original.addCookie(cookie);
    }

    @Override
    public boolean containsHeader(String name) {
      return original.containsHeader(name);
    }

    @Override
    public String encodeURL(String url) {
      return original.encodeURL(url);
    }

    @Override
    public String encodeRedirectURL(String url) {
      return original.encodeRedirectURL(url);
    }

    @SuppressWarnings("deprecation")
    @Override
    public String encodeUrl(String url) {
      return original.encodeUrl(url);
    }

    @SuppressWarnings("deprecation")
    @Override
    public String encodeRedirectUrl(String url) {
      return original.encodeRedirectUrl(url);
    }

    @Override
    public void sendError(int sc, String msg) throws IOException {
      original.sendError(sc, msg);
    }

    @Override
    public void sendError(int sc) throws IOException {
      original.sendError(sc);
    }

    @Override
    public void sendRedirect(String location) throws IOException {
      original.sendRedirect(location);
    }

    @Override
    public void setDateHeader(String name, long date) {
      original.setDateHeader(name, date);
    }

    @Override
    public void addDateHeader(String name, long date) {
      original.addDateHeader(name, date);
    }

    @Override
    public void setHeader(String name, String value) {
      original.setHeader(name, value);
    }

    @Override
    public void addHeader(String name, String value) {
      original.addHeader(name, value);
    }

    @Override
    public void setIntHeader(String name, int value) {
      original.setIntHeader(name, value);
    }

    @Override
    public void addIntHeader(String name, int value) {
      original.addIntHeader(name, value);
    }

    @Override
    public void setStatus(int sc) {
      original.setStatus(sc);
    }

    @SuppressWarnings("deprecation")
    @Override
    public void setStatus(int sc, String sm) {
      original.setStatus(sc, sm);
    }

    @Override
    public String getHeader(String arg0) {
      return original.getHeader(arg0);
    }

    @Override
    public Collection<String> getHeaderNames() {
      return original.getHeaderNames();
    }

    @Override
    public Collection<String> getHeaders(String arg0) {
      return original.getHeaders(arg0);
    }

    @Override
    public int getStatus() {
      return original.getStatus();
    }
  }
}
