package com.wordnik.swagger.codegen;

import com.samskivert.mustache.*;

import java.util.regex.Pattern;
import java.io.*;

public abstract class AbstractGenerator {

  public File writeToFile(String filename, String contents) throws IOException {
    System.out.println("writing file " + filename);
    File output = new File(filename);

    if(output.getParent() != null && !new File(output.getParent()).exists()) {
      File parent = new File(output.getParent());
      parent.mkdirs();
    }
    Writer out = new BufferedWriter(new OutputStreamWriter(
      new FileOutputStream(output), "UTF-8"));

    out.write(contents);
    out.close();
    return output;
  }

  public String readTemplate(String name) {
    try{
      Reader reader = getTemplateReader(name);
      if(reader == null)
        throw new RuntimeException("no file found");
      java.util.Scanner s = new java.util.Scanner(reader).useDelimiter("\\A");
      return s.hasNext() ? s.next() : "";
    }
    catch(Exception e) {
      e.printStackTrace();
    }
    throw new RuntimeException("can't load template " + name);
  }

  public Reader getTemplateReader(String name) {
    try{
      InputStream is = this.getClass().getClassLoader().getResourceAsStream(getCPResourcePath(name));
      if(is == null)
        is = new FileInputStream(new File(name));
      if(is == null)
        throw new RuntimeException("no file found");
      return new InputStreamReader(is);
    }
    catch(Exception e) {
      e.printStackTrace();
    }
    throw new RuntimeException("can't load template " + name);
  }

  private String getCPResourcePath(String name) {
    if (!"/".equals(File.separator))
      return name.replaceAll(Pattern.quote(File.separator), "/");
    return name;
  }
}