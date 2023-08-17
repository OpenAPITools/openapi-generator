package org.openapitools.server.model;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.openapitools.server.model.ModelFile;



public class FileSchemaTestClass   {

    private ModelFile _file;
    private List<ModelFile> files;

    /**
     * Default constructor.
     */
    public FileSchemaTestClass() {
    // JSON-B / Jackson
    }

    /**
     * Create FileSchemaTestClass.
     *
     * @param _file _file
     * @param files files
     */
    public FileSchemaTestClass(
        ModelFile _file, 
        List<ModelFile> files
    ) {
        this._file = _file;
        this.files = files;
    }



    /**
     * Get _file
     * @return _file
     */
    public ModelFile getFile() {
        return _file;
    }

    public void setFile(ModelFile _file) {
        this._file = _file;
    }

    /**
     * Get files
     * @return files
     */
    public List<ModelFile> getFiles() {
        return files;
    }

    public void setFiles(List<ModelFile> files) {
        this.files = files;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class FileSchemaTestClass {\n");
        
        sb.append("    _file: ").append(toIndentedString(_file)).append("\n");
        sb.append("    files: ").append(toIndentedString(files)).append("\n");
        sb.append("}");
        return sb.toString();
    }

    /**
     * Convert the given object to string with each line indented by 4 spaces
     * (except the first line).
    */
    private static String toIndentedString(Object o) {
        if (o == null) {
          return "null";
        }
        return o.toString().replace("\n", "\n    ");
    }
}

