package io.swagger.client;

import java.util.Arrays;
import java.util.List;

public class CollectionFormats {
    
    public static class CSVParams {

        protected List<String> params;
        
        public CSVParams() {
        }
        
        public CSVParams(List<String> params) {
            this.params = params;
        }
        
        public CSVParams(String... params) {
            this.params = Arrays.asList(params);
        }

        public List<String> getParams() {
            return params;
        }

        public void setParams(List<String> params) {
            this.params = params;
        }
        
        @Override
        public String toString() {
            return String.join(",", params);
        }
        
    }
    
    public static class SSVParams extends CSVParams {
        
        public SSVParams() {
        }
        
        public SSVParams(List<String> params) {
            super(params);
        }

        public SSVParams(String... params) {
            super(params);
        }

        @Override
        public String toString() {
            return String.join(" ", params);
        }
    }
    
    public static class TSVParams extends CSVParams {
        
        public TSVParams() {
        }
        
        public TSVParams(List<String> params) {
            super(params);
        }
        
        public TSVParams(String... params) {
            super(params);
        }

        @Override
        public String toString() {
            return String.join("\t", params);
        }
    }
    
    public static class PIPESParams extends CSVParams {
        
        public PIPESParams() {
        }
        
        public PIPESParams(List<String> params) {
            super(params);
        }
        
        public PIPESParams(String... params) {
            super(params);
        }

        @Override
        public String toString() {
            return String.join("|", params);
        }
    }
    
}
