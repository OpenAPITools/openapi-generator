package org.customimport;

public class CustomObject {

    private String field1;
    private Boolean field2;
    private int otherField;

    public CustomObject field1(String field1) {
        this.field1 = field1;
        return this;
    }

    public CustomObject field2(Boolean field2) {
        this.field2 = field2;
        return this;
    }

    public CustomObject otherField(int otherField) {
        this.otherField = otherField;
        return this;
    }

    public String getField1() {
        return field1;
    }

    public void setField1(String field1) {
        this.field1 = field1;
    }

    public Boolean getField2() {
        return field2;
    }

    public void setField2(Boolean field2) {
        this.field2 = field2;
    }

    public int getOtherField() {
        return otherField;
    }

    public void setOtherField(final int otherField) {
        this.otherField = otherField;
    }
}

