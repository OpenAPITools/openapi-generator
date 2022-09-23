package org.openapitools.server.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import java.io.File;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.UUID;



public class FormatTest   {

    private Integer integer;
    private Integer int32;
    private Long int64;
    private BigDecimal number;
    private Float _float;
    private Double _double;
    private BigDecimal decimal;
    private String string;
    private byte[] _byte;
    private File binary;
    private LocalDate date;
    private OffsetDateTime dateTime;
    private UUID uuid;
    private String password;
    private String patternWithDigits;
    private String patternWithDigitsAndDelimiter;

    /**
     * Default constructor.
     */
    public FormatTest() {
    // JSON-B / Jackson
    }

    /**
     * Create FormatTest.
     *
     * @param integer integer
     * @param int32 int32
     * @param int64 int64
     * @param number number
     * @param _float _float
     * @param _double _double
     * @param decimal decimal
     * @param string string
     * @param _byte _byte
     * @param binary binary
     * @param date date
     * @param dateTime dateTime
     * @param uuid uuid
     * @param password password
     * @param patternWithDigits A string that is a 10 digit number. Can have leading zeros.
     * @param patternWithDigitsAndDelimiter A string starting with &#39;image_&#39; (case insensitive) and one to three digits following i.e. Image_01.
     */
    public FormatTest(
        Integer integer, 
        Integer int32, 
        Long int64, 
        BigDecimal number, 
        Float _float, 
        Double _double, 
        BigDecimal decimal, 
        String string, 
        byte[] _byte, 
        File binary, 
        LocalDate date, 
        OffsetDateTime dateTime, 
        UUID uuid, 
        String password, 
        String patternWithDigits, 
        String patternWithDigitsAndDelimiter
    ) {
        this.integer = integer;
        this.int32 = int32;
        this.int64 = int64;
        this.number = number;
        this._float = _float;
        this._double = _double;
        this.decimal = decimal;
        this.string = string;
        this._byte = _byte;
        this.binary = binary;
        this.date = date;
        this.dateTime = dateTime;
        this.uuid = uuid;
        this.password = password;
        this.patternWithDigits = patternWithDigits;
        this.patternWithDigitsAndDelimiter = patternWithDigitsAndDelimiter;
    }



    /**
     * Get integer
     * minimum: 10
     * maximum: 100
     * @return integer
     */
    public Integer getInteger() {
        return integer;
    }

    public void setInteger(Integer integer) {
        this.integer = integer;
    }

    /**
     * Get int32
     * minimum: 20
     * maximum: 200
     * @return int32
     */
    public Integer getInt32() {
        return int32;
    }

    public void setInt32(Integer int32) {
        this.int32 = int32;
    }

    /**
     * Get int64
     * @return int64
     */
    public Long getInt64() {
        return int64;
    }

    public void setInt64(Long int64) {
        this.int64 = int64;
    }

    /**
     * Get number
     * minimum: 32.1
     * maximum: 543.2
     * @return number
     */
    public BigDecimal getNumber() {
        return number;
    }

    public void setNumber(BigDecimal number) {
        this.number = number;
    }

    /**
     * Get _float
     * minimum: 54.3
     * maximum: 987.6
     * @return _float
     */
    public Float getFloat() {
        return _float;
    }

    public void setFloat(Float _float) {
        this._float = _float;
    }

    /**
     * Get _double
     * minimum: 67.8
     * maximum: 123.4
     * @return _double
     */
    public Double getDouble() {
        return _double;
    }

    public void setDouble(Double _double) {
        this._double = _double;
    }

    /**
     * Get decimal
     * @return decimal
     */
    public BigDecimal getDecimal() {
        return decimal;
    }

    public void setDecimal(BigDecimal decimal) {
        this.decimal = decimal;
    }

    /**
     * Get string
     * @return string
     */
    public String getString() {
        return string;
    }

    public void setString(String string) {
        this.string = string;
    }

    /**
     * Get _byte
     * @return _byte
     */
    public byte[] getByte() {
        return _byte;
    }

    public void setByte(byte[] _byte) {
        this._byte = _byte;
    }

    /**
     * Get binary
     * @return binary
     */
    public File getBinary() {
        return binary;
    }

    public void setBinary(File binary) {
        this.binary = binary;
    }

    /**
     * Get date
     * @return date
     */
    public LocalDate getDate() {
        return date;
    }

    public void setDate(LocalDate date) {
        this.date = date;
    }

    /**
     * Get dateTime
     * @return dateTime
     */
    public OffsetDateTime getDateTime() {
        return dateTime;
    }

    public void setDateTime(OffsetDateTime dateTime) {
        this.dateTime = dateTime;
    }

    /**
     * Get uuid
     * @return uuid
     */
    public UUID getUuid() {
        return uuid;
    }

    public void setUuid(UUID uuid) {
        this.uuid = uuid;
    }

    /**
     * Get password
     * @return password
     */
    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    /**
     * A string that is a 10 digit number. Can have leading zeros.
     * @return patternWithDigits
     */
    public String getPatternWithDigits() {
        return patternWithDigits;
    }

    public void setPatternWithDigits(String patternWithDigits) {
        this.patternWithDigits = patternWithDigits;
    }

    /**
     * A string starting with 'image_' (case insensitive) and one to three digits following i.e. Image_01.
     * @return patternWithDigitsAndDelimiter
     */
    public String getPatternWithDigitsAndDelimiter() {
        return patternWithDigitsAndDelimiter;
    }

    public void setPatternWithDigitsAndDelimiter(String patternWithDigitsAndDelimiter) {
        this.patternWithDigitsAndDelimiter = patternWithDigitsAndDelimiter;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class FormatTest {\n");
        
        sb.append("    integer: ").append(toIndentedString(integer)).append("\n");
        sb.append("    int32: ").append(toIndentedString(int32)).append("\n");
        sb.append("    int64: ").append(toIndentedString(int64)).append("\n");
        sb.append("    number: ").append(toIndentedString(number)).append("\n");
        sb.append("    _float: ").append(toIndentedString(_float)).append("\n");
        sb.append("    _double: ").append(toIndentedString(_double)).append("\n");
        sb.append("    decimal: ").append(toIndentedString(decimal)).append("\n");
        sb.append("    string: ").append(toIndentedString(string)).append("\n");
        sb.append("    _byte: ").append(toIndentedString(_byte)).append("\n");
        sb.append("    binary: ").append(toIndentedString(binary)).append("\n");
        sb.append("    date: ").append(toIndentedString(date)).append("\n");
        sb.append("    dateTime: ").append(toIndentedString(dateTime)).append("\n");
        sb.append("    uuid: ").append(toIndentedString(uuid)).append("\n");
        sb.append("    password: ").append(toIndentedString(password)).append("\n");
        sb.append("    patternWithDigits: ").append(toIndentedString(patternWithDigits)).append("\n");
        sb.append("    patternWithDigitsAndDelimiter: ").append(toIndentedString(patternWithDigitsAndDelimiter)).append("\n");
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

