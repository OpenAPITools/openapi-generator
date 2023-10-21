package org.openapitools.server.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import java.io.File;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.UUID;



public class FormatTest   {

    private Integer _integer;
    private Integer int32;
    private Long int64;
    private BigDecimal number;
    private Float _float;
    private Double _double;
    private BigDecimal decimal;
    private String _string;
    private byte[] _byte;
    private File binary;
    private LocalDate _date;
    private OffsetDateTime _dateTime;
    private UUID _uuid;
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
     * @param _integer _integer
     * @param int32 int32
     * @param int64 int64
     * @param number number
     * @param _float _float
     * @param _double _double
     * @param decimal decimal
     * @param _string _string
     * @param _byte _byte
     * @param binary binary
     * @param _date _date
     * @param _dateTime _dateTime
     * @param _uuid _uuid
     * @param password password
     * @param patternWithDigits A string that is a 10 digit number. Can have leading zeros.
     * @param patternWithDigitsAndDelimiter A string starting with &#39;image_&#39; (case insensitive) and one to three digits following i.e. Image_01.
     */
    public FormatTest(
        Integer _integer, 
        Integer int32, 
        Long int64, 
        BigDecimal number, 
        Float _float, 
        Double _double, 
        BigDecimal decimal, 
        String _string, 
        byte[] _byte, 
        File binary, 
        LocalDate _date, 
        OffsetDateTime _dateTime, 
        UUID _uuid, 
        String password, 
        String patternWithDigits, 
        String patternWithDigitsAndDelimiter
    ) {
        this._integer = _integer;
        this.int32 = int32;
        this.int64 = int64;
        this.number = number;
        this._float = _float;
        this._double = _double;
        this.decimal = decimal;
        this._string = _string;
        this._byte = _byte;
        this.binary = binary;
        this._date = _date;
        this._dateTime = _dateTime;
        this._uuid = _uuid;
        this.password = password;
        this.patternWithDigits = patternWithDigits;
        this.patternWithDigitsAndDelimiter = patternWithDigitsAndDelimiter;
    }



    /**
     * Get _integer
     * minimum: 10
     * maximum: 100
     * @return _integer
     */
    public Integer getInteger() {
        return _integer;
    }

    public void setInteger(Integer _integer) {
        this._integer = _integer;
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
     * Get _string
     * @return _string
     */
    public String getString() {
        return _string;
    }

    public void setString(String _string) {
        this._string = _string;
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
     * Get _date
     * @return _date
     */
    public LocalDate getDate() {
        return _date;
    }

    public void setDate(LocalDate _date) {
        this._date = _date;
    }

    /**
     * Get _dateTime
     * @return _dateTime
     */
    public OffsetDateTime getDateTime() {
        return _dateTime;
    }

    public void setDateTime(OffsetDateTime _dateTime) {
        this._dateTime = _dateTime;
    }

    /**
     * Get _uuid
     * @return _uuid
     */
    public UUID getUuid() {
        return _uuid;
    }

    public void setUuid(UUID _uuid) {
        this._uuid = _uuid;
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
        
        sb.append("    _integer: ").append(toIndentedString(_integer)).append("\n");
        sb.append("    int32: ").append(toIndentedString(int32)).append("\n");
        sb.append("    int64: ").append(toIndentedString(int64)).append("\n");
        sb.append("    number: ").append(toIndentedString(number)).append("\n");
        sb.append("    _float: ").append(toIndentedString(_float)).append("\n");
        sb.append("    _double: ").append(toIndentedString(_double)).append("\n");
        sb.append("    decimal: ").append(toIndentedString(decimal)).append("\n");
        sb.append("    _string: ").append(toIndentedString(_string)).append("\n");
        sb.append("    _byte: ").append(toIndentedString(_byte)).append("\n");
        sb.append("    binary: ").append(toIndentedString(binary)).append("\n");
        sb.append("    _date: ").append(toIndentedString(_date)).append("\n");
        sb.append("    _dateTime: ").append(toIndentedString(_dateTime)).append("\n");
        sb.append("    _uuid: ").append(toIndentedString(_uuid)).append("\n");
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

