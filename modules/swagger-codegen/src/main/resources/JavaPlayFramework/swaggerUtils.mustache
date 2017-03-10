package swagger;

import javafx.util.Pair;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;

public class SwaggerUtils {

	/**
	 * Format to {@code Pair} objects.
	 *
	 * @param collectionFormat collection format (e.g. csv, tsv)
	 * @param name Name
	 * @param value Value
	 * @return A list of Pair objects
	 */
	public static List<Pair> parameterToPairs(String collectionFormat, String name, Object value){
		List<Pair> params = new ArrayList<Pair>();

		// preconditions
		if (name == null || name.isEmpty() || value == null) return params;

		Collection valueCollection = null;
		if (value instanceof Collection) {
			valueCollection = (Collection) value;
		} else {
			params.add(new Pair(name, parameterToString(value)));
			return params;
		}

		if (valueCollection.isEmpty()){
			return params;
		}

		// get the collection format
		collectionFormat = (collectionFormat == null || collectionFormat.isEmpty() ? "csv" : collectionFormat); // default: csv

		// create the params based on the collection format
		if (collectionFormat.equals("multi")) {
			for (Object item : valueCollection) {
				params.add(new Pair(name, parameterToString(item)));
			}

			return params;
		}

		String delimiter = ",";

		if (collectionFormat.equals("csv")) {
			delimiter = ",";
		} else if (collectionFormat.equals("ssv")) {
			delimiter = " ";
		} else if (collectionFormat.equals("tsv")) {
			delimiter = "\t";
		} else if (collectionFormat.equals("pipes")) {
			delimiter = "|";
		}

		StringBuilder sb = new StringBuilder() ;
		for (Object item : valueCollection) {
			sb.append(delimiter);
			sb.append(parameterToString(item));
		}

		params.add(new Pair(name, sb.substring(1)));

		return params;
	}

	/**
	 * Format the given parameter object into string.
	 *
	 * @param param Parameter
	 * @return String representation of the parameter
	 */
	public static String parameterToString(Object param) {
		if (param == null) {
			return "";
		} else if (param instanceof Date) {
			return formatDatetime((Date) param);
		} else if (param instanceof Collection) {
			StringBuilder b = new StringBuilder();
			for (Object o : (Collection)param) {
				if (b.length() > 0) {
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
	 * Format the given Date object into string (Datetime format).
	 *
	 * @param date Date object
	 * @return Formatted datetime in string representation
	 */
	public static String formatDatetime(Date date) {
		return new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX").format(date);
	}
}