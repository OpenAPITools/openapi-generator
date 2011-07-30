package $packageName$;

/**
 * Maintains the compatible server version against which the drive is written
 * @author ramesh
 *
 */
public class VersionChecker {

	private String compatibleVersion = "$apiVersion$";
	
	/**
	* Gets the version against which the driver code was written
	*/
	public String getCompatibleVersion() {
		return compatibleVersion;
	}
}