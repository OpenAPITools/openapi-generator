package $packageName$ {

/**
 * Maintains the compatible server version against which the drive is written
 * @author deepak
 *
 */
    public class VersionChecker {

        private var compatibleVersion: String = "$apiVersion$";

        /**
        * Gets the version against which the driver code was written
        */
        public function getCompatibleVersion(): String {
            return compatibleVersion;
        }
    }

}