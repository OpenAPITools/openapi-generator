package helper;

import org.approvaltests.core.Scrubber;

class StripTrailingWhitespaceScrubber implements Scrubber {

    @Override
    public String scrub(String input) {
        String replaced = input.replaceAll("\\s+\n", "\n");
        return replaced.trim();
    }
}
