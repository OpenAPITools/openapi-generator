/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.utils;

public class SemVer implements Comparable<SemVer> {

    public final int major;
    public final int minor;
    public final int revision;

    public SemVer(String versionString) {
        String[] tokens = versionString.split("\\.");
        major = Integer.parseInt(tokens[0]);
        minor = tokens.length < 2 ? 0 : Integer.parseInt(tokens[1]);
        revision = tokens.length < 3 ? 0 : Integer.parseInt(tokens[2]);
    }

    @Override
    public int compareTo(SemVer o) {
        int cmp = major - o.major;
        if (cmp != 0) return cmp;
        cmp = minor - o.minor;
        if (cmp != 0) return cmp;
        return revision - o.revision;
    }

    public boolean atLeast(String other) {
        return compareTo(new SemVer(other)) >= 0;
    }

    @Override
    public String toString() {
        return major + "." + minor + "." + revision;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + major;
        result = prime * result + minor;
        result = prime * result + revision;
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        SemVer other = (SemVer) obj;
        if (major != other.major)
            return false;
        if (minor != other.minor)
            return false;
        if (revision != other.revision)
            return false;
        return true;
    }

}