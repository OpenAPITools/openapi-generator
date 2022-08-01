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

package org.openapitools.codegen.online.service;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * This utility compresses a list of files to standard ZIP format file. It is able to compresses all
 * sub files and sub directories, recursively.
 *
 * @author Ha Minh Nam
 *
 */
public class ZipUtil {
    /**
     * A constants for buffer size used to read/write data.
     */
    private static final int BUFFER_SIZE = 4096;

    /**
     * Compresses a collection of files to a destination zip file.
     *
     * @param listFiles A collection of files and directories
     * @param destZipFile The path of the destination zip file
     * @throws FileNotFoundException if file not found
     * @throws IOException if IO exception occurs
     */
    public void compressFiles(List<File> listFiles, String destZipFile)
            throws IOException {

        try (FileOutputStream fileOutputStream = new FileOutputStream(destZipFile);
             ZipOutputStream zos = new ZipOutputStream(fileOutputStream)) {

            for (File file : listFiles) {
                if (file.isDirectory()) {
                    addFolderToZip(file, file.getName(), zos);
                } else {
                    addFileToZip(file, zos);
                }
            }

            zos.flush();
        }
    }

    /**
     * Adds a directory to the current zip output stream.
     *
     * @param folder the directory to be added
     * @param parentFolder the path of parent directory
     * @param zos the current zip output stream
     * @throws FileNotFoundException if file not found
     * @throws IOException if IO exception occurs
     */
    private void addFolderToZip(File folder, String parentFolder, ZipOutputStream zos)
            throws FileNotFoundException, IOException {
        for (File file : folder.listFiles()) {
            if (file.isDirectory()) {
                addFolderToZip(file, parentFolder + "/" + file.getName(), zos);
                continue;
            }

            zos.putNextEntry(new ZipEntry(parentFolder + "/" + file.getName()));

            try (FileInputStream fileInputStream = new FileInputStream(file);
                 BufferedInputStream bis = new BufferedInputStream(fileInputStream)) {
                byte[] bytesIn = new byte[BUFFER_SIZE];
                int read;
                while ((read = bis.read(bytesIn)) != -1) {
                    zos.write(bytesIn, 0, read);
                }
            }

            zos.closeEntry();

        }
    }

    /**
     * Adds a file to the current zip output stream.
     *
     * @param file the file to be added
     * @param zos the current zip output stream
     * @throws FileNotFoundException if file not found
     * @throws IOException if IO exception occurs
     */
    private static void addFileToZip(File file, ZipOutputStream zos) throws FileNotFoundException,
            IOException {
        zos.putNextEntry(new ZipEntry(file.getName()));

        try (FileInputStream fileInputStream = new FileInputStream(file);
             BufferedInputStream bis = new BufferedInputStream(fileInputStream)) {
            byte[] bytesIn = new byte[BUFFER_SIZE];
            int read;
            while ((read = bis.read(bytesIn)) != -1) {
                zos.write(bytesIn, 0, read);
            }
        }

        zos.closeEntry();
    }
}
