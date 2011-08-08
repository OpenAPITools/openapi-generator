/**
 *  Copyright 2011 Wordnik, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package com.wordnik.swagger.codegen.util;

import com.wordnik.swagger.codegen.exception.CodeGenerationException;

import java.io.*;

/**
 * User: deepakmichael
 * Date: 03/08/11
 * Time: 12:02 AM
 */
public class FileUtil {

    /**
     * Creates directory if doesn't exists and also cleans the files of given type if directory already contains some
     * files.
     *
     * @param classLocation
     * @param fileExtension
     */
    public static void createOutputDirectories(String classLocation, String fileExtension) {
        File outputLocation = new File(classLocation);
        outputLocation.mkdirs(); //make folder if necessary
        //clear contents
        clearFolder(classLocation, fileExtension);

    }

    /**
     * Deletes a fingle file and returns false fi file doesn't exists
     * @param sFilePath
     * @return
     */
    public static boolean deleteFile(String sFilePath) {
        File oFile = new File(sFilePath);
        if (!oFile.exists()) {
            return false;
        }
        return oFile.delete();
    }


    /**
     * Deleet all the files from the specified location
     * @param directoryLocation
     */
    public static void clearFolder(String directoryLocation) {
       File fDir = new File(directoryLocation);
       File[] files = fDir.listFiles();
       if(files != null) {
            for(File aFile : files) {
                aFile.delete();
            }
       }
    }

    // Clears the folder of the files with extension
    public static void clearFolder(String strFolder, final String strExt) {
        File fLogDir = new File(strFolder);
        File[] fLogs = fLogDir.listFiles(new FilenameFilter() {
            public boolean accept(File fDir, String strName) {
                return (strName.endsWith(strExt));
            }
        });
        if (fLogs != null) {
            for (int i = 0; i < fLogs.length; i++) {
                deleteFile(fLogs[i].getAbsolutePath());
            }
        }
    }


    public static void copyDirectory(File srcPath, File dstPath) {
        if (srcPath.isDirectory()) {
            if (!dstPath.exists()) {
                dstPath.mkdir();
            }

            String files[] = srcPath.list();
            for (int i = 0; i < files.length; i++) {
                copyDirectory(new File(srcPath, files[i]), new File(dstPath, files[i]));
            }
        } else {
            if (!srcPath.exists()) {
                throw new CodeGenerationException("Source folder does not exist");
            } else {
                try {
                    InputStream in = new FileInputStream(srcPath);
                    OutputStream out = new FileOutputStream(dstPath);

                    // Transfer bytes from in to out
                    byte[] buf = new byte[1024];
                    int len;
                    while ((len = in.read(buf)) > 0) {
                        out.write(buf, 0, len);
                    }
                    in.close();
                    out.close();
                } catch (IOException e) {
                    e.printStackTrace();
                    throw new CodeGenerationException("Copy directory operation failed");
                }
            }
        }
    }
}
