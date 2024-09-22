//
//  FileUtils.swift
//  SwaggerClientTests
//
//  Created by Bruno Coelho on 11/03/2020.
//  Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
//

import UIKit

class FileUtils {
    static func saveImage(imageName: String, image: UIImage) -> URL? {
        guard let documentsDirectory = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first else {
            return nil
        }

        let fileName = imageName
        let fileURL = documentsDirectory.appendingPathComponent(fileName)
        guard let data = image.jpegData(compressionQuality: 1) else { return nil }

        // Checks if file exists, removes it if so.
        deleteFile(fileURL: fileURL)

        do {
            try data.write(to: fileURL)
        } catch let error {
            print("error saving file with error", error)
            return nil
        }

        return fileURL
    }

    @discardableResult
    static func deleteFile(fileURL: URL) -> Bool {
        if FileManager.default.fileExists(atPath: fileURL.path) {
            do {
                try FileManager.default.removeItem(atPath: fileURL.path)
                print("Removed old image")
                return true
            } catch let removeError {
                print("couldn't remove file at path", removeError)
                return false
            }
        }
        return false
    }

}
