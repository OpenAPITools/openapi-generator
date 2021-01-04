<?php
/**
 * UploadFileBody
 */
namespace app\Models;

/**
 * UploadFileBody
 */
class UploadFileBody {

    /** @var string $additionalMetadata Additional data to pass to server*/
    private $additionalMetadata;

    /** @var \SplFileObject $file file to upload*/
    private $file;

}
