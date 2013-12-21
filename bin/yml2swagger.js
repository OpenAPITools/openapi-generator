fs = require('fs')
yaml = require('js-yaml')

var args = process.argv.splice(2);

if(args.length == 0) {
  process.exit(1);
}

var arg0 = args[0];
var outputdir = ".";

if(args.length > 1) {
  outputdir = args[1];
}

var file = fs.lstatSync(arg0);

if(file.isFile()) {
  convert(arg0, outputdir);
}
else if (file.isDirectory()) {
  var files = fs.readdirSync(arg0);
  files.map(function(item) {
    var filename = arg0 + "/" + item;
    var file = fs.lstatSync(filename);
    if(file.isFile()) {
      convert(filename, outputdir);
    }
  });
}

function convert(filename, outputdir) {
  console.log("converting " + filename);
  fs.readFile(filename, "utf8", function (err, data) {
    if(err) {
      console.log(err);
    }
    else {
      try {
        var js = yaml.load(data);
        var prettyJs = JSON.stringify(js, undefined, 2);
        var outputFilename = outputdir + "/" + filename.split("/").pop() + ".json";
        console.log("writing to " + outputFilename);
        fs.writeFile(outputFilename, prettyJs, function(err) {
          if(err) {
              console.log(err);
          }
        });
      }
      catch (err) {
        console.log(err);
      }
    }
  });
}
