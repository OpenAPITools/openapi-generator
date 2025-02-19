let fs   = require('fs');
let path = require('path');
let util = require('util');
let yaml = require('./js-yaml.js');
let samples = require('./samples.json');

class LabelMatch {
    constructor (match, label) {
        this.match = match;
        this.label = label;
    }
}

class FileError {
    constructor (file, actualLabels, expectedLabels) {
        this.file = file;
        this.actual = actualLabels;
        this.expected = expectedLabels;
    }
}

class TextError {
    constructor (text, actualLabels, expectedLabels) {
        this.text = text;
        this.actual = actualLabels;
        this.expected = expectedLabels;
    }
}

let labels = [];

function labelsForFile(file) {
    let body = fs.readFileSync(file);
    return labelsForText(body)
}

function labelsForText(text) {
    let addLabels = new Set();
    let body = text;
    for (const v of labels) {
        if (v.match.test(body)) {
            addLabels.add(v.label)
        }
        // reset regex state
        v.match.lastIndex = 0
    }
    return addLabels;
}

try {
    let config = yaml.safeLoad(fs.readFileSync('../auto-labeler.yml', 'utf8'));

    if (config && config.labels && Object.keys(config.labels).length > 0) {
        for (const labelName in config.labels) {
            if (config.labels.hasOwnProperty(labelName)) {
                let matchAgainst = config.labels[labelName];
                if (Array.isArray(matchAgainst)) {
                    matchAgainst.forEach(regex => {
                        // noinspection JSCheckFunctionSignatures
                        labels.push(new LabelMatch(new RegExp(regex, 'g'), labelName));
                    })
                }
            }
        }
    }

    if (labels.length === 0) {
        // noinspection ExceptionCaughtLocallyJS
        throw new Error("Expected to parse config.labels, but failed.")
    }

    let fileErrors = [];
    samples.files.forEach(function(tester){
        let file = path.normalize(path.join('..', '..', 'bin', tester.input));
        let expectedLabels = new Set(tester.matches);
        let actualLabels = labelsForFile(file);
        let difference = new Set([...actualLabels].filter(x => !expectedLabels.has(x)));
        if (difference.size > 0) {
            fileErrors.push(new FileError(file, actualLabels, expectedLabels));
        }
    });

    let textErrors = [];
    samples.text.forEach(function(tester){
        let expectedLabels = new Set(tester.matches);
        let actualLabels = labelsForText(tester.input);
        let difference = new Set([...actualLabels].filter(x => !expectedLabels.has(x)));
        if (difference.size > 0) {
            textErrors.push(new TextError(tester.input, actualLabels, expectedLabels));
        }
    });

    // These are separate (file vs text) in case we want to preview where these would fail in the file. not priority at the moment.
    if (fileErrors.length > 0) {
        console.warn('There were %d file tester errors', fileErrors.length);
        fileErrors.forEach(function(errs) {
            console.log("file: %j\n  actual: %j\n  expected: %j", errs.file, util.inspect(errs.actual), util.inspect(errs.expected))
        });
    }

    if (textErrors.length > 0) {
        console.warn('There were %d text tester errors', textErrors.length);
        textErrors.forEach(function(errs){
            console.log("input: %j\n  actual: %j\n  expected: %j", errs.text, util.inspect(errs.actual), util.inspect(errs.expected))
        })
    }

    let totalErrors = fileErrors.length + textErrors.length;
    if (totalErrors === 0) {
        console.log('Success!');
    } else {
        console.log('Failure: %d total errors', totalErrors);
    }
} catch (e) {
    console.log(e);
}

