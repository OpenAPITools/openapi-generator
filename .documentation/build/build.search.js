'use strict'

const fs = require('fs')
const path = require('path')

const removeMd = require('remove-markdown');

const config = require('../config')
const pagesConfig = require('../src/pageConfig')
const pages = pagesConfig.pages;

const countWords = (text) => {
  // exclude start and end white-space
  text = text.replace(/(^\s*)|(\s*$)/gi, '')
  // 2 or more space to 1
  text = text.replace(/[ ]{2,}/gi, ' ')
  // exclude newline with a start spacing
  text = text.replace(/\n /, '\n')
  return text.split(' ').filter((str) => { 
    return str !== '' }
  ).length || 0
}

const getContent = (page, parent) => {
  return new Promise((resolve, reject) => {
    let content = {
      name: page.name,
      description: page.description,
      owner: page.owner,
      path: (parent) ? `${parent}${page.route}` : page.route,
      text: '',
      wordCount: 0
    }
    if (page.markdown) {
      let mdPath = path.normalize(path.join(__dirname, '..', page.markdown));
      fs.readFile(mdPath, 'utf8', (err, data) => {
        let text = removeMd(data);
        content.text = text;
        content.wordCount = countWords(text);
        content.type = 'Content';
        return resolve(content);
      });
    } else {
      if (page.children) {
        content.type = 'TOC';
      }
      return resolve(content);
    }
  })
}

const traversTree = (pages, parent) => {
  let PAGES = [];

  return new Promise(async (resolve, reject) => {
    for (let i = 0; i < pages.length; i++) {
      let page = pages[i];

      let content = await getContent(page, parent);
      PAGES.push(content);
      if (page.children) {
        let CHILD_PAGES = await traversTree(page.children, content.path)
        if (CHILD_PAGES && Array.isArray(CHILD_PAGES)) {
          PAGES = PAGES.concat(CHILD_PAGES)
        }
      }
    }
    return resolve(PAGES);
  })
}

traversTree(pages)
  .then((PAGE_CONTENTS) => {
    fs.writeFile (
      path.join(config.build.assetsRoot, '/static/content.json'), 
      JSON.stringify(PAGE_CONTENTS),
      (err) => {
        if (err) throw err;
        console.log('complete');
      }
    );
  })
  .catch((error) => {
    console.log('error', error);
  });
