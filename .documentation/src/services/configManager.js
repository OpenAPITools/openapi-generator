import pageConfig from '../pageConfig.json'
import axios from 'axios'

const DefaultPageRenderer = () => import('@/views/DefaultPageRenderer')
var metaMap = {}

/**
 * processElement - recursively walk through a given node and enrich it with some additional
 * properties (to bring vue-tree-navigation and VueRouter in sync)
 */
function processElement (node, parent) {
  // add fully qualified path and reference to component
  node.path = node.route
  if (!parent) node.breadCrumb = []
  else node.breadCrumb = JSON.parse(JSON.stringify(parent.breadCrumb))
  let parentPath = (parent != null) ? (parent.path + node.route) : '/'

  node.breadCrumb.push({
    name: node.name,
    path: parentPath
  })

  node.component = DefaultPageRenderer
  if (parent != null) node.path = parent.path + node.route

  // process all childs and extracts the tiles (the next level of childs)
  let tiles = []
  if (node.children) {
    for (var i in node.children) {
      let child = node.children[i]
      tiles.push(
        {
          name: child.name,
          description: child.description,
          path: node.path + child.route,
          icon: child.icon,
          bgColor: child.bgColor,
          owner: child.owner,
          topics: child.topics
        }
      )
      processElement(child, node)
    }
  }

  // construct meta object we give to DefaultPageRenderer as part of the current route
  node.meta = {
    tiles: (tiles.length > 0) ? tiles : null,
    name: node.name,
    description: node.description,
    markdown: node.markdown,
    icon: node.icon,
    bgColor: node.bgColor,
    owner: node.owner,
    topics: node.topics,
    breadCrumb: node.breadCrumb
  }

  // store the meta data in our metaMap
  metaMap[node.path] = node.meta
}

/**
 * generateRoutingConfig - generates the routing for VueRouter out of the pageConfig.pages
 */
function generateRoutingConfig (baseConfig) {
  let pages = JSON.parse(JSON.stringify(baseConfig.pages))

  for (let i in pages) {
    let element = pages[i]
    processElement(element, null)
  }

  // configure the default landing page
  pages.push(
    {
      path: '/',
      redirect: baseConfig.landingPage
    }
  )
  return pages
}

export default {
  /**
   * getMetaById - gets the page metadata by a given id
   */
  getMetaById: function (route) {
    return metaMap[route]
  },

  /**
   * generates the routing config based on the tree
   */
  getRoutingConfig: function () {
    let routingConfig = generateRoutingConfig(this.getBaseConfig())

    // add more components

    return routingConfig
  },

  /**
   * getBaseConfig - returns the full json from pageConfig.json
   */
  getBaseConfig: function () {
    return pageConfig
  },

  /**
   * getPages - returns the pages section from base config (for vue-tree-navigation)
   */
  getPages: function () {
    return this.getBaseConfig().pages
  },

  /**
   * getStatus
   */
  getStatus: () => {
    return new Promise((resolve, reject) => {
      const pathName = window.location.pathname
      const path = `${pathName.substring(0, pathName.length - 1)}/static/content.json`

      axios.get(path)
        .then(response => {
          resolve(response.data)
        })
        .catch((error) => {
          return reject(error)
        })
    })
  }

}
