<template>
  <div id="app">
    <Header @toggleMenu="toggleMenu"/>
    <main>
      <nav class="main__nav" :class="{ active: isNavOpen }">
        <div class="nav__search">
          <input class="nav__search-input" placeholder="Type to search" v-model="searchKeywords" @keyup="search"/>
          <svg v-if="searchResultsVisible" class="nav__search-icon" tabindex="0" 
            @click="reset" 
            @keyup.enter="reset" 
            xmlns="http://www.w3.org/2000/svg" viewBox="0 0 52 52"><path d="M26 0C11.664 0 0 11.663 0 26s11.664 26 26 26 26-11.663 26-26S40.336 0 26 0zm0 50C12.767 50 2 39.233 2 26S12.767 2 26 2s24 10.767 24 24-10.767 24-24 24z"/><path d="M35.707 16.293a.999.999 0 0 0-1.414 0L26 24.586l-8.293-8.293a.999.999 0 1 0-1.414 1.414L24.586 26l-8.293 8.293a.999.999 0 1 0 1.414 1.414L26 27.414l8.293 8.293a.997.997 0 0 0 1.414 0 .999.999 0 0 0 0-1.414L27.414 26l8.293-8.293a.999.999 0 0 0 0-1.414z"/></svg>
        </div>
        <vue-tree-navigation :items="tocItems" :defaultOpenLevel="1" />
      </nav>
      <router-view v-if="!searchResultsVisible"></router-view>
      <div class="container" v-else>
        <h1 class="results__heading" v-if="searchResults.length">{{searchResults.length}} results matching "{{searchKeywords}}"</h1>
        <h1 class="results__heading" v-else>No results matching "{{searchKeywords}}"</h1>

        <ul class="results__search">
          <li class="result" v-for="(result, index) in searchResults" 
            :key="index">
            <header class="result__header">
              <router-link :to="result.path">
                <h2 class="result__title">{{ result.name }}</h2>
              </router-link>

              <span tabindex="0" v-for="(topic, idx) in result.topics" :key="idx" class="tag" @keyup.enter="searchTopic(topic)" @click="searchTopic(topic)">{{ topic }}</span>
            </header>
            <p>{{ result.text | truncate(300, '...') }}</p>
          </li>
        </ul>
      </div>
    </main>
  </div>
</template>

<script>
import ConfigManager from './services/configManager'
import Header from './components/Header'

export default {
  name: 'app',
  data () {
    return {
      baseUrl: 'https://pages.github.ibm.com/merlin/playbook/#',
      isNavOpen: false,
      tocItems: ConfigManager.getPages(),
      content: [],
      searchResultsVisible: false,
      searchResults: null,
      searchKeywords: null
    }
  },
  components: {
    Header
  },
  created () {
    ConfigManager.getStatus()
      .then((data) => {
        if (data !== undefined) this.content = data
      })
      .catch((error) => {
        console.log('error', error)
      })
  },
  methods: {
    toggleMenu () {
      this.isNavOpen = !this.isNavOpen
    },
    search ($event) {
      if (this.searchKeywords) {
        let query = Object.assign({}, this.$route.query, { search: this.searchKeywords })
        this.$router.push({ query })

        this.searchResultsVisible = true
        let filter = 'topic:'
        if (this.searchKeywords.indexOf('topic:') > -1) {
          this.searchResults = this.content.filter(result => {
            let topics = (result.topics || []).map((topic) => {
              return topic.toLowerCase()
            })

            let topicExists = false

            for (let i = 0; i < topics.length; i++) {
              if (topics[i].indexOf(this.searchKeywords.toLowerCase().replace(filter, '')) > -1) {
                topicExists = true
              }
            }

            return topicExists
          })
        } else {
          this.searchResults = this.content.filter(result => {
            return (result.text || '').toLowerCase().indexOf(this.searchKeywords.toLowerCase()) > -1 ||
              (result.name || '').toLowerCase().indexOf(this.searchKeywords.toLowerCase()) > -1
          })
        }
      } else {
        let query = this.$route.query
        delete query.search
        this.$router.push({ query })
        this.reset()
      }
    },
    searchTopic (topic) {
      let filter = `topic:${topic}`
      let query = Object.assign({}, this.$route.query, { search: filter })
      this.searchKeywords = filter
      this.search()
      this.$router.push({ query })
    },
    reset () {
      this.searchResultsVisible = false
      this.searchKeywords = ''
      this.searchResults = []
    }
  },
  watch: {
    $route: {
      immediate: true,
      handler: function (val, oldVal) {
        let url = window.location.href
        if (url.indexOf('?') > -1) {
          let queryString = url.substring(url.indexOf('?') + 1)
          let queryObj = JSON.parse('{"' + decodeURIComponent(queryString.replace(/&/g, '","').replace(/=/g, '": "')) + '"}')
          if (queryObj && queryObj.search) {
            this.searchKeywords = queryObj.search
            this.search()
          }
        } else {
          this.searchKeywords = ''
          this.searchResultsVisible = false
        }
      }
    }
  }
}
</script>

<style lang="scss">
  @import 'styles.scss';

  .topics {
    margin-left: 1rem;
  }

  .result__header {
    display: flex;
    flex-wrap: wrap;
    align-items: center;
    padding: 0.5rem 0;
  }

  .result__header > a {
    max-width: 100%;
  }

  h2.result__title {
    max-width: 100%;
    margin: 0;
    margin-right: 1rem;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  h2.result__title:hover {
    text-decoration: underline;
  }

  .tag {
    padding: 2px 10px;
    margin: 4px 2px;
    font-size: 14px;
    line-height: initial;
    color: white;
    text-align: center;
    text-decoration: none;
    white-space: nowrap;
    cursor: pointer;
    background-color: #888;
    border: none;
    border-radius: 20px;
  }

  .tag:hover {
    background-color: #424242;
  }
</style>
