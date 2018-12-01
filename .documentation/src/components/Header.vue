<template>
  <header class="header">
    <div class="header__brand">
      <svg
        ref="nav__open"
        tabindex="0"
        @keyup.enter="openNavigation"
        @click="openNavigation"
        class="header__hamburger" 
        viewBox="0 0 512 512" 
        xmlns="http://www.w3.org/2000/svg">
        <path d="M424 394H89a8 8 0 0 1 0-16h335a8 8 0 0 1 0 16zM424 265H89a8 8 0 0 1 0-16h335a8 8 0 0 1 0 16zM424 135H89a8 8 0 0 1 0-16h335a8 8 0 0 1 0 16z"/>
      </svg>
      <span class="header__name">{{ name }} <b>Playbook</b></span>
    </div>
    <nav class="nav"
      v-if="isVisible || isDesktop"
    >
        <svg
          ref="nav__close"
          class="nav__icon nav__icon--close"
          tabindex="0"
          @keyup.enter="closeNavigation"
          @click="closeNavigation" 
          viewBox="0 0 100 100" 
          xmlns="http://www.w3.org/2000/svg"><path d="M77.6 21.1l-28 28.1-28.1-28.1-1.9 1.9 28 28.1-28 28.1 1.9 1.9L49.6 53l28 28.1 2-1.9-28.1-28.1L79.6 23z"/></svg>
        <ul class="nav__list">
          <router-link
            v-for="item in list"
            :key="item.path"
            :to="item.path"
            class="nav__item"
          > {{ item.name }}
          </router-link>
        </ul>
    </nav>
  </header>
</template>

<script>
import ConfigManager from '../services/configManager'

export default {
  name: 'header-component',
  data () {
    return {
      isVisible: false,
      isDesktop: false,
      name: ConfigManager.getBaseConfig().appName,
      list: ConfigManager.getBaseConfig().headerNavigation
    }
  },
  created: function () {
    if (window.innerWidth >= 780) {
      this.isDesktop = true
    }
    window.addEventListener('resize', this.handleResize)
  },
  beforeDestroy: function () {
    window.removeEventListener('resize', this.handleResize)
  },
  methods: {
    handleResize (event) {
      if (event.currentTarget.innerWidth >= 780) {
        this.isDesktop = true
      } else {
        this.isDesktop = false
      }
    },
    openNavigation () {
      this.$emit('toggleMenu')
    },
    closeNavigation (event) {
      this.$emit('toggleMenu')
    }
  }
}
</script>
