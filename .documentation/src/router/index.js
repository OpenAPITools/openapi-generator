import Vue from 'vue'
import Router from 'vue-router'
import ConfigManager from '../services/configManager'

Vue.use(Router)

export default new Router({
  routes: ConfigManager.getRoutingConfig()
})
