<template>
    <ul class="breadcrumb">
      <li
        v-for="(breadcrumb, idx) in breadcrumbList"
        :key="idx"
        @click="routeTo(breadcrumb.path)"
        :class="{'linked': !!breadcrumb.link}">

        <a href="javascript:void(0);">{{ breadcrumb.name }}</a>

      </li>
    </ul>
</template>

<script>
import ConfigManager from '../services/configManager'

export default {
  name: 'Breadcrumb',
  data () {
    return {
      breadcrumbList: []
    }
  },
  mounted () { this.updateList() },
  watch: { '$route' () { this.updateList() } },
  methods: {
    routeTo (targetPath) {
      // console.log('shall route to ', targetPath)
      this.$router.push(targetPath)
      // if (this.breadcrumbList[pRouteTo].link) this.$router.push(this.breadcrumbList[pRouteTo].link)
    },
    updateList () {
      let currentPath = this.$router.currentRoute.path
      this.pageConfig = ConfigManager.getMetaById(currentPath)

      // calculate the breadcrumb
      // console.log('BreadCrumb ', JSON.stringify(this.pageConfig.breadCrumb))
      this.breadcrumbList = this.pageConfig.breadCrumb
    }
  }
}
</script>

<style scoped>

  ul.breadcrumb {
    position: sticky;
    top: 0;
    z-index: 1;
    padding-top: 8px;
    padding-bottom: 7px;
    padding-left: 1.2rem;
    margin: 0;
    list-style: none;
    background: #f8f8f8;
    border-bottom: 1px solid #ddd;
  }

  ul.breadcrumb li {
    display: inline;
    font-size: 0.9rem;
  }

  ul.breadcrumb li + li::before {
    padding: 8px;
    color: black;
    content: "/\00a0";
  }

  ul.breadcrumb li a {
    color: #0275d8;
    text-decoration: none;
  }

  ul.breadcrumb li a:hover {
    color: #01447e;
    text-decoration: underline;
  }
</style>