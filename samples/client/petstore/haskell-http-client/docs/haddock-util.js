// Haddock JavaScript utilities

var rspace = /\s\s+/g,
    rtrim = /^\s+|\s+$/g;

function spaced(s) { return (" " + s + " ").replace(rspace, " "); }
function trim(s)   { return s.replace(rtrim, ""); }

function hasClass(elem, value) {
  var className = spaced(elem.className || "");
  return className.indexOf( " " + value + " " ) >= 0;
}

function addClass(elem, value) {
  var className = spaced(elem.className || "");
  if ( className.indexOf( " " + value + " " ) < 0 ) {
    elem.className = trim(className + " " + value);
  }
}

function removeClass(elem, value) {
  var className = spaced(elem.className || "");
  className = className.replace(" " + value + " ", " ");
  elem.className = trim(className);
}

function toggleClass(elem, valueOn, valueOff, bool) {
  if (bool == null) { bool = ! hasClass(elem, valueOn); }
  if (bool) {
    removeClass(elem, valueOff);
    addClass(elem, valueOn);
  }
  else {
    removeClass(elem, valueOn);
    addClass(elem, valueOff);
  }
  return bool;
}


function makeClassToggle(valueOn, valueOff)
{
  return function(elem, bool) {
    return toggleClass(elem, valueOn, valueOff, bool);
  }
}

toggleShow = makeClassToggle("show", "hide");
toggleCollapser = makeClassToggle("collapser", "expander");

function toggleSection(id)
{
  var b = toggleShow(document.getElementById("section." + id));
  toggleCollapser(document.getElementById("control." + id), b);
  rememberCollapsed(id);
  return b;
}

var collapsed = {};
function rememberCollapsed(id)
{
  if(collapsed[id])
    delete collapsed[id]
  else
    collapsed[id] = true;

  var sections = [];
  for(var i in collapsed)
  {
    if(collapsed.hasOwnProperty(i))
      sections.push(i);
  }
  // cookie specific to this page; don't use setCookie which sets path=/
  document.cookie = "collapsed=" + escape(sections.join('+'));
}

function restoreCollapsed()
{
  var cookie = getCookie("collapsed");
  if(!cookie)
    return;

  var ids = cookie.split('+');
  for(var i in ids)
  {
    if(document.getElementById("section." + ids[i]))
      toggleSection(ids[i]);
  }
}

function setCookie(name, value) {
  document.cookie = name + "=" + escape(value) + ";path=/;";
}

function clearCookie(name) {
  document.cookie = name + "=;path=/;expires=Thu, 01-Jan-1970 00:00:01 GMT;";
}

function getCookie(name) {
  var nameEQ = name + "=";
  var ca = document.cookie.split(';');
  for(var i=0;i < ca.length;i++) {
    var c = ca[i];
    while (c.charAt(0)==' ') c = c.substring(1,c.length);
    if (c.indexOf(nameEQ) == 0) {
      return unescape(c.substring(nameEQ.length,c.length));
    }
  }
  return null;
}

function addMenuItem(html) {
  var menu = document.getElementById("page-menu");
  if (menu) {
    var btn = menu.firstChild.cloneNode(false);
    btn.innerHTML = html;
    menu.appendChild(btn);
  }
}

function styles() {
  var i, a, es = document.getElementsByTagName("link"), rs = [];
  for (i = 0; a = es[i]; i++) {
    if(a.rel.indexOf("style") != -1 && a.title) {
      rs.push(a);
    }
  }
  return rs;
}

function addStyleMenu() {
  var as = styles();
  var i, a, btns = "";
  for(i=0; a = as[i]; i++) {
    btns += "<li><a href='#' onclick=\"setActiveStyleSheet('"
      + a.title + "'); return false;\">"
      + a.title + "</a></li>"
  }
  if (as.length > 1) {
    var h = "<div id='style-menu-holder'>"
      + "<a href='#' onclick='styleMenu(); return false;'>Style &#9662;</a>"
      + "<ul id='style-menu' class='hide'>" + btns + "</ul>"
      + "</div>";
    addMenuItem(h);
  }
}

function setActiveStyleSheet(title) {
  var as = styles();
  var i, a, found;
  for(i=0; a = as[i]; i++) {
    a.disabled = true;
          // need to do this always, some browsers are edge triggered
    if(a.title == title) {
      found = a;
    }
  }
  if (found) {
    found.disabled = false;
    setCookie("haddock-style", title);
  }
  else {
    as[0].disabled = false;
    clearCookie("haddock-style");
  }
  styleMenu(false);
}

function resetStyle() {
  var s = getCookie("haddock-style");
  if (s) setActiveStyleSheet(s);
}


function styleMenu(show) {
  var m = document.getElementById('style-menu');
  if (m) toggleShow(m, show);
}


function pageLoad() {
  addStyleMenu();
  resetStyle();
  restoreCollapsed();
}

