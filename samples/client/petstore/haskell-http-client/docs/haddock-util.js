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
  rememberCollapsed(id, b);
  return b;
}

var collapsed = {};
function rememberCollapsed(id, b)
{
  if(b)
    delete collapsed[id]
  else
    collapsed[id] = null;

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



var max_results = 75; // 50 is not enough to search for map in the base libraries
var shown_range = null;
var last_search = null;

function quick_search()
{
    perform_search(false);
}

function full_search()
{
    perform_search(true);
}


function perform_search(full)
{
    var text = document.getElementById("searchbox").value.toLowerCase();
    if (text == last_search && !full) return;
    last_search = text;

    var table = document.getElementById("indexlist");
    var status = document.getElementById("searchmsg");
    var children = table.firstChild.childNodes;

    // first figure out the first node with the prefix
    var first = bisect(-1);
    var last = (first == -1 ? -1 : bisect(1));

    if (first == -1)
    {
        table.className = "";
        status.innerHTML = "No results found, displaying all";
    }
    else if (first == 0 && last == children.length - 1)
    {
        table.className = "";
        status.innerHTML = "";
    }
    else if (last - first >= max_results && !full)
    {
        table.className = "";
        status.innerHTML = "More than " + max_results + ", press Search to display";
    }
    else
    {
        // decide what you need to clear/show
        if (shown_range)
            setclass(shown_range[0], shown_range[1], "indexrow");
        setclass(first, last, "indexshow");
        shown_range = [first, last];
        table.className = "indexsearch";
        status.innerHTML = "";
    }


    function setclass(first, last, status)
    {
        for (var i = first; i <= last; i++)
        {
            children[i].className = status;
        }
    }


    // do a binary search, treating 0 as ...
    // return either -1 (no 0's found) or location of most far match
    function bisect(dir)
    {
        var first = 0, finish = children.length - 1;
        var mid, success = false;

        while (finish - first > 3)
        {
            mid = Math.floor((finish + first) / 2);

            var i = checkitem(mid);
            if (i == 0) i = dir;
            if (i == -1)
                finish = mid;
            else
                first = mid;
        }
        var a = (dir == 1 ? first : finish);
        var b = (dir == 1 ? finish : first);
        for (var i = b; i != a - dir; i -= dir)
        {
            if (checkitem(i) == 0) return i;
        }
        return -1;
    }


    // from an index, decide what the result is
    // 0 = match, -1 is lower, 1 is higher
    function checkitem(i)
    {
        var s = getitem(i).toLowerCase().substr(0, text.length);
        if (s == text) return 0;
        else return (s > text ? -1 : 1);
    }


    // from an index, get its string
    // this abstracts over alternates
    function getitem(i)
    {
        for ( ; i >= 0; i--)
        {
            var s = children[i].firstChild.firstChild.data;
            if (s.indexOf(' ') == -1)
                return s;
        }
        return ""; // should never be reached
    }
}

function setSynopsis(filename) {
    if (parent.window.synopsis && parent.window.synopsis.location) {
        if (parent.window.synopsis.location.replace) {
            // In Firefox this avoids adding the change to the history.
            parent.window.synopsis.location.replace(filename);
        } else {
            parent.window.synopsis.location = filename;
        }
    }
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

