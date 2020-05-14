window.onhashchange = function() {
  choose(window.location.href.toString());
}

var selectionImage;

function choose(url) {
  var f = url.split("/").slice(-1)[0].split("?")[0];
  if(f.match(/#/g) && f.match(/#/g).length > 0) {
    f = f.split("#")[0];
  }
  $('div.non-sidebar').empty();
  $('div.non-sidebar').load("operations/" + f + ".html", function(){
    goToAnchor();
    // rebind the models
    $(".model a").on("click", function(e){
      e.preventDefault();
      var model = $(this).parent().attr("data-model")
      var parentOffset = $(this).parent().offset();
      var encodedWord = encodeURI(model);

      if(!selectionImage) {
          selectionImage = $('<div>').attr({
          title: 'Model detail',
          target: '_blank',
          class: 'model-detail-popup'
        }).hide();
        $('div.model-container').append(selectionImage);
      }

      selectionImage.load("models/" + encodedWord + ".html")
      selectionImage.attr('href',url.replace('{term}',encodeURI(model))).css({
        left: e.pageX + 20,
        top:  e.pageY - 10,
        position: "absolute"
      }).fadeIn();
    })
  });

  $("body").on("click",function(e) {
    var target = $(e.target);
    if (target.parents(".model-detail-popup").length == 0 && target.parents('.model').length != 1) {
      if(document.querySelector('.model-detail-popup')) {
        document.querySelector('.model-detail-popup').style.display = 'none';
      }
    }
  });
}

function goToAnchor() {
  var doARead = $($('a')[0]).offset();
  var anchorArr = window.location.href.toString().split("#");
  if(anchorArr.length > 2) {
    var anchor = anchorArr[anchorArr.length-1];
    window.scrollTo(0,$('a[name='+anchor+']').offset().top - 80);
  }
}
function resize()
{
    $(".sidebar").css('height', $(window).height() -60);
    $("#content-window").css('height', $(window).height() -60);

}
$(function(){
    window.onresize = resize;
    resize();
    $(window).bind('hashchange', function() {
        choose(window.location.href.toString());
    });
});