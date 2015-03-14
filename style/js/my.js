$(document).ready(function(){

    $("div.figure p img").each(function() {
	// if ($(this).css("height") > 300) {
        var src = $(this).attr('src').replace('img_T','img_L');
        var title = $(this).parent().next().text();
        var a = $('<a/>').attr('href', src).attr('rel', 'lightbox[figures]').attr('title', title);
        $(this).wrap(a);
	// }
    });

    // http://be.twixt.us/jquery/codeExpander.php
    $("pre.example, pre.src").each(function() { // find specific elements
	if ($(this).height() > 300) { // filter on CSS height > 100
	    var _startHeight = parseInt($(this).css("height")); // store current height
	    $(this).css("height", "300px")  // set height to 100px
		.wrap("<div>") // add a div to group new elements
  		.parent() // Select previously added div
 	    // add new child for clickable area
  		.append("<div class=\"ceexpand\">Expand</div>")
  		.find("div.ceexpand") // select new child
  		.hover( // add a hover class
  		    function() { $(this).toggleClass("cehover") },
  		    function() { $(this).toggleClass("cehover") }
  		)
  		.toggle( // toggle the click action of the div
  		    // Position Closed -> Open It
	  	    function () {
	  		// swap the class and change the HTML
	  		$(this).toggleClass("cecollapse").html("Collapse")
	  		// select the pre and animate it open
	  		    .siblings("pre").animate({height: _startHeight});
	  	    },
  		    // Position Open -> Close It
	  	    function () {
	  		// swap the class and change the html
	  		$(this).toggleClass("cecollapse").html("Expand")
	  		// select the pre and animate it closed
	  		    .siblings("pre").animate({height: 300});
	  	    }
	  	);
	}
    });
});
