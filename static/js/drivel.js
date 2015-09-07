$(function() {
    sidepanel = $("#drivelside");
    currentPage = 0;
    getCategories();
});

getCategories = function() {
    $.ajax({
	type: "POST",
	url: "/drivel/categories",
	dataType: "json",
	data: { },
	success: function(data) {
	    initSidepanel(data);
	},
	error: function() { console.log("json error while get categories"); }
    });
    getPosts(0, 10);
}

getPosts = function(from, till) {
    dataObj = {
	from: from,
	till: till
    };
    $.ajax({
	type: "POST",
	url: "/drivel/posts",
	dataType: "json",
	data: { dat: dataObj },
	success: function(data) {
	    console.log(data);
	},
	error: function() { console.log("json error while get posts"); }
    });
}

initSidepanel = function(categories) {
    for(var i = 0; i < categories.length; i++) {
	sidepanel.append('<div class="drivelcat">' +
			 '<div class="drivelop drivelopOFF">' +
			 categories[i] + '</div></div>');
    }
    sidepanel.children().last().attr("id", "drivellastcat");
    var ops = $(".drivelop");
    for(var i = 0; i < ops.length; i++) {
	registerSelect($(ops[i]), a, b);
    }
}

a = function() {
    console.log("select");
}

b = function() {
    console.log("deselect");
}

registerSelect = function(div, selectFun, deselectFun) {
    div.on("mouseenter", function() {
	div.addClass("drivelopHover");
    });
    div.on("mouseout", function() {
	div.removeClass("drivelopHover");
    });
    div.on("mouseup", function() {
	if(div.attr("class").indexOf("drivelopON") > 0) {
	    div.removeClass("drivelopON");
	    div.addClass("drivelopOFF");
	    deselectFun();
	} else {
	    div.removeClass("drivelopOFF");
	    div.addClass("drivelopON");
	    selectFun();
	}
    });
}
