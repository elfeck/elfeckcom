$(function() {
    sidepanel = $("#drivelside");
    currentPage = 0;
    currentPostPostition = 0;
    postsPerPage = 1;
    posts = [];

    postOnly = $(".drivelpostonly");
    cats = [];

    getCategories();
    getNextPosts(postsPerPage + 1);

    $("#drivelforward").click(function(e) { changePage(1); });
    $("#drivelbackward").click(function(e) { changePage(-1); });
    $("#driveltotop").click(function(e) { window.scrollTo(0, 0); });
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
}

// min: amount = 1
getNextPosts = function(amount) {
    lastReq = amount;
    amount -= 1;
    var activeCats = []
    for(var i = 0; i < cats.length; i++) {
	if($(cats[i]).attr("class").indexOf("drivelopON") > 0) {
	    activeCats.push($(cats[i]).text());
	}
    }
    var postOnly = $(".drivelpostonly").attr("class").
	indexOf("drivelopON") > 0 ? 1 : 0;
    dataObj = {
	from: currentPostPostition,
	till: currentPostPostition + amount,
	cats: activeCats,
	postsOnly: postOnly
    };
    $.ajax({
	type: "POST",
	url: "/drivel/posts",
	dataType: "json",
	data: { dat: dataObj },
	success: function(data) {
	    processPosts(data);
	},
	error: function() { console.log("json error while get posts"); }
    });
}

processPosts = function(data) {
    if(data.length == 0) {
	console.log("no new posts");
    } else {
	togglePage($("#drivelforward"), data.length == lastReq);
	currentPostPostition += data.length;
	posts = posts.concat(data);
	//console.log(currentPostPostition);
	//console.log(posts);
    }
}

initSidepanel = function(categories) {
    for(var i = 0; i < categories.length; i++) {
	sidepanel.append('<div class="drivelcatcont">' +
			 '<div class="drivelcat drivelopOFF">' +
			 categories[i] + '</div></div>');
    }
    sidepanel.children().last().attr("id", "drivellastcat");
    cats = $(".drivelcat");
    for(var i = 0; i < cats.length; i++) {
	registerSelect($(cats[i]), a, a);
    }
    registerSelect($(".drivelpostonly"), a, a);
}

a = function() {

}

changePage = function(dir) {
    currentPage += dir;
    if(currentPage == 0) togglePage($("drivelbackward"), false);
    if(currentPage == 1) togglePage($("drivelbackward"), true);
}

togglePage = function(ele, toOn) {
    if(toOn) {
	ele.removeClass("drivelinactive");
	ele.addClass("drivelactive");
    }
    else {
	ele.removeClass("drivelactive");
	ele.addClass("drivelinactive");
    }
}

registerSelect = function(ele, selectFun, deselectFun) {
    ele.on("mouseenter", function() {
	ele.addClass("drivelopHover");
    });
    ele.on("mouseout", function() {
	ele.removeClass("drivelopHover");
    });
    ele.on("mouseup", function() {
	if(ele.attr("class").indexOf("drivelopON") > 0) {
	    ele.removeClass("drivelopON");
	    ele.addClass("drivelopOFF");
	    deselectFun();
	} else {
	    ele.removeClass("drivelopOFF");
	    ele.addClass("drivelopON");
	    selectFun();
	}
    });
}
