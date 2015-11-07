$(function() {
    sidepanel = $("#drivelside");
    body = $(".drivelcontent")
    currentPage = 0;
    currentPageRendered = false;

    currentPostPostition = 0;
    postsPerPage = 5;
    posts = [];

    postOnly = $(".drivelpostonly");
    cats = [];

    getCategories();
    getNextPosts(postsPerPage + 1);

    $("#drivelforward").click(function(e) { changePage(-1); });
    $("#drivelbackward").click(function(e) { changePage(1); });
    $("#driveltotop").click(function(e) { window.scrollTo(0, 0); });

    if(typeof registerButton === "function") initQuickpost();
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
    // haskell wants true and false upper case since its a type
    var postOnly = $(".drivelpostonly").attr("class").
	indexOf("drivelopON") > 0 ? "True" : "False";
    var activeCats = activeCats.length == 0 ? "" : activeCats.toString()
    dataObj = {
	from: currentPostPostition,
	till: currentPostPostition + amount,
	cats: activeCats,
	postOnly: postOnly
    };
    $.ajax({
	type: "POST",
	url: "/drivel/posts",
	dataType: "json",
	data: { dat: dataObj },
	success: function(data) {
	    console.log(data);
	    processPosts(data);
	},
	error: function() { console.log("json error while get posts"); }
    });
}

processPosts = function(data) {
    if(data.length == 0) {
	togglePage($("#drivelbackward"), false)
	//case a combi does not exist at all render blank page
	if(!currentPageRendered && currentPage == 0) {
	    renderCurrentPage()
	    return;
	}
    } else {
	togglePage($("#drivelbackward"), data.length == lastReq);
	currentPostPostition += data.length;
	posts = posts.concat(data);
	//console.log(currentPostPostition);
	//console.log(posts);
    }
    if(!currentPageRendered) {
	renderCurrentPage()
    }
}

initSidepanel = function(categories) {
    for(var i = 0; i < categories.length; i++) {
	sidepanel.append('<div class="drivelsbcatcont">' +
			 '<div class="drivelsbcat drivelopOFF">' +
			 categories[i] + '</div></div>');
    }
    sidepanel.children().last().attr("id", "drivellastcat");
    cats = $(".drivelsbcat");
    for(var i = 0; i < cats.length; i++) {
	registerSelect($(cats[i]), reset, reset);
    }
    registerSelect($(".drivelpostonly"), reset, reset);
}

reset = function() {
    currentPage = 0;
    currentPostPostition = 0;
    posts = []
    getNextPosts(postsPerPage + 1);
    currentPageRendered = false;
}

clearPage = function() {
    body.empty();
}

changePage = function(dir) {
    currentPage += dir;
    currentPageRendered = false;
    if(dir > 0) {
	//need new posts from DB
	if(currentPostPostition < postsPerPage * (currentPage + 1)) {
	    getNextPosts(postsPerPage + 1);
	} else {
	    if(currentPostPostition == postsPerPage * (currentPage + 1)) {
		getNextPosts(1);
	    }
	}
    } else {
	togglePage($("#drivelbackward"), true);
	renderCurrentPage();
    }
    if(currentPage == 0) togglePage($("#drivelforward"), false);
    if(currentPage > 0) togglePage($("#drivelforward"), true);
}

renderCurrentPage = function() {
    clearPage();
    var index = currentPage * postsPerPage;
    var end =  Math.min((currentPage + 1) * postsPerPage, posts.length)
    if(end == 0) {
	body.append('<div class="drivelpost"><div class="emptypost">' +
		    '</br></br></br></br>Nothing with those categories yet' +
		    ':(</br>Maybe some day!</div></div>');
    }
    for(var i = index; i < end; ++i){
	body.append('<div class="drivelpost">' +
		    posts[i] +
		    '</div>');
	if(i < end - 1) {
	    body.append('<div class="drivelspace"></div>');
	}
    }
    runKatex();
    currentPageRendered = true;
}

runKatex = function() {
    renderMathInElement(
        document.body,
        {
            delimiters: [
                {left: "$$", right: "$$", display: true},
                {left: "$", right: "$", display: false},
            ]
        }
    );
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


//
// Quickpost
//
initQuickpost = function() {
    qparea = $("#qpcontent");
    qparea.val("Write something witty, would you?");
    qpaccess = $("#qpaccess");
    qpaccess.val("0");
    qpbut = $("#qpsubmitbutton");
    qpinittext = "Write something witty, would you?";

    qpBlock = false;
    qpClicked = false;

    qparea.on("focus", function(e) {
	showQP();
    });
    qpaccess.on("focus", function(e) {
	showQP();
    });
    qparea.on("focusout", function(e) {
	if(!qpBlock) hideQP();
    });
    qpaccess.on("focusout", function(e) {
	if(!qpBlock) hideQP();
    });
    qpbut.on("focusout", function(e) {
	if(!qpBlock) hideQP();
    });
    qpbut.on("mouseenter", function(e) {
	qpBlock = true;
    });
    qpbut.on("mouseout", function(e) {
	if(qpClicked) hideQP();
	qpClicked = false;
	qpBlock = false;
    });
    qpbut.on("click", function(e) {
	qpClicked = true;
    });
    registerButton(qpbut, submitQP);
}

submitQP = function() {
    if(qparea.val() == "" || !isInt(qpaccess.val())) {
	qpFlash(false);
	return;
    }
    var dataObj = {
	title: "",
	categories: "",
	content: qparea.val(),
	type: 2,
	access: qpaccess.val(),
	pid: 0,
	submitType: 0
    };
    $.ajax({
	type: "POST",
	url: "/edit/submit",
	dataType: "json",
	data: { dat: dataObj },
	success: function(data) {
	    if(data.substring(0, 3) == "yey") {
		qpFlash(true);
		qparea.val("");
		hideQP();
		reset();
	    } else {
		qpFlash(false);
	    }
	},
	error: function() { console.log("JSON error!"); }
    });
}

qpFlash = function(success) {
    var col = success ? "#aaffaa" : "#ffaaaa"
    qparea.effect("highlight", {color: col}, 1000);
}

showQP = function() {
    if(qparea.val() == qpinittext) qparea.val("");
    qparea.css("height", "40px");
    qparea.css("background-color", "white");
    qparea.css("font-style", "normal");
    qparea.css("color", "black");
    qpaccess.css("background-color", "white");
    qpaccess.css("color", "black");
    qpbut.css("display", "block");
}

hideQP = function() {
    qparea.css("height", "16px");
    qparea.css("background-color", "#ededed");
    qparea.css("color", "#aaaaaa");
    qpaccess.css("background-color", "#ededed");
    qpaccess.css("color", "#aaaaaa");
    qpbut.css("display", "none");
    if(qparea.val() == "" || qparea.val() == qpinittext) {
	qparea.val(qpinittext);
	qparea.css("font-style", "italic");
    }
}

isInt = function(value) {
    return !isNaN(value) &&
	parseInt(Number(value)) == value &&
	!isNaN(parseInt(value, 10));
}
