$(function() {
    title = $("#eTitle");
    title.on("keyup", submitPreview);

    categories = $("#eCategories");
    categories.on("keyup", submitPreview);

    area = $("#eArea");
    area.on("keyup", submitPreview);
    area.on("change", submitPreview);

    type = $("#eType");
    type.on("keyup", submitPreview);

    access = $("#eAccess");
    access.on("keyup", submitPreview);

    list = $("#eList");
    list.on("change", loadPost);

    preview = $("#ePreview");
    postid = $("#ePostid");
    postdate = $("#ePostdate");
    responsefield = $("#eResponsefield");

    deletefield = $("#eDeletefield");
    deletefield.on("keyup", function() {
	if(deletefield.val() == "DEL") {
	    deletefield.addClass("delDanger");
	} else {
	    deletefield.removeClass("delDanger");
	}
    });

    submitbutton = $("#eSubmitbutton"); registerButton(submitbutton, submit);

    clearAll();
    list.focus();
    loadPost();
});

submitPreview = function() {
    dataObj = packData();
    if(getSelectedId() == 0) {
	dataObj["type"] = 3;
	dataObj["access"] = 10;
	dataObj["pid"] = getSelectedId();
    }
    $.ajax({
	type: "POST",
	url: "/edit/preview",
	dataType: "json",
	data: { dat: dataObj },
	success: function(data) {
	    preview.html(data);
	    runKatex(preview[0]);
	},
	error: function() { jsonError("errorJson in submitPreview"); }
    });
};

submit = function() {
    if(!validateMinimal()) {
	respond("ney: misng info");
	return;
    }
    selId = getSelectedId();
    t = -1;
    if(selId == 0) t = 0;
    if(selId != 0) t = 1;
    if(selId != 0 && deletefield.val() == "DEL") t = 2;
    dataObj = packData();
    dataObj["pid"] = selId;
    dataObj["submitType"] = t;
    $.ajax({
	type: "POST",
	url: "/edit/submit",
	dataType: "json",
	data: { dat: dataObj },
	success: function(data) {
	    respond(data);
	    deletefield.val("");
	    deletefield.removeClass("delDanger");
	},
	error: function() { jsonError("errorJson in submit"); }
    });
}

loadPost = function() {
    var selId = getSelectedId();
    var dataObj = { pid: selId }
    $.ajax({
	type: "POST",
	url: "/edit/loadpost",
	dataType: "json",
	data: { dat: dataObj },
	success: function(data) {
	    postid.val(data[0]);
	    postdate.val(procTime(data[1]["crtDate"]));
	    area.val(data[1]["content"]);
	    title.val(procTitle(data[1]["title"]));
	    categories.val(procCat(data[1]["categories"]));
	    type.val(data[1]["ptype"]);
	    access.val(data[1]["access"]);
	    area.change();
	},
	error: function() { jsonError("errorJson in loadPost"); }
    });
}

packData = function() {
    var t = type.val();
    var a = access.val();
    if(t == "") t = "0";
    if(a == "") a = "5";
    var dataObj = {
	title: title.val(),
	categories: categories.val(),
	content: area.val(),
	type: t,
	access: a
    }
    return dataObj;
}

getSelectedId = function() {
    var selected = list.find("option:selected");
    return selected.attr("id");
}

validateMinimal = function() {
    return area.val() != "" && isInt(type.val()) && isInt(access.val());
}

isInt = function(value) {
    return !isNaN(value) &&
	parseInt(Number(value)) == value &&
	!isNaN(parseInt(value, 10));
}

procTime = function(time) {
    return time.replace("T", " ").substring(0, 16);
}

procTitle = function(title) {
    if(title == null) return "";
    else return title;
}

procCat = function(cats) {
    if(cats == null) return "";
    else return cats.join(", ");
}

jsonError = function(m) {
    console.log(m);
    respond("ney: error json");
}

respond = function(m) {
    if(m.substring(0, 3) == "yey") {
	responsefield.addClass("responseYey");
	responsefield.removeClass("responseNey");
    } else {
	responsefield.removeClass("responseYey");
	responsefield.addClass("responseNey");
    }
    console.log(m.substring(0, 3));
    responsefield.html(m);
}

clearAll = function() {
    title.val(""); categories.val(""); area.val("");
    type.val(""); access.val("");
    postid.val("");  postdate.val(""); responsefield.text("");
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
