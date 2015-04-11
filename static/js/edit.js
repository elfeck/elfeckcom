$(function() {
    var tit = $("#edittitle");
    var cat = $("#editcategories");
    var cont = $("#editarea");
    var preview = $("#editpreview");

    submitFun = function() {
	var dataObj = {
	    title: tit.val(),
	    categories: cat.val(),
	    content: cont.val()
	};
	$.ajax({
	    type: "POST",
	    url: "/edit/preview",
	    dataType: "json",
	    data: {
		dat: dataObj
	    },
	    success: function(data) {
		console.log("Got json!");
		preview.html(data);
	    },
	    error: function() {
		console.log("nooo error");
	    }
	});
    };
    tit.on("keyup", submitFun);
    cat.on("keyup", submitFun);
    cont.on("keyup", submitFun);
});
