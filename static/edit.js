$(function() {
    var ta = $("#editarea");
    ta.on("keyup", function() {
	var dataObj = {
	    content: ta.val()
	};
	$.ajax({
	    type: "POST",
	    url: "/edit/submit",
	    dataType: "json",
	    data: {
		o: dataObj
	    },
	    success: function(data) {
		console.log("yey success " + data);
	    },
	    error: function() {
		console.log("nooo error");
	    }
	});
    });
});
