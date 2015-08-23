$(function() {
    sidepanel = $("#drivelside");
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
    })
}

initSidepanel = function(categories) {
    for(var i = 0; i < categories.length; i++) {
	sidepanel.append('<div class="drivelop">' + categories[i] + '</div>');
    }
}
