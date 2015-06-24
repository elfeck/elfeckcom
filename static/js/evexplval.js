validateAll = function() {
    if(!validateRegion(region)) return false;
    for(var i = 0; i < sites.length; i++) {
	if(!validateSite(sites[i])) return false;
	if(!validateType(types[i])) return false;
    }
    return true;
}

validateRegion = function(input) {
    if(allowedRegions.indexOf(input.val()) < 0) {
	input.addClass("valError");
	return false;
    } else {
	input.removeClass("valError");
	return true;
    }
};
validateRegionH = function() { validateRegion($(this)) };

validateSite = function(input) {
    var index = -1;
    for(var i = 0; i < sites.length; ++i) {
	if(input[0] == sites[i][0]) index = i;
    }
    bothEmpty = input.val() == "" && types[index].val() == "";
    if(!bothEmpty && allowedSites.indexOf(input.val()) < 0) {
	input.addClass("valError");
	return false;
    } else {
	input.removeClass("valError");
	return true;
    }
};
validateSiteH = function() { validateSite($(this)) };

validateType = function(input) {
    if(allowedTypes.indexOf(input.val()) < 0) {
	input.addClass("valError");
	return false;
    } else {
	input.removeClass("valError");
	return true;
    }
};
validateTypeH = function() { validateType($(this)) };
