window.onload = function() {
    var logoutlink = document.getElementById("logoutlink");
    if(!(logoutlink === undefined || logoutlink == null)) {
	logoutlink.onclick = clearLocal;
    }
};

clearLocal = function() {
    localStorage.removeItem("activeCats");
    localStorage.removeItem("postsOnly");
    localStorage.removeItem("currentPage");
    localStorage.removeItem("timestamp");
    return true;
}
