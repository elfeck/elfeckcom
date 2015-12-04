window.onload = function() {
  var logoutlink = document.getElementById("logoutlink");
  var hdl = document.getElementsByClassName("hdl1");
  if(!(logoutlink === undefined || logoutlink == null)) {
    logoutlink.onclick = clearLocal;
  }
  // dirty fix for index
  for(var i = 0; i < hdl.length; i++) {
    if(hdl[i].innerHTML == "Contact") {
      hdl[i].style.width = "430px";
    }
  }
};

clearLocal = function() {
  localStorage.removeItem("activeCats");
  localStorage.removeItem("postsOnly");
  localStorage.removeItem("currentPage");
  localStorage.removeItem("timestamp");
  return true;
}
